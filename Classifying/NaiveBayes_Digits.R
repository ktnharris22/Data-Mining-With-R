#Katie Harris
#ASG 6

#SETUP
setwd("C:/Users/ktnharris22/Desktop/")
trainData <- read.table("zip.train",sep=" ")
trainData <- trainData[,-258]
testData <- read.table("zip.test", sep=" ")
testData <- testData[,-258]

#CREATE DATAFRAME FOR MEANS AND VARIANCES
params.means<- as.data.frame(matrix(0,10,256))
row.names(params.means)<-c(0:9)
colnames(params.means)<-c(1:256)
params.variances<- as.data.frame(matrix(0,10,256))
row.names(params.variances)<-c(0:9)
colnames(params.variances)<-c(1:256)

class.labels<-c(0:9)

#LEARN MEANS and VARIANCES for Each column of every data group
for(i in 1:10){
  d1<-subset(trainData,trainData[,1]==i-1)
  params.means[i,]<-colMeans(d1[,2:257])
  vr<-NULL
  for(j in 1:256){
    vari<-sum((d1[,j+1]-params.means[i,j])^2)/nrow(d1)
    vr<-c(vr,vari)
  }
  params.variances[i,]<-vr
}

#SET PRIORS
priors<-table(trainData[,1])/nrow(trainData)

#NAIVE BAYES FUNCTION
#pred<-NULL
naive.Bayes.Predictor<-function(vsample,means,vars,prs){
  probs<-NULL
  for(m in 1:10){
    p<-1
    for(n in 1:256){
      if(vars[m,n]==0){
        if(vsample[n]==means[m,n]){
          pr<-1
        }
        else{
          pr<-0
        }
      }else{
        pr<-dnorm(vsample[n],as.numeric(means[m,n]),as.numeric(sqrt(vars[m,n])))
      }
     p<-p*pr
    }
    p<-p*prs[m]
    probs<-c(probs,p)
  }
  pred<<-class.labels[which.max(probs)]
  #cat("row: ", i , "; prediction",pred,"\n")
  return(probs)
}

#RUN NAIVE BAYE'S on TESTDATA
test<-testData[,-1]
ncorrect<-0
contingencyTable<-matrix(0,10,10)

for(i in 1:nrow(test)){
  t<-test[i,]
  t<-as.vector(t,mode="numeric")
  naive.Bayes.Predictor(t,params.means,params.variances,priors)
  
  contingencyTable[testData[i,1]+1,pred+1]<-contingencyTable[testData[i,1]+1,pred+1]+1
  
  if(pred==testData[i,1]){
    ncorrect<-ncorrect+1
  }
}

#PRINTING
#1
cat("Learned Means: ","\n")
print(as.matrix(params.means))
cat("Learned Variances: ","\n")
print(as.matrix(params.variances))
cat("Priors: ","\n")
print(as.matrix(priors))
#2
accuracy<-ncorrect/nrow(test)
cat("ACCURACY: ",accuracy*100,'%','\n')
#3
colnames(contingencyTable)<-c(0:9)
rownames(contingencyTable)<-c(0:9)
print(as.table(contingencyTable))
#Although the algorithm generally does well. It's most common mistakes were predicting a 9 when
#it really was a 4, predicting an 8 when it really was a 3, and predicting a 8 when it really was 0.
#Mistaking 9 for a 4 was the most common by far which makes sense since these numbers look most alike.
#The curve on the top part being the biggest difference. 
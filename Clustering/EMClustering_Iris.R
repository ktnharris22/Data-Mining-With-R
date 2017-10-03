#Katie Harris
#ASG 5

setwd("C:/Users/ktnharris22/Desktop/")
data <- read.table("iris.txt",sep=",")
d<-as.matrix(data[,1:5])
labels<-data[,5]

m<-matrix(as.numeric(d[,1:4]),150,4)
k<-3
n<-nrow(m)

c1<-m[1:50,1:4]
c2<-m[51:100,1:4]
c3<-m[101:150,1:4]

#Initial Settings
mu<-(matrix((cbind(c1[1,],c2[1,],c3[1,])),4,k))
variances<-matrix(1,4,k) 
priors<-matrix(1/k,k,1)
w<-matrix(0,3,n)
t<-1
eDist<-1
#page 349
while(eDist>.001){
  pMeans<-mu
  #Expectation Step
  for(i in 1:k){
    for(j in 1:n){
        w[i,j]<-dnorm(m[j, 1],mu[1, i],sqrt(variances[1, i]))*
          dnorm(m[j, 2],mu[2, i],sqrt(variances[2, i]))*
          dnorm(m[j, 3],mu[3, i],sqrt(variances[3, i]))*
          dnorm(m[j, 4],mu[4, i],sqrt(variances[4, i]))*priors[i]
    }
  }
  
  w<-w/matrix(colSums(w),3,n,byrow=T)
  
  
  #Maximization Step
  for (i in 1:k) {
    mu[,i] <-(w[i,,drop=FALSE]%*%(m[,]))/as.numeric(w[i,,drop=FALSE]%*%matrix(1,nrow=n,ncol=1))
    Z.i <- (t(t(m)-mu[,i]))
    Z.i.2 <- Z.i^2
    variances[,i] <-(w[i,,drop=FALSE]%*%(Z.i.2))/as.numeric(w[i,,drop=FALSE]%*%matrix(1,nrow=n,ncol=1))
  }
  
  priors <- matrix(rowSums(w)/n,ncol=1)
  
  #Convergence Testing
  eDist <- NULL
  for (i in 1:k)
  {
    s <- NULL
    for (j in 1:4)
    {
      s <- c(s,((pMeans[j,i] - mu[j,i])^2))
    }
    s <- sum(s)
    eDist<-c(eDist,sqrt(s))
  }
  eDist <- sum(eDist)
  
  print(t)
  t<-t+1
}
#PRINT ANSWERS
c1<-NULL
c2<-NULL
c3<-NULL
for(i in 1:150){
  max<-0
  for(j in 1:3){
    if(w[j,i]>max){
      max<-w[j,i]
      val<-j
      r<-i
    }
  }
  if(val==1){
    c1<-c(r,c1)
  }
  else if(val==2){
    c2<-c(r,c2)
  }
  if(val==3){
    c3<-c(r,c3)
  }
}

print("CLUSTER 1")
for(i in c1){
  cat(as.character(labels[i]),"\n")
}
cat("\n")
print("CLUSTER 2")
for(i in c2){
  cat(as.character(labels[i]),"\n")
  
}
cat("\n")

print("CLUSTER 3")
for(i in c3){
  cat(as.character(labels[i]),"\n")
}


#Katie Harris
#ASG4 
q
setwd("C:/Users/ktnharris22/Desktop/")
d <- read.table("poisonous-mushroom.txt",sep=" ")
d<-d[,1:22]
d<-as.matrix(d)
minsup<-3000
v1<-1:117
tidSets<-list()

Pinit<-NULL
#find all x values which have t(x)>=minsup
for(a in v1){
  s<-sum(a==d)
  if(s>=minsup){
    Pinit<-c(Pinit,as.character(v1[a]))
  }
}

#create tidsets
for(i in Pinit){
  w<-which(d==as.numeric(i),arr.ind = T)
  tidSets[[i]]<-w[,1]
}


#page 227/228
fPattern<-NULL
AB<-NULL
Prefixes<-NULL
ECLAT <- function(P,minsup){
  for(a in 1:(length(P))){
    cat((P[a]),"\n",sep=" ")
      for(b in 2:length(P)){
        if(Greater(P[a],P[b])){
          Join(P[a],P[b])
          iSect<-intersect(unlist(tidSets[P[a]]),unlist(tidSets[P[b]]))
          supAB<-length(iSect)
          if(supAB>=minsup){
            tidSets[[AB]]<<-iSect
            Prefixes<-c(Prefixes,AB)
            cat(AB, "\n",sep=" ")
          }
        }
      }
    }
  if((length(Prefixes)>1)){
    ECLAT(Prefixes,minsup)
    Prefixes<-NULL
  }
}


Greater<-function(X,Y){
  if(is.na(X)| is.na(Y)){
    return(FALSE)
  }
  else{
    A<-strsplit(X," ")
    B<-strsplit(Y, " ")
    
    A<-unlist(A)
    B<-unlist(B)

    if((length(A)==1)&(length(B)==1)){
      bool<-(as.integer(A[length(A)])<as.integer(B[length(B)]))
    }
    else{
      
      Ashort<-A[1:(length(A)-1)]
      Bshort<-B[1:(length(B)-1)]
      bool<-((identical(Ashort,Bshort))&(A[length(A)]<B[length(B)]))
    }
    
    return(bool)  
  }
}

Join<-function(X,Y){
  B<-strsplit(Y, " ")
  B<-unlist(B)
  AB<<-paste(X,as.character(B[length(B)]),sep = " ")
  #cat("AB: ",AB, "\n", sep = " ")
}

#CALL of Funtion
ECLAT(Pinit,minsup)

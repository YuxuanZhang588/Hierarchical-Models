## ----echo=FALSE---------------------------------------------------------------
gdotplot<-function(y,g,xlab="group",ylab="response",mcol="blue",
                   ocol="lightblue",sortgroups=TRUE,...)
{
  m<-length(unique(g)) 
  rg<-rank( tapply(y,g,mean),ties.method="first")  
  if(sortgroups==FALSE){ rg<-1:m ; names(rg)<-unique(g)} 
  plot(c(1,m),range(y),type="n",xlab=xlab,ylab=ylab)

  for(j in unique(g))
  {
    yj<-y[g==j]
    rj<-rg[ match(as.character(j),names(rg)) ]
    nj<-length(yj)
    segments( rep(rj,nj) ,max(yj),rep(rj,nj),min(yj),col="gray")
    points( rep(rj,nj), yj,col=ocol,...)
    points(rj,mean(yj),pch=16,cex=1.5,col=mcol)
  }
}


## ----echo=FALSE---------------------------------------------------------------
set.seed(2) ; m<-7 ; tau<-1 ; n<-3 ; mu<-50
theta<-sort(mu + rnorm(m,0,tau))
Y<-list()
for(j in 1:m){ Y[[j]]<-rnorm(n,theta[j],1) }
ybar<-sapply(Y,mean)
that<-  ( ybar*n + mu/(tau^2) )/( n+1/tau^2 )


## ----echo=FALSE---------------------------------------------------------------
YBAR<-THAT<-NULL
for(s in 1:1000)
{
  Y<-list()
  for(j in 1:m){ Y[[j]]<-rnorm(n,theta[j],1) }
  ybar<-sapply(Y,mean)
  that<-  ( ybar*n + mu/(tau^2) )/( n+1/tau^2 )
  YBAR<-rbind(YBAR,ybar)
  THAT<-rbind(THAT,that)
}


## ----echo=FALSE---------------------------------------------------------------
sigma<-1
zq<-qnorm(.975) 
CIU<-outer(YBAR, c(-1,1)*zq*sigma/sqrt(n) ,"+") 
CIE<-outer(THAT, c(-1,1)*zq/sqrt(1/tau^2 + n/sigma^2) ,"+")   

CVU<-sweep(-CIU[,,1],2,theta,"+")>0&sweep(CIU[,,2],2,theta,"-")>0
CVE<-sweep(-CIE[,,1],2,theta,"+")>0&sweep(CIE[,,2],2,theta,"-")>0


## ----echo=FALSE,fig.height=4.5------------------------------------------------
s<-21
plot(c(1,m+.65),range(c(unlist(Y),theta+2.5,theta-2.5)),
     type="n",xlab="group",ylab="")
abline(h=mu,lty=2,col="gray")
points(1:m,theta,pch=16,col="orange",cex=3) 
segments(1:m-.05,CIU[s,,1],1:m-.05,CIU[s,,2],col="blue" ) 
segments(1:m+.05,CIE[s,,1],1:m+.05,CIE[s,,2],col="red" ) 


## ----echo=FALSE,fig.height=4.5------------------------------------------------
s<-22
plot(c(1,m+.65),range(c(unlist(Y),theta+2.5,theta-2.5)),
     type="n",xlab="group",ylab="")
abline(h=mu,lty=2,col="gray")
points(1:m,theta,pch=16,col="orange",cex=3) 
segments(1:m-.05,CIU[s,,1],1:m-.05,CIU[s,,2],col="blue" ) 
segments(1:m+.05,CIE[s,,1],1:m+.05,CIE[s,,2],col="red" ) 


## ----echo=FALSE,fig.height=4.5------------------------------------------------
s<-23
plot(c(1,m+.65),range(c(unlist(Y),theta+2.5,theta-2.5)),
     type="n",xlab="group",ylab="")
abline(h=mu,lty=2,col="gray")
points(1:m,theta,pch=16,col="orange",cex=3)
segments(1:m-.05,CIU[s,,1],1:m-.05,CIU[s,,2],col="blue" )
segments(1:m+.05,CIE[s,,1],1:m+.05,CIE[s,,2],col="red" )


## ----echo=FALSE,fig.height=4.5------------------------------------------------
s<-24
plot(c(1,m+.65),range(c(unlist(Y),theta+2.5,theta-2.5)),
     type="n",xlab="group",ylab="")
abline(h=mu,lty=2,col="gray")
points(1:m,theta,pch=16,col="orange",cex=3) 
segments(1:m-.05,CIU[s,,1],1:m-.05,CIU[s,,2],col="blue" ) 
segments(1:m+.05,CIE[s,,1],1:m+.05,CIE[s,,2],col="red" ) 


## ----echo=FALSE,fig.height=4.5------------------------------------------------
s<-25
plot(c(1,m+.65),range(c(unlist(Y),theta+2.5,theta-2.5)),
     type="n",xlab="group",ylab="")
abline(h=mu,lty=2,col="gray")
points(1:m,theta,pch=16,col="orange",cex=3)
segments(1:m-.05,CIU[s,,1],1:m-.05,CIU[s,,2],col="blue" )
segments(1:m+.05,CIE[s,,1],1:m+.05,CIE[s,,2],col="red" )


## ----echo=FALSE,fig.height=4.5------------------------------------------------
s<-26
plot(c(1,m+.65),range(c(unlist(Y),theta+2.5,theta-2.5)),
     type="n",xlab="group",ylab="")
abline(h=mu,lty=2,col="gray")
points(1:m,theta,pch=16,col="orange",cex=3)
segments(1:m-.05,CIU[s,,1],1:m-.05,CIU[s,,2],col="blue" )
segments(1:m+.05,CIE[s,,1],1:m+.05,CIE[s,,2],col="red" )


## ----echo=FALSE,fig.height=4.5------------------------------------------------
s<-27
plot(c(1,m+.65),range(c(unlist(Y),theta+2.5,theta-2.5)),
     type="n",xlab="group",ylab="")
abline(h=mu,lty=2,col="gray")
points(1:m,theta,pch=16,col="orange",cex=3)
segments(1:m-.05,CIU[s,,1],1:m-.05,CIU[s,,2],col="blue" )
segments(1:m+.05,CIE[s,,1],1:m+.05,CIE[s,,2],col="red" )


## ----echo=FALSE,fig.height=4.5------------------------------------------------
s<-28
plot(c(1,m+.65),range(c(unlist(Y),theta+2.5,theta-2.5)),
     type="n",xlab="group",ylab="")
abline(h=mu,lty=2,col="gray")
points(1:m,theta,pch=16,col="orange",cex=3) 
segments(1:m-.05,CIU[s,,1],1:m-.05,CIU[s,,2],col="blue" ) 
segments(1:m+.05,CIE[s,,1],1:m+.05,CIE[s,,2],col="red" ) 


## ----echo=FALSE,fig.height=4.5------------------------------------------------
s<-29
plot(c(1,m+.65),range(c(unlist(Y),theta+2.5,theta-2.5)),
     type="n",xlab="group",ylab="")
abline(h=mu,lty=2,col="gray")
points(1:m,theta,pch=16,col="orange",cex=3)
segments(1:m-.05,CIU[s,,1],1:m-.05,CIU[s,,2],col="blue" )
segments(1:m+.05,CIE[s,,1],1:m+.05,CIE[s,,2],col="red" )


## ----echo=FALSE,fig.height=4.5------------------------------------------------
s<-30
plot(c(1,m+.65),range(c(unlist(Y),theta+2.5,theta-2.5)),
     type="n",xlab="group",ylab="")
abline(h=mu,lty=2,col="gray")
points(1:m,theta,pch=16,col="orange",cex=3)
segments(1:m-.05,CIU[s,,1],1:m-.05,CIU[s,,2],col="blue" )
segments(1:m+.05,CIE[s,,1],1:m+.05,CIE[s,,2],col="red" )


## ----echo=FALSE,fig.height=4--------------------------------------------------
alpha<-.10
load("~/Dropbox/Talks/FabCI/R/radon_simres_alpha0.1")
par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))

pbcover<-bcover/s
plot(thetag[ng>1],pbcover[ng>1],xlab=expression(theta),
     ylab="predictive interval coverage",pch=16,
     col="black")

segments(thetag,pbcover+1.96*sqrt(pbcover*(1-pbcover)/s),
         thetag,pbcover-1.96*sqrt(pbcover*(1-pbcover)/s),col="gray" )
abline(h=1-alpha,lty=2,col="gray")


## ----echo=FALSE,fig.height=4--------------------------------------------------
alpha<-.10
load("~/Dropbox/Talks/FabCI/R/nels_simres_alpha0.1")
par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))

pbcover<-bcover/s
plot(thetag[ng>1],pbcover[ng>1],xlab=expression(theta),
     ylab="predictive interval coverage",pch=16,
     col="black")

segments(thetag,pbcover+1.96*sqrt(pbcover*(1-pbcover)/s),
         thetag,pbcover-1.96*sqrt(pbcover*(1-pbcover)/s),col="gray" )
abline(h=1-alpha,lty=2,col="gray")


## ----echo=FALSE,fig.height=3,fig.width=3,fig.align='center'-------------------
par(mar=c(0,0,0,0),mgp=c(0,0,0))
radon<-read.csv("SSRS.csv")
radium<-read.csv("radium.csv")
colnames(radium)[1]<-"stfips"

# GL states
radium<-radium[  radium$st %in% c("MN", "WI", "MI", "IN") ,]
xycounty<-cbind( radium$lon,radium$lat)

zradon<-nradon<-NULL
for(i in 1:nrow(radium))
{
  Z<-radon[ radon$stfips==radium$stfips[i] &
            radon$cntyfips==radium$ctfips[i] ,]
  z<-Z$activity
  z<-log( z/2 + sqrt(z ^ 2 / 4 + 0.25 ^ 2))
  zradon<-c(zradon, mean(z))
  nradon<-c(nradon,length(z))
}

iradon<-apply(outer(zradon,quantile(zradon,prob=c(.2,.4,.6,.8),na.rm=TRUE),">"),1,sum)/4
iradon<-iradon/4
miss<-which(is.na(iradon))
iradon[is.na(iradon)]<-0
cradon<-rgb( .25+iradon/1.0,.75-iradon/1.0,.25)
cradon[miss]<-"white"

plot(xycounty,pch=0,cex=1.25,lwd=2,col="gray",
     xlab="",ylab="",xaxt="n",yaxt="n",bty="n")
points(xycounty,col=cradon,pch=15,cex=1 )


## ----echo=-1------------------------------------------------------------------
radon<-readRDS(url("https://www2.stat.duke.edu/~pdh10/Teaching/610/Code/radonMN.rds"))  
y<-log(radon$radon)
g<-radon$county  

tapply(y,g,mean)[1:20]

table(g)[1:20]


## ----eval=FALSE---------------------------------------------------------------
## ## unbiased intervals
## fitLM<-lm(y ~ -1 + as.factor(g))
## uCI<-confint(fitLM)
## 
## ## EBayes intervals
## library(lme4)
## fitHM<-lmer(y ~ (1|g))
## blupInfo<-as.data.frame(ranef(fitHM,condVar=TRUE))
## bEst<-fixef(fitHM) + blupInfo[,4]
## bSE<-blupInfo[,5]
## bCI<-bEst + qnorm(.975)* outer( bSE ,c(-1,1))
## 
## 
## ## FAB intervals
## library(FABInference)
## fit<-lmFAB( y ~ -1,  model.matrix(~ -1+g) )
## fCI<-fit$FABci


## ----echo=FALSE,results='hide'------------------------------------------------
## unbiased intervals
fitLM<-lm(y ~ -1 + as.factor(g)) 
uCI<-confint(fitLM) 

## EBayes intervals
library(lme4) 
fitHM<-lmer(y ~ (1|g)) 
blupInfo<-as.data.frame(ranef(fitHM,condVar=TRUE)) 
bEst<-fixef(fitHM) + blupInfo[,4]  
bSE<-blupInfo[,5] 
bCI<-bEst + qnorm(.975)* outer( bSE ,c(-1,1)) 


## FAB intervals
library(FABInference) 
fit<-lmFAB( y ~ -1,  model.matrix(~ -1+g) )
fCI<-fit$FABci 


## ----echo=FALSE,fig.height=5--------------------------------------------------
ybar<-tapply(y,g,mean) 
ugroups<-unique(g) 
rgroups<-rank(ybar) 
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0)) 
plot(c(1,length(ugroups)),range(c(uCI,bCI,fCI)),type="n") 
for(i in 1:length(ugroups)){ 
  segments(rgroups[i]-.25,bCI[i,1],rgroups[i]-.25,bCI[i,2] ,col="blue")   
  segments(rgroups[i]+.25,fCI[i,1],rgroups[i]+.25,fCI[i,2] ,col="red")  
  segments(rgroups[i],uCI[i,1],rgroups[i],uCI[i,2] ,col="black")  
}
points(1:length(ugroups),sort(ybar),pch=4)
points(1:length(ugroups)-.25,bEst[order(ybar)] )


## ----echo=FALSE,fig.height=4.5------------------------------------------------
wU<-apply(uCI,1,diff) 
wB<-apply(bCI,1,diff) 
wF<-apply(fCI,1,diff) 
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0)) 
plot(wU,wF,col="red",xlab="unbiased width") ; abline(0,1 )
points(wU,wB,col="blue")


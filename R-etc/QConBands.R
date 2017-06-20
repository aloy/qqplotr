###########################################
# Quantile Confidence Bands               #
# Functions accompanying the Buja et al.  #
# TAS paper on TS-confidence bands        #
# be sure to give credit in the rd        #
###########################################

# required libraries
# library(mvtnorm)    # for the multivariate normal distribution
# library(robustbase) # for robust estimates for the mean and sd


###################################
# a few functions that are useful #
###################################

Qn.scale<-function(x){
Qn(x,finite.corr=FALSE)
}

Qn.location<-function(x){
s_Qn(x,mu.too=TRUE)[[1]]
}


out<-function(curve,upper,lower){
k<-length(which(curve>upper | curve<lower))
return(k>0)
}



###############################################################
# The confidence band function                                #
# The function returns a list with the following:             #
# 1. lower,upper      the normal scale confidence bands       #
# 2. individual.alpha the final individual significance level #
###############################################################

QQ.cb<-function(x.sample,mu=0,sigma=1,M.sim=1000,alpha=0.05,plot=TRUE){

n<-length(x.sample)
upper.ci<-rep(NA,n)
lower.ci<-rep(NA,n)
p.value <-matrix(NA,nrow=n,ncol=M.sim)
sim=NULL

# simulate data
for(i in 1:M.sim)
	sim<-cbind(sim,sort(runif(n)))

# widen the CI to get a simultanoues 1-alpha CI
for(i in 1:n){
tmp<-pbeta(sim[i,],shape1=i,shape2=n+1-i)
p.value[i,]<-apply(cbind(tmp,1-tmp),1,min)
}

critical.values<-apply(p.value,2,min)
C.crit<-quantile(critical.values,prob=alpha)

upper.ci<-qbeta(1-C.crit,shape1=1:n,shape2=n+1-(1:n))
lower.ci<-qbeta(C.crit,shape1=1:n,shape2=n+1-(1:n))


# now translate back to normal
norm.upper<-qnorm(upper.ci)
norm.lower<-qnorm(lower.ci)

if(plot==TRUE){
q.prob<-qnorm((1:n)/(n+1))
plot(q.prob,norm.upper,type="l",col="red",ylim=c(-3,3),xlim=c(-3,3),ylab="Sample Quantile",xlab="Sample Quantile")
lines(q.prob,norm.lower,col="red")
z.sample<-(x.sample-mu)/sigma
points(q.prob,sort(z.sample),pch=19,cex=0.6)
}

return(list(lower=norm.lower,upper=norm.upper))
}


###############################################################
# The confidence band function with unknown parameters        #
# The function returns a list with the following:             #
# 1. lower,upper      the normal scale confidence bands       #
# 2. individual.alpha the final individual significance level #
###############################################################


QQ.UNcb<-function(x.sample,M.sim=1000,alpha=0.05,plot=TRUE,center.func=Qn.location,scale.func=Qn.scale){


n<-length(x.sample)
upper.ci<-rep(NA,n)
lower.ci<-rep(NA,n)
p.value <-matrix(NA,nrow=n,ncol=M.sim)

sim=NULL
for(i in 1:M.sim)
	sim<-cbind(sim,sort(rnorm(n)))


# here is the difference... scale it -- so you are estimating the values
center<-apply(sim,2,center.func)
scale<-apply(sim,2,scale.func)
sim<-sweep(sweep(sim,2,center,FUN="-"),2,scale,FUN="/")


# convert the norm to beta
sim<-t(apply(sim,1,pnorm))

# widen the CI to get a simultanoues 1-alpha CI
for(i in 1:n){
tmp<-pbeta(sim[i,],shape1=i,shape2=n+1-i)
p.value[i,]<-apply(cbind(tmp,1-tmp),1,min)
}

critical.values<-apply(p.value,2,min)
C.crit<-quantile(critical.values,prob=alpha)

upper.ci<-qbeta(1-C.crit,shape1=1:n,shape2=n+1-(1:n))
lower.ci<-qbeta(C.crit,shape1=1:n,shape2=n+1-(1:n))


# now translate back to normal
norm.upper<-qnorm(upper.ci)
norm.lower<-qnorm(lower.ci)

if(plot==TRUE){
q.prob<-qnorm((1:n)/(n+1))
plot(q.prob,norm.upper,type="l",col="red",ylim=c(-3,3),xlim=c(-3,3),ylab="Sample Quantile",xlab="Sample Quantile")
lines(q.prob,norm.lower,col="red")
z.sample<-(x.sample-center.func(x.sample))/scale.func(x.sample)
points(q.prob,sort(z.sample),cex=0.6,pch=19)
}

return(list(lower=norm.lower,upper=norm.upper))
}


#############################
# Example                   #
#############################

# x<-rnorm(100)
# par(mfrow=c(2,1),mar=c(2,2,2,2))
# system.time(QQ.cb(x))
# 
# 
# x<-rt(100,df=100)
# par(mfrow=c(2,1),mar=c(2,2,2,2))
# QQ.cb(x,sigma=sqrt(100/(100-2)))
# QQ.UNcb(x)




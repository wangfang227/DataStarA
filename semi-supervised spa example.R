#########################
##Cora AI Example
########################
library("spa")
data("coraAI")
y<-coraAI$class
x<-coraAI$journals
g<-coraAI$cite
keep<-which(as.vector(apply(g,1,sum)>1))
setdiff(1:length(y),keep)
y<-y[keep]
x<-x[keep,]
g<-g[keep,keep]
set.seed(100)
n<-dim(x)[1]
Ns<-as.vector(apply(x,2,sum))
Ls<-sapply(1:length(Ns),function(i)sample(which(x[,i]==1),ceiling(0.035*Ns[i])))
L<-NULL
for(i in 1:length(Ns)) L<-c(L,Ls[[i]])
U<-setdiff(1:n,L)
ord<-c(L,U)
m<-length(L)
y1<-y
y1[U]<-NA
A1<-as.matrix(g)
gc<-spa(y1,graph=A1,control=spa.control(dissimilar=FALSE))
gc
tab=table(fitted(gc)[U]>0.5,y[U])
1-sum(diag(tab))/sum(tab)
sum(apply(A1[U,L],1,sum)==0)/(n-m)*100
pred<-update(gc,ynew=y1,gnew=A1,dat=list(k=length(U),l=Inf))
tab<-table(pred[U]>0.5,y[U])
1-sum(diag(tab))/sum(tab)
gc<-update(gc,ynew=y1,gnew=A1,dat=list(k=length(U),l=Inf),trans.update=TRUE)
gc
gjc<-spa(y1,x,g,control=spa.control(diss=FALSE))
tab<-table((fitted(gjc)>0.5)[U],y[U])
1-sum(diag(tab))/sum(tab)
gjc1<-update(gjc,ynew=y1,xnew=x,gnew=A1,dat=list(k=length(U),l=Inf),trans.update=TRUE)
gjc1
tab<-table((fitted(gjc1)>0.5)[U],y[U])
1-sum(diag(tab))/sum(tab)

#########################
##Linear Regression
########################
set.seed(100)
x<-matrix(rnorm(40),20) ; z=cbind(x,1)
y<-z%*%rnorm(3)+rnorm(20,,0.5)  ## Simulate model
H<-z%*%solve(t(z)%*%z,t(z))  ## Compute semi-supervised hat matrix
L<-1:5    ##Labeled data
U<-6:20   ##Unlabeled data
yh<-c(y[L],rep(0,15))  ##Initialize unlabeled data
ftfls<-function(i){  ## Define FTF(ls) (FTF least squares)
    .GlobalEnv$yh[L]=y[L]
    .GlobalEnv$yh=H%*%.GlobalEnv$yh
}
ftfres<-sapply(1:200,ftfls)  ## Fit FTF(ls) with i=1:200
(yh<-as.vector(yh)) ## Semi-supervised least squares fit
dat<-data.frame(y=y,x)  ##data for supervised lm call
supls<-predict(lm(y~.,data=dat,subset=L),newdata=dat)
apply((ftfres-supls)^2,2,sum)[c(1:2,199:200)]  ##Compute error/i=1:200

#########################
##Spa Package
########################
library("spa")
set.seed(100)  ## generate supervised
dat<-spa.sim(type="moon")
Dij<-as.matrix(daisy(dat[,-1]))
DNN<-knnGraph(Dij,dist=TRUE,k=5) ## optional
DFL<-floyd(DNN)  ## optional
L<-which(!is.na(dat$y))
U<-which(is.na(dat$y))
gsup<-spa(dat$y[L],graph=Dij[L,L])
gsemi<-spa(dat$y,graph=Dij)  
gsup
gsemi
gsemi$model$parm.est
newobs<-c(0,0)
newnode<-as.matrix(sqrt(apply(cbind(dat$x1-newobs[1],dat$x2-newobs[2])^2,1,sum)))
round(predict(gsemi,gnew=newnode),3)
newnodes<-sqrt(t(sapply(1:10-5,function(j)(dat$x1-4)^2+(dat$x2-j)^2)))
round(predict(gsemi,gnew=newnodes),3)
dat<-rbind(dat,spa.sim(100,0))
gsemi<-update(gsemi,ynew=dat$y,,as.matrix(daisy(dat[,-1])),trans.update=T)

#########################
##Swiss Example
########################
library("scatterplot3d")  ##Required for this example
n<-1000
set.seed(100)
z1<-runif(n,0,5*pi)
z2<-runif(n,6,10)

perf<-function(a){
	tab=table(a$f[U]>0.5,y[U])
	sum(diag(tab))/sum(tab)
}
x1<-z1*sin(z1)
x2<-z1*cos(z1)
x3<-z2-8

x<-cbind(x1,x3,x2)
xsq=x[,1]^2+x[,2]^2+x[,3]^2
y=rep(100,n)
inc=15
p1=  ceiling(max(xsq)/inc)
for(i in 0:p1)
y[xsq<inc*(i+1) & xsq>=i*inc]=i%%2

gr<-gray(c(.3,.7))[y+1]
scatterplot3d(x1,x3,x2,ylim=c(-8,8),color=gr,pch=16,cex.symbols=1,cex.axis=1.2,angle=30)
title("Simulated Swiss Roll",cex.main=2);title("\n\nTrue Classes")
## Make k=6 NN graph plot
g1<-knnGraph(x,k=6,weighted=FALSE)
pl<-scatterplot3d(x1,x3,x2,ylim=c(-8,8),color=gr,pch=16,
				  cex.symbols=1.2,type="n",cex.axis=1.2,
				  angle=45)
for(i in 1:n){
	ind<-which(g1[,i]>0)
	for(j in 1:length(ind)){
		pnts<-pl$xyz.convert(c(x[i,1],x[ind[j],1]),c(x[i,2],x[ind[j],2]),c(x[i,3],x[ind[j],3]))
		lines(pnts$x,pnts$y,col=1,lwd=1.5,lty=1)
	}
}
title("Simulated Swiss Roll",cex.main=2)
title("\n\nk=6 Nearest Neighbor Graph")

## One sample run
set.seed(100)
L=sample(1:n,ceiling(0.1*n))
U=setdiff(1:n,L)
y1=y
y1[U]=NA

g6<-knnGraph(x,k=6)
Dij<-floyd(g6)
gself<-spa(y1,graph=g6)
tab=table(fitted(gself)[U]>0.5,y[U]>0.5)
sum(diag(tab))/sum(tab)

#########################
##Protien Example
########################

data("protLoc")
y<-protLoc$class
gs<-protLoc$inter
gs

A<-gs[[1]]+gs[[2]]
dim(A)
str(A)
diag(A)<-13
resp<-as.data.frame(sapply(1:nlevels(y),function(i)as.numeric(y==levels(y)[i])))
names(resp)<-levels(y)
y<-resp[,20]
n<-length(y)

set.seed(100)
L<-sample(which(!is.na(y)),0.5*n)
tuneind<-setdiff(which(!is.na(y)),L)
uind<-which(is.na(y))
U<-c(tuneind,uind)
y1<-rep(0,n)
y1[L]<-y[L]
y1[U]<-NA

g<-spa(y1,graph=A,control=spa.control(diss=FALSE))
tab<-table(y[L],fitted(g)[L]>0.5);a1<-sum(diag(tab))/sum(tab)
tab<-table(y[U],fitted(g)[U]>0.5);a2<-sum(diag(tab))/sum(tab)
a3<-sum(apply(A[U,L],1,sum)==0)/(length(U))*100
cat("Labeled Acc.=",round(a1,4)," \nUnlabeled Acc.=",round(a2,4)," \n% zero A_ULx 1=",round(a3,4),"\n")


levs<-c(0,1,2,5,10,100,length(U))
lss<-c(seq(0,10,length=3),Inf)
tunes<-matrix(0,length(levs),length(lss))
t1<-proc.time()
for(j in 1:length(levs)){
	for(i in 1:length(lss)){
		g1<-update(g,ynew=y1,,gnew=A,dat=list(k=j,l=lss[i]))
		tab<-table(g1[U]>0.5,y[U])
		tunes[j,length(lss)-i+1]<-sum(diag(tab))/sum(tab)
		cat("j=",j," i=",i,"time=",(proc.time()-t1)/60,"\n")
	}
}
tlev<-unique(apply(tunes,1,which.max))
tls<-apply(tunes[,tlev],2,which.max)
i1<-which.max(diag(tunes[tls,tlev]))
olev<-tls[i1]
otls<-tlev[i1]

gprot<-update(g,ynew=y1,gnew=A,dat=list(k=olev,l=otls),trans.update=TRUE)
tab<-table(y[L],fitted(gprot)[L]>0.5);a1<-sum(diag(tab))/sum(tab)
tab<-table(y[U],fitted(gprot)[U]>0.5);a2<-sum(diag(tab))/sum(tab)
cat("Labeled Acc.=",round(a1,4)," \nUnlabeled Acc.=",round(a2,4),"\n")

fitted(gprot)[uind]

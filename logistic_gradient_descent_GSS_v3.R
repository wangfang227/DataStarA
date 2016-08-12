
library("lda")
library("lattice")
library("topicmodels")
library("XML")
library("tm")
library("slam")
library("SnowballC")
library("RTextTools")
library("ggplot2")

# read in dataset
d0=read.csv("C:\\Work\\gssdata\\OCCSLF-INDSLF.csv",header=TRUE,stringsAsFactors=FALSE)
dim(d0)
colnames(d0)

# random sort the dataset
set.seed(1) ## make reproducible here, but not if generating many random samples
rand <- sample(nrow(d0))
#rand
d0=d0[rand,]
dim(d0)
summary(d0$OCC10)

# delete the rows without a occupation code: 4820 rows to 4685 rows
d=d0[!is.na(d0$OCC10),]
dim(d)
colnames(d)

attach(d)

#table(OCC10)

d[10<=OCC10,"occ2"]=1
d[3600<=OCC10,"occ2"]=2
d[4700<=OCC10,"occ2"]=3
d[6000<=OCC10,"occ2"]=4
d[7700<=OCC10,"occ2"]=5

colnames(d)

table(d$occ2)
levels(d$occ2)

num.iterations <- 4685

attach(d)

d$mp=0
d[d$occ2==1,"mp"]=1 #management and professional
table(d$mp)

tr=d[1:1000,]
trmp=tr[tr$mp==1,]

#define the vocabulary using the training data
trmpcorpus <- Corpus(VectorSource(trmp$MAINSLF))

skipWords <- function(x) removeWords(x, stopwords("english"))
funcs <- list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
a <- tm_map(trmpcorpus, FUN = tm_reduce, tmFuns = funcs)
a.dtm1 <- TermDocumentMatrix(a, control = list(wordLengths = c(4,20))) 


N <- 5
find=findFreqTerms(a.dtm1, N)
find
lf=length(find)
lf

m <- as.matrix(a.dtm1)
v <- sort(rowSums(m), decreasing=TRUE)
head(v, lf)
summary(v)
wordrank=names(v)[1:lf]


# detect the words in the dictionary
corpus <- Corpus(VectorSource(d$MAINSLF))
ins=inspect(TermDocumentMatrix(corpus,control=list(dictionary = find)))
#       control=list(dictionary = c("care","children","health","help","make","manage","management","medical","patients","people","program","programs","research","school","students","take","taught","teach","work","worked"))))
dim(ins)
tins=t(ins)
dim(tins)
colnames(tins)
tins=tins[,wordrank]
colnames(tins)
#cov(tins)
colMeans(tins)

#train the logistic model
x <- tins
dim(x)
colnames(x)
y <- d$mp
length(y)
table(y)
yx=cbind(as.numeric(y),x)
rate0=colMeans(yx[yx[,1]==0,2:dim(yx)[2]])
rate0
rate1=colMeans(yx[yx[,1]==1,2:dim(yx)[2]])
rate1

id=c(1:82)
plot(rate0,pch=16, col="green",xlab="words", ylab="rates of using the word", main="Rates")
#abline(lm(rate0~id), col="green")
points(rate1,pch=16, col="red")

plot (c(1,82),c(0,0.07),type="n", # sets the x and y axes scales,
      pch=16,      
      xlab="Words",ylab="Rates", main="Word using rates") # adds titles to the axes

points(1:82,rate0,col="green",lwd=2.5,pch=16) # adds a line for defense expenditures 

points(1:82,rate1,col="red",lwd=2.5, pch=16) # adds a line for health expenditures 

legend(50,0.06,c("Non-managment","Management"),lty=c(1,1), # gives the legend appropriate symbols (lines)
       
       lwd=c(2.5,2.5),col=c("green","red")) # gives the legend lines the correct color and width


# Function to standardize input values
zscore <- function(x, mean.val=NA) {
	if(is.matrix(x)) return(apply(x, 2, zscore, mean.val=mean.val))
	if(is.data.frame(x)) return(data.frame(apply(x, 2, zscore, mean.val=mean.val)))
	if(is.na(mean.val)) mean.val <- mean(x)
	sd.val <- sd(x)
	if(all(sd.val == 0)) return(x) # if all the values are the same
	(x - mean.val) / sd.val 
}

# Standardize the features
x.scaled <- zscore(x)

# Gradient descent function
grad <- function(x, y, theta) {
  gradient <- t(x) %*% (y-1/(1 + exp(-x %*% t(theta))))
  return(t(gradient))
}

gradient.descent <- function(x, y, alpha=0.1, num.iterations=num.iterations, threshold=1e-5, output.path=FALSE) {
  
    # Add x_0 = 1 as the first column
    m <- if(is.vector(x)) length(x) else nrow(x)
    if(is.vector(x) || (!all(x[,1] == 1))) x <- cbind(rep(1, m), x)
    if(is.vector(y)) y <- matrix(y)
    x <- apply(x, 2, as.numeric)

    num.features <- ncol(x)
    
    # Initialize the parameters
    theta <- matrix(rep(0, num.features), nrow=1)

    # Look at the values over each iteration
    theta.path <- theta
    for (i in 1:num.iterations) {
      theta <- theta + alpha * grad(x, y, theta)
      if(all(is.na(theta))) break
      theta.path <- rbind(theta.path, theta)
      if(i > 2) if(all(abs(theta - theta.path[i-1,]) < threshold)) break 
    }
    
    if(output.path) return(theta.path) else return(theta.path[nrow(theta.path),])
}

summary(glm(y ~ x, family = binomial, data=tr))

unscaled.theta <- gradient.descent(x=x, y=y, num.iterations=num.iterations, output.path=TRUE)
scaled.theta <- gradient.descent(x=x.scaled, y=y, num.iterations=num.iterations, output.path=TRUE)

dim(x)
rowSums(scaled.theta)# the first row is all 0, should use the secnd to the num.iterations rows
dim(unscaled.theta)
dim(scaled.theta)
unscaled.theta[num.iterations,]
scaled.theta[num.iterations,]

#summary(glm(y ~ x, family = binomial, data=te))

qplot(1:(nrow(scaled.theta)), scaled.theta[,1], geom=c("line"), xlab="iteration", ylab="theta_1")
qplot(1:(nrow(scaled.theta)), scaled.theta[,2], geom=c("line"), xlab="iteration", ylab="theta_2")
qplot(1:(nrow(scaled.theta)), scaled.theta[,dim(x)[2]], geom=c("line"), xlab="iteration", ylab="theta_87")


#qplot(1:(nrow(scaled.theta)), scaled.theta, geom=c("line"), xlab="iteration", ylab="theta_1")

# Look at output for various different alpha values
vary.alpha <- lapply(c(1e-12, 1e-7, 1e-3, 0.1,0.5, 0.9), function(alpha) gradient.descent(x=x.scaled, y=y, alpha=alpha, num.iterations=num.iterations, output.path=TRUE))

par(mfrow = c(2, 3))
for (j in 1:6) {
  plot(vary.alpha[[j]][,2], ylab="area (alpha=1e-9)", xlab="iteration", type="l")
}


error_rate <- function(x,theta){
  
#make predictions and calculate error rate
cx=cbind(rep(1,dim(x)[1]),x)
dim(cx)
dim(theta)

exp(cx%*%t(theta[2:(num.iterations+1),]))

p=exp(cx%*%t(theta[2:(num.iterations+1),]))/(1+exp(cx%*%t(theta[2:(num.iterations+1),])))
summary(p)
dim(p)

prediction=matrix(0,dim(x)[1],num.iterations)
dim(prediction)

for (i in 1:num.iterations){
prediction[,i]=(p[,i]>0.5)}
summary(prediction[,1])
summary(prediction[,num.iterations])

error_rate=rep(0,num.iterations)
for(i in 1:num.iterations){
error_rate[i]=sum(abs(d$mp-prediction[,i]))/length(d$mp)}

summary(error_rate)
error_rate[1000]
return(error_rate)
qplot(1:(length(error_rate)), error_rate, geom=c("line"), xlab="iteration", ylab="error rate")
}

unscaled_error=error_rate(x=x, theta=unscaled.theta)
scaled_error=error_rate(x=x.scaled, theta=scaled.theta)

qplot(1:(length(unscaled_error)), unscaled_error, geom=c("line"), xlab="iteration", ylab="error rate",main="unscaled theta")
summary(unscaled_error)

qplot(1:(length(scaled_error)), scaled_error, geom=c("line"), xlab="iteration", ylab="error rate", main="scaled theta")
summary(scaled_error)

summary(vary.alpha)
summary(vary.alpha[1])















############################## read in the data point one by one #######################################

num.iterations

# Gradient descent function
grad <- function(x, y, theta) {
  gradient <- (1 / num.iterations) * x * as.numeric((1/(1 + exp(-x %*% t(theta))) - y))
  return(gradient)
}

gradient.descent <- function(x, y, alpha=1e-3, num.iterations=num.iterations, threshold=1e-16, output.path=FALSE) {
  
  # Add x_0 = 1 as the first column
  m <- if(is.vector(x)) length(x) else nrow(x)
  if(is.vector(x) || (!all(x[,1] == 1))) x <- cbind(rep(1, m), x)
  if(is.vector(y)) y <- matrix(y)
  x <- apply(x, 2, as.numeric)
  
  num.features <- ncol(x)
  
  # Initialize the parameters
  theta <- matrix(rep(0, num.features), nrow=1)
  
  # Look at the values over each iteration
  theta.path <- theta
  for (i in 1:num.iterations) {
    theta <- theta - alpha * grad(x[i,], y[i], theta)
    if(all(is.na(theta))) break
    theta.path <- rbind(theta.path, theta)
    if(i > 2) if(all(abs(theta - theta.path[i-1,]) < threshold)) break 
  }
  
  if(output.path) return(theta.path) else return(theta.path[nrow(theta.path),])
}

summary(glm(y ~ x, family = binomial, data=tr))

unscaled.theta <- gradient.descent(x=x, y=y, num.iterations=num.iterations, output.path=TRUE)
scaled.theta <- gradient.descent(x=x.scaled, y=y, num.iterations=num.iterations, output.path=TRUE)

dim(x)
rowSums(scaled.theta)# the first row is all 0, should use the secnd to the num.iterations rows
dim(unscaled.theta)
dim(scaled.theta)
unscaled.theta[num.iterations,]
scaled.theta[num.iterations,]

#summary(glm(y ~ x, family = binomial, data=te))

qplot(1:(nrow(scaled.theta)), scaled.theta[,1], geom=c("line"), xlab="iteration", ylab="theta_1")
qplot(1:(nrow(scaled.theta)), scaled.theta[,2], geom=c("line"), xlab="iteration", ylab="theta_2")
qplot(1:(nrow(scaled.theta)), scaled.theta[,dim(x)[2]], geom=c("line"), xlab="iteration", ylab="theta_87")


#qplot(1:(nrow(scaled.theta)), scaled.theta, geom=c("line"), xlab="iteration", ylab="theta_1")

# Look at output for various different alpha values
vary.alpha <- lapply(c(1e-5, 1e-4, 1e-3, 1e-2, 0.1, 0.5), function(alpha) gradient.descent(x=x.scaled, y=y, alpha=alpha, num.iterations=num.iterations, output.path=TRUE))

par(mfrow = c(2, 3))
for (j in 1:6) {
  plot(vary.alpha[[j]][,2], ylab="area (alpha=1e-9)", xlab="iteration", type="l")
}

dim(vary.alpha[[4]])

dim(unscaled.theta)

theta=unscaled.theta
#x0=x
#x=x0

theta=vary.alpha[[1]]


cal_error_rate <- function(x,theta){

  #make predictions and calculate error rate
  cx=cbind(rep(1,dim(x)[1]),x)
  dim(cx)
  dim(theta)
  dimtheta=dim(theta)[1]
  if (dimtheta>num.iterations) dimtheta=num.iterations
  
  #exp(cx%*%t(theta[1:dimtheta,])) # last iteration is omitted
  
  p=exp(cx%*%t(theta[1:dimtheta,]))/(1+exp(cx%*%t(theta[1:dimtheta,])))
  summary(p)
  dim(p)
  
  prediction=matrix(0,dim(x)[1],num.iterations)
  dim(prediction)
  
  for (i in 1:dimtheta){
    prediction[,i]=(p[,i]>0.5)}
  summary(prediction[,1])
  summary(prediction[,num.iterations])
  
  error_rate=rep(0,num.iterations)
  for(i in 1:num.iterations){
    error_rate[i]=sum(abs(d$mp-prediction[,i]))/length(d$mp)}
  
  #summary(error_rate)
  error_rate[1000]
  return(error_rate)
  qplot(1:(length(error_rate)), error_rate, geom=c("line"), xlab="iteration", ylab="error rate")
  #lines(1:(length(error_rate)), error_rate,col=j,lwd=2.5,pch=16)
}

cal_error_rate(x=x.scaled, theta=vary.alpha[[1]])

# all use scaled theta 
errormatrix=matrix(0,6,num.iterations)
dim(errormatrix)
#par(mfrow = c(2, 3))
#for (j in 1:6) {
#errormatrix[j,]=error_rate(x=x.scaled, theta=vary.alpha[[j]])
#}

par(mfrow = c(2, 3))
for (j in 1:6) {
plot(1:(length(scaled_error)),errormatrix[j,], col=j,lty=5,ylab="error rate", 
     xlab="number of training cases")
}

par(mfrow = c(1,1))
plot(c(1,num.iterations), c(0.25,0.4),ylab="error rate", xlab="number of training cases")
#axis(1, at = seq(0, 5000, by = 200), las=2)
axis(side = 1, at = x,labels = T)
for (j in 1:6) {
lines(1:(length(scaled_error)),errormatrix[j,], col=j)
}

minindex=matrix(0,6,1)
for (i in 1:6){
minindex[i]=which(errormatrix[i,]==min(errormatrix[i,]))}
minindex

par(mfrow = c(2, 3))
for (j in 1:6) {
  plot(vary.alpha[[j]][,2], ylab="area (alpha=1e-9)", xlab="iteration", type="l")
}

for (j in 1:6) {summary(errormatrix[j,])}
summary(errormatrix[1,])
summary(errormatrix[2,])
summary(errormatrix[3,])
summary(errormatrix[4,])
summary(errormatrix[5,])
summary(errormatrix[6,])

unscaled_error=error_rate(x=x, theta=unscaled.theta)
scaled_error=error_rate(x=x.scaled, theta=scaled.theta)

qplot(1:(length(unscaled_error)), unscaled_error, geom=c("line"), xlab="iteration", ylab="error rate",main="unscaled theta")
summary(unscaled_error)

qplot(1:(length(scaled_error)), scaled_error, geom=c("line"), xlab="iteration", ylab="error rate", main="scaled theta")
summary(scaled_error)

summary(vary.alpha)
summary(vary.alpha[1])





################### K mean clustering of occupation

clustering <- function(k){

corpus <- Corpus(VectorSource(d[,"occ"]))
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeNumbers)
#corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus, content_transformer(tolower))
corput=tm_map(corpus,stripWhitespace)
corpus=tm_map(corpus,removeWords,c(stopwords("english"),"freedoms","freedom","of","right","rights","free"))
corpus=tm_map(corpus,stemDocument)

summary(corpus)

Sys.setlocale("LC_COLLATE", "C")

d_dtm0 <- DocumentTermMatrix(corpus, 
   control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
     removeNumbers = TRUE, removePunctuation = TRUE))

freqwords=findFreqTerms(d_dtm0,lowfreq=5,highfreq=100)
#freqwords
#colnames(d_dtm0)
length(freqwords)

#x=d_dtm0[,freqwords], use frequent words is not a good choice
x=d_dtm0

set.seed(k)
#speech, religion, vote, arm, other
fit=kmeans(x, k)
#fit$cluster
#table(fit$cluster)
pred=fit$cluster
table(pred)

length(fit$cluster)
return(fit$cluster)
}

# cluster Q5, 6 seems to be the best number of clustering

occ_cluster=clustering(5)
plot(d_cluster$occ_cluster,d$occ2)
table(occ_cluster)
d_cluster=cbind(d[,c("SU_ID","OCCSLF","occ2")],occ_cluster)
tb=table(d_cluster$occ2, d_cluster$occ_cluster)
#t.test(d_cluster$occ2, d_cluster$occ_cluster)
chisq.test(d_cluster$occ2, d_cluster$occ_cluster)
chisq.test(tb)
#the two test are the same




############## function to find the frequent words in each category
#q5clustering: the derived cluster variable from above
#Q5: the texts
#k: how many clusters, k-mean clustering
#n: words having frequency >=n will be returned as frequent/descriptive words

freqwordsk <-function(occ_cluster,OCCSLF,k,n){
vall=list()
for (i in 1:k){
d_clusterk=d_cluster[d_cluster$occ_cluster==i,]
dim(d_clusterk)
names(d_clusterk)
corpus <- Corpus(VectorSource(d_clusterk[,"OCCSLF"]))
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeNumbers)
#corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus, content_transformer(tolower))
corput=tm_map(corpus,stripWhitespace)
corpus=tm_map(corpus,removeWords,c(stopwords("english")))
corpus=tm_map(corpus,stemDocument)

summary(corpus)

d_dtmk <- DocumentTermMatrix(corpus, 
   control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
     removeNumbers = TRUE, removePunctuation = TRUE))

set.seed(k)
freqwordsk=findFreqTerms(d_dtmk,lowfreq=n,highfreq=500)

v=colSums(as.matrix(d_dtmk))
v=sort(v,decreasing=TRUE)

vall[[i]]=list(freqwordsk,v)}

return(vall)

}
# frequent words of q5
freqwordsk(occ_cluster,OCCSLF,5,10)
#clustering doesn't work





################ LDAVis

options(stringsAsFactors = FALSE)
reviews=OCCSLF
reviews=as.character(reviews)
is.factor(reviews)
length(reviews)
names(reviews)=d[,"SU_ID"]

library(mallet)

setwd("C:\\Work\\gssdata\\LDAvis")

download.file("http://jmlr.org/papers/volume5/lewis04a/a11-smart-stop-list/english.stop", "stopwords.txt")

########## generate the function using different number of topics

ldatopic <-function(nt){

instance <- mallet.import(names(reviews), reviews, "stopwords.txt")
model <- MalletLDA(num.topics = nt)
model$loadDocuments(instance)
freqs <- mallet.word.freqs(model)

stopwords <- as.character(subset(freqs, term.freq < 1)$words)
writeLines(c(readLines("stopwords.txt"), stopwords, "of", "freedom","freedoms","right","rights"),  "stopwords2.txt")
instance2 <- mallet.import(names(reviews), reviews, "stopwords2.txt")

set.seed(1234)
model2 <- MalletLDA(num.topics = nt)
set.seed(1234)
model2$loadDocuments(instance2)
set.seed(1234)
freqs2 <- mallet.word.freqs(model2)

# this takes about 4.5 minutes on a macbook pro laptop with 2GB RAM and a 2.26GHz processor 
set.seed(1234)
model2$train(4685)
set.seed(1234)
phi <- t(mallet.topic.words(model2, smoothed = TRUE, normalized = TRUE))
phi.count <- t(mallet.topic.words(model2, smoothed = TRUE, normalized = FALSE))
topic.words <- mallet.topic.words(model2, smoothed = TRUE, normalized = FALSE)
topic.counts <- rowSums(topic.words)
topic.proportions <- topic.counts/sum(topic.counts)
vocab <- model2$getVocabulary()


# LDAvis can be installed from GitHub via `devtools::install_github("cpsievert/LDAvis")`
library(LDAvis)
set.seed(1234)
out <- check.inputs(K = nt, W = length(vocab), phi = phi, 
                    term.frequency = apply(phi.count, 1, sum), 
                    vocab = vocab, topic.proportion = topic.proportions)
# Relabel topics so that topics are numbered in decreasing order of frequency.
set.seed(1234)
colnames(out$phi) <- seq_len(out$K)

json <- with(out, createJSON(K = nt, phi, term.frequency, 
                   vocab, topic.proportion))

dirname=paste('GSS',nt,sep='_')
dirname

serVis(json, out.dir = dirname, open.browser =FALSE)
}


ldatopic(4)
#open html
out.dir="C:\\Work\\gssdata\\LDAvis\\GSS_4"
servd <- suppressMessages(suppressWarnings(require('servr')))
httd(dir = out.dir)

ldatopic(6)
#open html
out.dir="C:\\Work\\gssdata\\LDAvis\\GSS_6"
servd <- suppressMessages(suppressWarnings(require('servr')))
httd(dir = out.dir)

ldatopic(10)
#open html
out.dir="C:\\Work\\gssdata\\LDAvis\\GSS_10"
servd <- suppressMessages(suppressWarnings(require('servr')))
httd(dir = out.dir)

set.seed(1234)
ldatopic(25)
#open html
out.dir="C:\\Work\\gssdata\\LDAvis\\GSS_25"
servd <- suppressMessages(suppressWarnings(require('servr')))
httd(dir = out.dir)

ldatopic(100)
#open html
out.dir="C:\\Work\\gssdata\\LDAvis\\GSS_100"
servd <- suppressMessages(suppressWarnings(require('servr')))
httd(dir = out.dir)















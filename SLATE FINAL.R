## this project is to predict the occupation of respondents 
## according to their verbatim description about what they do for livign

#topic models
library(topicmodels) #fit topic models
library(lattice) #improve R graph
library(tm)

#read in html files
library(RCurl)
library(XML)

# supervised SVM model
library("lda")
library("lattice")
library("topicmodels")
library("XML")
library("tm")
library("slam")
library("Snowball")
library("RTextTools")
library("ggplot2")

#semi supervised model
library(spa)

#lasso supervised model
library(glmnet)
library(randomForest)

install.packages("RTextTools")
install.packages("snowball")
install.packages("bitops")

d=read.csv("D:\\Machine_learning\\GSS\\OCCSLF-INDSLF.csv",header=TRUE)

dim(d)
names(d)
str(d)
#attributes(d)
d[1:5,]
d=d[!is.na(d$INDUS10),]
#table(d$OCC10)
#table(d$INDUS10)
dim(d)
d[10<=d$OCC10,"occ2"]=1
d[3600<=d$OCC10,"occ2"]=2
d[4700<=d$OCC10,"occ2"]=3
d[6000<=d$OCC10,"occ2"]=4
d[7700<=d$OCC10,"occ2"]=5
attach(d)

var(d$OCC10)
cov(d$OCC10, as.numeric(d$INDUS10))
cor(d$OCC10, as.numeric(d$INDUS10))
l1=lm(d$OCC10~as.numeric(d$INDUS10))
summary(l1)

par(mfrow=c(2,2))
plot(density(as.numeric(d$OCC10)))
plot(density(as.numeric(d$INDUS10)))
plot(ecdf(as.numeric(OCC10)))
plot(ecdf(as.numeric(INDUS10)))

y=rnorm(length(OCC10),0,1)
qqplot(y,OCC10)
qqnorm(OCC10)#the two yields the same plots
qqplot(y,INDUS10)
qqnorm(INDUS10)
lines(y~y)

table(d$OCC10)
plot(OCC10, INDUS10)
hist(OCC10,prob=T)
lines(density(OCC10))
d1=d[,c("OCC10","INDUS10")]
pairs(d1)

########################################## simple data exploration ended

########################################## read in the html in to plain text 

###################### 2
# load packages
#install.packages("RCurl")
#install.packages("XML")
#install.packages("bitops")
library(RCurl)
library(XML)

d0=read.csv("C:\\Work\\gssdata\\ds_training.csv",header=T)
d0[1,]
colnames(d0)

length(d0$url)

#################1 working: this one is the final used method

remove_HTML_markup <- function(s) {
  doc <- htmlTreeParse(s, asText = TRUE, trim = FALSE)
  xmlValue(xmlRoot(doc))
}

# parse html
#plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
#cat(paste(plain.text, collapse = " "))
#strsplit(test,"\r\n\t\t\t\t\t\t\t")

mylist=list()

dim(d0)

url=d0[!is.na(d0$url),"url"]
length(url)


#get the training data
d0=d0[d0$categorization!='',1:13]
dim(d0)

#randomly sample 100 cases
selected=sample(1:dim(d0)[1],100)
selected

d=d0[selected,1:13]
dim(d)


for (i in 1:100){  
  
  # download html
  html <- getURL(d$url[i], followlocation = TRUE,ssl.verifypeer = FALSE)
  
  # parse html
  doc = htmlParse(html, asText=TRUE)
  plain.text <- xpathSApply(doc, "//p", xmlValue)
  #cat(paste(plain.text, collapse = "\n"))
  test=paste(plain.text, collapse = " ")
  mylist[[i]]=test
  
}

names(mylist)
str(mylist)

mylist=as.vector(mylist)
length(mylist)
mylist[100]

cats=d[,"categorization"]
length(cats)
table(cats[1:100])

mylist2=cbind(cats[1:100],mylist)
dim(mylist2)
colnames(mylist2)=c("cats","paper")
#mylist3=data.frame(mylist2)

#install.packages("SnowballC")
library(SnowballC)

corpus=Corpus(VectorSource(mylist2[,"paper"]))
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeNumbers)
corpus=tm_map(corpus,tolower)
corput=tm_map(corpus,stripWhitespace)
corpus=tm_map(corpus,removeWords,stopwords("english"))
corpus=tm_map(corpus,stemDocument)
inspect(corpus[1])


dm=TermDocumentMatrix(corpus,control=list(stemming=TRUE, stopwords="english",removeNumbers=TRUE,
                                          removePunchtuation=TRUE,minWordLength=3))

dim(dm)

#install.packages("wordcloud")
library(wordcloud)
#visulization
v=rowSums(as.matrix(dm))
sort(v,decreasing=TRUE)
#wordcloud(names(v),v,minfreq=80,maxfreq=100)


#most frequent words and words association
freqwords=findFreqTerms(dm,lowfreq=10,highfreq=80)
freqwords
rownames(dm)
murderassociate=findAssocs(dm,'murder',0.3)#choose crime from the word cloud
cor(as.vector(dm["murder",]),as.vector(dm["wound",]))
murderwords=rownames(murderassociate)
murderwords
length(murderwords)

#topic models

?TermDocumentMatrix

attributes(dm)
table(dm$i)
table(dm$j)

#### Y: murder or non-murder, x: freq words
dm_matrix=as.matrix(dm)
dim(dm_matrix)
colnames(dm_matrix)
allwords=rownames(dm_matrix)#all the 4519 words

dm_matrix=t(dm_matrix)
dim(dm_matrix)

#use the demongraphic variables and the words in [10,60]
#"sample","date","gender","age","ageGroup","city","state","lat","lng" has missing, can't be used
x=as.matrix(cbind(d[1:100,c("gender")],dm_matrix[,freqwords]))
dim(x)#turn to 666 variables

# use all the words, and leave everything to lasso
x=as.matrix(cbind(d[1:100,c("gender")],dm_matrix[,allwords]))
dim(x)#turn to 666 variables

#fit a lasso model, use freq words
#install.packages("glmnet")
library(glmnet)

#murder or non-murder
y0=d[1:100,"categorization"]
y0=data.frame(y0)
y0$cats=0
y0[y0[,1]=="murder","cats"]=1
table(y0$cats)
y=as.matrix(y0$cats)


n=80

summary(x)
#use 90 as training
l1=glmnet(x[1:n,],y[1:n],family="binomial")
p1=predict(l1,x[(n+1):100,],type="response",s=0.01)
summary(l1)
summary(p1)
c1=(p1>0.5)
c1[which(c1==TRUE)]=1
c1[which(c1!=TRUE)]=0
table(c1)
which(c1==0)

#compare with the real y
test=data.frame(cbind(c1,y[(n+1):100]))
table(test$X1,test$V2)
test$score=0
test[test$X1==test$V2,"score"]=1
mean(test$score)
# 65% accurate using freq words
# 70% use all the words

#6 level multiple nominal logistic
dim(d)
y0=d[1:100,"categorization"]
y0=data.frame(y0)
table(y0)
y=as.matrix(y0)

#y changed, x kept to be murder associated words
dim(x);dim(y)

#cv.glmnet(x[1:n,],y[1:n],family="multinomial")#lambda.min=0.1
l2=glmnet(x[1:n,],y[1:n],family="multinomial")

p2=predict(l2,x[(n+1):100,],type="response",s=0.01)
#summary(l2)
#summary(p2)
p2=data.frame(p2)
p2

maxindex=NULL
for (i in 1:20){
  maxindex[i]=colnames(p2)[which.max(p2[i,])]}
table(maxindex)

c4=gsub(".1","",maxindex)
table(c4)

#compare with the real y
test=data.frame(cbind(c4,y[81:100]))
test
table(test$c4)
table(test$c4,test$V2)
#70% use freq words
#75% use all the words: it's better to use all the words, and leave everything to lasso


#fit a lasso model, use murder associated words; 
#y:murder or non-murder, x: murder assocaited words

dim(dm_matrix)

#"sample","date","gender","age","ageGroup","city","state","lat","lng" has missing, can't be used
x=as.matrix(cbind(d[1:100,c("gender")],dm_matrix[,murderwords]))
dim(x)#turn to 192 variables

dim(x);dim(y)
l2=glmnet(x[1:n,],y[1:n],family="binomial")
p2=predict(l2,x[(n+1):100,],type="response",s=0.01)
summary(l2)
summary(p2)
c2=(p2>0.5)
c2[which(c2==TRUE)]=1
c2[which(c2!=TRUE)]=0
table(c2)
which(c2==0)

#compare with the real y
test=data.frame(cbind(c2,y[(n+1):100]))
test
table(test$X1,test$V2)
test$score=0
test[test$X1==test$V2,"score"]=1
table(test$score)
mean(test$score)
#75% accurate rate

#cross validation
par(mfrow=c(1,1))
l3=cv.glmnet(x,y,family="binomial")
l3
plot(l3)
p3=predict(l3,x,type="response",s=0.012)
summary(l3)
summary(p3)
c3=(p3>0.5)
c3[which(c3==TRUE)]=1
c3[which(c3!=TRUE)]=0
table(c3)
#need to use the minimum lambda: lambda.min

#6 level multiple nominal logistic
y0=d[1:100,"categorization"]
y0=data.frame(y0)
table(y0)
y=as.matrix(y0)

#y changed, x kept to be murder associated words
dim(x);dim(y)

cv.glmnet(x[1:n,],y[1:n],family="multinomial")#lambda.min=0.1
l2=glmnet(x[1:n,],y[1:n],family="multinomial")

p2=predict(l2,x[(n+1):100,],type="response",s=0.01)
summary(l2)
summary(p2)
p2=data.frame(p2)
p2

maxindex=NULL
for (i in 1:20){
maxindex[i]=colnames(p2)[which.max(p2[i,])]}
table(maxindex)

c4=gsub(".1","",maxindex)
table(c4)

#compare with the real y
test=data.frame(cbind(c4,y[81:100]))
test
table(test$c4)
table(test$c4,test$V2)
#70%


############################# topic model next

library("lattice")
library("topicmodels")
library(slam)

######### why: only the word bag is transffered

d_dtm=t(dm)

dmatrix=as.matrix(d_dtm)

?row_sums

length(d_dtm$v)
length(d_dtm$i)

# this requires the matrix to be: row is a document, column is a word
term_tfidf <- 
  tapply(d_dtm$v/row_sums(d_dtm)[d_dtm$i], d_dtm$j, mean) *
  log2(nDocs(d_dtm)/col_sums(d_dtm > 0))
summary(term_tfidf)
d_dtm <- d_dtm[,term_tfidf >= 0.01]
d_dtm <- d_dtm[row_sums(d_dtm) > 0,]
summary(col_sums(d_dtm))

dim(d_dtm)

tifidfwords=colnames(d_dtm)

k <- 6
SEED <- 2014

# this fitted 4 models at one time

d_TM <- 
  list(VEM = LDA(d_dtm, k = k, control = list(seed = SEED)),
       VEM_fixed = LDA(d_dtm, k = k, 
                       control = list(estimate.alpha = FALSE, seed = SEED)),
       Gibbs = LDA(d_dtm, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000, 
                                  thin = 100, iter = 1000)),
       CTM = CTM(d_dtm, k = k, 
                 control = list(seed = SEED, 
                                var = list(tol = 10^-4), em = list(tol = 10^-3))))

d_TM

sapply(d_TM[1:2], slot, "alpha")

methods <- c("VEM", "VEM_fixed", "Gibbs", "CTM")
DF <- data.frame(posterior = unlist(lapply(d_TM, function(x) apply(posterior(x)$topics, 1, max))),
                 method = factor(rep(methods,
                                     each = nrow(posterior(d_TM$VEM)$topics)), methods))
print(histogram(~ posterior | method, data = DF, col = "white", as.table = TRUE,
                xlab = "Probability of assignment to the most likely topic",
                ylab = "Percent of total", layout = c(4, 1)))

sapply(d_TM, function(x) 
  mean(apply(posterior(x)$topics, 
             1, function(z) - sum(z * log(z)))))

Topic <- topics(d_TM[["VEM"]], 1)
table(Topic)
#the distribution is very different from the true distribution, 
#and the topics are not interpretable
#topic models may not be a good choice for most of the cases

Terms <- terms(d_TM[["VEM"]],20)
Terms[,1:6]

table(cats[1:100])

######## topic model doesn't work


###################### supervised SVM model
library("lda")
library("lattice")
library("topicmodels")
library("XML")
library("tm")
library("slam")
library("Snowball")
library("RTextTools")
library("ggplot2")

# use the already generated term document matrix, without tfidf weighting
d_dtm=t(dm)
dim(d_dtm)

#the difference of this document matrix is, it used tfidf weight
dm=TermDocumentMatrix(corpus,control=list(weighting = function(x) weightTfIdf(x),
                                          stemming=TRUE, stopwords="english",removeNumbers=TRUE,
                                          removePunchtuation=TRUE,minWordLength=3))

dim(dm)

d_dtm=t(dm)
dim(d_dtm)

n=80 #still use 80 training and 20 test
#generate the container
d$cats=as.numeric(d$categorization)
table(d$cats)
table(d$categorization)

#character categories don't work
container <- create_container(d_dtm, d$cats, trainSize=1:n,
                              testSize=(n+1):100, virgin=FALSE)

# training the model using different algorithm
SVM <- train_model(container,"SVM",use_sgd = FALSE)
SVM_sgd <- train_model(container,"SVM",use_sgd = TRUE)
#GLMNET <- train_model(container,"GLMNET")

?train_model

#classify model
SVM_CLASSIFY <- classify_model(container, SVM)
SVMsgd_CLASSIFY <- classify_model(container, SVM_sgd)

names(SVM_CLASSIFY)
SVM_CLASSIFY$SVM_LABEL
SVM_CLASSIFY$SVM_PROB
############# all classfied to murder!

names(SVMsgd_CLASSIFY)

table(SVM_CLASSIFY$SVM_LABEL,d$cats[(n+1):100])
#all to murder has 75% accurate rate, this is about the same to GLMNET(lasso)

analytics <- create_analytics(container,cbind(SVM_CLASSIFY,SVMsgd_CLASSIFY))

# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
topic_summary
alg_summary <- analytics@algorithm_summary
alg_summary
ens_summary <-analytics@ensemble_summary
ens_summary
doc_summary <- analytics@document_summary

#cross validation
SVM <- cross_validate(container, 3, "SVM")



##################### a stochastic gradient descent experiment to see how many we need to label
########Do we really need to analyze 2000 labeld cases?########################################

dm=TermDocumentMatrix(corpus,control=list(stemming=TRUE, stopwords="english",removeNumbers=TRUE,
                                          removePunchtuation=TRUE,minWordLength=3))

dim(dm)


#most frequent words and words association
freqwords=findFreqTerms(dm,lowfreq=10,highfreq=80)
freqwords
length(freqwords)
rownames(dm)
murderassociate=findAssocs(dm,'murder',0.3)#choose crime from the word cloud
cor(as.vector(dm["murder",]),as.vector(dm["wound",]))
murderwords=rownames(murderassociate)
murderwords
length(murderwords)

#only use certain words
d_find=TermDocumentMatrix(corpus,control=list(stemming=TRUE, stopwords="english",removeNumbers=TRUE,
                                              removePunchtuation=TRUE,minWordLength=3,
                                              dictionary=murderwords))

dim(d_find)
d_dtm=t(dm)#use all the words or use frequent words
dim(d_dtm)

#standardize variables
x=as.matrix(d_dtm)
summary(x)
x.scale=scale(x, center = TRUE, scale = TRUE)
dim(x.scale)
#test: the same
summary(x.scale[,1])
summary((x[,1]-mean(x[,1]))/sd(x[,1]))

#use the binary murder or not as dependent variable
y0=d[1:100,"categorization"]
y0=data.frame(y0)
y0$cats=0
y0[y0[,1]=="murder","cats"]=1
table(y0$cats)
y=as.matrix(y0$cats)

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

theta=gradient.descent(x=x.scale, y=y, alpha=0.0001, num.iterations=100, output.path=TRUE)

dim(theta)

#100+1 observations, the first observation is all 1

#calculate the accurate rate 

#see how the theta converges
par(mfrow=c(2,2))
qplot(1:(nrow(theta)), theta[,2], geom=c("line"), xlab="iteration", ylab="theta")
qplot(1:(nrow(theta)), theta[,70], geom=c("line"), xlab="iteration", ylab="theta")
qplot(1:(nrow(theta)), theta[,500], geom=c("line"), xlab="iteration", ylab="theta")
qplot(1:(nrow(theta)), theta[,700], geom=c("line"), xlab="iteration", ylab="theta")

num.iterations=dim(x)[1]
# Look at output for various different alpha values
vary.alpha <- lapply(c(1e-12, 1e-7, 1e-3, 0.01,0.1,0.5), function(alpha) gradient.descent(x=x.scale, y=y, alpha=alpha, num.iterations=num.iterations, output.path=TRUE))

par(mfrow = c(2, 3))
for (j in 1:6) {
  plot(vary.alpha[[j]][,2], ylab="c(1e-12, 1e-7, 1e-3, 0.01,0.1,0.5)", xlab="iteration", type="l")
}

#calculate error rate

x=x.scale

error_rate <- function(x,theta){
  
  #make predictions and calculate error rate
  cx=cbind(rep(1,dim(x)[1]),x)
  dim(cx)
  dim(theta)
  
  p=exp(cx%*%t(theta[2:101,]))/(1+exp(cx%*%t(theta[2:101,])))
  summary(p)
  #dim(p)
  
  prediction=matrix(0,dim(x)[1],100)
  #dim(prediction)
  
  for (i in 1:100){
    prediction[,i]=(p[,i]>0.5)}
  summary(prediction)
  
  error_rate=rep(0,num.iterations)
  for(i in 1:num.iterations){
    error_rate[i]=table(y,prediction[,i])[1,1]+table(y,prediction[,i])[2,2]/length(y)
  }
  
  error_rate
  #summary(error_rate)
  return(error_rate)
  qplot(1:(length(error_rate)), error_rate, geom=c("line"), xlab="iteration", ylab="error rate")
}

#scaled error
#test different alpha

par(mfrow=c(2,3))
alpha=c(0.00001,0.0001,0.0001,0.001,0.01,0.02)
error.all=NULL

for (i in 1:length(alpha)){
theta=gradient.descent(x=x.scale, y=y, alpha=alpha[i], num.iterations=100, output.path=TRUE)
error=error_rate(x=x, theta=theta)
summary(error)
plot(error,type="l")
error.all=cbind(error.all, error)
}

summary(error.all)
min(error.all[,5])
which(error.all[,5]==min(error.all[,5]))
#reached lowest error rate using only two articles



##################### semi supervised model
#install.packages("upclass")
#library(upclass)

#install.packages("spa")
library(spa)

y0=d[1:100,"categorization"]
y0=data.frame(y0)
y0$cats=0
y0[y0[,1]=="murder","cats"]=1
table(y0$cats)
y=as.matrix(y0$cats)

x=as.matrix(d_dtm)
dim(x)
dim(d_dtm)

#use upclass
#n=80
#fitup <- upclassifymodel(x[1:n,],y[1:n],x[(n+1):100,],y[(n+1):100])
#fitup

#use spa: the x input is the covaraicne of the d_dtm matrix, try to find the relationship of variables
n=60 #mimic the proportion of the entire dataset
n

L=1:n
U=(n+1):100

A=x
y1=rep(0,100)
y1[1:n]=y[1:n]
y1[(n+1):100]=NA
table(y1)

dim(x)
dim(cov(t(x)))

A=cov(t(x))
#summary(A)

g=spa(y1, graph = A, control = spa.control(diss = FALSE))
tab<-table(y[L],fitted(g)[L]>0.5);a1<-sum(diag(tab))/sum(tab)
tab<-table(y[U],fitted(g)[U]>0.5);a2<-sum(diag(tab))/sum(tab)
a3<-sum(apply(A[U,L],1,sum)==0)/(length(U))*100
a1;a2;a3
cat("Labeled Acc.=",round(a1,4)," \nUnlabeled Acc.=",round(a2,4)," \n% zero A_ULx 1=",round(a3,4),"\n")


score=cbind(y,fitted(g)[L]>0.5,rep(1,100))
dim(score)
score[score[,1]!=score[,2],3]=0
score
mean(score[,3])#overall accurate rate
mean(score[L,3])#labeled accurate rate
mean(score[U,3])#unlabeled accurate rate


tab=table(y[L],fitted(g)[L]>0.5)
fitted(g)[L]>0.5
fitted(g)[L]
tab
#sum(diag(tab))/sum(tab)

tab=table(y[U],fitted(g)[U]>0.5)
fitted(g)[U]>0.5
tab

#sum(diag(tab))/sum(tab)


########################### try semi-supervised on accident

  
y0=d[1:100,"categorization"]
y0=data.frame(y0)
table(y0)
y0names=names(table(y0))
y0names

score_all=matrix(0,5,3)
score_combine=NULL


###### the first one is the binorminal models
for (i in 2:6){ 
y0$cats=0
y0[y0[,1]==y0names[i],"cats"]=1
table(y0$cats)
y=as.matrix(y0$cats)

n=60 #mimic the proportion of the entire dataset
n

L=1:n
U=(n+1):100

A=x
y1=rep(0,100)
y1[1:n]=y[1:n]
y1[(n+1):100]=NA
table(y1)

dim(x)
dim(cov(t(x)))

A=cov(t(x))
#summary(A)

g=spa(y1, graph = A, control = spa.control(diss = FALSE))
tab<-table(y[L],fitted(g)[L]>0.5);a1<-sum(diag(tab))/sum(tab)
tab<-table(y[U],fitted(g)[U]>0.5);a2<-sum(diag(tab))/sum(tab)
a3<-sum(apply(A[U,L],1,sum)==0)/(length(U))*100
a1;a2;a3
cat("Labeled Acc.=",round(a1,4)," \nUnlabeled Acc.=",round(a2,4)," \n% zero A_ULx 1=",round(a3,4),"\n")

score=cbind(y,fitted(g)>0.5,rep(1,100))
dim(score)
score[score[,1]!=score[,2],3]=0
score
mean(score[,3])#overall accurate rate
mean(score[L,3])#labeled accurate rate
mean(score[U,3])#unlabeled accurate rate

score_combine=cbind(score_combine,score)

score_all[(i-1),1]=mean(score[,3])
score_all[(i-1),2]=mean(score[L,3])
score_all[(i-1),3]=mean(score[U,3])

tab=table(y[L],fitted(g)[L]>0.5)
fitted(g)[L]>0.5
fitted(g)[L]
tab
#sum(diag(tab))/sum(tab)

tab=table(y[U],fitted(g)[U]>0.5)
fitted(g)[U]>0.5
tab
}

rownames(score_all)=y0names[2:6]
colnames(score_all)=c("overall","Labeled","Unlabeled")
score_all
score_combine

predict=score_combine[,c(2,5,8,11,14)]
colnames(predict)=c("accident", "defense",  "murder",   "other",    "police")
predict
summary(predict)

###### the second one is the mutinorminal models

score=matrix(0,length(y),5)

n=90 #mimic the proportion of the entire dataset
n


for (i in 2:6){ 

  y0$cats=0
  y0[y0[,1]==y0names[i],"cats"]=1
  table(y0$cats)
  y=as.matrix(y0$cats)
  
  L=1:n
  U=(n+1):100
  
  A=x
  y1=rep(0,100)
  y1[1:n]=y[1:n]
  y1[(n+1):100]=NA
  table(y1)
  
  dim(x)
  dim(cov(t(x)))
  
  A=cov(t(x))
  #summary(A)
  
  g=spa(y1, graph = A, control = spa.control(diss = FALSE))
  tab<-table(y[L],fitted(g)[L]>0.5);a1<-sum(diag(tab))/sum(tab)
  tab<-table(y[U],fitted(g)[U]>0.5);a2<-sum(diag(tab))/sum(tab)
  a3<-sum(apply(A[U,L],1,sum)==0)/(length(U))*100
  a1;a2;a3
  cat("Labeled Acc.=",round(a1,4)," \nUnlabeled Acc.=",round(a2,4)," \n% zero A_ULx 1=",round(a3,4),"\n")
  
  score[,i-1]=fitted(g)
}

score
colnames(score)=y0names[2:6]

colnames(score)

predict=rep(NA,100)
predict

score[is.na(score)]=0

#### make predictions, use the largest probability
for (i in 1:100){
#test[i]=which.max(score[i,])
predict[i]=c("accident", "defense",  "murder",   "other",    "police")[which.max(score[i,])]
}

length(predict)
table(predict)
label=as.vector(d0[1:100,"categorization"])
label
compare=data.frame(cbind(label,predict))
compare

summary(rowSums(score))
compare$right=0
compare
compare[as.numeric(compare$label)-1==as.numeric(compare$predic),"right"]=1
as.numeric(compare$label)-1
as.numeric(compare$predic)
mean(compare$right)



###################################################################################
############# use the entire dataset subtracted by Bob
library(spa)

      setwd("C:\\Work\\gssdata\\Slate_BagOWords.csv")
      dall=read.csv("Slate_BagOWords.csv",header=TRUE)
      
      y=dall[,"categorization"]
      ynames=names(table(y))
      ynames
      y=data.frame(y)
      table(y)
dim(y);colnames(y)
      
      gL=which(dall$categorization!='')#this is the labeled data
      U=which(dall$categorization=='')#this is the un-labeled data
      
      score_all=matrix(0,6,3)
      score_combine=NULL
      
      ###### fit the mutinorminal models
      
      p=dim(dall)[2]
      p
      n=dim(dall)[1]
      n

x=dall[,3:p]
crimewords=findAssocs(as.matrix(dall),"crime",0.5)
length(crimewords)
      
      score=matrix(0,dim(dall)[1],6)
      
      #x=dall[,3:p]
      A=cov(t(dall[,crimewords]))
      dim(A)

table(y)

ynames

i=1
for (i in 1:6){ 
  
  y$cats=rep(0,n)
  colnames(y)
  y$cats[which(y$y==ynames[i+1])]=1
  table(y[,"cats"])
  y1=as.matrix(y$cats)
  table(y1)
  table(y$y)
  
  y1[U]=NA

  #summary(A)
  
  g=spa(y1, graph = A, control = spa.control(diss = FALSE))
  score[,i]=as.matrix(fitted(g))
}

score
colnames(score)=ynames[2:7]

scorenames=colnames(score)

predict=rep(NA,n)
predict

score[is.na(score)]=0

#### make predictions, use the largest probability
for (i in 1:n){
  #test[i]=which.max(score[i,])
  predict[i]=scorenames[which.max(score[i,])]
}

length(predict)
table(predict)
label=as.vector(dall[1:n,"categorization"])
label
compare=data.frame(cbind(label,predict))
compare

summary(rowSums(score))
compare$right=0
compare
compare[as.numeric(compare$label)-1==as.numeric(compare$predic),"right"]=1
as.numeric(compare$label)-1
as.numeric(compare$predic)
mean(compare$right)


############# simple classification: decision tree, nearest mean, SVM
install.packages("rpart")
install.packages("e1071")
install.packages("class")
install.packages("party")
install.packages("zoo")
install.packages("lattice")

library(rpart)
library(e1071)
library(class)
library(party)
library(zoo)


#use the demongraphic variables and the words in [10,60]
#"sample","date","gender","age","ageGroup","city","state","lat","lng" has missing, can't be used
x=as.matrix(cbind(d[1:100,c("gender")],dm_matrix[,freqwords]))
#x=as.matrix(cbind(d[1:100,c("gender")],dm_matrix[,allwords]))
dim(x)#turn to 514 variables

#murder or non-murder
y0=d[1:100,"categorization"]
y0=data.frame(y0)
y0$cats=0
y0[y0[,1]=="murder","cats"]=1
table(y0$cats)
y=as.matrix(y0$cats)

dim(y)
dim(x)

n=70
ytrain=data.frame(y[1:n,])
xtrain=data.frame(x[1:n,])
yx=cbind(ytrain,xtrain)
#names(yx)
colnames(yx)[1:2]=c("cats","gender")

ytest=data.frame(y[(n+1):100,])
xtest=data.frame(x[(n+1):100,])
yxtest=cbind(ytest,xtest)
#names(yxtest)
colnames(yxtest)[1:2]=c("cats","gender")

#rpart from rpar
fit=rpart(as.factor(cats)~.,data=yx)
plot(fit)
fit$y
p1=predict(fit,yxtest,type="class")
sum(yxtest$cats==p1)/20


#knn from class
x=data.frame(x)
y=as.factor(y)
xtrain=x[1:n,]
ytrain=y[1:n]
xtest=x[(n+1):100,]
ytest=y[(n+1):100]

fit=knn(xtrain,xtest,cl=ytrain,k=3)
fit
p2=fit
sum(ytest==p2)/length(ytest)

# svm

drops="higgin"
xtrain=xtrain[,!(names(xtrain) %in% drops)]
xtest=xtest[,!(names(xtest) %in% drops)]

fit=svm(xtrain,ytrain,kernal="linear",cost=10)
#summary(xtrain)

p3=predict(fit,xtest)
sum(p3==ytest)/length(ytest)


plot(x[,1],rowSums(x),pch=19,col=as.numeric(y))
table(x[,1])
table(x$V1)

#emselble these three fitted values
pall=data.frame(cbind(p1,p2,p3))
pall$emp2=(pall$p1==2)+(pall$p2==2)+(pall$p3==2)
pall$emp1=1-((pall$p1==2)+(pall$p2==2)+(pall$p3==2))

pall$emp=0
for (i in 1:20){
if(pall$emp2[i]>=2) pall$emp[i]=1}

table(pall$emp)

sum(pall$emp==ytest)/length(ytest)

table(pall$emp,ytest)

pall

############ random forest
#install.packages("randomForest")
library("randomForest")

n=60

rf_accrate=rep(0,20)
for (i in 1:20){
fit=randomForest(xtrain,ytrain)
rf_accrate[i]=sum(ytest==predict(fit,xtest))/length(ytest)}

summary(rf_accrate)
rf_accrate

################### random forest is the best!!!!!!!!!! use freqwords 80 training points, 85% median
#################### using all the words 80 training poinst, the median got down to 80%!!

################## use freq words, 70 training points, 87% median
######## conclusion: best methods SVM, lasso and random forest, simplest random forest, and also the best
######## the traing data is not the larger the better, the words is not the more the better

########  60 training points, freq words, 86% accurate rate







############################### the things I did above (except topic model)
# are all supervised (or semi-supervised) learning, training data involved, for non-training data learning
# it's clustering
############################### for the eamples below, I will ignore the training data,
#treat every case as not labeled point

################### first method, kmeans clustering 

## library class is for k neareat neighbor
library(class)

## library kmeans is for k-means clustering

fit=kmeans(x,2)
?kmeans
#kmeans is built in R stats, no particular package needed


fit$center

names(fit)

pred=fit$cluster

length(pred)

length(y)

compare=cbind(y,pred)

dim(compare)

table(compare[,1],compare[,2])

################## oh my god, a simplest 2 means clustering 
#############without using any traing data points gets the accurate rate 75%

#### try a 5 means clustering
y0=d[1:100,"categorization"]
y0=as.numeric(y0)-1
table(y0)

fit=kmeans(x,6)
fit$cluster
table(fit$cluster)
pred=fit$cluster
table(pred)

# find the center
names(fit)

fit$center

#use the centers to do nearest neighbor, then compare the predicted clusters with true values

xtrain=data.frame(fit$center)
ytrain=1:6

xtest=data.frame(x)
ytest=data.frame(y0)
dim(xtest)
names=colnames(xtest)

knnfit=knn(xtrain,xtest,as.factor(ytrain),k=1)

dim(fit$center)

#to find the map between two dataset, merge the two data frames
dim(xtrain)
yx=cbind(ytest,xtest)
dim(yx)

pred=cbind(fit$center,cluster=c(1:6))
colnames(pred)

yxcluster=merge(yx,pred,by=names,all.x=TRUE)
dim(yxcluster)
kkknames(yxcluster)
dim(yxcluster)

table(yxcluster$cluster,yxcluster$y0)
yxcluster[y$cluster^=]

#plot
plot(yxcluster$V1,as.numeric(yxcluster$y0),col=fit$cluster,pch=19)


################ do a simple social network analysis using R

#install.packages("igraph")

library(igraph)

dim(x)

A=cov(t(x))
A=as.matrix(t(x))%*%as.matrix(x)

g=graph.adjacency(A)
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

set.seed(2014)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)

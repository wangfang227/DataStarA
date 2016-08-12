setwd("C:\\Work\\Hong kong protest")
d=read.csv("C:\\Work\\Hong kong protest\\hong kong 2014.csv",head=T)
names(d)

#regression
d$d2=d$Day^2
#l1=glm(Number~Day+d2+Arrest,family="poisson", data=d)
l1=lm(Number~Day+d2+Arrest,data=d)
summary(l1)
plot(d$Day,d$Number,type="b")
points(d$Day, predict(l1),type="l",col="red")
predict(l1)
max(d$Day)

#predict number of people in the next 30 days
Day=27:30
length(Day)
Arrest=rep(5,length(Day))
newdata=data.frame(cbind(Day,Arrest))
newdata$d2=newdata$Day^2
newdata
p1=predict(l1, newdata=newdata, interval="confidence") 
p1
dim(p1)
length(Day)

newdata$Number=p1[,1]
newdata
d2=rbind(d[,names(newdata)],newdata)
plot(d2$Day,d2$Number,type="b")
points(d2$Day, predict(l1,newdata=d2),type="l",col="red")
dev.copy(jpeg,filename="hongkong2014.jpg");
dev.off ()



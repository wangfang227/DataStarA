d=read.csv("C:\\Work\\Fun projects\\Redfin analysis\\redfin_2014-10-07-60540_1year.txt", head=T)
summary(d)
detach(d)
attach(d)
l1=lm(LAST.SALE.PRICE~SQFT)
l2=lm(LAST.SALE.PRICE~SQFT+I(SQFT^2))
l3=lm(LAST.SALE.PRICE~SQFT+I(SQFT^2)+YEAR.BUILT)

anova(l1,l2)
summary(l1)
print(l1)
plot(SQFT,LAST.SALE.PRICE)
abline(l1, col="red")
points(SQFT,predict(l2))
csurv()

#### read in the URL and analyze the description
# load packages
install.packages("RCurl")
install.packages("XML")
install.packages("bitops")
library(RCurl)
library(XML)

remove_HTML_markup <- function(s) {
  doc <- htmlTreeParse(s, asText = TRUE, trim = FALSE)
  xmlValue(xmlRoot(doc))
}

dim(d)
url=as.character(d[!is.na(d$URL),"URL"])
length(url)
url[1]

mylist=list()

for (i in 1:10){  
  
  # download html
  html <- getURL(url[i], followlocation = TRUE,ssl.verifypeer = FALSE)
  
  # parse html
  doc = htmlParse(html, asText=TRUE)
  plain.text <- xpathSApply(doc, "//p", xmlValue)
  #cat(paste(plain.text, collapse = "\n"))
  test=paste(plain.text, collapse = " ")
  mylist[[i]]=test
  
}

mylist #didn't work

#Use all the other variables 
names(d)
d[,c("ID","ADDRESS","NEXT.OPEN.HOUSE.DATE","NEXT.OPEN.HOUSE.START.TIME","NEXT.OPEN.HOUSE.END.TIME",
"RECENT.REDUCTION.DATE","URL","SOURCE","LISTING.ID","DAYS.ON.MARKET","ORIGINAL.SOURCE","STATUS","SALE.TYPE","HOME.TYPE",
"STATE","CITY","INTERESTED","FAVORITE", "LAST.SALE.DATE","LOCATION","LIST.PRICE","ORIGINAL.LIST.PRICE")]<-list(NULL)
summary(d)
names(d)
d$IS.SHORT.SALE[is.na(IS.SHORT.SALE)]="FALSE"
persq=LAST.SALE.PRICE/SQFT

summary(d)
xlist=names(d)[1:15]
xlist
xnames=paste(xlist,collapse="+")
xnames

names(d)
l5=lm(LAST.SALE.PRICE~ZIP+BEDS+BATHS+SQFT+LOT.SIZE+YEAR.BUILT+I(YEAR.BUILT^2)+PARKING.SPOTS+PARKING.TYPE+LATITUDE+LONGITUDE+IS.SHORT.SALE,data=d)
summary(l5)
par(mfrow=c(1,1))
plot(LATITUDE,LAST.SALE.PRICE,pch=20)
*abline(lm(LAST.SALE.PRICE~LATITUDE),col="red")
plot(BEDS,LAST.SALE.PRICE,pch=20)
plot(BATHS,LAST.SALE.PRICE,pch=20)
plot(YEAR.BUILT,LAST.SALE.PRICE,pch=20)
aggregate(LAST.SALE.PRICE,list(BEDS),summary)
aggregate(LAST.SALE.PRICE,list(BATHS),summary)
#east houses are more expensive
# year: 6.853e+01/(2*2.699e+05), houses built before 1950 are at historical area, so expensive; 

#model of persquare feet price
l6=lm(persq~ZIP+BEDS+BATHS+LOT.SIZE+YEAR.BUILT+I(YEAR.BUILT^2)+PARKING.SPOTS+PARKING.TYPE+LATITUDE+LONGITUDE+IS.SHORT.SALE,data=d)
summary(l6)

#plots of sale price
par(mfrow=c(2,2))
plot(BEDS,LAST.SALE.PRICE,pch=20)
plot(BATHS,LAST.SALE.PRICE,pch=20)
plot(YEAR.BUILT,LAST.SALE.PRICE,pch=20)
l7=lm(LAST.SALE.PRICE~YEAR.BUILT+I(YEAR.BUILT^2))
l7$coef
#plot(LAST.SALE.PRICE~YEAR.BUILT,pch=20)
curve(l7$coef["(Intercept)"]+l7$coef["YEAR.BUILT"]*YEAR.BUILT+l7$coef["I(YEAR.BUILT^2)"]*I(YEAR.BUILT^2), 1900, 2014, xname = "YEAR.BUILT",col="red",add=TRUE)
plot(LATITUDE,LAST.SALE.PRICE,pch=20)

#plots of per square feet price
setwd("C:\\Work\\Redfin analysis")
jpeg('naperville 60540.jpg')
title(main="Napervile 60540 $/sqft")
par(mfrow=c(2,2))
plot(BEDS,persq,pch=20)
abline(lm(persq~BEDS),col="red")
plot(BATHS,persq,pch=20)
abline(lm(persq~BATHS),col="red")
plot(YEAR.BUILT,persq,pch=20)
l7=lm(persq~YEAR.BUILT+I(YEAR.BUILT^2))
l7$coef
#plot(LAST.SALE.PRICE~YEAR.BUILT,pch=20)
curve(l7$coef["(Intercept)"]+l7$coef["YEAR.BUILT"]*YEAR.BUILT+l7$coef["I(YEAR.BUILT^2)"]*I(YEAR.BUILT^2), 1900, 2014, xname = "YEAR.BUILT",col="red",add=TRUE)
plot(LATITUDE,persq,pch=20)
abline(lm(persq~LATITUDE),col="red")
dev.off()

#t test to see if number of beds affect price
beds=cbind(persq,BEDS)
beds34=beds[BEDS==4||BEDS==3,]
t.test(persq,BEDS,alpha=0.95)



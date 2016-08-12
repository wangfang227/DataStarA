#want to answer the question:
#1 which major is the best one to land a job?
#2 which major is the best one for long term career development?

#used the BLS occupation data, 2013 SDR data, 2012 SED data, 
#and 2010 Current college graduate data downloaded from their website.

#read in the BLS data
options(stringsAsFactors=F ) 
bls=read.csv("/Users/wangfang/Documents/Work/Fun projects/Major analysis/oes_data_2015.csv",head=T, na.strings = "N/A",stringsAsFactors=FALSE)
str(bls)
names(bls)
dim(bls)
table(bls$occ.code)
levels(bls$occ.code)
table(bls$occ.title)
bls[1:22,]
#only check national level, major
#we want to exclude "management" as a major as we are making advices for new graduates
delete=c("Management Occupations")
#bls$area_type=='1' is national data & bls$naics_title=="Cross-industry" & !(bls$occ_title %in% delete)

bls1=bls[bls$group=="major" & bls$area_type=='1' & bls$naics_title=="Cross-industry",]
dim(bls1)
bls1=data.frame(bls1)
table(bls1$occ.title)
table(bls1$area_title,as.character(bls1$occ_code))
bls1[1:5,]
length(bls1$jobs_1000)
length(bls1$occ_title)
summary(as.numeric(bls1$jobs_1000))
summary(as.numeric(bls1$tot_emp))
summary(as.numeric(bls1$a_mean))
bls1[which.max(as.numeric(bls1$a_mean)),]
bls2=bls1[,c("area_title","occ.code","occ.title","tot_emp","a_mean","a_median")]
bls2=cbind(bls2[,c(1:3)],sapply(bls2[,4:6],as.numeric))
apply(bls2[,c("tot_emp","a_mean","a_median")],2,summary)
bls2=data.frame(bls2)
dim(bls2)
#bls2[is.na(bls2)]=0
attach(bls2)
str(bls2)
bls2[which.max(bls2[,"tot_emp"]),]#office support has most position
bls2[which.max(bls2[,"a_median"]),]#management has highest mediam income
bls2[which.max(bls2[,"a_mean"]),]

bls2

aggregate(as.numeric(bls2$a_median),by=list(bls2$occ_title, bls2$area_title),mean)

#SED: use table 44 48 49
#SDR: use table 4_1 51 53



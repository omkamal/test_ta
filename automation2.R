rm(list=ls())
library(reshape)
require (plyr)


setwd("C:/Users/okamal.MGC/Documents/R/TA Telecom/expirement")
b_top_services <- read.table(file="clipboard",header=T,sep="\t",fill=T,quote="")
b <- read.table(file="b.txt",header=T,sep="\t",fill=T,quote="",fileEncoding="Latin-1")
b <- read.table(file="./bu.txt",header=F,sep="\t",fill=T,quote="",encoding="UTF-8")

load(file="./data.R")
load(file="./data_sample.R")

load(file="./cat_service.R")
load(file="./b_top_services.R")
str(b_top_services)

all_services <- levels(cat_service[,1])

cat_ser <- split(cat_service,cat_service$TypeName,drop=T)
cat_top_list <- split(b_top_services,b_top_services$Cat,drop=T)

cat_cat <- names(cat_ser)
lbl_list <- list()
cat_top <- list()

for(i in cat_top_list) {
  df2 <- as.data.frame(i)
  #df3 <- (setdiff(all_services,df2[,1]))
  cat_top <- append(cat_top,list(droplevels(df2[,2]))) 
  
}
names(cat_top) <- names(cat_top_list)

for(i in cat_ser) {
  df2 <- as.data.frame(i)
  #df3 <- (setdiff(all_services,df2[,1]))
  lbl_list <- append(lbl_list,list(droplevels(df2[,1]))) 
  
}
names(lbl_list) <- names(cat_ser)

str(b2)

#b$Active <- as.numeric(b$Active)-1
#b$InActive <- as.numeric(b$InActive)-1
#b$Suspended <- as.numeric(b$Suspended)-1
str(b)

#colnames(b)[4] <- "ServiceStatus"
b <- b2[b2$Active==1 & b2$InActive==0 & b2$Suspended==0,]
#b3 <- b[b$ServiceStatus=="Active",]
#b2 <- b
#r4 <- dim(b2)[1]
#idx1 <- sample(r4,100000)
#b2 <- b2[idx1,]
#b <- b2[,c(1,2,3,4,11,12,13)]
#save(b2,file="./data_sample.R")
#save(b2,file="./data_full_sample.R")
#colnames(b2)[4] <- "ServiceStatus"


#m1 <- melt(b,id.vars=c("MSISDN","ServiceStatus","NAME_EN","TypeName"))
m1 <- melt(b2[1:20000,],id.vars=c("MSISDN","ServiceStatus","NAME_EN","TypeName"),variable.name=c("Active"))
cc1 <- cast(m1,MSISDN~NAME_EN,length,subset=variable=="Active")
lbl <- colnames(cc1[,-1])
d5 <- apply(cc1,1,pick_service)
df6 <- ldply (d5, data.frame)
colnames(df6)[1]<-"MSISDN"
str(df6)

for(i in 2:9)
{
  print(i)
  c1 <- i*10000+1
  c2 <- (i+1)*10000
  m2 <- melt(b2[c1:c2,],id.vars=c("MSISDN","ServiceStatus","NAME_EN","TypeName"),variable.name=c("Active"))
  cc2 <- cast(m2,MSISDN~NAME_EN,length,subset=variable=="Active")
  lbl <- colnames(cc2[,-1])
  d5 <- apply(cc2,1,pick_service)
  df_tmp <- ldply (d5, data.frame)
  colnames(df_tmp)[1]<-"MSISDN"
  df6 <- rbind(df6,df_tmp)
  print(dim(df6))
  
}

df_camp <- data.frame("Current_services"=character(0),"Current_cat"=character(0),"Expirement"=character(0),"Camp_cat"=character(0),"Camp_Service"=character(0))
pick_service <- function(r1) {
  r2<- as.vector(r1)
  v1 <- lbl[as.logical(r2)]
  #print("--")
  #print(v1);print(length(v1))
  #print(v1)
  cat_type <- cat_service[(cat_service[,1] %in% v1),2]
  #print(cat_type)
  #print("--")
  
  #now randamize the expirement
  same_cat <- as.logical(sample(0:1,1))
  if(length(cat_type)==length(cat_cat)) {same_cat=TRUE}
  if(same_cat) {
    #print("-- Same Category")
    #print(length(cat_type))
    exp_label="Same Category - Different Service"
    new_cat <- sample(cat_type,1)
    #print(cat_top[[new_cat]])
    #print(new_cat)
    v2 <- setdiff(cat_top[[new_cat]],v1)
    #print(v2)
    #break()
    
  } else {
    exp_label="Diff Category"
    diff_cat <- setdiff(cat_cat,cat_type)
    new_cat <- sample(diff_cat,1)
    #print("-- Different Category")
    #print(new_cat)
    v2 <- cat_top[[new_cat]]
    #print(v2)
  }
  s1 <- sample(v2,1)
  return(data.frame("Current_services"=v1,"Current_cat"=cat_type,"Expirement"=exp_label,"Camp_cat"=new_cat,Camp_Service=s1))
}
b3<-b2
rm(b2)

load(file="./data_full_sample.R")

df7 <- merge(df6,b2,by.x=c("MSISDN","Current_services"),by.y=c("MSISDN","NAME_EN"))
#df_results <- data.frame(df6)
write.table(df6,file="./exp_3_reduced.csv",sep=",",col.names=T,row.names=F)
write.table(df7,file="./exp_3.csv",sep=",",col.names=T,row.names=F)



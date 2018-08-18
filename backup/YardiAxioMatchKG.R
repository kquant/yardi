rm(list=ls(all=TRUE))
library(data.table)
library(tidyr)
require(reshape2)
library("RODBC")
library("dplyr")
library("FNN")
library("sp")
library("SearchTrees")
library(stringdist)

# MACROS
DATA_PATH="C:/Users/kgeraghty/Documents/Projects/MRG/"
YARDI_FILE = paste0(DATA_PATH, "YardiData.csv")
AXIO_FILE = paste0(DATA_PATH, "AxioData.csv")

#
# Read Data
#
yardi_df <- read.csv(YARDI_FILE, stringsAsFactors = FALSE)
axio_df <- read.csv(AXIO_FILE, stringsAsFactors = FALSE)
AXIO_DROP_COLUMNS <- c("AC_AREAPERUNIT.1","AC_PROJID.1","FILENAME","MRG_CLASS","AC_QUANTITY.1","AC_NAME.1", "SNO")
axio_df<- axio_df[,!(names(axio_df) %in% AXIO_DROP_COLUMNS)]

#
# KNN find the five nearest neighbors
#
coordinates(axio_df)<- ~AC_LONGITUDE + AC_LATITUDE
coordinates(yardi_df) <- ~LONG + LAT
z<-get.knnx(coordinates(axio_df), coordinates(yardi_df), k=5)

#
# Build the Analysis Data Frame
#
YARDI_COLNAMES<- c("YARDI_ID","NAME","PRIOR_NAME","ADDRESS", "CITY","LAT","LONG","UNITS","OWNERID","OWNERNAME","STORY","MANAGER","MGMT" )
AXIO_COLNAMES<-c("AC_SNO", "AC_LATITUDE", "AC_LONGITUDE", "AC_COMPID", "AC_MANAGEMENTCOMPANY", "AC_PROJID", "AC_NAME", "AC_ADDRESS", "AC_CITY", "AC_STATE", "AC_ZIP", "AC_YEARBUIT", "AC_QUANTITY", "AC_AREAPERUNIT", "AC_LEVEL", "AC_YEARBUILT_AGAIN", "AXIOSUBMNEM")
ANALYSIS_COLNAMES<-c(YARDI_COLNAMES, cbind(paste0(AXIO_COLNAMES,"_1"), paste0(AXIO_COLNAMES,"_2"),paste0(AXIO_COLNAMES,"_3"),paste0(AXIO_COLNAMES,"_4"),paste0(AXIO_COLNAMES,"_5")))

axio_df<-as.data.frame(axio_df, stringsAsFactors = FALSE)

df<-cbind(as.data.frame(yardi_df, stringsAsFactors = FALSE), as.data.frame(axio_df[z$nn.index[,1],]))
df<-cbind(as.data.frame(df, stringsAsFactors = FALSE), as.data.frame(axio_df[z$nn.index[,2],]))
df<-cbind(as.data.frame(df, stringsAsFactors = FALSE), as.data.frame(axio_df[z$nn.index[,3],]))
df<-cbind(as.data.frame(df, stringsAsFactors = FALSE), as.data.frame(axio_df[z$nn.index[,4],]))
df<-cbind(as.data.frame(df, stringsAsFactors = FALSE), as.data.frame(axio_df[z$nn.index[,5],]))
colnames(df)<-ANALYSIS_COLNAMES


#
# Assess Column Level Similarity Scorres
#
#df<-cbind(df, as.data.frame(z$nn.dist,stringsAsFactors = FALSE))
df$AC_ADDRESS_1_Score<-ifelse((df$ADDRESS == df$AC_ADDRESS_1),1,0)
df$AC_ADDRESS_2_Score<-ifelse((df$ADDRESS == df$AC_ADDRESS_2),1,0)
df$AC_ADDRESS_3_Score<-ifelse((df$ADDRESS == df$AC_ADDRESS_3),1,0)
df$AC_ADDRESS_4_Score<-ifelse((df$ADDRESS == df$AC_ADDRESS_4),1,0)
df$AC_ADDRESS_5_Score<-ifelse((df$ADDRESS == df$AC_ADDRESS_5),1,0)






# creating an empty dataframe
t <- cbind(as.data.frame(axio_df[1,]),as.data.frame(yardi_df[zknx$nn.index[1,1],]))
t <- t[FALSE,]


for (i in 1:length(axiodata$AC_ADDRESS)){
  
  zzzz<- vector()
  
  
  ####Expand loop number range for more geo matches
  for(n in 1:5){
    ####
    zzz<-trimws(as.character(axiodata[i,]$AC_ADDRESS)) == trimws(as.character(yardidata[zknx$nn.index[i,n],]$Address))
    zzzz<- append(zzzz,zzz)
  }
  
  n<-min(which(zzzz == TRUE))
  if(length(unique(zzzz))==1){next}
  
  else{
    
    z <- n
    
    m <- cbind(as.data.frame(axiodata[i,]),as.data.frame(yardidata[zknx$nn.index[i,z],]))
    
    t<- rbind(t,m)
    
  }
}

#####EXACT NAME MATCH

leftover <- setdiff(as.data.frame(axiodata),t[,names(as.data.frame(axiodata))])

attach(leftover)
coordinates(leftover) <- ~AC_LONGITUDE + AC_LATITUDE
detach(leftover)


zkny<-get.knnx(coordinates(yardidata),coordinates(leftover),k=5)


for (i in 1:length(leftover$AC_ADDRESS)){
  
  zzzz<- vector()
  
  
  ####Expand loop number range for "fuzzier" matches
  for(n in 1:5){
    ####
    zzz<-as.character(leftover[i,]$AC_ADDRESS) == as.character(yardidata[zkny$nn.index[i,n],]$Address)
    zzzz<- append(zzzz,zzz)
  }
  
  n<-min(which(zzzz == TRUE))
  if(length(unique(zzzz))==1){next}
  
  else{
    
    z <- n
    
    m <- cbind(as.data.frame(leftover[i,]),as.data.frame(yardidata[zkny$nn.index[i,z],]))
    
    t<- rbind(t,m)
    
  }
}

####EXACT UNITS MATCH

leftover <- setdiff(as.data.frame(axiodata),t[,names(as.data.frame(axiodata))])

attach(leftover)
coordinates(leftover) <- ~AC_LONGITUDE + AC_LATITUDE
detach(leftover)


zkny<-get.knnx(coordinates(yardidata),coordinates(leftover),k=3)


for (i in 1:length(leftover$AC_ADDRESS)){
  
  zzzz<- vector()
  
  
  ####Expand loop number range for "fuzzier" matches
  for(n in 1:3){
    ####
    zzz<-trimws(as.character(leftover[i,]$AC_QUANTITY)) == trimws(as.character(yardidata[zkny$nn.index[i,n],]$Units))
    zzzz<- append(zzzz,zzz)
  }
  
  n<-min(which(zzzz == TRUE))
  if(length(unique(zzzz))==1){next}
  
  else{
    
    z <- n
    
    m <- cbind(as.data.frame(leftover[i,]),as.data.frame(yardidata[zkny$nn.index[i,z],]))
    
    t<- rbind(t,m)
    
  }
}


###END EXACT MATCH


###START FUZZY ###START WITH ADDRESS




leftover <- setdiff(as.data.frame(axiodata),t[,names(as.data.frame(axiodata))])

attach(leftover)
coordinates(leftover) <- ~AC_LONGITUDE + AC_LATITUDE
detach(leftover)


zkny<-get.knnx(coordinates(yardidata),coordinates(leftover),k=3)



###Loop for ADDRESS

for (i in 1:length(leftover$AC_QUANTITY)){
  
  zzzz<- c(FALSE)
  
  
  ####Expand loop number range for "fuzzier" matches -- currently exact match
  for(n in 0:0){
    ####
    
    
    zzz<-length(agrep(leftover[i,]$AC_ADDRESS, yardidata[zkny$nn.index[i,],]$Address, max=n)) == 1
    
    zzzz<- append(zzzz,zzz)
  }
  
  n<-min(which(zzzz == TRUE))-2
  
  
  if(length(unique(zzzz))==1){next}
  
  else{
    
    z <- agrep(leftover[i,]$AC_ADDRESS, yardidata[zkny$nn.index[i,],]$Address, max=n)
    
    
    m <- cbind(as.data.frame(leftover[i,]),as.data.frame(yardidata[zkny$nn.index[i,z],]))
    
    
    t<- rbind(t,m)
    
  }
}

####FUZZY-ISH MATCH NAME


leftover <- setdiff(as.data.frame(axiodata),t[,names(as.data.frame(axiodata))])

attach(leftover)
coordinates(leftover) <- ~AC_LONGITUDE + AC_LATITUDE
detach(leftover)


zkny<-get.knnx(coordinates(yardidata),coordinates(leftover),k=4)



###Loop for Name

for (i in 1:length(leftover$AC_NAME)){
  
  zzzz<- c(FALSE)
  
  
  ####Expand loop number range for "fuzzier" matches -- currently exact match
  for(n in 0:1){
    ####
    
    
    zzz<-length(agrep(leftover[i,]$AC_NAME, yardidata[zkny$nn.index[i,],]$Name, max.distance=list(cost=n,substitutions=1,deletions=n, insertions=n))) == 1
    
    zzzz<- append(zzzz,zzz)
  }
  
  n<-min(which(zzzz == TRUE))-2
  
  
  if(length(unique(zzzz))==1){next}
  
  else{
    
    z <- agrep(leftover[i,]$AC_NAME, yardidata[zkny$nn.index[i,],]$Name, max.distance=list(cost=n,substitutions=1,deletions=n, insertions=n))
    
    
    m <- cbind(as.data.frame(leftover[i,]),as.data.frame(yardidata[zkny$nn.index[i,z],]))
    
    
    t<- rbind(t,m)
    
  }
}

####REMOVE "APARTMENTS" STRING FUZZY MATCH

leftover <- setdiff(as.data.frame(axiodata),t[,names(as.data.frame(axiodata))])

attach(leftover)
coordinates(leftover) <- ~AC_LONGITUDE + AC_LATITUDE
detach(leftover)


zkny<-get.knnx(coordinates(yardidata),coordinates(leftover),k=3)

lftnames<-names(leftover)
leftover$AC_NAME1<-leftover$AC_NAME
leftover$AC_NAME1<- gsub("Apartments","",leftover$AC_NAME1)
leftover$AC_NAME1 <- trimws(leftover$AC_NAME1)



###Loop for Name

for (i in 1:length(leftover$AC_NAME1)){
  
  zzzz<- c(FALSE)
  
  
  ####Expand loop number range for "fuzzier" matches -- currently exact match
  for(n in 0:0){
    ####
    
    
    zzz<-length(agrep(leftover[i,]$AC_NAME1, yardidata[zkny$nn.index[i,],]$Name, max.distance=list(cost=n,substitutions=0,deletions=n, insertions=n))) == 1
    
    zzzz<- append(zzzz,zzz)
  }
  
  n<-min(which(zzzz == TRUE))-2
  
  
  if(length(unique(zzzz))==1){next}
  
  else{
    
    z <- agrep(leftover[i,]$AC_NAME1, yardidata[zkny$nn.index[i,],]$Name, max.distance=list(cost=n,substitutions=n,deletions=n, insertions=n))
    
    
    m <- cbind(as.data.frame(leftover[i,lftnames]),as.data.frame(yardidata[zkny$nn.index[i,z],]))
    
    
    t<- rbind(t,m)
    
  }
}

####PRIOR NAME FUZZY

leftover <- setdiff(as.data.frame(axiodata),t[,names(as.data.frame(axiodata))])

attach(leftover)
coordinates(leftover) <- ~AC_LONGITUDE + AC_LATITUDE
detach(leftover)


zkny<-get.knnx(coordinates(yardidata),coordinates(leftover),k=4)



###Loop for Prior Name

for (i in 1:length(leftover$AC_NAME)){
  
  zzzz<- c(FALSE)
  
  
  ####Expand loop number range for "fuzzier" matches -- currently exact match
  for(n in 0:1){
    ####
    
    
    zzz<-length(agrep(leftover[i,]$AC_NAME, yardidata[zkny$nn.index[i,],]$Prior_Name, max.distance=list(cost=n,substitutions=1,deletions=n, insertions=n))) == 1
    
    zzzz<- append(zzzz,zzz)
  }
  
  n<-min(which(zzzz == TRUE))-2
  
  
  if(length(unique(zzzz))==1){next}
  
  else{
    
    z <- agrep(leftover[i,]$AC_NAME, yardidata[zkny$nn.index[i,],]$Prior_Name, max.distance=list(cost=n,substitutions=1,deletions=n, insertions=n))
    
    
    m <- cbind(as.data.frame(leftover[i,]),as.data.frame(yardidata[zkny$nn.index[i,z],]))
    
    
    t<- rbind(t,m)
    
  }
}



####Yardi Leftovers Match Attempt

leftover <- setdiff(as.data.frame(axiodata),t[,names(as.data.frame(axiodata))])


attach(leftover)
coordinates(leftover) <- ~AC_LONGITUDE + AC_LATITUDE
detach(leftover)

zkny<-get.knnx(coordinates(yardidata),coordinates(leftover),k=3)
yardileftover<-yardidata[zkny$nn.index[,1],]
yardileftover <- rbind(yardidata[zkny$nn.index[,2],],yardileftover)
yardileftover <- rbind(yardidata[zkny$nn.index[,3],],yardileftover)
yardileftover<-distinct(as.data.frame(yardileftover))

attach(yardileftover)
coordinates(yardileftover) <- ~Long + Lat
detach(yardileftover)

zkny<-get.knnx(coordinates(leftover),coordinates(yardileftover),k=3)



for (i in 1:length(yardileftover$Name)){
  
  zzzz<- c(FALSE)
  
  
  ####Expand loop number range for "fuzzier" matches 
  for(n in 0:0){
    ####
    
    
    zzz<-length(agrep(yardileftover[i,]$Name, leftover[zkny$nn.index[i,],]$AC_NAME, max=n)) == 1
    
    zzzz<- append(zzzz,zzz)
  }
  
  n<-min(which(zzzz == TRUE))-2
  
  
  if(length(unique(zzzz))==1){next}
  
  else{
    
    z <- agrep(yardileftover[i,]$Name, leftover[zkny$nn.index[i,],]$AC_NAME, max=n)
    
    
    m <- cbind(as.data.frame(yardileftover[i,]),as.data.frame(leftover[zkny$nn.index[i,z],]))
    
   
    t<- rbind(t,m)
    
  }
}

######################END GOOD

####LEFTOVER POTENTIAL MATCHES



leftover <- setdiff(as.data.frame(axiodata),t[,names(as.data.frame(axiodata))])

attach(leftover)
coordinates(leftover) <- ~AC_LONGITUDE + AC_LATITUDE
detach(leftover)

zkny<-get.knnx(coordinates(yardidata),coordinates(leftover),k=3)

unmatched <- t[FALSE,]

for(i in 1:length(leftover$AC_ADDRESS)){


r<-m[FALSE] 

for(z in 1:3){
m <- cbind(as.data.frame(leftover[i,]),as.data.frame(yardidata[zkny$nn.index[i,z],]))
r <- rbind(r,m)
}

unmatched <- rbind(r,unmatched)
}


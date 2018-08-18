#
# Match Yardi Properties to Axio Properties
#
# Input:
#         File of Yardi Properties with descriptive data
#         File of Axio Properties with descriptive data
#
# Output:
#         File of Yardi Properties matched to Axio Properties
#

#
# Set up the environment
#
# Production R code in subdirectory /bin
# Latest R code in subdirectory /dev
# Data files in subdirectory /data
# Documentation in subdirectory /docs
#
rm(list=ls(all=TRUE))
WORKING_DIRECTORY<-"C:/Users/kgeraghty/Documents/Projects/MRG/dev/"
INCLUDES_FILE<-"YardiIncludes.R"
setwd(WORKING_DIRECTORY)
source(paste0(WORKING_DIRECTORY,INCLUDES_FILE))

#
# Read Data
#
yardi_df <- read.csv(YARDI_FILE, stringsAsFactors = FALSE)
axio_df <- read.csv(AXIO_FILE, stringsAsFactors = FALSE)
axio_df<- axio_df[,!(names(axio_df) %in% AXIO_DROP_COLUMNS)]

#
# KNN find the five nearest neighbors
#
coordinates(axio_df)<- ~AC_LONGITUDE + AC_LATITUDE
coordinates(yardi_df) <- ~LONG + LAT
z<-get.knnx(coordinates(axio_df), coordinates(yardi_df), k=5)
axio_df<-as.data.frame(axio_df, stringsAsFactors = FALSE)

#
# Build the Analysis Data Frame (df)
#
# Each row contains:
#       Yardi Property Attributes
#       5 Axio Record attributes
#
df<-cbind(as.data.frame(yardi_df, stringsAsFactors = FALSE), as.data.frame(axio_df[z$nn.index[,1],]))
df<-cbind(as.data.frame(df, stringsAsFactors = FALSE), as.data.frame(axio_df[z$nn.index[,2],]))
df<-cbind(as.data.frame(df, stringsAsFactors = FALSE), as.data.frame(axio_df[z$nn.index[,3],]))
df<-cbind(as.data.frame(df, stringsAsFactors = FALSE), as.data.frame(axio_df[z$nn.index[,4],]))
df<-cbind(as.data.frame(df, stringsAsFactors = FALSE), as.data.frame(axio_df[z$nn.index[,5],]))
colnames(df)<-ANALYSIS_COLNAMES

#
# Great Circle Distance 
#
#df<-cbind(df, as.data.frame(z$nn.dist,stringsAsFactors = FALSE))



#
# Assess Column Level Similarity Scorres
#
#       Append scores for similarity for each major attribute in Axio to the the Yardi Attribute
#
# Address Score
#
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


library("RODBC")
library("dplyr")
library("FNN")
library("sp")
library("SearchTrees")


###ENTER SNOWFLAKE DBs and USERNAME/PASSWORD BELOW

conn <- odbcConnect("MRGMONTHLYDATA", uid="*****",pwd="*****")



#Get Yardi Data
yardiquery <- paste("SELECT
                    
                    PROPERTY_ID as \"YardiID\",
                    PROPERTY_NAME as \"Name\",
                    PROPERTY_PRIORNAMES as \"Prior_Name\",
                    PROPERTY_ADDRESS as \"Address\",
                    PROPERTY_CITY as \"City\",
                    PROPERTY_LATITUDE as \"Lat\",
                    PROPERTY_LONGITUDE as \"Long\",
                    PROPERTY_UNITS as \"Units\",
                    OWNER_ID as \"OwnerID\",
                    OWNER_NAME as \"OwnerName\",
                    BLDG_STORYMAX as \"Story\",
                    MANAGER_NAME as \"Manager\",
                    MANAGER_NAME as \"Mgmt\"
                    
                    FROM yardi_matrix.talend.properties
                    
                    WHERE (PROPERTY_AFFORDABLEHOUSING <> 'A' or PROPERTY_AFFORDABLEHOUSING is null) and (PROPERTY_MILITARYHOUSING <> 'A' or PROPERTY_MILITARYHOUSING is null)  AND (PROPERTY_AGERESTRICTED <> 'A' or PROPERTY_AGERESTRICTED is null)  and (PROPERTY_STUDENTHOUSING <> 'A' or PROPERTY_STUDENTHOUSING is null) and PROPERTY_LONGITUDE is not null
                    ")

yardidata <- sqlQuery(conn, yardiquery)



#Get Axio Data


axioquery <- paste ("SELECT
                    
                    *
                    FROM rev_mgmt.talend.support_db ac
                    INNER JOIN axio.talend.Community_support sd
                    ON ltrim (rtrim (sd.ac_PROJID)) = ltrim (rtrim (ac.ac_PROJID))
                    
                    ")


axiodata <- sqlQuery(conn, axioquery)


drops <- c("AC_AREAPERUNIT.1","AC_PROJID.1","FILENAME","MRG_CLASS","AC_QUANTITY.1","AC_NAME.1", "SNO")

axiodata <- axiodata[ , !(names(axiodata) %in% drops)]



###BUILD GEOREFERENCE



attach(axiodata)
coordinates(axiodata) <- ~AC_LONGITUDE + AC_LATITUDE
detach(axiodata)

attach(yardidata)
coordinates(yardidata) <- ~Long + Lat
detach(yardidata)


###KNN
zknx<-get.knnx(coordinates(yardidata),coordinates(axiodata),k=5)



### -- EXACT ADDRESS MATCH


t <- cbind(as.data.frame(axiodata[1,]),as.data.frame(yardidata[zknx$nn.index[1,1],]))
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


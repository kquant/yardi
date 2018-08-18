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
DATA_PATH="C:/Users/kgeraghty/Documents/Projects/MRG/data/"
YARDI_FILE = paste0(DATA_PATH, "YardiData.csv")
AXIO_FILE = paste0(DATA_PATH, "AxioData.csv")
YARDI_COLNAMES<- c("YARDI_ID","NAME","PRIOR_NAME","ADDRESS", "CITY","LAT","LONG","UNITS","OWNERID","OWNERNAME","STORY","MANAGER","MGMT" )
AXIO_COLNAMES<-c("AC_SNO", "AC_LATITUDE", "AC_LONGITUDE", "AC_COMPID", "AC_MANAGEMENTCOMPANY", "AC_PROJID", "AC_NAME", "AC_ADDRESS", "AC_CITY", "AC_STATE", "AC_ZIP", "AC_YEARBUIT", "AC_QUANTITY", "AC_AREAPERUNIT", "AC_LEVEL", "AC_YEARBUILT_AGAIN", "AXIOSUBMNEM")
AXIO_DROP_COLUMNS <- c("AC_AREAPERUNIT.1","AC_PROJID.1","FILENAME","MRG_CLASS","AC_QUANTITY.1","AC_NAME.1", "SNO")
ANALYSIS_COLNAMES<-c(YARDI_COLNAMES, cbind(paste0(AXIO_COLNAMES,"_1"), paste0(AXIO_COLNAMES,"_2"),paste0(AXIO_COLNAMES,"_3"),paste0(AXIO_COLNAMES,"_4"),paste0(AXIO_COLNAMES,"_5")))

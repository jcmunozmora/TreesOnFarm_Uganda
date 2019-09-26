##### Package
library("readstata13")
library(plyr)
library(dplyr)
library(tidyr)
library(Gmisc) # Transition Games

path_work <- "/Users/juan-carlosm/Dropbox/Documents/Projects_papers/2019/Munoz-etal_2019/"
graph_path <- paste0(path_work,"paper/graph/")
save_data <-  paste0(path_work,"01_DataSets/")
path_who <- paste0(save_data,"inputs/")


############################
##------- Preliminares
############################

  ##### Household Roster
  hh_roster_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Household/GSEC1.dta"))
  hh_roster_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC1.dta"))
  hh_roster_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC1.dta"))
  hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC1.dta"))

  #### Basic data
  hh_roster_05 <- hh_roster_05 %>% transmute(hhid=HHID,region=region)
  hh_roster_10 <- hh_roster_10 %>% transmute(hhid=HHID,region=region)
  hh_roster_11 <- hh_roster_11 %>% transmute(hhid=HHID,region=region) 
  hh_roster_14 <- hh_roster_14 %>% transmute(hhid=HHID,region=region)

############################
##------- Imputation Funtion
############################
  
imputeout <- function(var,hh_region) {
    org_na <- colnames(var)
    #### Gen the Avarege per Region
    colnames(hh_region)=c("hhid","region")
    colnames(var)=c("hhid","cropID","var1","var2","var3")
    var_reg <- var %>% inner_join(hh_region) %>% group_by(region) %>% summarise(sd=sd(var1,na.rm=TRUE),med=median(var1,na.rm=TRUE))
    #### Change Value
    var <- var %>% inner_join(hh_region) %>% inner_join(var_reg) %>% mutate(imp=ifelse(var1<(med-3*sd)|var1>(med+3*sd),med,var1)) %>% select(-region,-med,-sd,-var1)
    #### Labelts
    colnames(var) <- c(org_na[1:2],org_na[4],org_na[5],org_na[3])
    return(var)
  }

##### Package
library("readstata13")
library(plyr)
library(dplyr)
library(tidyr)

### Directories
path_work <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/"
graph_path <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Paper/Graph/"
save_data <-  "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/"
path_who <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/inputs/"

###########################
###  Livestock
############################

#-------------
##### 2005-2006
#-------------
large_lvst_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC10A.dta"))
small_lvst_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC10B.dta"))
poult_lvst_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC10C.dta"))

#-------------
##### 2010-2011
#-------------
large_lvst_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Agriculture/AGSEC6A.dta"))
small_lvst_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Agriculture/AGSEC6B.dta"))
poult_lvst_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Agriculture/AGSEC6C.dta"))

#-------------
##### 2011-2012
#-------------
large_lvst_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Agriculture/AGSEC6A.dta"))
small_lvst_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Agriculture/AGSEC6B.dta"))
poult_lvst_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Agriculture/AGSEC6C.dta"))

#-------------
##### 2013-14
#-------------
large_lvst_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC6A.dta"))
small_lvst_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC6B.dta"))
poult_lvst_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC6C.dta"))

###########################
###  Get Data
############################
get_na_zero <- function(x) { ifelse(is.finite(x),x,0) }
  
  #-------------
  ##### 2005-2006
  #-------------
  large_05 <- large_lvst_05 %>% filter(a10aq4=="yes") %>% group_by(HHID) %>% summarise(lvst_large=sum(a10aq5,na.rm=TRUE))
  small_05 <- small_lvst_05 %>% filter(a10bq4=="yes") %>% group_by(HHID) %>% summarise(lvst_small=sum(a10bq5,na.rm=TRUE))
  poult_05 <- poult_lvst_05 %>% filter(a10cq4=="yes") %>% group_by(HHID) %>% summarise(lvst_poul=sum(a10cq5,na.rm=TRUE))
  lvst_05 <- large_05 %>% full_join(small_05) %>% full_join(poult_05) %>% transmute(hhid=HHID,lvst_large=get_na_zero(lvst_large),lvst_small=get_na_zero(lvst_small), lvst_poul=get_na_zero(lvst_poul))
  
  #-------------
  ##### 2010-11
  #-------------
  large_10 <- large_lvst_10 %>% filter(a6aq4=="Yes") %>% group_by(HHID) %>% summarise(lvst_large=sum(a6aq7,na.rm=TRUE))
  small_10 <- small_lvst_10 %>% filter(a6bq4=="Yes") %>% group_by(HHID) %>% summarise(lvst_small=sum(a6bq7,na.rm=TRUE))
  poult_10 <- poult_lvst_10 %>% filter(a6cq4=="Yes") %>% group_by(HHID) %>% summarise(lvst_poul=sum(a6cq7,na.rm=TRUE))
  lvst_10 <- large_10 %>% full_join(small_10) %>% full_join(poult_10) %>% transmute(hhid=HHID,lvst_large=get_na_zero(lvst_large),lvst_small=get_na_zero(lvst_small), lvst_poul=get_na_zero(lvst_poul))
  
  #-------------
  ##### 2011-12
  #-------------
  large_11 <- large_lvst_11 %>% filter(a6aq2=="Yes") %>% group_by(HHID) %>% summarise(lvst_large=sum(a6aq3a,na.rm=TRUE))
  small_11 <- small_lvst_11 %>% filter(a6bq2=="Yes") %>% group_by(HHID) %>% summarise(lvst_small=sum(a6bq3a,na.rm=TRUE))
  poult_11 <- poult_lvst_11 %>% filter(a6cq2=="Yes") %>% group_by(HHID) %>% summarise(lvst_poul=sum(a6cq3a,na.rm=TRUE))
  lvst_11 <- large_11 %>% full_join(small_11) %>% full_join(poult_11) %>% transmute(hhid=HHID,lvst_large=get_na_zero(lvst_large),lvst_small=get_na_zero(lvst_small), lvst_poul=get_na_zero(lvst_poul))
  
  #-------------
  ##### 2013-14
  #-------------
  large_14 <- large_lvst_14 %>% filter(a6aq2=="Yes") %>% group_by(hh) %>% summarise(lvst_large=sum(a6aq3a,na.rm=TRUE))
  small_14 <- small_lvst_14 %>% filter(a6bq2=="Yes") %>% group_by(hh) %>% summarise(lvst_small=sum(a6bq3a,na.rm=TRUE))
  poult_14 <- poult_lvst_14 %>% filter(a6cq2=="Yes") %>% group_by(hh) %>% summarise(lvst_poul=sum(a6cq3a,na.rm=TRUE))
  lvst_14 <- large_14 %>% full_join(small_14) %>% full_join(poult_14) %>% transmute(hhid=hh,lvst_large=get_na_zero(lvst_large),lvst_small=get_na_zero(lvst_small), lvst_poul=get_na_zero(lvst_poul))

  ############################
  ##------- Fix 2014-ID
  ############################
  
  hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC1.dta"))
  
  fix_id_14 <- function(x) {
    #### Data
    mer <- hh_roster_14 %>% transmute(HHID_old=HHID_old,hhid=HHID) %>% filter(!duplicated(hhid))
    x <- x %>% left_join(mer) %>% mutate(hhid_05=hhid,hhid=as.character(HHID_old)) %>% select(-HHID_old)
  }
  
  lvst_14 <- fix_id_14(lvst_14)
  
  lvst_11 <- lvst_11 %>% mutate(hhid=as.character(hhid))
  
  save(lvst_05,lvst_10,lvst_11,lvst_14,file=paste0(save_data,"livestock.Rda"))
  
  
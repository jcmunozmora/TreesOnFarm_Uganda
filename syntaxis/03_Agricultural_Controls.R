##### Package
library("readstata13")
library(plyr)
library(dplyr)
library(tidyr)
library(Gmisc) # Transition Games

### Directories
path_work <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/"
graph_path <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Paper/Graph/"
save_data <-  "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/"
path_who <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/inputs/"


######################
####--------  Share of the land on trees on farm -- 205-06
######################
  ############################################
  ###############------------- Area per crop
  #############################################
  crop_land_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC4A.dta"))
  
  crop_land_05 <- crop_land_05 %>% transmute(hhid=HHID,parcel_id=a4aq1,plot_id=a4aq2,cropID=a4aq5b,plot_size=a4aq3,share_crop=(a4aq6/100)) %>% filter(!is.na(share_crop)) %>% transmute(hhid,parcel_id,plot_id,cropID,area_crop=plot_size*share_crop) %>%
    mutate(
    ##### First Classification - With Bananas
   trees_a=ifelse((cropID %in% c(710,700,750,760,770,741,742,744)),"Fruit Trees",
ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))),
  ##### Second Classification - With Bananas
trees_b=ifelse((cropID %in% c(710,700,750,760,770)),"Fruit Trees",
ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))))
  #### Building Aggregation for the first variable
  crop_land_05_def_1 <- crop_land_05 %>% group_by(hhid,parcel_id,trees_a) %>% summarise(area_crop=sum(area_crop,na.rm=TRUE)) %>% 
    ### ---> Spread the data and rename
    spread(trees_a,area_crop,fill=0) %>% 
    rename(fruit=`Fruit Trees`,other_crop=`Other Crops`,cash=`Tree Cash Crops`,timber=`Trees for Timber/Fuelwood`) %>%
    ### ---> Rename all varibles
    setNames(c(names(.)[1], paste0("def1_",names(.)[-1]))) %>% rename(parcel_id=def1_parcel_id)
  
  #### Building Aggregation for the Second variable
  crop_land_05_def_2 <- crop_land_05 %>% group_by(hhid,parcel_id,trees_b) %>% summarise(area_crop=sum(area_crop,na.rm=TRUE)) %>% 
    ### ---> Spread the data and rename
    spread(trees_b,area_crop,fill=0) %>% 
    rename(fruit=`Fruit Trees`,other_crop=`Other Crops`,cash=`Tree Cash Crops`,timber=`Trees for Timber/Fuelwood`) %>%
    ### ---> Rename all varibles
    setNames(c(names(.)[1], paste0("def2_",names(.)[-1]))) %>% rename(parcel_id=def2_parcel_id) %>% mutate(parce_a_es=(def2_fruit+def2_other_crop+def2_timber+def2_cash))
  
  #### Merge information
  
  crop_a_05 <- crop_land_05_def_1 %>% inner_join(crop_land_05_def_2) %>% select(def1_fruit,parce_a_es,starts_with("def2_")) %>% 
    ### Make the aggregations
    transmute(treeonfarm_w_b=(def1_fruit+def2_timber+def2_cash),treesonfarm_wo_b=(def2_fruit+def2_timber+def2_cash),fruit_w_b=def1_fruit,fruit_wo_b=def2_fruit,cash=def2_cash,timber=def2_timber,land_size_est=parce_a_es)
  
  #############################################
  ###############------------- Land Size
  #############################################
  
  #####---- Land Size (Ownd)
  land_05_a <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC2A.dta"))
  
  land_05_a <- land_05_a %>% transmute(hhid=HHID,parcel_id=a2aq2,land_size=ifelse(is.na(a2aq4),a2aq5,a2aq4)) 
  
  #####---- Land Size (Use Rights)
  land_05_b <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC2B.dta"))
  
  land_05_b <- land_05_b %>% transmute(hhid=HHID,parcel_id=a2bq2,land_size=ifelse(is.na(a2bq4),a2bq5,a2bq4)) 
  
  #### Append Data Set
  land_05 <- rbind(land_05_a,land_05_b)
  
  #############################################
  ###############------------- Final Merge
  #############################################

  m_are_crop_05 <- crop_a_05 %>% left_join(land_05) %>% filter(!is.na(land_size)) %>%
    mutate(land_size=ifelse(land_size_est>land_size,land_size_est,land_size)) %>%
    #### Gen the aggregation by hiig
    group_by(hhid) %>% summarise_all(sum) %>% select(-parcel_id) %>%
    #### Build the shares
    transmute(hhid=hhid,sh_tof_w_b=treeonfarm_w_b/land_size,sh_tof_wo_b=treesonfarm_wo_b/land_size,sh_fruit_w_b=fruit_w_b/land_size,sh_fruit_wo_b=fruit_wo_b/land_size,sh_cash=cash/land_size,sh_timber=timber/land_size,land_size=land_size)
  
  
  ######################
  ####--------  Share of the land on trees on farm -- 2013-14
  ######################
  #############################################
  ###############------------- Area per crop
  #############################################
  crop_land_14 <- read.dta13(paste0(path_work,"00_RawData/2010-11/AGSEC4A.dta"))
  
  crop_land_14 <- crop_land_14 %>% transmute(hhid=hh,parcel_id=parcelID,plot_id=plotID,cropID=cropID,area_crop=a4aq7,year_planted=a4aq9_2) %>% filter(!is.na(area_crop)) %>%
    mutate(
      ##### First Classification - With Bananas
      trees_a=ifelse((cropID %in% c(710,700,750,760,770,741,742,744)),"Fruit Trees",
                     ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
                            ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))),
      ##### Second Classification - With Bananas
      trees_b=ifelse((cropID %in% c(710,700,750,760,770)),"Fruit Trees",
                     ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
                            ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))))
  
  #### Building Aggregation for the first variable
  crop_land_14_def_1 <- crop_land_14 %>% group_by(hhid,parcel_id,trees_a) %>% summarise(area_crop=sum(area_crop,na.rm=TRUE)) %>% 
    ### ---> Spread the data and rename
    spread(trees_a,area_crop,fill=0) %>% 
    rename(fruit=`Fruit Trees`,other_crop=`Other Crops`,cash=`Tree Cash Crops`) %>%
    ### ---> Rename all varibles
    setNames(c(names(.)[1], paste0("def1_",names(.)[-1]))) %>% rename(parcel_id=def1_parcel_id)
  
  #### Building Aggregation for the Second variable
  crop_land_14_def_2 <- crop_land_14 %>% group_by(hhid,parcel_id,trees_b) %>% summarise(area_crop=sum(area_crop,na.rm=TRUE)) %>% 
    ### ---> Spread the data and rename
    spread(trees_b,area_crop,fill=0) %>% 
    rename(fruit=`Fruit Trees`,other_crop=`Other Crops`,cash=`Tree Cash Crops`) %>%
    ### ---> Rename all varibles
    setNames(c(names(.)[1], paste0("def2_",names(.)[-1]))) %>% rename(parcel_id=def2_parcel_id) %>% mutate(parce_a_es=(def2_fruit+def2_other_crop+def2_cash))
  
  #### Merge information
  
  crop_a_14 <- crop_land_14_def_1 %>% inner_join(crop_land_14_def_2) %>% select(def1_fruit,parce_a_es,starts_with("def2_")) %>% 
    ### Make the aggregations
    transmute(treeonfarm_w_b=(def1_fruit+def2_cash),treesonfarm_wo_b=(def2_fruit+def2_cash),fruit_w_b=def1_fruit,fruit_wo_b=def2_fruit,cash=def2_cash,land_size_est=parce_a_es)
  
  
  
    
  
######################
####--------  Share of the land on trees on farm -- 2010-11
######################
  ######################
  ####--------  Land Size
  ######################
  
  ### Land Own
  #### 2010-11
  land_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Agriculture/AGSEC2A.dta"))
  
  land_10 <- land_10 %>% transmute(hhid=HHID,land_size=ifelse(is.na(a2aq4),a2aq5,a2aq4)) %>% group_by(hhid) %>% summarise(land_own=sum(land_size,na.rm=TRUE))
  
  #### 2011-12
  land_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Agriculture/AGSEC2A.dta"))
  land_11 <- land_11 %>% transmute(hhid=HHID,land_size=ifelse(is.na(a2aq4),a2aq5,a2aq4)) %>% group_by(hhid) %>% summarise(land_own=sum(land_size,na.rm=TRUE))
   
######################
####--------  Share of the land on trees on farm -- 2013-14
######################
  #############################################
  ###############------------- Area per crop
  #############################################
  crop_land_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC4A.dta"))
  
  crop_land_14 <- crop_land_14 %>% transmute(hhid=hh,parcel_id=parcelID,plot_id=plotID,cropID=cropID,area_crop=a4aq7,year_planted=a4aq9_2) %>% filter(!is.na(area_crop)) %>%
    mutate(
      ##### First Classification - With Bananas
      trees_a=ifelse((cropID %in% c(710,700,750,760,770,741,742,744)),"Fruit Trees",
                     ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
                            ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))),
      ##### Second Classification - With Bananas
      trees_b=ifelse((cropID %in% c(710,700,750,760,770)),"Fruit Trees",
                     ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
                            ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))))
  
  #### Building Aggregation for the first variable
  crop_land_14_def_1 <- crop_land_14 %>% group_by(hhid,parcel_id,trees_a) %>% summarise(area_crop=sum(area_crop,na.rm=TRUE)) %>% 
    ### ---> Spread the data and rename
    spread(trees_a,area_crop,fill=0) %>% 
    rename(fruit=`Fruit Trees`,other_crop=`Other Crops`,cash=`Tree Cash Crops`) %>%
    ### ---> Rename all varibles
    setNames(c(names(.)[1], paste0("def1_",names(.)[-1]))) %>% rename(parcel_id=def1_parcel_id)
  
  #### Building Aggregation for the Second variable
  crop_land_14_def_2 <- crop_land_14 %>% group_by(hhid,parcel_id,trees_b) %>% summarise(area_crop=sum(area_crop,na.rm=TRUE)) %>% 
    ### ---> Spread the data and rename
    spread(trees_b,area_crop,fill=0) %>% 
    rename(fruit=`Fruit Trees`,other_crop=`Other Crops`,cash=`Tree Cash Crops`) %>%
    ### ---> Rename all varibles
    setNames(c(names(.)[1], paste0("def2_",names(.)[-1]))) %>% rename(parcel_id=def2_parcel_id) %>% mutate(parce_a_es=(def2_fruit+def2_other_crop+def2_cash))
  
  #### Merge information
  
  crop_a_14 <- crop_land_14_def_1 %>% inner_join(crop_land_14_def_2) %>% select(def1_fruit,parce_a_es,starts_with("def2_")) %>% 
    ### Make the aggregations
    transmute(treeonfarm_w_b=(def1_fruit+def2_cash),treesonfarm_wo_b=(def2_fruit+def2_cash),fruit_w_b=def1_fruit,fruit_wo_b=def2_fruit,cash=def2_cash,land_size_est=parce_a_es)
  
  
  
  
  
  
  #############################################
  ###############------------- Land Size
  #############################################
  
  
  #####---- Land Size (Ownd)
  land_14_a <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC2A.dta"))
  
  land_14_a <- land_14_a %>% transmute(hhid=hh,parcel_id=parcelID,land_size=ifelse(is.na(a2aq4),a2aq5,a2aq4)) %>% group_by(hhid,parcel_id) %>% summarise(land_own=sum(land_size,na.rm=TRUE))

  
  #####---- Land Size (Use Rights)
  land_14_b <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC2B.dta"))
  
  land_14_b <- land_14_b %>% transmute(hhid=hh,parcel_id=parcelID,land_size=ifelse(is.na(a2bq4),a2bq5,a2bq4)) %>% group_by(hhid,parcel_id) %>% summarise(land_own=sum(land_size,na.rm=TRUE)) 
  
  #### Append Data Set
  land_14 <- rbind(land_14_a,land_14_b)
  
  #############################################
  ###############------------- Final Merge
  #############################################
  
  m_are_crop_14 <- crop_a_14 %>% left_join(land_14) %>% filter(!is.na(land_own)) %>%
    mutate(land_size=ifelse(land_size_est>land_own,land_size_est,land_own)) %>%
    #### Gen the aggregation by hiig
    group_by(hhid) %>% summarise_all(sum) %>% select(-parcel_id) %>%
    #### Build the shares
    transmute(hhid=hhid,sh_tof_w_b=treeonfarm_w_b/land_size,sh_tof_wo_b=treesonfarm_wo_b/land_size,sh_fruit_w_b=fruit_w_b/land_size,sh_fruit_wo_b=fruit_wo_b/land_size,sh_cash=cash/land_size,land_size=land_size)
  

  #############################################
  ###############------------- Area per crop
  #############################################
  
  
######################
####--------  Save Data Set
######################  
  
  #######################
  ### Fix ID - 2013-14
  #######################
  
  hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC1.dta"))
  
  fix_id_14 <- function(x) {
    #### Data
    mer <- hh_roster_14 %>% transmute(HHID_old=HHID_old,hhid=HHID) %>% filter(!duplicated(hhid))
    x <- x %>% left_join(mer) %>% mutate(hhid_05=hhid,hhid=as.character(HHID_old)) %>% select(-HHID_old)
  }
  
  ###----> 
  m_are_crop_14 <- fix_id_14(m_are_crop_14)
  
  land_11 <- land_11 %>% mutate(hhid=as.character(hhid))
  
  #### To avoid damage the information from the other
  land_05 <- m_are_crop_05
  land_14 <- m_are_crop_14
  
  #### Save Data
  save(land_05,land_10,land_11,land_14,file=paste0(save_data,"agr_controls.Rda"))
  
  
  
  
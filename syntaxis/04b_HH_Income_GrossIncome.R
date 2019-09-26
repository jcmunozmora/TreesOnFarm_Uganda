source(file="/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/Syntaxis_v1/04a_HH_Income_aux.R",verbose=FALSE)

###########################
### Function - Trees On Farm
############################


get_trees <- function(x,wave) {
  org_va=colnames(x)
  colnames(x)=c("hhid","crop_id","v")
  x$hhid <- as.character(x$hhid)
  x <- x %>% mutate(trees=ifelse((crop_id %in% c(710,700,750,760,770,741,742,744)),"Fruit Trees",
                                 ifelse(crop_id %in% c(860,820,830,810),"Tree Cash Crops",
                                        ifelse(crop_id %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops")))) %>% group_by(hhid,trees) %>% summarise(n_crop=n()) %>% mutate(wav=wave)
  return(x)
}

###########################
### Open Files
###########################

  ###################
  #### 2005 - 06
  ###################
  
  # -- Crop Level 
  crop_disp_A <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC7A.dta"))
  crop_disp_B <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC7B.dta"))
  
  crop_disp_A <- crop_disp_A %>% mutate(cropid=a7aq2b)
  crop_disp_B <- crop_disp_B %>% mutate(cropid=a7bq2b)

  land_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC2A.dta"))
  
  land_05 <- land_05 %>% transmute(hhid=HHID,parcel_id=,plot_id=land_size=ifelse(is.na(a2aq4),a2aq5,a2aq4)) %>% group_by(hhid,parcel_id,plot_id) %>% summarise(land_own=sum(land_size,na.rm=TRUE))
  
  # -----------------------
  # --- Land Information  
  # -----------------------

  
  #--- End DataSEt
  get_na_zero <- function(x) { ifelse(is.finite(x),x,0) }
  get_na_zero_div <- function(x) { ifelse(is.finite(x),ifelse(x>1,1,x),0) }

  crop_05 <- crop_disp_A %>% full_join(crop_disp_B) %>% filter(!is.na(cropid)) %>% mutate(cropID=cropid,val_sold_1=get_na_zero(a7aq5),val_sold_2=get_na_zero(a7bq5),q_harvest_1=get_na_zero(a7aq3b),q_harvest_2=get_na_zero(a7bq3b),q_self_1=get_na_zero(a7aq9),q_self_2=get_na_zero(a7bq9)) %>% transmute(hhid=HHID,cropID=cropID,val_sold=val_sold_1+val_sold_2,self_con=get_na_zero_div((q_self_1+q_self_2)/(q_harvest_1+q_harvest_2)),q_harvest=(q_harvest_1+q_harvest_2))  %>% imputeout(.,hh_roster_05)
    
  ###################
  #### 2010 - 11
  ###################

  # -- Parcel Level
  crop_disp_A <- read.dta13(paste0(path_work,"00_RawData/2010-11/Agriculture/AGSEC5A.dta"))
  crop_disp_B <- read.dta13(paste0(path_work,"00_RawData/2010-11/Agriculture/AGSEC5B.dta"))

  
  

  
  
  #--- End DataSEt
  crop_10 <- crop_disp_A %>% full_join(crop_disp_B) %>% filter(!is.na(cropID)) %>% mutate(val_sold_1=get_na_zero(a5aq8),val_sold_2=get_na_zero(a5bq8),q_harvest_1=get_na_zero(a5aq6a),q_harvest_2=get_na_zero(a5bq6a),q_self_1=get_na_zero(a5aq13),q_self_2=get_na_zero(a5bq13)) %>% transmute(hhid=HHID,cropID=cropID,val_sold=val_sold_1+val_sold_2,self_con=get_na_zero_div((q_self_1+q_self_2)/(q_harvest_1+q_harvest_2)),q_harvest=(q_harvest_1+q_harvest_2))  %>% imputeout(.,hh_roster_10)
  

  ###################
  #### 2011 - 12
  ###################
  
  # -- Parcel Level
  crop_disp_A <- read.dta13(paste0(path_work,"00_RawData/2011-12/Agriculture/AGSEC5A.dta"),convert.factors=FALSE)
  crop_disp_B <- read.dta13(paste0(path_work,"00_RawData/2011-12/Agriculture/AGSEC5B.dta"),convert.factors=FALSE)
  
  #--- End DataSEt
  crop_11 <- crop_disp_A %>% full_join(crop_disp_B) %>% filter(!is.na(cropID)) %>% mutate(val_sold_1=get_na_zero(a5aq8),val_sold_2=get_na_zero(a5bq8),q_harvest_1=get_na_zero(a5aq6a),q_harvest_2=get_na_zero(a5bq6a),q_self_1=get_na_zero(a5aq13),q_self_2=get_na_zero(a5bq13)) %>% transmute(hhid=as.character(HHID),cropID=cropID,val_sold=val_sold_1+val_sold_2,self_con=get_na_zero_div((q_self_1+q_self_2)/(q_harvest_1+q_harvest_2)),q_harvest=(q_harvest_1+q_harvest_2)) %>% imputeout(.,hh_roster_11)

  ###################
  #### 2013 - 14
  ###################
  
  # -- Parcel Level
  crop_disp_A <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC5A.dta"))
  crop_disp_B <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC5B.dta"))
  
  #--- End DataSEt
  crop_14 <- crop_disp_A %>% full_join(crop_disp_B) %>% filter(!is.na(cropID)) %>% mutate(val_sold_1=get_na_zero(a5aq8),val_sold_2=get_na_zero(a5bq8),q_harvest_1=get_na_zero(a5aq6a),q_harvest_2=get_na_zero(a5bq6a),q_self_1=get_na_zero(a5aq13),q_self_2=get_na_zero(a5bq13)) %>% transmute(hhid=hh,cropID=cropID,val_sold=val_sold_1+val_sold_2,self_con=get_na_zero_div((q_self_1+q_self_2)/(q_harvest_1+q_harvest_2)),q_harvest=(q_harvest_1+q_harvest_2))  %>% imputeout(.,hh_roster_14)
    
############################
##------- Agricultural Gross Income
############################

  #### Function Get Values 
  gross_income <- function(x) {
    ##### Include TreesOnFarm classification
    x <- x %>% mutate(trees=ifelse((cropID %in% c(710,700,750,760,770,741,742,744)),"Fruit Trees",
                                   ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
                                          ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))))
    
    ##### ----- Income
    
    #### Agricultural Income
    inc_total <- x %>% group_by(hhid) %>% summarise(inc_ag_total=sum(val_sold,na.rm=TRUE))
    #### Income per crop
    inc_crop <- x %>% group_by(hhid,trees) %>% summarise(inc_ag=sum(val_sold,na.rm=TRUE)) %>% spread(trees,inc_ag,fill=0) %>% transmute(inc_fruit=`Fruit Trees`,inc_cash=`Tree Cash Crops`,inc_other=`Other Crops`) %>% left_join(inc_total)
    
    ### Share Inncome
    est_share <- function(x) { ifelse(is.finite(x),x,0) }
    inc_crop <- inc_crop %>% mutate(share_fruit=est_share(inc_fruit/inc_ag_total),share_treesonfarm=est_share((inc_fruit+inc_cash)/inc_ag_total))
    
    ##### ----- Self Consumpion
    harv <- x %>% group_by(hhid,trees) %>% summarise(harv_q=sum(q_harvest,na.rm=TRUE)) %>% spread(trees,harv_q,fill=0) %>% transmute(harv_q_fruit=`Fruit Trees`,harv_q_cash=`Tree Cash Crops`,harv_q_other=`Other Crops`) 
    
    
    ##### ----- Self Consumpion
    sh_self <- x %>% group_by(hhid,trees) %>% summarise(self_con=mean(self_con,na.rm=TRUE)) %>% spread(trees,self_con,fill=0) %>% transmute(self_con_fruit=`Fruit Trees`,self_con_cash=`Tree Cash Crops`,self_con_other=`Other Crops`) %>% left_join(harv) %>%  left_join(inc_crop)
  
    ### Return 
    return(sh_self)
  }
  
  #### Open files
  inc_gross_agr_05 <- gross_income(crop_05)
  inc_gross_agr_10 <- gross_income(crop_10)
  inc_gross_agr_11 <- gross_income(crop_11)
  inc_gross_agr_14 <- gross_income(crop_14)

  ############################
  ##------- Fix 2014-ID
  ############################
  
  hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC1.dta"))
  
  fix_id_14 <- function(x) {
    #### Data
    mer <- hh_roster_14 %>% transmute(HHID_old=HHID_old,hhid=HHID) %>% filter(!duplicated(hhid))
    x <- x %>% ungroup(hhid) %>% left_join(mer) %>% mutate(hhid_05=hhid,hhid=as.character(HHID_old)) %>% select(-HHID_old)
  }
  
  inc_gross_agr_14 <- fix_id_14(inc_gross_agr_14)
  
  save(inc_gross_agr_05,inc_gross_agr_10,inc_gross_agr_11,inc_gross_agr_14,file=paste0(save_data,"inc_gross_inc.Rda"))
  
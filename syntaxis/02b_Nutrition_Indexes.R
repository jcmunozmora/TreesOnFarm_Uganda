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

#######################
### WHO information (source: http://www.who.int/childgrowth/standards/en/)
#######################


###### Function Get data
get_who_st <- function(file,sex_v) {
  data <- read.table(paste0(path_who,file),header=TRUE)
  data <- data %>% select(-starts_with("P")) %>% mutate(ch_sex=sex_v)
}

## height-for-age (HA) --> to moths
hfa <- rbind(get_who_st("lhfa_boys_p_exp.txt",0),get_who_st("lhfa_girls_p_exp.txt",1))
hfa <- hfa %>% mutate(ch_age_m=round(Day/30.4375,0)) %>% group_by(ch_age_m,ch_sex) %>% summarise(mean=max(M),sd=max(S),L=max(L))

## weight-for-age (WA), 
wfa <- rbind(get_who_st("wfa_boys_p_exp.txt",0),get_who_st("wfa_girls_p_exp.txt",1))
wfa <- wfa %>% mutate(ch_age_m=round(Age/30.4375,0)) %>% group_by(ch_age_m,ch_sex) %>% summarise(mean=max(M),sd=max(S),L=max(L))

## weight-for-height (WH)
wfh_0_2 <- rbind(get_who_st("tab_wfl_boys_p_0_2.txt",0),get_who_st("tab_wfl_girls_p_0_2.txt",1))
wfh_2_5 <- rbind(get_who_st("tab_wfh_boys_p_2_5.txt",0),get_who_st("tab_wfh_girls_p_2_5.txt",1))
  ### Note: i.e., weight-for- length table values should be used from 45 to 86 cm and weight-for-height table values should be used from 87 cm to 120 cm.

wfh_0_2 <- wfh_0_2 %>% filter(Length<=86) %>% transmute(ch_sex,ch_height_cm=Length,mean=M,sd=S,L=L)
wfh_2_5 <- wfh_2_5 %>% filter(Height>86) %>% transmute(ch_sex,ch_height_cm=Height,mean=M,sd=S,L=L)
wfh <- rbind(wfh_0_2,wfh_2_5)

#######################
### Fix ID - 2013-14
#######################

hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC1.dta"))

fix_id_14 <- function(x) {
  #### Data
  mer <- hh_roster_14 %>% transmute(HHID_old=HHID_old,hhid=HHID) %>% filter(!duplicated(hhid))
  x <- x %>% left_join(mer) %>% mutate(hhid_05=hhid,hhid=as.character(HHID_old)) %>% select(-HHID_old)
}

#######################
### Members
#######################

#### 00 - Trees On Farm - Dynamic (ID - OK)
load(paste0(save_data,"aux_nutr_members.Rda"))

#######################
### Antropometric Information
#######################

  ##### Individual Information Controls
  hh_roster_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC2.dta"))
hh_roster_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC2.dta"))
  hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC2.dta"))

##### Antropemetrics Information
  antr_09 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC6.dta"))
  antr_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC6A.dta"))
  antr_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC6_1.dta"))

  #####-------------
  #### 2010-2011
  ####--------------
  get_inf_09 <- antr_09 %>% 
          ### Basic Information - Childreen
        transmute(PID=PID,pid=h6q2,hhid=HHID,age_m=h6q4,height_cm=ifelse(is.na(h6q28a),h6q28b,h6q28a),weight_kg=h6q27) %>% inner_join(hh_roster_10,by="PID") %>% mutate(sex=ifelse(h2q3=="Female",1,0)) %>% select(hhid,PID,pid,age_m,height_cm,weight_kg,sex) %>%
        ### Basic Information from Main Care Giver
    left_join(membre_10,by=c("hhid","pid"),suffix = c("", "_caregiver"))
  colnames(get_inf_09) <- c("hhid","PID","pid_cg","ch_age_m","ch_height_cm","ch_weight_kg","ch_sex","mcg_age","mcg_sex","mcg_sch","mcg_read")
  
  get_inf_09 <- get_inf_09 %>% filter(!is.na(ch_height_cm)|!is.na(ch_weight_kg))
  
  #####-------------
  #### 2011-2012
  ####--------------
  get_inf_11 <- antr_11 %>% 
    ### Basic Information - Childreen
    transmute(PID=PID,pid=h6q2,hhid=HHID,age_m=h6q4,height_cm=ifelse(is.na(h6q28a),h6q28b,h6q28a),weight_kg=h6q27) %>% inner_join(hh_roster_11,by="PID") %>% mutate(sex=ifelse(h2q3=="Female",1,0)) %>% select(hhid,PID,pid,age_m,height_cm,weight_kg,sex) %>%
    ### Basic Information from Main Care Giver
    left_join(membre_10,by=c("hhid","pid"),suffix = c("", "_caregiver"))
  colnames(get_inf_11) <- c("hhid","PID","pid_cg","ch_age_m","ch_height_cm","ch_weight_kg","ch_sex","mcg_age","mcg_sex","mcg_sch","mcg_read")
  
  View(antr_11[antr_11$PID=="102100020106",])

  get_inf_11 <- get_inf_11 %>% filter(!is.na(ch_height_cm)|!is.na(ch_weight_kg))
  

  #####-------------
  #### 2013-14
  ####--------------
  
  get_inf_14 <- antr_14 %>% 
    ## Basic Information - Childreen
    transmute(PID=PID,pid=h6q2,hhid=HHID,age_m=h6q4,height_cm=ifelse(is.na(h6q28a),h6q28b,h6q28a),weight_kg=h6q27a) %>% inner_join(hh_roster_14,by="PID") %>% mutate(sex=ifelse(h2q3=="Female",1,0)) %>% select(hhid,PID,PID_Old,pid,age_m,height_cm,weight_kg,sex) %>%
    ### Basic Information from Main Care Giver
    left_join(membre_14,by=c("hhid","pid"),suffix = c("", "_caregiver"))
  
  colnames(get_inf_14) <- c("hhid","PID","PID_Old","pid_cg","ch_age_m","ch_height_cm","ch_weight_kg","ch_sex","mcg_age","mcg_sex","mcg_sch","mcg_read")
 
  get_inf_14 <- get_inf_14 %>% filter(!is.na(ch_height_cm)|!is.na(ch_weight_kg))  
    
  get_inf_14 <- fix_id_14(get_inf_14)

#######################
### Antropometric Information
#######################
  
##### Antropemetrics Information
get_z_score_age_10 <- function(data) {
  ## height-for-age (HA) --> to moths
  data_ha <- data %>% filter(!is.na(ch_height_cm)) %>% left_join(hfa,by=c("ch_age_m","ch_sex")) %>% mutate(ha_zscore=(((ch_height_cm/mean)**L-1)/(sd*L))) %>% select(hhid,PID,ha_zscore) %>% mutate(stunting=ifelse(ha_zscore>-2,0,1))
  
  ## weight-for-age (WA), 
  data_wa <- data %>% filter(!is.na(ch_weight_kg)) %>% left_join(wfa,by=c("ch_age_m","ch_sex")) %>% mutate(wa_zscore=(((ch_weight_kg/mean)**L-1)/(sd*L))) %>% select(hhid,PID,wa_zscore) %>% mutate(under_weight=ifelse(wa_zscore>-2,0,1))
  
  ## weight-for-height (WH) 
  data_wh <- data %>% filter(!is.na(ch_weight_kg)) %>% left_join(wfh,by=c("ch_height_cm","ch_sex")) %>% mutate(wh_zscore=(((ch_weight_kg/mean)**L-1)/(sd*L))) %>% select(hhid,PID,wh_zscore) %>% mutate(wasting=ifelse(wh_zscore>-2,0,1))

  ### Return
  data_end <- join_all(list(data,data_wh,data_wa,data_ha), by=c('hhid','PID'), type='full')
  
}
  

##### Antropemetrics Information
get_z_score_age_15 <- function(data) {
    ## height-for-age (HA) --> to moths
    data_ha <- data %>% filter(!is.na(ch_height_cm)) %>% left_join(hfa,by=c("ch_age_m","ch_sex")) %>% mutate(ha_zscore=(((ch_height_cm/mean)**L-1)/(sd*L))) %>% select(hhid_05,PID,ha_zscore) %>% mutate(stunting=ifelse(ha_zscore>-2,0,1))
    
    ## weight-for-age (WA), 
    data_wa <- data %>% filter(!is.na(ch_weight_kg)) %>% left_join(wfa,by=c("ch_age_m","ch_sex")) %>% mutate(wa_zscore=(((ch_weight_kg/mean)**L-1)/(sd*L))) %>% select(hhid_05,PID,wa_zscore) %>% mutate(under_weight=ifelse(wa_zscore>-2,0,1))
    
    ## weight-for-height (WH) 
    data_wh <- data %>% filter(!is.na(ch_weight_kg)) %>% left_join(wfh,by=c("ch_height_cm","ch_sex")) %>% mutate(wh_zscore=(((ch_weight_kg/mean)**L-1)/(sd*L))) %>% select(hhid_05,PID,wh_zscore) %>% mutate(wasting=ifelse(wh_zscore>-2,0,1))
    
    ### Return
    data_end <- join_all(list(data,data_wh,data_wa,data_ha), by=c('hhid_05','PID'), type='full')

  }
  
  
  # Child overweight : Weight-for-height greater than +2 standard deviations of the WHO Child Growth Standards median.
  # Child underweight : Weight-for-age less than -2 standard deviations of the WHO Child Growth Standards median.
  # Stunting : Height-for-age less than -2 standard deviations of the WHO Child Growth Standards median.
  # Wasting : Weight-for-height less than -2 standard deviations of the WHO Child Growth Standards median.
  
nutr_10 <- get_z_score_age_10(get_inf_09)
nutr_11 <- get_z_score_age_10(get_inf_11)
nutr_14 <- get_z_score_age_15(get_inf_14)

save(nutr_10,nutr_11,nutr_14,file=paste0(save_data,"nutrition.Rda"))



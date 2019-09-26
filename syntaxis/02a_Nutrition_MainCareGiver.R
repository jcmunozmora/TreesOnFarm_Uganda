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

hh_roster_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC2.dta"))
hh_roster_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC2.dta"))
hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC2.dta"))

#-- age
hh_roster_10 <- hh_roster_10 %>% mutate(age0=2011-h2q9c) %>% mutate(age=ifelse(age0<0,0,ifelse(age0==2010,1,age0)))

#-- age
hh_roster_11 <- hh_roster_11 %>% mutate(age0=2011-h2q9c) %>% mutate(age=ifelse(age0<0,0,ifelse(age0==2010,1,age0)))

#########################
#################
### Household Head Controls
#################
########################
  
  get_hh_head <- function(hh_roster) {
    ### Get 
    colnames(hh_roster)=c("hhid","per","sex","age")
    hh_roster <- hh_roster %>% mutate(pid=per,age=age,sex=ifelse(sex=="FEMALE"|sex=="Female",1,0)) %>% select(hhid,pid,age,sex)
  }
  
  hh_memb_10 <- get_hh_head(hh_roster_10[,c("HHID","PID","h2q3","age")])
  hh_memb_11 <- get_hh_head(hh_roster_11[,c("HHID","PID","h2q3","age")])
  
  #### 
  hh_memb_14 <- hh_roster_14 %>% transmute(hhid=HHID,pid=PID,age=h2q8,sex=ifelse(h2q3=="FEMALE"|h2q3=="Female",1,0))

#########################
#################
### Household Head Controls - Education
#################
########################
  
  ### 2010-11
  ind_edu_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC4.dta"))
  ### 2010-11
  ind_edu_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC4.dta"))

  ## 2014-15
  ind_edu_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC4.dta"))


  get_hh_head <- function(x) {
    colnames(x)=c("hhid","pid","edu_level","read")
    x <- x %>% transmute(hhid=hhid,pid=pid,sch=edu_level,read=ifelse(read=="unable to read and write",0,1))
  }
  
  edu_10 <- get_hh_head(ind_edu_10[,c("HHID","PID","h4q7","h4q4")])
  edu_11 <- get_hh_head(ind_edu_11[,c("HHID","PID","h4q7","h4q4")])
  edu_14 <- get_hh_head(ind_edu_14[,c("HHID","PID","h4q7","h4q4")])
  
#########################
#################
### Final - Household Controls
#################
########################

membre_10 <-  hh_memb_10 %>% left_join(edu_10) 
  
membre_11 <-  hh_memb_11 %>% left_join(edu_11) 

membre_14 <-  hh_memb_14 %>% left_join(edu_14) 
  
save(membre_10,membre_11,membre_14,file=paste0(save_data,"aux_nutr_members.Rda"))





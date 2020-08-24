##### Package
library("readstata13")
library(plyr)
library(dplyr)
library(tidyr)

### Directories
path_work <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/"
graph_path <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Paper/Graph/"
save_data <-  "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/"

#########################
#################
### General Location Characteristics
#################
########################

  ### 2010-11
  hh_gral_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Geovariables/UNPS 201011 Cons Agg.dta"))
hh_gral_10 <- hh_gral_10 %>% select(HHID,region,poor10,equiv,cpexp30,quintile)
colnames(hh_gral_10) <- c("hhid","region","poor_10","equiv","cpexp30","quintile")

  ### 2011-12
  hh_gral_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Geovariables/UNPS 2011-12 Consumption Aggregate.dta"))
  hh_gral_11 <- hh_gral_11 %>% select(HHID,poor,equiv,cpexp30)
  colnames(hh_gral_11) <- c("hhid","poor_11","equiv","cpexp30")

  ### 2013-2014
  hh_gral_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/UNPS 2013-14 Consumption Aggregate.dta"))
  hh_gral_14 <- hh_gral_14 %>% select(HHID,region,poor_13,equiv,cpexp30,quints)
  colnames(hh_gral_14) <- c("hhid","region","poor_13","equiv","cpexp30","quints")

#########################
#################
### Household Size
#################
########################

  ### Adult Equivalent Uganda:
    #The following equivalent scale is used in WB Uganda (1995):  An adult equivalent scale giving all the adults (18+) the weight one and the children a weight depending on their age.
  # 0-11 mths --> 0.27
  # 1         --> 0.37
  # 2         --> 0.45
  # 3-4       --> 0.52
  # 5-6       --> 0.62
  # 7-9       --> 0.70
  # 10-11     --> 0.73
  # 12-13     --> 0.80
  # 14-15     --> 0.88
  # 16-17     --> 0.95
  # 18+       --> 1
  ## Source: The following equivalent scale is used in WB Uganda (1995) - https://www.wider.unu.edu/sites/default/files/WIID/PDF/Uganda.pdf

  hh_roster_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Household/GSEC2.dta"))
  hh_roster_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC2.dta"))
  hh_roster_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC2.dta"))
  hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC2.dta"))

## hhsize
  get_hhsize <- function(hh_roster) {
    colnames(hh_roster)=c("hhid","age")
    hh_roster <- hh_roster %>% group_by(hhid,age) %>% summarise(n_per=n()) %>% mutate(mem_ae=
    ifelse(age==0,n_per*0.27,
      ifelse(age==1,n_per*0.37,
        ifelse(age==2,n_per*0.45,
          ifelse(age==3|age==4,n_per*0.52,
            ifelse(age==5|age==6,n_per*0.62,
              ifelse(age==7|age==8|age==9,n_per*0.7,
                ifelse(age==10|age==11,n_per*0.73,
                  ifelse(age==12|age==13,n_per*0.80,
                    ifelse(age==14|age==15,n_per*0.88,
                      ifelse(age==16|age==17,n_per*0.95,n_per))))))))))) %>%
                        group_by(hhid) %>% summarise(hhsize=sum(n_per,na.rm=TRUE),hhsize_ae=sum(mem_ae,na.rm=TRUE))
  }

##### hhsize
  hh_roster_05 <- mutate(hh_roster_05,age=2006-h2q8c)
  hhsize_05 <- get_hhsize(hh_roster_05[,c("HHID","age")])

  hh_roster_10 <- hh_roster_10 %>% mutate(age0=2011-h2q9c) %>% mutate(age=ifelse(age0<0,0,ifelse(age0==2010,1,age0)))
  hhsize_10 <- get_hhsize(hh_roster_10[,c("HHID","age")])

  hh_roster_11 <- hh_roster_11 %>% mutate(age0=2011-h2q9c) %>% mutate(age=ifelse(age0<0,0,ifelse(age0==2010,1,age0)))
  hhsize_11 <- get_hhsize(hh_roster_11[,c("HHID","age")])

  hhsize_14 <- get_hhsize(hh_roster_14[,c("HHID","h2q8")])

#########################
#################
### Dependency Ratio ()
#################
########################

  get_dependecy_ratio <- function(hh_roster) {
      ### Get
      colnames(hh_roster)=c("hhid","age")
      hh_roster1 <- hh_roster %>% mutate(age_w=ifelse(age>=15 & age<=65,1,0),age_nw=ifelse(age<15|age>65,1,0)) %>% group_by(hhid) %>% summarise(nw=sum(age_nw,na.rm=TRUE),w=sum(age_w,na.rm=TRUE)) %>% transmute(hhid=hhid,dep_ratio=ifelse(is.finite(nw/w),nw/w,1))
  }

  dep_r_05 <- get_dependecy_ratio(hh_roster_05[,c("HHID","age")])
  dep_r_10 <- get_dependecy_ratio(hh_roster_10[,c("HHID","age")])
  dep_r_11 <- get_dependecy_ratio(hh_roster_11[,c("HHID","age")])
  dep_r_14 <- get_dependecy_ratio(hh_roster_14[,c("HHID","h2q8")])

#########################
#################
### Household Head Controls
#################
########################

  get_hh_head <- function(hh_roster) {
    ### Get
    colnames(hh_roster)=c("hhid","per","sex","age")
    hh_roster <- hh_roster %>% filter(per==1) %>% mutate(hh_head_age=age,hh_head_sex=ifelse(sex=="FEMALE"|sex=="Female",1,0)) %>% select(hhid,hh_head_age,hh_head_sex)
  }

  hh_head_05 <- get_hh_head(hh_roster_05[,c("HHID","pid","h2q4","age")])
  hh_head_10 <- get_hh_head(hh_roster_10[,c("HHID","h2q1","h2q3","age")])
  hh_head_11 <- get_hh_head(hh_roster_11[,c("HHID","h2q1","h2q3","age")])
  hh_head_14 <- get_hh_head(hh_roster_14[,c("HHID","h2q1","h2q3","h2q8")])

#########################
#################
### Household Head Controls - Education
#################
########################

  ### 2004-05
  ind_edu_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Household/GSEC4.dta"))

  ### 2010-11
  ind_edu_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC4.dta"))
  ind_edu_10 <- hh_roster_10 %>% mutate(pid=h2q1) %>% select(HHID,PID,pid) %>% left_join(ind_edu_10)

  ### 2011-12
  ind_edu_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC4.dta"))
  ind_edu_11 <- hh_roster_11 %>% mutate(pid=h2q1) %>% select(HHID,PID,pid) %>% left_join(ind_edu_11)

  ## 2014-15
  ind_edu_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC4.dta"))
  ind_edu_14 <- hh_roster_14 %>% mutate(pid=h2q1) %>% select(HHID,PID,pid) %>% left_join(ind_edu_14)

  get_hh_head <- function(x) {
    colnames(x)=c("hhid","pid","edu_level","read")
    x <- x %>% filter(pid==1) %>% transmute(hhid=hhid,hh_head_sch=edu_level,hh_head_read=ifelse(read=="unable to read and write",0,1))
  }

  edu_05 <- get_hh_head(ind_edu_05[,c("HHID","pid","h4q4","h4q12")])
  edu_10 <- get_hh_head(ind_edu_10[,c("HHID","pid","h4q7","h4q4")])
  edu_11 <- get_hh_head(ind_edu_11[,c("HHID","pid","h4q7","h4q4")])
  edu_14 <- get_hh_head(ind_edu_14[,c("HHID","pid","h4q7","h4q4")])

#########################
#################
### Household Charcteristis
#################
########################

  ind_edu_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Household/GSEC4.dta"))
  hh_house_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC9A.dta"))

  hh_house_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC9A.dta"))


#################
### Household Charcteristis
#################
########################

  hh_int_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Household/GSEC1.dta"))

  hh_int_05 <- hh_int_05 %>% transmute(hhid=HHID,comm=comm,stratum=stratum,urban=urban,int_month=h1bq2b,int_year=h1bq2c)

  hh_int_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC1.dta"))

  hh_int_10 <- hh_int_10 %>% transmute(hhid=HHID,comm=comm,stratum=region,urban=ifelse(urban=="Urban",1,0),int_month=month,int_year=year)

  hh_int_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC1.dta"))

  hh_int_11 <- hh_int_11 %>% transmute(hhid=HHID,comm=comm,stratum=region,urban=ifelse(urban=="Urban",1,0),int_month=month,int_year=year)

  hh_int_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC1.dta"))

  hh_int_14 <- hh_int_14 %>% transmute(hhid_05=HHID,hhid=as.character(HHID_old),ea=h1aq1a,stratum=region,urban=ifelse(urban=="Urban",1,0),int_month=month,int_year=year)


#########################
#### Fix ID 2013-14
#########################

  fix_id_14 <- function(x) {
    #### Data
    mer <- hh_roster_14 %>% transmute(HHID_old=HHID_old,hhid=HHID) %>% filter(!duplicated(hhid))
    x <- x %>% left_join(mer) %>% mutate(hhid_05=hhid,hhid=as.character(HHID_old)) %>% select(-HHID_old)
  }

  ### Fix IND
  hh_gral_14 <- fix_id_14(hh_gral_14)
  edu_14 <- fix_id_14(edu_14)
  hhsize_14 <- fix_id_14(hhsize_14)
  dep_r_14 <- fix_id_14(dep_r_14)
  hh_head_14 <- fix_id_14(hh_head_14)

#########################
#################
### Final - Household Controls
#################
########################

  hhcontrols_05 <-  hhsize_05 %>% left_join(dep_r_05) %>% left_join(hh_head_05) %>% left_join(edu_05) %>% left_join(hh_int_05)

  hhcontrols_10 <-  hhsize_10 %>% left_join(dep_r_10) %>% left_join(hh_head_10) %>% left_join(edu_10) %>% left_join(hh_gral_10) %>% left_join(hh_int_10)

  hhcontrols_11 <-  hhsize_11 %>% left_join(dep_r_11) %>% left_join(hh_head_11) %>% left_join(edu_11)  %>% left_join(hh_gral_11) %>%  left_join(hh_int_11)

hhcontrols_14 <-  hhsize_14 %>% left_join(dep_r_14,by=c("hhid_05","hhid")) %>% left_join(hh_head_14,by=c("hhid_05","hhid")) %>% left_join(edu_14,by=c("hhid_05","hhid")) %>% left_join(hh_gral_14,by=c("hhid_05","hhid")) %>% left_join(hh_int_14,by=c("hhid_05","hhid"))

save(hhcontrols_05,hhcontrols_10,hhcontrols_11,hhcontrols_14,file=paste0(save_data,"hhcontrols.Rda"))

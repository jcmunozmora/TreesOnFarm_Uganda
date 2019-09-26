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
###  Expenditures
############################

  #-------------
  ##### 2005-2006
  #-------------
  exp_food_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Household/GSEC14A.dta"),convert.factors = FALSE)
  exp_nondurable_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Household/GSEC14B.dta"))
  exp_durable_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Household/GSEC14C.dta"))

  #-------------
  ##### 2010-2011
  #-------------
  exp_food_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC15b.dta"),convert.factors = FALSE)
  exp_nondurable_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC15c.dta"))
  exp_durable_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC15d.dta"))
  
  #-------------
  ##### 2011-2012
  #-------------
  exp_food_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC15b.dta"),convert.factors = FALSE)
  exp_nondurable_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC15c.dta"))
  exp_durable_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC15d.dta"))
  
  #-------------
  ##### 2013-14
  #-------------
  exp_food_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC15b.dta"))
  exp_nondurable_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC15c.dta"))
  exp_durable_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC15d.dta"))

###########################
## Imputation 
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
  
###########################
###  Food Expenditures
############################
  
  ##--- Food Expenditure
get_exp_food <- function(x,day) {
  colnames(x) <- c("hhid","q_pur","q_aw","q_ow","q_fre","p_pur","p_aw","p_ow","p_fre")
  x <- x %>% group_by(hhid) %>%
    summarize(quantitypurch=sum(q_pur,na.rm=TRUE),
              quantityaway=sum(q_aw,na.rm=TRUE),
              quantityown=sum(q_ow,na.rm=TRUE),
              quantityfree=sum(q_fre,na.rm=TRUE),
              valuepurch=sum(p_pur,na.rm=TRUE),
              valueaway=sum(p_aw,na.rm=TRUE),
              valueown=sum(p_ow,na.rm=TRUE),
              valuefree=sum(p_fre,na.rm=TRUE)) %>% transmute(hhid=as.character(hhid),exp_food=(valuepurch+valueaway+valueown+valuefree)*day,exp_food_pur=valuepurch,exp_food_away=valueaway,exp_food_own=valueown,exp_food_free=valuefree)
  return(x)
}
  
  ##--- Imputation Ins
  imputeout <- function(var,hh_region) {
    org_na <- colnames(var)
    #### Gen the Avarege per Region
    colnames(hh_region)=c("hhid","region")
    colnames(var)=c("hhid","var1")
    ### Over Variables
    var_reg <- var %>% inner_join(hh_region) %>% group_by(region) %>% summarise(sd=sd(var1,na.rm=TRUE),med=median(var1,na.rm=TRUE))
    #### Change Value
    var <- var %>% inner_join(hh_region) %>% inner_join(var_reg) %>% mutate(imp=ifelse(var1<(med-3*sd)|var1>(med+3*sd),med,var1)) %>% select(-region,-med,-sd,-var1)
    #### Labelts
    colnames(var) <- org_na
    return(var)
  }
  
  #### Inputation Food
  get_food <- function(food1,dat) {
    food <- food1
    exp_food <- imputeout(food[,c("hhid","exp_food")],dat)
    exp_food_pur <- imputeout(food[,c("hhid","exp_food_pur")],dat)
    exp_food_away <- imputeout(food[,c("hhid","exp_food_away")],dat)
    exp_food_own <- imputeout(food[,c("hhid","exp_food_own")],dat)
    exp_food_free <- imputeout(food[,c("hhid","exp_food_free")],dat)
    ### end
    end <- join_all(list(exp_food,exp_food_pur,exp_food_away,exp_food_own,exp_food_free),by="hhid",type="left")
    return(end)
  }
  
  ### Food
  food_05 <- get_food(get_exp_food(exp_food_05[,c("HHID","h14aq4","h14aq6","h14aq8","h14aq10","h14aq5","h14aq7","h14aq9","h14aq11")],52),hh_roster_05)
  
  food_10 <-get_food(get_exp_food(exp_food_10[,c("hh","h15bq4","h15bq6","h15bq8","h15bq10","h15bq5","h15bq7","h15bq9","h15bq11")],52),hh_roster_10)
  food_11 <-get_food(get_exp_food(exp_food_11[,c("HHID","h15bq4","h15bq6","h15bq8","h15bq10","h15bq5","h15bq7","h15bq9","h15bq11")],52),hh_roster_11)

  food_14 <-get_food(get_exp_food(exp_food_14[,c("HHID","h15bq4","h15bq6","h15bq8","h15bq10","h15bq5","h15bq7","h15bq9","h15bq11")],52),hh_roster_14)

  
###########################
###  Food Expenditures -- Futures
############################

fun_food <- function(exp_cat) {
  
  #### 
  get_beverage <- exp_cat %>% mutate(food_type=ifelse(item_code %in% c(148,149,151,152,153,154,155,156,160,161,175),
  
  get_cereals <- c(110,111,112,113,114,115,116,172,173,190,191)
  
  get_fruit <- c(130,132,133,134,169,170,171,174)
  
  get_meat <- c(117,118,119,120,121,122,123,124)
  
  get_milk <- c(125, 126)
  
  get_nuts <- c(142,143,144,146,163)
  
  get_oil <- c(150,127,128,129)
  
  get_out_food <- c(157,158,159)
  
  get_pulses  <- c(162,140,141,145)
  
  get_starches <- c(101,102,103,104,105,106,107,108,109,131,180,181,182)
  
  get_sugar <- c(147)
  
  get_vegetables <- c(135,136,137,138,139,164,165,166,167,168)

}
  
###########################
###  No-durable Expenditures
############################
  
  ##--- 
  get_exp_nondurable <- function(x,name,day) {
    colnames(x) <- c("hhid","q_pur","q_ow","q_fre","p_pur","p_ow","p_fre")
    x <- x %>% group_by(hhid) %>%
      summarize(quantitypurch=sum(q_pur,na.rm=TRUE),
                quantityown=sum(q_ow,na.rm=TRUE),
                quantityfree=sum(q_fre,na.rm=TRUE),
                valuepurch=sum(p_pur,na.rm=TRUE),
                valueown=sum(p_ow,na.rm=TRUE),
                valuefree=sum(p_fre,na.rm=TRUE)) %>% transmute(hhid=as.character(hhid),exp=(valuepurch+valueown+valuefree)*day)
    colnames(x) <- c("hhid",name)
    return(x)
  }
  
  ### No Durables
  
  nodu_05 <-imputeout(get_exp_nondurable(exp_nondurable_05[,c("HHID","h14bq4","h14bq6","h14bq8","h14bq5","h14bq7","h14bq9")],"exp_nodurable",12),hh_roster_05)
  
  nodu_10 <-imputeout(get_exp_nondurable(exp_nondurable_10[,c("hh","h15cq4","h15cq6","h15cq8","h15cq5","h15cq7","h15cq9")],"exp_nodurable",12),hh_roster_10)
  
  nodu_11 <-imputeout(get_exp_nondurable(exp_nondurable_11[,c("HHID","h15cq4","h15cq6","h15cq8","h15cq5","h15cq7","h15cq9")],"exp_nodurable",12),hh_roster_11)
  
  nodu_14 <-imputeout(get_exp_nondurable(exp_nondurable_14[,c("HHID","h15cq4","h15cq6","h15cq8","h15cq5","h15cq7","h15cq9")],"exp_nodurable",12),hh_roster_14) 
  
  
###########################
###  Durables
############################
  
  ##--- Nodurable
  ##--- 
  get_exp_durable <- function(x,name,day) {
    colnames(x) <- c("hhid","p_pur","p_ow","p_fre")
    x <- x %>% group_by(hhid) %>%
      summarize(valuepurch=sum(p_pur,na.rm=TRUE),
                valueown=sum(p_ow,na.rm=TRUE),
                valuefree=sum(p_fre,na.rm=TRUE)) %>% transmute(hhid=as.character(hhid),exp=(valuepurch+valueown+valuefree)*day)
    colnames(x) <- c("hhid",name)
    return(x)
  }
  
  ### No Durables
  
  du_05 <-imputeout(get_exp_durable(exp_durable_05[,c("HHID","h14cq3","h14cq4","h14cq5")],"exp_durable",1),hh_roster_05)
  
  du_10 <-imputeout(get_exp_durable(exp_durable_10[,c("hh","h15dq5","h15dq7","h15dq9")],"exp_durable",1),hh_roster_10)
  
  du_11 <-imputeout(get_exp_durable(exp_durable_11[,c("HHID","h15dq5","h15dq7","h15dq9")],"exp_durable",1),hh_roster_11)
  
  du_14 <-imputeout(get_exp_durable(exp_durable_14[,c("HHID","h15dq3","h15dq4","h15dq5")],"exp_durable",1),hh_roster_14)


###########################
###  expenditures
############################
  
  # 05
  exp_05 <- join_all(list(food_05,nodu_05,du_05),by="hhid",type="full") %>% mutate(tot_exp=(exp_food+exp_nodurable+exp_durable))
  
  # 10-11
  exp_10 <- join_all(list(food_10,nodu_10,du_10),by="hhid",type="full") %>% mutate(tot_exp=(exp_food+exp_nodurable+exp_durable))
  exp_10$hhid <- as.character(exp_10$hhid)
  
  # 11-12
  exp_11 <- join_all(list(food_11,nodu_11,du_11),by="hhid",type="full") %>% mutate(tot_exp=(exp_food+exp_nodurable+exp_durable))
  exp_10$hhid <- as.character(exp_10$hhid)
  
  # 13-14
  exp_14 <- join_all(list(food_14,nodu_14,du_14),by="hhid",type="full") %>% mutate(tot_exp=(exp_food+exp_nodurable+exp_durable))


  ############################
  ##------- Fix 2014-ID
  ############################
  
  hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC1.dta"))
  
  fix_id_14 <- function(x) {
    #### Data
    mer <- hh_roster_14 %>% transmute(HHID_old=HHID_old,hhid=HHID) %>% filter(!duplicated(hhid))
    x <- x %>% left_join(mer) %>% mutate(hhid_05=hhid,hhid=as.character(HHID_old)) %>% select(-HHID_old)
  }
  exp_14 <- fix_id_14(exp_14)
  
  save(exp_05,exp_10,exp_11,exp_14,file=paste0(save_data,"expenditure.Rda"))

  
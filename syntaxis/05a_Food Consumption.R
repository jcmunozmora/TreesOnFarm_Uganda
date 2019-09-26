##### Package
library("readstata13")
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)

### Directories
path_work <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/"
graph_path <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Paper/Graph/"
save_data <-  "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/"
path_who <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/inputs/"

###########################
###  Food Consumption - 2014 
############################

  exp_food_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Household/GSEC14A.dta"),convert.factors = FALSE)
  exp_food_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC15b.dta"),convert.factors = FALSE)
  exp_food_11 <- read.dta13(paste0(path_work,"00_RawData/2011-12/Household/GSEC15b.dta"),convert.factors = FALSE)
  exp_food_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC15b.dta"),convert.factors = FALSE)

###########################
###  Conversition Units to grams
############################
  qt_cov <- read_excel("/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/Conversion_Food_List.xlsx",sheet="Quantity_Conver")
  food_list <- read_excel("/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/Conversion_Food_List.xlsx",sheet="Food_Code")
  food_cat <- read_excel("/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/Conversion_Food_List.xlsx",sheet="Food_Code_Cat")
  
  ### Food Composition 
  food_comp <- read_excel("/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/Conversion_Food_List.xlsx",sheet="Food_Composition")
  
  food_comp <- food_comp %>% group_by(food_group) %>% summarise(energy_kcal_mean=mean(energy_kcal,na.rm=TRUE))
  
###########################
###  Food Expenditures -- Futures
############################
  

  fun_food <- function(x) {
    
    colnames(x) <- c("hhid","item_code","unit_type","q_pur","q_aw","q_ow","q_fre","p_pur","p_aw","p_ow","p_fre")
    
    ### Prepare data
    get_val <- function(x) {ifelse(is.finite(x),x,0)}
    exp_cat <- x %>% 
      #### Gen data
      transmute(hhid=hhid,item_code=item_code,unit_type=unit_type,qt=(get_val(q_pur)+get_val(q_aw)+get_val(q_ow)+get_val(q_fre)),vl=(get_val(p_pur)+get_val(p_aw)+get_val(p_ow)+get_val(p_fre))) %>% filter(qt>0) %>% filter(!is.na(unit_type)) %>%
      ###### Join whith food categories
      left_join(food_cat) %>%
      ###### Join Calories in take
      inner_join(food_comp) %>%
      #### Join with conversion rate
      left_join(qt_cov) %>% 
      #### Join with specific type-food
      left_join(food_list) %>% 
      #### Gen gr_Equivalente for the aggreate
      mutate(gr_eqv=ifelse(is.finite(Equiv_grm_0),Equiv_grm_0,gr_unit)) %>%
    filter(gr_eqv>0) %>%
      ##### Collapse nformation
      group_by(hhid,food_group) %>% 
      ### Gen Variables
      summarise(intake_kcal=sum(((qt*gr_eqv)/100)*energy_kcal_mean,na.rm=TRUE)) %>%
      ### Spread
      spread(food_group,intake_kcal,fill=0) %>%
      ### mutate
      transmute(itk_kcal_beverage=BEVERAGES,itk_kcal_beans=`BEANS, NUTS, AND SEEDS`,itk_kcal_fats=`FATS AND OILS`,itk_kcal_fruits=`FRUITS AND FRESH/PURE FRUIT JUICES`,itk_kcal_grain=`GRAINS AND GRAIN PRODUCTS`,itk_kcal_meat=`MEATS, POULTRY, AND INSECTS`,itk_kcal_milk=`MILK AND DAIRY`,itk_kcal_root=`ROOTS AND TUBERS`,itk_kcal_sugar=`SUGARS AND SWEETS`,itk_kcal_vegta=`VEGETABLES`) %>%
    #### Gen Total total itk_kcal
    mutate(itk_kcal_total=itk_kcal_beverage+itk_kcal_beans+itk_kcal_fats+itk_kcal_fruits+itk_kcal_grain+itk_kcal_meat+itk_kcal_milk+itk_kcal_root+itk_kcal_sugar+itk_kcal_vegta)
    
  }

###########################
###  Food Data
############################

itk_kcal_05 <-  fun_food(exp_food_05[,c("HHID","h14aq2","h14aq3","h14aq4","h14aq6","h14aq8","h14aq10","h14aq5","h14aq7","h14aq9","h14aq11")])
  
itk_kcal_10 <-  fun_food(exp_food_10[,c("hh","itmcd","untcd","h15bq4","h15bq6","h15bq8","h15bq10","h15bq5","h15bq7","h15bq9","h15bq11")])  
  
itk_kcal_11 <-  fun_food(exp_food_11[,c("HHID","itmcd","untcd","h15bq4","h15bq6","h15bq8","h15bq10","h15bq5","h15bq7","h15bq9","h15bq11")]) 

itk_kcal_14 <-  fun_food(exp_food_14[,c("HHID","itmcd","untcd","h15bq4","h15bq6","h15bq8","h15bq10","h15bq5","h15bq7","h15bq9","h15bq11")])
##### Values
  
###########################
###  Save Data
############################

hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC1.dta"))

fix_id_14 <- function(x) {
  #### Data
  mer <- hh_roster_14 %>% transmute(HHID_old=HHID_old,hhid=HHID) %>% filter(!duplicated(hhid))
  x <- x %>% left_join(mer) %>% ungroup(hhid) %>% mutate(hhid_05=hhid,hhid=as.character(HHID_old)) %>% select(-HHID_old)
}
itk_kcal_14 <- fix_id_14(itk_kcal_14)

save(itk_kcal_05,itk_kcal_10,itk_kcal_11,itk_kcal_14,file=paste0(save_data,"calorie_intake.Rda"))
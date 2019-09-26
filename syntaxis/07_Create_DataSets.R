##################
##### Package
#################

###--- Data Managment
  library(readstata13)
  library(plyr)
  library(dplyr)
  library(tidyr)
  ###--- OLS
  library(stargazer) # Get tables print
  library(lmtest)
  library(multiwayvcov)
  library(sandwich)
  
  ### Directories
  path_work <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/"
  graph_path <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Paper/Graph/"
  save_data <-  "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/"
  path_who <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/inputs/"
  tables_path <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/06_Paper/tablets/"
  graph_path <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/06_Paper/Graph/"

##################
##### Load Data
#################
  
  #### 00 - Trees On Farm - Dynamic (ID - OK)
  load(paste0(save_data,"treesonfarm.Rda"))
  
  #### 01 - Households Controls (ID - OK)
  load(paste0(save_data,"hhcontrols.Rda"))
  
  #### 02 - Nutrition (ID - OK)
  load(paste0(save_data,"nutrition.Rda"))
  
  #### 03 - Agricultural Controls ()||@|… 
  load(paste0(save_data,"agr_controls.Rda"))
  
  #### 04 - Gross Agricultural Income
  load(paste0(save_data,"inc_gross_inc.Rda"))
  
    ### Test DataSet
    f <- function(x) { ifelse(x>0,1,0)}
    
    get_share <- function(inc_gross_agr_05) {inc_gross_agr_05 %>% ungroup(hhid) %>%  transmute(fruit=f(harv_q_fruit),cash=f(harv_q_cash)) %>% summarise(share_fruit=sum(fruit)/n(),share_cash=sum(cash)/n())}
  
    get_share(inc_gross_agr_05)
    get_share(inc_gross_agr_10)
    get_share(inc_gross_agr_11)
    get_share(inc_gross_agr_14)
    
    
  #### 05 - Expenditures
  load(paste0(save_data,"expenditure.Rda"))
  
  #### 05a - Calorie Intake
  load(paste0(save_data,"calorie_intake.Rda"))
  
  #### 06 - Livestock
  load(paste0(save_data,"livestock.Rda"))
  
##################
##### Build Data Sets
#################
  repl_na <- function(x) { replace(x,is.na(x),0) }
  
  mer_data <- function(hhcontrols_05,land_05,exp_05,itk_kcal_05,inc_gross_agr_05,lvst_05) {
  data05_hh <- hhcontrols_05 %>% 
    ### Land
    left_join(land_05) %>% mutate(land_size=repl_na(land_size),sh_tof_w_b=repl_na(sh_tof_w_b),sh_tof_wo_b=repl_na(sh_tof_wo_b),sh_fruit_w_b=repl_na(sh_fruit_w_b),sh_fruit_wo_b=repl_na(sh_fruit_wo_b),sh_cash=repl_na(sh_cash)) %>%
    ## Expenditure
    left_join(exp_05) %>% mutate(exp_food=repl_na(exp_food),exp_food_pur=repl_na(exp_food_pur),exp_food_away=repl_na(exp_food_away),exp_food_own=repl_na(exp_food_own),exp_food_free=repl_na(exp_food_free),exp_nodurable=repl_na(exp_nodurable),exp_durable=repl_na(exp_durable),tot_exp=repl_na(tot_exp)) %>%
    ### Calorie intake
    left_join(itk_kcal_05) %>% mutate(itk_kcal_beverage=repl_na(itk_kcal_beverage),itk_kcal_beans=repl_na(itk_kcal_beans),itk_kcal_fats=repl_na(itk_kcal_fats),itk_kcal_fruits=repl_na(itk_kcal_fruits),itk_kcal_grain=repl_na(itk_kcal_grain),itk_kcal_meat=repl_na(itk_kcal_meat),itk_kcal_milk=repl_na(itk_kcal_milk),itk_kcal_root=repl_na(itk_kcal_root),itk_kcal_sugar=repl_na(itk_kcal_sugar),itk_kcal_vegta=repl_na(itk_kcal_vegta),itk_kcal_total=repl_na(itk_kcal_total)) %>%
    ### Income Agricultaure
    left_join(inc_gross_agr_05) %>% mutate(self_con_fruit=repl_na(self_con_fruit),self_con_cash=repl_na(self_con_cash),self_con_other=repl_na(self_con_other),inc_fruit=repl_na(inc_fruit),inc_cash=repl_na(inc_cash),inc_other=repl_na(inc_other),inc_ag_total=repl_na(inc_ag_total),share_fruit=repl_na(share_fruit),share_treesonfarm=repl_na(share_treesonfarm)) %>%
    ### Livestock
    left_join(lvst_05) %>% mutate(lvst_large=repl_na(lvst_large),lvst_small=repl_na(lvst_small),lvst_poul=repl_na(lvst_poul))
    ### Return
    return(data05_hh)
  }
      
  ### Households Level Data Sets
  data05_hh <- mer_data(hhcontrols_05,land_05,exp_05,itk_kcal_05,inc_gross_agr_05,lvst_05)
  itk_kcal_10$hhid <- as.character(itk_kcal_10$hhid)
  # data10_hh <- mer_data(hhcontrols_10,land_10,exp_10,itk_kcal_10,inc_gross_agr_10,lvst_10)
  #data11_hh <- mer_data(hhcontrols_11,land_11,exp_11,itk_kcal_11,inc_gross_agr_11,lvst_11)
  data14_hh <- mer_data(hhcontrols_14,land_14,exp_14,itk_kcal_14,inc_gross_agr_14,lvst_14)
  
  ### Households Level Data Sets
  #data10_nutr <- nutr_10 %>% select(-pid_cg) %>% inner_join(data10_hh)
  #data11_nutr <- nutr_11 %>% select(-pid_cg) %>% inner_join(data11_hh)
  nutr_14$PID_Old[nchar(nutr_14$PID_Old)==0]="0"
  data14_nutr <- nutr_14 %>% inner_join(data14_hh)
  
  #### Save Data in Stata
  library(foreign)
  write.dta(data05_hh,paste0(save_data,"data05_hh.dta"))
  
  #write.dta(data10_hh,paste0(save_data,"data10_hh.dta"))
  #write.dta(data10_nutr,paste0(save_data,"data10_nutr.dta"))
  
  #write.dta(data11_hh,paste0(save_data,"data11_hh.dta"))
  #write.dta(data11_nutr,paste0(save_data,"data11_nutr.dta"))
  
  write.dta(data14_hh,paste0(save_data,"data14_hh.dta"))
  write.dta(data14_nutr,paste0(save_data,"data14_nutr.dta"))
  
  #save(data05_hh,data10_hh,data14_hh,data10_nutr,data14_nutr,file=paste0(save_data,"TreesOnFarm_Uganda_04012017.Rda"))
  
  save(data05_hh,data14_hh,file=paste0(save_data,"TreesOnFarm_Uganda_12012017.Rda"))

  
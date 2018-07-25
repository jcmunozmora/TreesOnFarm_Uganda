##################
##### Package
#################

###--- Data Managment
  library(readstata13)
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(foreign) ## Export Data 
  
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
  
  #### 05 - Expenditures
  load(paste0(save_data,"expenditure.Rda"))
  
  #### 05a - Calorie Intake
  load(paste0(save_data,"calorie_intake.Rda"))
    ### Preliminary Work
    itk_kcal_10$hhid <- as.character(itk_kcal_10$hhid)
  
  #### 06 - Livestock
  load(paste0(save_data,"livestock.Rda"))
  
##################
##### Preliminary - 
#################
  
     ### Test DataSet
    f <- function(x) { ifelse(x>0,1,0)}
    
    get_share <- function(inc_gross_agr_05) {inc_gross_agr_05 %>% ungroup(hhid) %>%  transmute(fruit=f(harv_q_fruit),cash=f(harv_q_cash)) %>% summarise(share_fruit=sum(fruit)/n(),share_cash=sum(cash)/n())}
  
    get_share(inc_gross_agr_05)
    get_share(inc_gross_agr_10)
    get_share(inc_gross_agr_11)
    get_share(inc_gross_agr_14)
    
##################
##### Build Data Sets
#################
  
    #### Function 2: Merge Data Set
  mer_data <- function(text,defl) {

    #### Variable
    ##-------- MAIN --
    data05_hh <- eval(parse(text =paste0("hhcontrols_",text))) %>% 
    ### Land
    left_join(eval(parse(text =paste0("land_",text)))) %>% 
    ## Expenditure 
    left_join(eval(parse(text =paste0("exp_",text)))) %>%
    ### Calorie intake
    left_join(eval(parse(text =paste0("itk_kcal_",text)))) %>%
    ### Income Agriculture
    left_join(eval(parse(text =paste0("inc_gross_agr_",text)))) %>%
    ### Livestock
    left_join(eval(parse(text =paste0("lvst_",text)))) %>%
    ### Clean Data
    mutate_at(vars(sh_tof_w_b:lvst_poul),funs(((function(x){replace(x,is.na(x),0)})(.)))) %>%
    #### Deflacted data
      mutate_at(vars(exp_food:tot_exp,inc_fruit:inc_ag_total),funs(((function(x){x *defl})(.)))) %>%
    #### Main variblaes
      mutate(hhsize_ae=ifelse(hhsize_ae==0,hhsize,hhsize_ae)) %>%
    ### Expenditure in Foor Per Capita
      mutate(exp_food=exp_food_pur+exp_food_away+exp_food_own+exp_food_free) %>%
       mutate_at(vars(exp_food:tot_exp),funs(((function(x){x/hhsize_ae})(.)))) %>%
    ### Create the WAve Variable
      mutate(wave=text)
    
    return(data05_hh)
  }
   
    
  ### Households Level Data Sets
  panel <- rbind(mer_data("05",1.520308938),mer_data("10",1),mer_data("11",1.094444912),mer_data("14",0.914632051))
  
  colnames(mer_data("05",1.520308938))
  colnames(mer_data("10",1.520308938))
  
  ### Panel data
  write.dta(panel,paste0(save_data,"Uganda_Panel_06282018.dta"))

  
  ### Households Level Data Sets
  #data10_nutr <- nutr_10 %>% select(-pid_cg) %>% inner_join(data10_hh)
  #data11_nutr <- nutr_11 %>% select(-pid_cg) %>% inner_join(data11_hh)
  nutr_14$PID_Old[nchar(nutr_14$PID_Old)==0]="0"
  data14_nutr <- nutr_14 %>% inner_join(data14_hh)
  
  #### Save Data in Stata
  
  write.dta(data05_hh,paste0(save_data,"data05_hh.dta"))
  
  #write.dta(data10_hh,paste0(save_data,"data10_hh.dta"))
  #write.dta(data10_nutr,paste0(save_data,"data10_nutr.dta"))
  
  #write.dta(data11_hh,paste0(save_data,"data11_hh.dta"))
  #write.dta(data11_nutr,paste0(save_data,"data11_nutr.dta"))
  
  write.dta(data14_hh,paste0(save_data,"data14_hh.dta"))
  write.dta(data14_nutr,paste0(save_data,"data14_nutr.dta"))
  
  #save(data05_hh,data10_hh,data14_hh,data10_nutr,data14_nutr,file=paste0(save_data,"TreesOnFarm_Uganda_04012017.Rda"))
  
  save(data05_hh,data14_hh,file=paste0(save_data,"TreesOnFarm_Uganda_12012017.Rda"))

  
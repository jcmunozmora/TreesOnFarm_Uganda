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

path_work <- "/Users/juan-carlosm/Dropbox/Documents/Projects_papers/2019/Munoz-etal_2019/"
graph_path <- paste0(path_work,"paper/graph/")
save_data <-  paste0(path_work,"01_DataSets/")
path_who <- paste0(save_data,"inputs/")


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
  load(paste0(save_data,"TreesOnFarm_Uganda_04012017.Rda"))
      
  
##################
##### Define Controls
#################

### Households Controls "dep_ratio"

  ### Trees On Farm 
  treesonfarm <- c("self_con_fruit")

  ### Individual Analysis
  ind_controls <- c("ch_sex")

  ### Main Care Giver
  main_care <- c("ch_sex","mcg_age","mcg_sex","mcg_read")
  
  ### Households Controls
  hh_controls <- c("hhsize_ae","dep_ratio")
  
  ### Households Controls
  hh_head <- c("hh_head_age","hh_head_sex","hh_head_read")

  ### Agricultural 
  hh_welfare <- c("poor_13","cpexp30","land_own")
  
# Controls 
controls <- c(trees,care_giver,hh_controls,hh_head,agr)

##################
##### Build Data Sets
#################


reg_jc <- function(depv,cont,dat) {
  ##### Define Controls 
  common <- eval(parse(text=paste0("~ ",cont[1])))
  for (c in cont[2:length(cont)]) {
    common <- eval(parse(text=paste0("update(common, ~ . +",c,")")))
  }
  ##### Specification
  equation <- eval(parse(text=paste0("update(common,",depv, "~ . + as.factor(region) + as.factor(ch_age_m) ",")")))
  #### Regressions
  reg <- lm(equation, 
            data=dat)
  ### Cluster Standard Errors
  return(reg)
}

##################
##### Descriptive Statistics
#################
### Funtion Get Lables
stargazer(data14_nutr,title="Summary Statistics",type="text")

##################
##### Table 
#################

reg1 <- reg_jc("stunting",c("self_con_fruit",hh_controls,main_care,hh_controls,"lvst_large","lvst_small"),data10_nutr)
reg1.sd <- cluster.vcov(reg1,data10_nutr$region)

stargazer(reg1,se=list(reg1.sd),type = "text", title="Descriptive statistics", digits=3, out=paste0(tables_path,"table1.csv"),align=TRUE)



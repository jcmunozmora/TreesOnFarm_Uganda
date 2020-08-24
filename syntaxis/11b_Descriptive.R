##################
##### Package
#################

### -- Graph
library(ggplot2)
library(lubridate)
theme_set(theme_bw())
library(latex2exp) ## Latex
library(grid) ## Package for several graphs
library(gridExtra) ## Package for several graphs

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
path_work <- "/Users/jcmunoz/Dropbox/Documents/Projects_papers/2020/Miller_etal_2020/TreesOnFarm_Uganda/"
graph_path <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Paper/Graph/"
save_data <-  paste0(path_work,"01_DataSets/")
path_who <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/inputs/"
tables_path <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/06_Paper/tablets/"
graph_path <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/06_Paper/Graph/"

##################
##### Load Data
#################

load(paste0(save_data,"TreesOnFarm_Uganda_04012017.Rda"))

sample_end <- read.dta13(paste0(save_data,"sample_end.dta"))

##################
##### Main Data
#################

baseline <- sample_end %>% inner_join(data05_hh) %>% setNames(paste0('w0_', names(.))) %>% mutate(hhid=w0_hhid) %>% inner_join(data14_hh)

##################
##### Descritive data Consumption
#################

consu_05 <- data05_hh %>% inner_join(sample_end) %>% transmute(hhid=hhid,tot_exp=log(((tot_exp*1.520308938)/hhsize_ae)+0.01),treeosfarm=ifelse((harv_q_fruit+harv_q_cash)>0,1,0)) 
consu_05$treeosfarm <- factor(consu_05$treeosfarm,levels = c(1,0),labels=c("Yes","No"))

#### --- Data
consu_14 <- data14_hh %>% inner_join(sample_end) %>% transmute(hhid=hhid,tot_exp=log(((tot_exp*1.520308938)/hhsize_ae)+0.01),treeosfarm=ifelse((harv_q_fruit+harv_q_cash)>0,1,0)) %>% mutate(treeosfarm=replace(treeosfarm,is.na(treeosfarm),0))
consu_14$treeosfarm <- factor(consu_14$treeosfarm,levels = c(1,0),labels=c("Yes","No"))


  
g_05 <- ggplot(consu_05, aes(tot_exp)) + geom_density(aes(fill=factor(treeosfarm)), alpha=0.8) + labs(fill="TreesOnFarm") + xlab(TeX('ln($C_{i,2005-06}$)'))+scale_x_continuous(breaks = seq(0,20,2),limits=c(11,16))
g_14 <- ggplot(consu_14, aes(tot_exp)) + geom_density(aes(fill=factor(treeosfarm)), alpha=0.8) + labs(fill="TreesOnFarm") + xlab(TeX('ln($C_{i,2013-14}$)'))+scale_x_continuous(breaks = seq(0,20,2),limits=c(11,16))
                                                                                    
ggsave(paste0(graph_path ,"change_consumption.pdf"),grid.arrange(g_05,g_14),dpi=300)

t.test(consu_14$tot_exp,consu_05$tot_exp.,var.equal=TRUE)

##################
##### Descritive data Consumption
#################

cal_05 <- data05_hh %>% inner_join(sample_end) %>% transmute(hhid=hhid,itk_kcal_total=(itk_kcal_total/hhsize_ae)/7,treeosfarm=ifelse((harv_q_fruit+harv_q_cash)>0,1,0)) 
cal_05$treeosfarm <- factor(cal_05$treeosfarm,levels = c(1,0),labels=c("Yes","No"))

#### --- Data
cal_14 <- data14_hh %>% inner_join(sample_end) %>% transmute(hhid=hhid,itk_kcal_total=(itk_kcal_total/hhsize_ae)/7,treeosfarm=ifelse((harv_q_fruit+harv_q_cash)>0,1,0)) %>% mutate(treeosfarm=replace(treeosfarm,is.na(treeosfarm),0))
cal_14$treeosfarm <- factor(cal_14$treeosfarm,levels = c(1,0),labels=c("Yes","No"))


# Create break points and labels for axis ticks
brks <- seq(0, 20, 5)
lbls <-  paste0(as.character(seq(0, 20, 5)))

# Plot
ggplot(economics[1:100, ], aes(date, returns_perc)) + 
  geom_area() + 
  scale_x_date(breaks=brks, labels=lbls) + 
  theme(axis.text.x = element_text(angle=90)) + 
  
  g_05 <- ggplot(cal_05, aes(itk_kcal_total)) + geom_density(aes(fill=factor(treeosfarm)), alpha=0.8) + labs(fill="TreesOnFarm") + xlab(TeX('Calorie intake per adult equivalent (2005-06)'))+scale_x_continuous(breaks = seq(0,10000,5000),limits=c(0,10000))+scale_y_continuous(breaks = seq(0,0.1,0.0002),labels=paste0(as.character(seq(0,0.1,0.0002))))
g_14 <- ggplot(cal_14, aes(itk_kcal_total)) + geom_density(aes(fill=factor(treeosfarm)), alpha=0.8) + labs(fill="TreesOnFarm") + xlab(TeX('Calorie intake per adult equivalent (2013-14)'))+scale_x_continuous(breaks = seq(0,10000,5000),limits=c(0,10000))+scale_y_continuous(breaks = seq(0,0.1,0.0002),labels=paste0(as.character(seq(0,0.1,0.0002))))

ggsave(paste0(graph_path ,"change_calorie.pdf"),grid.arrange(g_05,g_14),dpi=300)

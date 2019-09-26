##################
##### Package
#################

library(xtable)
library(readstata13)
library(stargazer)
library(ggplot2)
library(ggrepel) ## Labeling Graphs
library(scales) ### Convert Numeros
library("RColorBrewer") ## Colors Pallets 
library(ggmap)
library(maps)
library(mapdata)
library(ggmap)
library(ggthemes)
library(plyr)
library(dplyr)
library(tidyr)

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
path_work <- "/Users/juan-carlosm/Dropbox/Documents/Projects_papers/2019/Munoz-etal_2019/"
graph_path <- paste0(path_work,"paper/graph/")
save_data <-  paste0(path_work,"01_DataSets/")
path_who <- paste0(save_data,"inputs/")

##################
##### Load Data
#################

load(paste0(save_data,"TreesOnFarm_Uganda_12012017.Rda"))
sample_end <- read.dta13(paste0(save_data,"sample_end.dta"))

##################
##### Main Data
#################

baseline <- sample_end %>% inner_join(data05_hh) %>% setNames(paste0('w0_', names(.))) %>% mutate(hhid=w0_hhid) %>% inner_join(data14_hh)

##################
##### Descritive data Consumption
#################

consu_05 <- data05_hh %>% inner_join(sample_end) %>% transmute(hhid=hhid,tot_exp=log(((tot_exp*1.520308938)/hhsize_ae)+0.01),treeosfarm=sh_fruit_w_b) %>% mutate(wave="2004-05")

#### --- Data
consu_14 <- data14_hh %>% inner_join(sample_end) %>% transmute(hhid=hhid,tot_exp=log(((tot_exp*0.914632051)/hhsize_ae)+0.01),treeosfarm=sh_fruit_w_b) %>% mutate(wave="2013-14")
  

#### Data merge
ch_data <- merge(consu_05,consu_14,by="hhid") 
ch_data <- ch_data %>% mutate(change=(treeosfarm.y-treeosfarm.x),never=ifelse(treeosfarm.y==0 & treeosfarm.x==0,1,0)) %>% select(hhid,change,never)

### Final Data Set
data <- rbind(consu_05,consu_14)

ds <- data %>% inner_join(ch_data)

decrease <- ggplot(subset(ds,change<0), aes(tot_exp)) + geom_density(aes(fill=factor(wave)), alpha=0.8) + labs(fill="Wave") + xlab(TeX('ln($C_{i}$)'))+scale_x_continuous(breaks = seq(0,20,2),limits=c(11,16))

increase <- ggplot(subset(ds,change>0), aes(tot_exp)) + geom_density(aes(fill=factor(wave)), alpha=0.8) + labs(fill="Wave") + xlab(TeX('ln($C_{i}$)'))+scale_x_continuous(breaks = seq(0,20,2),limits=c(11,16))

nochange <- ggplot(subset(ds,change==0), aes(tot_exp)) + geom_density(aes(fill=factor(wave)), alpha=0.8) + labs(fill="Wave") + xlab(TeX('ln($C_{i}$)'))+scale_x_continuous(breaks = seq(0,20,2),limits=c(11,16))

ggsave(paste0(graph_path ,"change_decrease.pdf"),decrease,dpi=300)
ggsave(paste0(graph_path ,"change_increase.pdf"),increase,dpi=300)
ggsave(paste0(graph_path ,"change_nochange.pdf"),nochange,dpi=300)


##################
##### Map Locations
#################

allocation <- ggplot(ds, aes(treeosfarm)) + geom_density(aes(fill=factor(wave)), alpha=0.8) + labs(fill="Wave") + xlab("Share Land Own allocated to Trees On Farm")

ggsave(paste0(graph_path ,"treesonfarm.pdf"),allocation,dpi=300)

ds %>% group_by(wave) %>% summarise(mean(treeosfarm),sd(treeosfarm))
mean($treeosfarm)

##################
##### Map Locations
#################

data_loca <- read.dta13(paste0(path_work,"/00_RawData/2011-12/Geovariables/UNPS_Geovars_1112.dta"))

loc_map <- merge(sample_end,data_loca,by.x="hhid",by.y="HHID",all=FALSE)

### Make The map
api_key="AIzaSyA7Fj7sUxbiZWbCVIFBKuBQDvj-5YV_g9Q"

wl <- map_data("world") %>% filter(region %in% c("Uganda"))

bc_bbox <- make_bbox(lat = lat, lon = long, data = wl)
turkey <- get_map(location = bc_bbox,zoom=7)

map1 <- ggmap(turkey) +
  scale_x_continuous(limits = c(min(wl$long),max(wl$long)), expand = c(0, 0)) +
  scale_y_continuous(limits = c(min(wl$lat),max(wl$lat)), expand = c(0, 0))+ geom_point(data = loc_map, mapping = aes(x = lon_mod, y = lat_mod))+geom_polygon(color = "black", fill = "gray")+labs(color="Sampling Strata")+theme_map()+theme(legend.position="bottom", plot.margin = unit(c(0.01,0.01,0,0.5), "cm"),text=element_text(family="Helvetica", face="bold", size=12))

## -- Save 
ggsave(paste0(graph_path,"sampling.pdf"),map1,dpi=300)



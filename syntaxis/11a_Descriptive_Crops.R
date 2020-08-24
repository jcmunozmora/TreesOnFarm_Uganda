##### Package
library("readstata13")
library(plyr)
library(dplyr)
library(tidyr)
library(Gmisc) # Transition Games

### Directories
path_work <- "/Users/jcmunoz/Dropbox/Documents/Projects_papers/2019/Munoz-etal_2019/"
graph_path <- paste0(path_work,"paper/graph/")
save_data <-  paste0(path_work,"01_DataSets/")
path_who <- paste0(save_data,"inputs/")

sample_end <- read.dta13(paste0(save_data,"sample_end.dta"))

#############################################
###############------------- Area per crop
#############################################
crop_land_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC4A.dta"))

crop_land_05 <- crop_land_05 %>% transmute(hhid=HHID,parcel_id=a4aq1,plot_id=a4aq2,cropID=a4aq5b,plot_size=a4aq3,share_crop=(a4aq6/100)) %>% filter(!is.na(share_crop)) %>% transmute(hhid,parcel_id,plot_id,cropID,area_crop=plot_size*share_crop) %>%
  mutate(
    ##### First Classification - With Bananas
    trees_a=ifelse((cropID %in% c(710,700,750,760,770,741,742,744)),"Fruit Trees",
                   ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
                          ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))),
    ##### Second Classification - With Bananas
    trees_b=ifelse((cropID %in% c(710,700,750,760,770)),"Fruit Trees",
                   ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
                          ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))))

#### Building Aggregation for the first variable
crops_05_06 <- crop_land_05 %>% filter(trees_a %in% c("Fruit Trees","Tree Cash Crops")) %>% group_by(hhid,parcel_id,cropID) %>% summarise(area_crop=mean(area_crop,na.rm=TRUE)) 


#############################################
###############------------- Land Size
#############################################

#####---- Land Size (Ownd)
land_05_a <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC2A.dta"))

land_05_a <- land_05_a %>% transmute(hhid=HHID,parcel_id=a2aq2,land_size=ifelse(is.na(a2aq4),a2aq5,a2aq4)) 

#####---- Land Size (Use Rights)
land_05_b <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC2B.dta"))

land_05_b <- land_05_b %>% transmute(hhid=HHID,parcel_id=a2bq2,land_size=ifelse(is.na(a2bq4),a2bq5,a2bq4)) 

#### Append Data Set
land_05 <- rbind(land_05_a,land_05_b)

#############################################
###############------------- Final Merge
#############################################

m_are_crop_05 <- crops_05_06 %>% left_join(land_05) %>% filter(!is.na(land_size)) %>%
  #### Gen the aggregation by hiig
  group_by(hhid,cropID) %>% select(area_crop,land_size) %>% mutate(share_area=area_crop/land_size) %>% summarise(m_a=mean(share_area,na.rm=TRUE)) 

end_05_06 <- sample_end %>% left_join(m_are_crop_05) %>% filter(!is.na(m_a)) %>% group_by(cropID) %>% summarise(share=(n()/1060))
  

######################
####--------  Share of the land on trees on farm -- 2013-14
######################

#############################################
###############------------- Area per crop
#############################################
crop_land_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC4A.dta"))

crop_land_14 <- crop_land_14 %>% transmute(hhid=hh,parcel_id=parcelID,plot_id=plotID,cropID=cropID,area_crop=a4aq7,year_planted=a4aq9_2) %>% filter(!is.na(area_crop)) %>%
  mutate(
    ##### First Classification - With Bananas
    trees_a=ifelse((cropID %in% c(710,700,750,760,770,741,742,744)),"Fruit Trees",
                   ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
                          ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))),
    ##### Second Classification - With Bananas
    trees_b=ifelse((cropID %in% c(710,700,750,760,770)),"Fruit Trees",
                   ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
                          ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))))

#### Building Aggregation for the first variable
#### Building Aggregation for the first variable
crops_13_14 <- crop_land_14 %>% filter(trees_a %in% c("Fruit Trees","Tree Cash Crops")) %>% group_by(hhid,parcel_id,cropID) %>% summarise(area_crop=mean(area_crop,na.rm=TRUE)) 

#############################################
###############------------- Land Size
#############################################


#####---- Land Size (Ownd)
land_14_a <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC2A.dta"))

land_14_a <- land_14_a %>% transmute(hhid=hh,parcel_id=parcelID,land_size=ifelse(is.na(a2aq4),a2aq5,a2aq4)) %>% group_by(hhid,parcel_id) %>% summarise(land_own=sum(land_size,na.rm=TRUE))


#####---- Land Size (Use Rights)
land_14_b <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC2B.dta"))

land_14_b <- land_14_b %>% transmute(hhid=hh,parcel_id=parcelID,land_size=ifelse(is.na(a2bq4),a2bq5,a2bq4)) %>% group_by(hhid,parcel_id) %>% summarise(land_own=sum(land_size,na.rm=TRUE)) 

#### Append Data Set
land_14 <- rbind(land_14_a,land_14_b)

#############################################
###############------------- Final Merge
#############################################


m_are_crop_14 <- crops_13_14 %>% left_join(land_14) %>% filter(!is.na(land_own)) %>%
  #### Gen the aggregation by hiig
  group_by(hhid,cropID) %>% select(area_crop,land_own) %>% mutate(share_area=area_crop/land_own) %>% summarise(m_a=mean(share_area,na.rm=TRUE)) 

m_are_crop_14$cropID[m_are_crop_14$cropID %in% c(811,812)]=810

hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC1.dta"))

fix_id_14 <- function(x) {
  #### Data
  mer <- hh_roster_14 %>% transmute(HHID_old=HHID_old,hhid=HHID) %>% filter(!duplicated(hhid))
  x <- x %>% left_join(mer) %>% ungroup(hhid) %>% mutate(hhid_05=hhid,hhid=as.character(HHID_old)) %>% select(-HHID_old)
}

###----> 
m_are_crop_14 <- fix_id_14(m_are_crop_14)

end_13_14 <- sample_end %>% left_join(m_are_crop_14) %>% filter(!is.na(m_a)) %>% group_by(cropID) %>% summarise(share=(n()/1060)) %>% mutate(wave="2013-14")

end_05_06 <- mutate(end_05_06,wave="2005-06")

data_end <- rbind(end_05_06,end_13_14)

data_end <- data_end %>%   mutate(
  ##### First Classification - With Bananas
  trees=ifelse((cropID %in% c(710,700,750,760,770,741,742,744)),"Fruit Trees",
                 ifelse(cropID %in% c(860,820,830,810,811,812),"Tree Cash Crops",
                        ifelse(cropID %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops"))))


data_end$cropID <- factor(data_end$cropID,levels = c(700,710,741,742,744,750,760,770,810,820,830),labels=c("Oranges","Paw paw","Food Banana","Beer Banana","Sweet Banana","Mango","Jackfruit","Avocado","Coffee all","Cocoa","Tea"))



##### Graph
library(ggplot2)
library("RColorBrewer") ## Colors Pallets 
library(scales)
library(wesanderson)
graph <- ggplot(data = data_end, aes(x = cropID, y = share)) +
  geom_col(aes(fill = cropID) , show.legend = FALSE)+coord_flip() +
  
  geom_label(aes(label = paste0(percent(share)),
                                                 y =-0.05, fill = cropID),
             show.legend = FALSE,
             size = 3, label.padding = unit(0.1, "lines")) + expand_limits(y =-0.1) + facet_wrap(~ trees+wave)+scale_colour_manual(values = wes_palette(name="GrandBudapest1"))+labs(y="",x="")+scale_y_continuous(breaks = seq(0,1,0.1))+theme(axis.text.x=element_blank(),axis.line.x=element_blank())

ggsave("/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/06_Paper/Graph/crops_pdf.pdf",graph,dpi=300)

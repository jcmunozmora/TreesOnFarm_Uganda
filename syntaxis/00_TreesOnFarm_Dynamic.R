##### Package
  library("readstata13")
  library(dplyr)
  library(tidyr)
  library(Gmisc) # Transition Games
  
  ### Directories
  path_work <- "/Users/jcmunoz/Dropbox/Documents/Projects_papers/2019/Munoz-etal_2019/"
  graph_path <- paste0(path_work,"paper/graph/")
  save_data <-  paste0(path_work,"01_DataSets/")
  path_who <- paste0(save_data,"inputs/")
  
  
##### Household Roster
  hh_roster_05 <- read.dta13(paste0(path_work,"00_RawData/2005-06/Household/GSEC1.dta"))
  hh_roster_10 <- read.dta13(paste0(path_work,"00_RawData/2010-11/Household/GSEC1.dta"))
  hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC1.dta"))
  
  fix_id_14 <- function(x) {
    #### Data
    mer <- hh_roster_14 %>% transmute(HHID_old=HHID_old,hhid=HHID) %>% filter(!duplicated(hhid))
    x <- x %>% left_join(mer) %>% ungroup(hhid) %>% mutate(hhid_05=hhid,hhid=as.character(HHID_old)) %>% dplyr::select(-HHID_old)
  }

  
#### Get data of the present HH
  hh_panel_org <- hh_roster_14 %>% 
    transmute(HHID=as.character(HHID_old)) %>% 
    inner_join(hh_roster_10,by="HHID") %>% 
    dplyr::select(HHID,spitoff09_10,spitoff10_11) %>% 
    inner_join(hh_roster_05,by="HHID") %>% 
    transmute(hhid=HHID)
  
####################
##### Dynamic of Trees On Farm - Within the Farm 
####################

  ### Trees Adopter
  trees_05 <- read.dta13(paste0(path_work,"/00_RawData/2005-06/Agriculture/AGSEC7A.dta"))
  trees_10 <- read.dta13(paste0(path_work,"/00_RawData/2010-11/Agriculture/AGSEC4A.dta"))
  trees_14 <- read.dta13(paste0(path_work,"/00_RawData/2013-14/AGSEC4A.dta"))

  ### TreesOnFarm Dynamic

  get_trees <- function(x,wave) {
    colnames(x)=c("hhid","crop_id")
    x$hhid <- as.character(x$hhid)
    x <- x[!is.na(x$crop_id),]
    x <- x %>% 
      mutate(trees=ifelse((crop_id %in% c(710,700,750,760,770,741,742,744)),"Fruit Trees",
          ifelse(crop_id %in% c(860,820,830,810),"Tree Cash Crops",
                 ifelse(crop_id %in% c(880,970,990,950,960),"Trees for Timber/Fuelwood","Other Crops")))) %>% group_by(hhid,trees) %>% dplyr::summarise(n_crop=n()) %>% mutate(wav=wave)
    return(x)
  }

treesonfarm_05_06 <- as.data.frame(get_trees(trees_05[,c("HHID","a7aq2b")],"2005-06"))
treesonfarm_10_11 <- as.data.frame(get_trees(trees_10[,c("HHID","cropID")],"2010-11"))      
treesonfarm_13_14 <- fix_id_14(get_trees(trees_14[,c("hh","cropID")],"2013-14"))

#####
g_d <- function(x) { ifelse(x>0,1,0)}
trees_05_06 <- treesonfarm_05_06 %>% spread(trees,n_crop,fill=0) %>% transmute(hhid=hhid,fr_05=g_d(`Fruit Trees`),oth_05=g_d(`Other Crops`),cash_05=g_d(`Tree Cash Crops`),wood_05=g_d(`Trees for Timber/Fuelwood`))

trees_13_14 <- treesonfarm_13_14 %>% spread(trees,n_crop,fill=0) %>% transmute(hhid=hhid,fr_13=g_d(`Fruit Trees`),oth_13=g_d(`Other Crops`),cash_13=g_d(`Tree Cash Crops`))

t_05_13 <- treesonfarm_05_06 %>%  inner_join(treesonfarm_13_14,by = c("hhid","trees")) %>%
  transmute(hhid=hhid,fr_t05=ifelse(!is.na(n_crop.x),1,0),fr_t10=ifelse(!is.na(n_crop.y),1,0),fr_t13=ifelse(!is.na(n_crop),1,0))




treesonfarm <- treesonfarm_05_06 %>% inner_join(hh_panel_org) %>%
                 left_join(treesonfarm_10_11,by = c("hhid","trees")) %>%
                     left_join(treesonfarm_13_14,by = c("hhid","trees")) %>%
                         filter(trees=="Fruit Trees") %>% right_join(hh_panel_org) %>%
                           transmute(hhid=hhid,fr_t05=ifelse(!is.na(n_crop.x),1,0),fr_t10=ifelse(!is.na(n_crop.y),1,0),fr_t13=ifelse(!is.na(n_crop),1,0))
  
rm(treesonfarm_05_06,treesonfarm_10_11,treesonfarm_13_14,trees_05,trees_10,trees_14)

save(treesonfarm,file=paste0(save_data,"treesonfarm.Rda"))


###################
##### Transition Graph
####################

gen_transition <- function(trans_matrix,t1,t2) {
  
  ##### Gen Function for formatting labels
  output_perc <-function(txt, n) sprintf("%s\n[%.0f%%]", txt, n)
  
  ##### Gen Labels
  box_txt <- 
    cbind(mapply(output_perc, 
                 txt = c("Without \n Fruit Trees","With Fruit Trees"), 
                 n = prop.table(rowSums(trans_matrix))*100),
          mapply(output_perc, 
                 txt = c("Without \n Fruit Trees","With Fruit Trees"), 
                 n = prop.table(colSums(trans_matrix))*100))
  
  title=""
  ### Save the graphs
  transitionPlot(trans_matrix, type_of_arrow = "simple",box_width = 1/4,cex=1,new_page=TRUE,main=title, box_label=c(t1,t2),box_txt = box_txt)

}

pdf(file=paste0(graph_path,"00_TreesOnFarm_Dynamic_a.pdf"))
gen_transition(table(treesonfarm$fr_t05,treesonfarm$fr_t10),"2005-06","2010-11")
dev.off()


pdf(file=paste0(graph_path,"00_TreesOnFarm_Dynamic_b.pdf"))
gen_transition(table(treesonfarm$fr_t10,treesonfarm$fr_t13),"2010-11","2013-14")
dev.off()




source(file="/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/Syntaxis_v1/04a_HH_Income_aux.R",verbose=FALSE)

graph_path <- "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/06_Paper/Graph/"

###########################
### Function - Trees On Farm
############################

#--- End DataSEt
get_na_zero <- function(x) { ifelse(is.finite(x),x,0) }
get_na_zero_div <- function(x) { ifelse(is.finite(x),ifelse(x>1,1,x),0) }
g_d <- function(x) { ifelse(x>0,1,0)}

 ### -- Graph Transition
gen_transition <- function(trans_matrix,t1,t2) {

  ##### Gen Function for formatting labels
  output_perc <-function(txt, n) sprintf("%s\n[%.0f%%]", txt, n)

  ##### Gen Labels
  box_txt <-
    cbind(mapply(output_perc,
                 txt = c("Without \n Trees On Farm","With \n Trees On Farm"),
                 n = prop.table(rowSums(trans_matrix))*100),
          mapply(output_perc,
                 txt = c("Without \n Trees On Farm","With \n Trees On Farm"),
                 n = prop.table(colSums(trans_matrix))*100))

  title=""
  ### Save the graphs
  library(RColorBrewer)
  transitionPlot(trans_matrix, type_of_arrow = "simple",box_width = 1/4,cex=1,new_page=TRUE,main=title, box_label=c(t1,t2),box_txt = box_txt,fill_start_box = brewer.pal(n = 2, name = "Pastel1"),
                 fill_end_box = brewer.pal(n = 2, name = "Pastel1"),txt_start_clr = "black", txt_end_clr = "black")

}
mapply

library(package, help, pos = 2, lib.loc = NULL)
%>%  fasdf %>%






##### ------------------------------- ######
#


###########################
### Open Files
###########################

###################
#### 2005 - 06
###################

# -- Crop Level
crop_disp_A <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC7A.dta"))
crop_disp_B <- read.dta13(paste0(path_work,"00_RawData/2005-06/Agriculture/AGSEC7B.dta"))

crop_disp_A <- crop_disp_A %>% mutate(cropid=a7aq2b)
crop_disp_B <- crop_disp_B %>% mutate(cropid=a7bq2b)


crop_05 <- crop_disp_A %>% full_join(crop_disp_B) %>% filter(!is.na(cropid)) %>% mutate(cropID=cropid,val_sold_1=get_na_zero(a7aq5),val_sold_2=get_na_zero(a7bq5),q_harvest_1=get_na_zero(a7aq3b),q_harvest_2=get_na_zero(a7bq3b),q_self_1=get_na_zero(a7aq9),q_self_2=get_na_zero(a7bq9)) %>% transmute(hhid=HHID,cropID=cropid,val_sold=val_sold_1+val_sold_2,self_con=get_na_zero_div((q_self_1+q_self_2)/(q_harvest_1+q_harvest_2)),q_harvest=(q_harvest_1+q_harvest_2))  %>% imputeout(.,hh_roster_05) %>% select(hhid,cropID,q_harvest) %>%
  ### Trees
  mutate(trees=ifelse((cropID %in% c(710,700,750,760,770,741,742,744)),"fruit_05",
                      ifelse(cropID %in% c(860,820,830,810,811,812),"cash_05",
                             ifelse(cropID %in% c(880,970,990,950,960),"wood_05","other_05")))) %>%
  ## Collapse Information
  group_by(hhid,trees) %>% summarise(total_h=sum(q_harvest,na.rm=TRUE)) %>%
  ## Spreat data
  spread(trees,total_h,fill=0) %>% mutate(treesonfarm_05=fruit_05+cash_05+wood_05) %>% summarise_all(g_d)


###################
#### 2013 - 14
###################
 
# -- Parcel Level
crop_disp_A <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC5A.dta"))
crop_disp_B <- read.dta13(paste0(path_work,"00_RawData/2013-14/AGSEC5B.dta"))

#--- End DataSEt
crop_14 <- crop_disp_A %>% full_join(crop_disp_B) %>%
  ### Create the Aggregate
  filter(!is.na(cropID)) %>% mutate(val_sold_1=get_na_zero(a5aq8),val_sold_2=get_na_zero(a5bq8),q_harvest_1=get_na_zero(a5aq6a),q_harvest_2=get_na_zero(a5bq6a),q_self_1=get_na_zero(a5aq13),q_self_2=get_na_zero(a5bq13)) %>%
  ### Save this data
  transmute(hhid=hh,cropID=cropID,val_sold=val_sold_1+val_sold_2,self_con=get_na_zero_div((q_self_1+q_self_2)/(q_harvest_1+q_harvest_2)),q_harvest=(q_harvest_1+q_harvest_2))  %>% imputeout(.,hh_roster_14) %>% select(hhid,cropID,q_harvest) %>%
  ### Trees
  mutate(trees=ifelse((cropID %in% c(710,700,750,760,770,741,742,744)),"fruit_13",
                      ifelse(cropID %in% c(860,820,830,810,811,812),"cash_13",
                             ifelse(cropID %in% c(880,970,990,950,960),"wood_13","other_13")))) %>%
  ## Collapse Information
  group_by(hhid,trees) %>% summarise(total_h=sum(q_harvest,na.rm=TRUE)) %>%
  ## Spreat data
  spread(trees,total_h,fill=0) %>% mutate(treesonfarm_13=fruit_13+cash_13) %>% summarise_all(g_d)
  ## Create Dummy


###########################
### Fix ID
###########################
hh_roster_14 <- read.dta13(paste0(path_work,"00_RawData/2013-14/GSEC1.dta"))

fix_id_14 <- function(x) {
  #### Data
  mer <- hh_roster_14 %>% transmute(HHID_old=HHID_old,hhid=HHID) %>% filter(!duplicated(hhid))
  x <- x %>% ungroup(hhid) %>% left_join(mer) %>% mutate(hhid_05=hhid,hhid=as.character(HHID_old)) %>% select(-HHID_old)
}

crop_14 <- fix_id_14(crop_14)

###########################
### Final Analysis
###########################

sample_end <- read.dta13("/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/sample_end.dta")

tree_end <- sample_end  %>% left_join(crop_05) %>% left_join(crop_14) %>% group_by(hhid) %>% summarise_all(get_na_zero)


pdf(file=paste0(graph_path,"00_TreesOnFarm_Dynamic.pdf"))
gen_transition(table(tree_end$treesonfarm_05,tree_end$treesonfarm_13),"2005-06","2013-14")
dev.off()

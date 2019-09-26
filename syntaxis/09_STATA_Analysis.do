use "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/data14_nutr.dta", clear

keep if hhid!="NA"

merge n:1 hhid using `wave1', keep(matched)





global ind_controls "ch_sex ch_age"

replace mcg_read=0 if mcg_sch==.

global main_care "mcg_age mcg_sex mcg_read"

global livestock "lvst_large lvst_small"

global hh_control "hhsize_ae dep_ratio"

global hh_head "hh_head_age hh_head_sex hh_head_read"

global hh_welfare "poor_13 cpexp30"

*** DepenValur
tostring int_month, gen(month)
tostring int_year, gen(year)
cap drop int_date
egen int_date=concat(int_year int_month )
egen int_d=group(int_date)


gen change_fruit=(harv_q_fruit==0 & w1_harv_q_fruit>0)
gen change_fruit=(harv_q_fruit==0 & w1_harv_q_fruit>0)




replace harv_q_fruit=0 if harv_q_fruit==.
gen d_fruit=(harv_q_fruit!=0)


xi: probit under_weight self_con_fruit  $ind_controls $main_care $livestock $hh_control poor_13  urban i.stratum i.int_d , robust


probit stunting change_fruit ch_sex ch_age $main_care $livestock $hh_control  $hh_welfare urban i.stratum i.int_d , robust

probit under_weight share_fruit $ind_controls $main_care $livestock $hh_control  $hh_welfare urban i.stratum i.int_d , robust

probit wasting share_fruit $ind_controls $main_care $livestock $hh_control poor_13  urban i.stratum i.int_d , robust


replace fr_t05=fr_t05 if fr_t05==.

ivprobit wasting (share_fruit=fr_t05) $ind_controls $main_care $livestock $hh_control poor_13  urban i.stratum i.int_d ,  vce(cluster stratum)


ivprobit under_weight (share_fruit=fr_t05),  vce(cluster stratum)



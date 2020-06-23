
 
global main_path "/Users/juan-carlosm/Dropbox/Documents/Projects_papers/2020/Miller_etal_2020/TreesOnFarm_Uganda"


 global tables "$main_path/02_Paper/tables"
 
*** **************************
*** 00 - Prepare Baseline Data -- 2005-06
*** **************************

use "$main_path/01_DataSets/data05_hh.dta", clear

drop exp_food
gen exp_food=exp_food_pur+exp_food_away+exp_food_own+exp_food_free
foreach var of varlist exp_food exp_food_pur- tot_exp inc_fruit- inc_ag_total {
	replace `var'=0 if `var'==.
	replace `var'=`var'*1.520308938
	***********
	**********
}

replace hhsize_ae=hhsize if hhsize_ae==0

renvars *, prefix("w0_")
rename w0_hhid hhid

duplicates drop hhid, force

tempfile wave0
save `wave0',replace

*** **************************
*** 01 - Prepare Baseline Data -- 2009-10
*** **************************

use "$main_path/01_DataSets/data10_hh.dta", clear

drop exp_food
gen exp_food=exp_food_pur+exp_food_away+exp_food_own+exp_food_free
foreach var of varlist exp_food exp_food_pur- tot_exp inc_fruit- inc_ag_total {
	replace `var'=0 if `var'==.
	replace `var'=`var'*1.520308938
	***********
	**********
}

replace hhsize_ae=hhsize if hhsize_ae==0

renvars *, prefix("w1_")
rename w1_hhid hhid

duplicates drop hhid, force

tempfile wave1
save `wave1',replace

*** **************************
*** 01 - Prepare Baseline Data -- 2009-10
*** **************************

use "$main_path/01_DataSets/data11_hh.dta", clear

drop exp_food
gen exp_food=exp_food_pur+exp_food_away+exp_food_own+exp_food_free
foreach var of varlist exp_food exp_food_pur- tot_exp inc_fruit- inc_ag_total {
	replace `var'=0 if `var'==.
	replace `var'=`var'*1.520308938
	***********
	**********
}

replace hhsize_ae=hhsize if hhsize_ae==0

renvars *, prefix("w1_")
rename w1_hhid hhid

duplicates drop hhid, force

tempfile wave1
save `wave1',replace



*** **************************
*** 01 - Prepare Baseline Data -- 2013-14
*** **************************

use "$main_path/01_DataSets/data14_hh.dta", clear

*** Only Rural Households
drop if urban==1

*** Only Those households present in boths
drop if hhid=="NA"
gen hhid_s=strlen(hhid)
keep if hhid_s==10
drop hhid_s
duplicates drop hhid, force

*** Merge baseline information
merge 1:1 hhid using `wave0', keep(matched) nogen
drop if w0_harv_q_fruit==.
replace harv_q_fruit=0 if harv_q_fruit==.

*** Migration Variable
merge 1:1 hhid using "$main_path/01_DataSets/Panel_Structure_Data_Uganda.dta", nogen keep(matched) 

rename hhid HHID
merge 1:1 HHID using  "$main_path/01_DataSets/UNPS_Geovars_1112.dta", nogen keep(master matched) keepusing(ssa_aez09_x)

******************************
** 

gen hhid_05=hhid

merge 1:1 hhid_05 using "$main_path/01_DataSets/data10_hh.dta"




*** fix this fixed effect
sort ea ssa_aez09_x
bys ea: replace ssa_aez09_x=ssa_aez09_x[_n-1] if ssa_aez09_x==.

*** Gen Main Variables
drop exp_food
gen exp_food=exp_food_pur+exp_food_away+exp_food_own+exp_food_free
foreach var of varlist exp_food exp_food_pur- tot_exp inc_fruit- inc_ag_total  {
	replace `var'=`var'*0.914632051
	gen part1_`var'=ln((`var'/hhsize_ae)+0.01)
	gen part2_`var'=ln((w0_`var'/w0_hhsize_ae)+0.01)
	gen ch_`var'=part1_`var'-part2_`var'
}


*** Main Variables --- Income
gen harv_q_treeonfarm=(harv_q_fruit+harv_q_cash)
gen w0_harv_q_treeonfarm=(w0_harv_q_fruit+w0_harv_q_cash)
foreach i in fruit cash other treeonfarm {
	** Replace
	replace w0_harv_q_`i'=0 if w0_harv_q_`i'==.
	replace harv_q_`i'=0 if harv_q_`i'==.
	*** Gen Dummy
	gen d_`i'=(harv_q_`i'>0)
	gen d_w0_`i'=(w0_harv_q_`i'>0)
	*** Gen Transition Dummies
	gen new_`i'=(w0_harv_q_`i'==0 & harv_q_`i'>0)
	gen always_`i'=(w0_harv_q_`i'>0 & harv_q_`i'>0)
	gen drop_`i'=(w0_harv_q_`i'>0 & harv_q_`i'==0)
	gen never_`i'=(w0_harv_q_`i'==0 & harv_q_`i'==0)
}

**** Changes on share of land
foreach i in tof_w_b tof_wo_b fruit_w_b fruit_wo_b cash{
	cap drop w0 w1
	gen w0=w0_sh_`i'
	gen w1=sh_`i'
	gen ch_`i'=w1-w0
	gen d_`i'_more=(w1>w0)
	gen d_`i'_same=(w1==w0)
	gen d_`i'_less=(w1<w0)
	gen d_`i'_never=(w1==0 & w0==0)
}

**** Changes in Variable
gen land_size_ln=ln(land_size+0.01)
gen w0_land_size_ln=ln(w0_land_size+0.01)
foreach var of varlist harv_q_fruit harv_q_cash harv_q_other lvst_large lvst_small lvst_poul hhsize_ae dep_ratio land_size_ln {
	gen ch_`var'=`var'-w0_`var'
}

**** Calorie Intake
foreach var of varlist itk_kcal_beverage itk_kcal_beans itk_kcal_fats itk_kcal_fruits itk_kcal_grain itk_kcal_meat itk_kcal_milk itk_kcal_root itk_kcal_sugar itk_kcal_vegta itk_kcal_total {

	cap drop `var'_part1 w0_`var'_part1 ch_`var'
	gen `var'_part1=ln((`var'/hhsize_ae)+0.01)
	gen w0_`var'_part1=ln((w0_`var'/w0_hhsize_ae)+0.01)
	gen ch_`var'=`var'_part1-w0_`var'_part1
}


***** global - controls
global hh_controls "w0_hhsize_ae w0_dep_ratio"
global hh_head "w0_hh_head_age w0_hh_head_sex w0_hh_head_sch"
replace w0_hh_head_sch=0 if w0_hh_head_sch==.
global hh_welfare "w0_lvst_large w0_lvst_small w0_land_size_ln"
global int_cod "w0_sh_tof_w_b w0_sh_fruit_w_b w0_sh_cash"

***** Labels
do "$main_path/syntaxis/09_labels.do"

*****************----------------------------------
**********------------- Table 1: Change on the Consumption
*****************---------------------------------- 

***** global - Main Data
global hvl_frt "d_fruit_w_b_more d_fruit_w_b_less"
global hvl_cash "d_cash_more d_cash_less"
global hvl_tfm "d_tof_w_b_more d_tof_w_b_less"

eststo clear

foreach depvar in tot_exp exp_food exp_food_own exp_durable {
	foreach i in tof_w_b fruit_w_b cash {
		eststo: xi: reg ch_`depvar' d_`i'_more d_`i'_less migra  $hh_head $hh_controls $hh_welfare w0_sh_`i' part2_`depvar'  i.stratum i.ssa_aez09_x, cluster(stratum)

	}
}

esttab using "$tables/table_main_diff.csv", fragment star(* 0.1 ** 0.05 *** 0.01)   nonumber nomtitles replace brackets mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)" "(11)" "(12)") label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N r2, fmt(%9.0f  %9.3f) labels("Observations" "R-squared" ))  indicate( "Strata FE= _Ist*" "Agroecological FE= _Issa*") keep($hvl_tfm  $hvl_frt $hvl_cash migra  $hh_head $hh_controls $hh_welfare $int_cod part2_*) order($hvl_tfm $hvl_frt $hvl_cash  migra $hh_head $hh_controls $hh_welfare $int_cod part2_*)



*****************----------------------------------
**********------------- Table 1: Change on the Consumption -- Without Fruit Trees
*****************---------------------------------- 

***** global - Main Data
global hvl_frt "d_fruit_wo_b_more d_fruit_wo_b_less"
global hvl_cash "d_cash_more d_cash_less"
global hvl_tfm "d_tof_wo_b_more d_tof_wo_b_less"

eststo clear

foreach depvar in tot_exp exp_food exp_food_own exp_durable {
	foreach i in tof_wo_b fruit_wo_b cash {
		eststo: xi: reg ch_`depvar' d_`i'_more d_`i'_less migra  $hh_head $hh_controls $hh_welfare w0_sh_`i' part2_`depvar'  i.stratum i.ssa_aez09_x, cluster(stratum)

	}
}

esttab using "$tables/table_main_diff_wo_f.csv", fragment star(* 0.1 ** 0.05 *** 0.01)   nonumber nomtitles replace brackets mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)" "(11)" "(12)") label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N r2, fmt(%9.0f  %9.3f) labels("Observations" "R-squared" ))  indicate( "Strata FE= _Ist*" "Agroecological FE= _Issa*") keep($hvl_tfm  $hvl_frt $hvl_cash migra  $hh_head $hh_controls $hh_welfare $int_cod part2_*) order($hvl_tfm $hvl_frt $hvl_cash  migra $hh_head $hh_controls $hh_welfare $int_cod part2_*)

*****************----------------------------------
**********------------- Table 1: Summary Statistics
*****************---------------------------------- 

	***- Data
	replace itk_kcal_total=(itk_kcal_total/7)/hhsize_ae
	replace w0_itk_kcal_total=(w0_itk_kcal_total/7)/hhsize_ae

	foreach i in tot_exp exp_food exp_food_own exp_durable{
		replace `i'=(`i'/12)/hhsize_ae
		replace w0_`i'=(w0_`i'/12)/w0_hhsize_ae
	}

	drop if hh_head_age==.

	**-- DAta 0
	eststo clear
	ereturn clear
	eststo: estpost summarize sh_tof_w_b sh_fruit_w_b sh_cash tot_exp exp_food exp_food_own exp_durable hhsize_ae dep_ratio hh_head_age hh_head_sex land_size lvst_large lvst_small, listwise

	local j=1
	esttab using "$tables/table_desc_1.csv", cells("N mean(fmt(%9.3f)) sd(fmt(%9.3f)) " ) label nodepvar  nonum replace noobs nomtitles  fragment collabels("Observations" "Mean" "Std. Deviation" "Q50" "Q90")

	**-- DAta 1
	eststo clear
	ereturn clear
	mat N=J(3,10,0)
	eststo: quietly estpost summarize w0_sh_tof_w_b  w0_sh_fruit_w_b  w0_sh_cash w0_tot_exp w0_exp_food w0_exp_food_own w0_exp_durable w0_hhsize_ae w0_dep_ratio w0_hh_head_age w0_hh_head_sex w0_land_size w0_lvst_large w0_lvst_small, listwise
	local j=1

	esttab using "$tables/table_desc_2.csv", cells("N mean(fmt(%9.3f)) sd(fmt(%9.3f)) " ) label nodepvar  nonum replace noobs nomtitles  fragment collabels("Observations" "Mean" "Std. Deviation" "Q50" "Q90")


*** T-test
ttest tot_exp=w0_tot_exp if d_tof_w_b_more==1
ttest tot_exp=w0_tot_exp if d_tof_w_b_less==1
ttest tot_exp=w0_tot_exp if d_tof_w_b_same==1


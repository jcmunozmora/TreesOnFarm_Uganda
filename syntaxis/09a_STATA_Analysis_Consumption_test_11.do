 global tables "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/06_Paper/tablets"
 
*** **************************
*** 00 - Prepare Baseline Data -- 2005-06
*** **************************


use "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/data05_hh.dta", clear

foreach var of varlist exp_food- tot_exp inc_fruit- inc_ag_total {
	replace `var'=0 if `var'==.
	replace `var'=`var'*1.520308938
}

replace hhsize_ae=hhsize if hhsize_ae==0

renvars *, prefix("w0_")
rename w0_hhid hhid

duplicates drop hhid, force

tempfile wave0
save `wave0',replace


*** **************************
*** 01 - Prepare Baseline Data -- 2013-14
*** **************************

use "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/data11_hh.dta", clear

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
merge 1:1 hhid using "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/Panel_Structure_Data_Uganda.dta", nogen keep(matched) keepusing(migra)

rename hhid HHID
merge 1:1 HHID using  "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/00_RawData/2011-12/Geovariables/UNPS_Geovars_1112.dta", nogen keep(master matched) keepusing(ssa_aez09_x)

*** fix this fixed effect
sort ea ssa_aez09_x
bys ea: replace ssa_aez09_x=ssa_aez09_x[_n-1] if ssa_aez09_x==.

*** Gen Main Variables
foreach var of varlist exp_food- tot_exp inc_fruit- inc_ag_total  {
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


**** Changes in Variable
replace land_own=ln(land_own+0.01)
foreach var of varlist harv_q_fruit harv_q_cash harv_q_other lvst_large lvst_small lvst_poul  hhsize_ae dep_ratio land_own{
	cap gen ch_`var'=`var'-w0_`var'
}

**** Calorie Intake
foreach var of varlist itk_kcal_beverage itk_kcal_beans itk_kcal_fats itk_kcal_fruits itk_kcal_grain itk_kcal_meat itk_kcal_milk itk_kcal_root itk_kcal_sugar itk_kcal_vegta itk_kcal_total {

	gen `var'_part1=ln((`var'/hhsize_ae)+0.01)
	gen w0_`var'_part1=ln((w0_`var'/w0_hhsize_ae)+0.01)
	gen ch_`var'=`var'_part1-w0_`var'_part1
}


***** global - Main Data
global hvl_frt "new_fruit always_fruit drop_fruit"
global hvl_cash "new_cash always_cash drop_cash"
global hvl_tfm "new_treeonfarm always_treeonfarm drop_treeonfarm"

***** global - controls
global hh_controls "ch_hhsize_ae ch_dep_ratio"
global hh_head_fix "hh_head_age hh_head_sex"
global hh_welfare "ch_lvst_large ch_lvst_small ch_land_own"

***** Labels
do "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/Syntaxis_v1/09_labels.do"


*****************----------------------------------
**********------------- Table 1: Summary Statistics
*****************---------------------------------- 
	
	***- Data
	replace itk_kcal_total=(itk_kcal_total/7)/hhsize_ae
	replace w0_itk_kcal_total=(w0_itk_kcal_total/7)/hhsize_ae

	replace tot_exp=(tot_exp/12)/hhsize_ae
	replace w0_tot_exp=(w0_tot_exp/12)/hhsize_ae

	drop if hh_head_age==.

	**-- DAta 0
	eststo clear
	ereturn clear
	eststo: estpost summarize d_treeonfarm d_fruit d_cash tot_exp itk_kcal_total hhsize_ae dep_ratio hh_head_age hh_head_sex land_own lvst_large lvst_small, listwise

	local j=1
	esttab using "$tables/table_desc_1.csv", cells("N mean(fmt(%9.3f)) sd(fmt(%9.3f)) " ) label nodepvar  nonum replace noobs nomtitles  fragment collabels("Observations" "Mean" "Std. Deviation" "Q50" "Q90")

	**-- DAta 1
	eststo clear
	ereturn clear
	mat N=J(3,10,0)
	eststo: quietly estpost summarize d_w0_treeonfarm d_w0_fruit d_w0_cash w0_tot_exp w0_itk_kcal_total w0_hhsize_ae w0_dep_ratio w0_hh_head_age w0_hh_head_sex w0_land_own w0_lvst_large w0_lvst_small, listwise
	local j=1

	esttab using "$tables/table_desc_2.csv", cells("N mean(fmt(%9.3f)) sd(fmt(%9.3f)) " ) label nodepvar  nonum replace noobs nomtitles  fragment collabels("Observations" "Mean" "Std. Deviation" "Q50" "Q90")



*****************----------------------------------
**********------------- Table 1: Change on the Consumption
*****************---------------------------------- 
eststo clear

	*** Trees On Farm
	eststo: xi: reg ch_tot_exp $hvl_tfm migra $hh_head_fix $hh_controls $hh_welfare i.stratum i.ssa_aez09_x, cluster(stratum)
	eststo: xi: reg ch_tot_exp $hvl_cash migra $hh_head_fix $hh_controls $hh_welfare i.stratum i.ssa_aez09_x, cluster(stratum)
	eststo: xi: reg ch_tot_exp $hvl_frt migra $hh_head_fix $hh_controls $hh_welfare i.stratum i.ssa_aez09_x, cluster(stratum)

	eststo: xi: reg ch_itk_kcal_total  $hvl_tfm migra $hh_head_fix $hh_controls $hh_welfare i.stratum i.ssa_aez09_x , cluster(stratum)
	eststo: xi: reg ch_itk_kcal_total  $hvl_cash migra $hh_head_fix $hh_controls $hh_welfare i.stratum i.ssa_aez09_x , cluster(stratum)
	eststo: xi: reg ch_itk_kcal_total  $hvl_frt migra $hh_head_fix $hh_controls $hh_welfare i.stratum i.ssa_aez09_x , cluster(stratum)


esttab using "$tables/table_main_diff.csv", fragment star(* 0.1 ** 0.05 *** 0.01)   nonumber nomtitles replace brackets mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" ) label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N r2, fmt(%9.0f  %9.3f) labels("Observations" "R-squared" ))  indicate( "Strata FE= _Istr*" "Agroecological FE= _Issa*") keep($hvl_tfm $hvl_cash $hvl_frt migra $hh_head_fix $hh_controls $hh_welfare) order($hvl_tfm $hvl_cash $hvl_frt migra $hh_head_fix $hh_controls $hh_welfare)







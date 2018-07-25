 global tables "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/06_Paper/tablets"
 
*** **************************
*** 00 - Prepare Baseline Data -- 2005-06
*** **************************


use "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/Uganda_Panel_06282018.dta", clear

*** Only Rural Households
drop if urban==1

*** Gen Main Variables

foreach var of varlist exp_food exp_food_pur- tot_exp inc_fruit- inc_ag_total  {
	replace `var'=ln(`var'+0.01)
}

*** Main Variables --- Income
gen harv_q_treeonfarm=(harv_q_fruit+harv_q_cash)


foreach i in fruit cash other treeonfarm {
	** Replace
	replace harv_q_`i'=0 if harv_q_`i'==.
	*** Gen Dummy
	gen d_`i'=(harv_q_`i'>0)
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
do "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/Syntaxis_v1/09_labels.do"

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


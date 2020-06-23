*** **************************
*** **************************

global main_path "/Users/juan-carlosm/Dropbox/Documents/Projects_papers/2020/Miller_etal_2020/TreesOnFarm_Uganda"

 global tables "$main_path/02_Paper/tables"

*** 2005-06 **************************
*** 00 - Prepare Baseline Data -- 2005-06
*** **************************

use "$main_path/01_DataSets/data05_hh.dta", clear


foreach var of varlist exp_food exp_food_pur- tot_exp inc_fruit- inc_ag_total {
	replace `var'=0 if `var'==.
	replace `var'=`var'*1.520308938
	***********
	**********
}

replace hhsize_ae=hhsize if hhsize_ae==0

*renvars *,  postfix("95")
*rename hhid95 hhid
gen wave=1995

duplicates drop hhid, force

tempfile wave0
save `wave0',replace

*** 2009-10 **************************
*** 01 - Prepare Baseline Data -- 2009-10
*** **************************

use "$main_path/01_DataSets/data10_hh.dta", clear

foreach var of varlist exp_food exp_food_pur- tot_exp inc_fruit- inc_ag_total {
	replace `var'=0 if `var'==.
	replace `var'=`var'*1.051752836
	***********
	**********
}

replace hhsize_ae=hhsize if hhsize_ae==0

gen wave=2010

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

*** DAta
foreach var of varlist exp_food exp_food_pur- tot_exp inc_fruit- inc_ag_total  {
	replace `var'=`var'*0.965554564
}

merge 1:1 hhid using "$main_path/01_DataSets/Panel_Structure_Data_Uganda.dta", nogen keep(matched) keepusing(migra)

rename hhid HHID
merge 1:1 HHID using  "$main_path/00_RawData/2011-12/Geovariables/UNPS_Geovars_1112.dta", nogen keep(matched) keepusing(ssa_aez09_x)

rename HHID hhid

*renvars *,  postfix("13")
*rename hhid13

gen wave=2013

append using `wave0'
bys hhid: gen obs0=_N
keep if obs0==2

append using `wave1', force
bys hhid: gen obs1=_N
keep if obs0==2|obs1==3

replace migra=0 if migra==.
egen id= group(hhid)
xtset id wave

*** fix this fixed effect
sort id ssa_aez09_x
bys id: replace ssa_aez09_x=ssa_aez09_x[_n-1] if ssa_aez09_x==.

*** Fix
cap drop stra1
gen stra1=stratum if wave==1995
sort id stra1
bys id: replace stra1=stra1[_n-1] if stra1==.

sort id ea
bys id: replace ea=ea[_n-1] if ea==.

*** fix
replace int_year=wave if int_year==.
replace int_month=1 if int_month==.

********
*** Gen Main Dependent Variable

gen exp_goods=exp_nodurable+exp_durable
foreach var of varlist exp_food   exp_food_pur- tot_exp inc_fruit- inc_ag_total exp_goods {
	cap drop ln_`var'_ae
	gen ln_`var'_ae=ln((`var'/hhsize_ae)+0.01)
}


***** global - controls

global hh_controls " migra dep_ratio"
global hh_head "hh_head_age hh_head_sex hh_head_sch"
replace hh_head_sch=0 if hh_head_sch==.
cap drop land_size_ln
gen land_size_ln=ln(land_size+0.01)
global hh_welfare "lvst_large lvst_small land_size_ln"
global int_cod "sh_tof_w_b sh_fruit_w_b sh_cash"

bys id: gen n_obs=_N

***** Labels
do "$main_path/syntaxis/09_labels.do"

*****************----------------------------------
**********------------- Table 1: Change on the Consumption
*****************----------------------------------

global wo ""sh_tof_wo_b" "sh_fruit_wo_b" "sh_fruit_wo_b sh_cash""

global w ""sh_tof_w_b" "sh_fruit_w_b"  "sh_fruit_w_b sh_cash""

eststo clear
foreach depvar in ln_tot_exp_ae ln_exp_food_ae ln_exp_durable_ae ln_exp_nodurable_ae  {
	foreach cont in $wo {
		eststo: xi: xtreg `depvar'  `cont' $hh_controls $hh_head $hh_welfare i.int_year  , fe cluster(ssa_aez09_x)
}
}

esttab using "$tables/table_main_diff_new.csv", fragment star(* 0.1 ** 0.05 *** 0.01)   nonumber nomtitles replace brackets mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)" "(11)" "(12)") label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N r2, fmt(%9.0f  %9.3f) labels("Observations" "R-squared" ))  keep(sh_tof_wo_b sh_fruit_wo_b sh_cash)


*****************----------------------------------
**********------------- Table 1: Change on the Consumption -- Without Fruit Trees
*****************----------------------------------

eststo clear
foreach depvar in ln_tot_exp_ae ln_exp_food_ae ln_exp_durable_ae ln_exp_nodurable_ae  {
	foreach cont in $w {
		eststo: xi: xtreg `depvar'  `cont' $hh_controls $hh_head $hh_welfare i.int_year  , fe cluster(ssa_aez09_x)
}
}

esttab using "$tables/table_main_diff_new_wo.csv", fragment star(* 0.1 ** 0.05 *** 0.01)   nonumber nomtitles replace brackets mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)" "(11)" "(12)") label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N r2, fmt(%9.0f  %9.3f) labels("Observations" "R-squared" ))  keep(sh_tof_w_b sh_fruit_w_b sh_cash)





***** global - Main Data
global hvl_frt "d_fruit_wo_b_more d_fruit_wo_b_less"
global hvl_cash "d_cash_more d_cash_less"
global hvl_tfm "d_tof_wo_b_more d_tof_wo_b_less"
1110a
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

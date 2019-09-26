
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
*** 00 - Prepare Baseline Data -- 2005-06
*** **************************

********------------    2013-14: 
use "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/data14_nutr.dta", clear

**---------
**** Fix Agro-ecological FE
rename hhid HHID
merge n:1 HHID using  "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/00_RawData/2011-12/Geovariables/UNPS_Geovars_1112.dta", nogen keep(master matched) keepusing(ssa_aez09_x)
*** fix this fixed effect
sort ea ssa_aez09_x
bys ea: replace ssa_aez09_x=ssa_aez09_x[_n-1] if ssa_aez09_x==.
**---------

**---------
*** Only Those households present in boths
*drop if urban==1
*drop if HHID=="NA"
gen hhid_s=strlen(HHID)
*gen hhid=substr(HHID,1,10)
*keep if hhid_s==10
* rename hhid_par hhid
**---------
*** Merge baseline information
*merge 1:1 hhid using `wave0', keep(matched) nogen


*** Merge Data Anterior
*drop if PID_Old=="0"
rename HHID hhid
merge n:1 hhid using `wave0', keep(master matched) force



*** Main Variables --- Income
gen harv_q_treeonfarm=(harv_q_fruit+harv_q_cash)
gen w0_harv_q_treeonfarm=(w0_harv_q_fruit+w0_harv_q_cash)
foreach i in fruit cash other treeonfarm {
	replace w0_harv_q_`i'=0 if w0_harv_q_`i'==.
	replace harv_q_`i'=0 if harv_q_`i'==.
	gen new_`i'=(w0_harv_q_`i'==0 & harv_q_`i'>0)
	gen always_`i'=(w0_harv_q_`i'>0 & harv_q_`i'>0)
	gen drop_`i'=(w0_harv_q_`i'>0 & harv_q_`i'==0)
	gen never_`i'=(w0_harv_q_`i'==0 & harv_q_`i'==0)
}

***** ---- Change the nutritional 
foreach var in fruit cash other treeonfarm {
	gen d_`var'=(harv_q_`var'>0)
	gen d_w0_`var'=(w0_harv_q_`var'>0)
	*** Replace
	*gen d_w1_`var'=(w1_harv_q_`var'>0)
	*gen d_w0_`var'=(harv_q_`var'>0)
	*gen d_`var'=(w1_harv_q_`var'==0 & harv_q_`var'>0)
}



***** ---- Change Nutritional Status
* foreach var in stunting wasting under_weight  {
* 	gen d_`var'=(w1_`var'==1 & `var'==0)
* 	replace d_`var'=-1 if (w1_`var'==0 & `var'==1)
* }

**** ----- Fixed Effect
tostring int_month, gen(mo)
tostring int_year,gen(yr)
egen intef_fe=concat(yr mo)



***---- Regression

** Child controls
gen ch_age_m_sq=ch_age_m^2
global ch_controls "ch_sex ch_age_m ch_age_m_sq"

**-- Main Care Giver
replace mcg_read=0 if mcg_sch==.
gen mcg_age_sq=mcg_age^2

global main_care "mcg_age mcg_age_sq mcg_sex mcg_sch"

**-- Livestock
global livestock "lvst_large lvst_small"

**-- Household Control
global hh_control "hhsize_ae dep_ratio"

global hh_head "hh_head_age hh_head_sex hh_head_read"

**-- Welfare Controls
gen cpexp30_g=(cpexp30/hhsize_ae)
replace cpexp30=ln((cpexp30/hhsize_ae)+0.01)
global hh_welfare "poor_13 cpexp30"

global controls "$ch_controls $main_care $livestock $hh_control $hh_welfare"




*****************----------------------------------
**********------------- Table 1: Nutrition 
*****************---------------------------------- 
eststo clear
	*** Stunting
	eststo: xi: probit stunting share_treesonfarm $controls   i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)
	predict resid1
	eststo: xi: probit stunting share_fruit $controls cpexp30 i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)

	*** Under_Weight
	eststo: xi: probit under_weight share_treesonfarm $controls cpexp30 i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)
	eststo: xi: probit under_weight share_fruit ch_age_m $controls cpexp30 i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)

	*** Wasting
	eststo: xi: probit wasting share_treesonfarm $controls cpexp30 i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)
	eststo: xi: probit wasting share_fruit $controls cpexp30 i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)

	*** Stunting
esttab using "$tables/table_nutr_chil.csv", fragment star(* 0.1 ** 0.05 *** 0.01)   nonumber nomtitles replace brackets mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" ) label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N r2, fmt(%9.0f  %9.3f) labels("Observations" "R-squared" ))  indicate( "Strata FE= _Istr*" "Agroecological FE= _Issa*" "Moth Interview FE=_Iint*") keep(share_treesonfarm share_fruit $controls cpexp30) order(share_treesonfarm share_fruit $controls cpexp30)


*****************----------------------------------
**********------------- Table 1: Nutrition 
*****************---------------------------------- 


**-- DAta 0
	eststo clear
	ereturn clear
	eststo: estpost summarize share_treesonfarm  share_fruit $controls cpexp30_g , listwise

	local j=1
	esttab using "$tables/table_desc_3.csv", cells("count mean(fmt(%9.3f)) sd(fmt(%9.3f)) min(fmt(%9.3f)) max(fmt(%9.3f)) " ) label nodepvar  nonum replace noobs nomtitles  fragment collabels("Observations" "Mean" "Std. Deviation" "Min" "Max")





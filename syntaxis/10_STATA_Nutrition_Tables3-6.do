
global main_path "/Users/jcmunoz/Dropbox/Documents/Projects_papers/2020/Miller_etal_2020/TreesOnFarm_Uganda"

global tables "$main_path/02_Paper/tables"

*** **************************
*** 00 - Prepare Baseline Data -- 2005-06
*** **************************

********------------    2013-14:
use "$main_path/01_DataSets/data14_nutr.dta", clear

**---------
**** Fix Agro-ecological FE
rename hhid HHID
merge n:1 HHID using  "$main_path/01_DataSets/UNPS_Geovars_1112.dta", nogen keep(master matched) keepusing(ssa_aez09_x)
*** fix this fixed effect
sort ea ssa_aez09_x
bys ea: replace ssa_aez09_x=ssa_aez09_x[_n-1] if ssa_aez09_x==.
**---------

**---------
*** Only Those households present in boths
gen hhid_s=strlen(HHID)
rename HHID hhid
merge n:1 hhid using `wave0', keep(master matched) force


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
replace poor_13=(poor_13==2)

global controls "$ch_controls $main_care $livestock $hh_control $hh_welfare"

***** Labels
do "$main_path/syntaxis/08_labels.do"

*****************----------------------------------
**********------------- TABLE 3 | Summary statistics for key household characteristics, UNPS 2013–2014.
*****************----------------------------------


	eststo clear
	ereturn clear
	eststo: estpost summarize share_treesonfarm  share_fruit cpexp30_g poor_13  $controls  , listwise

	local j=1
	esttab using "$tables/table_desc_3.csv", cells("mean(fmt(%9.3f)) sd(fmt(%9.3f)) min(fmt(%9.3f)) max(fmt(%9.3f)) " ) label nodepvar  nonum replace noobs nomtitles  fragment collabels("Mean" "Std. Deviation" "Min" "Max")


*****************----------------------------------
**********------------- TABLE 6 | Probit estimates for the relationship between health and nutrition status for children between 6 and 59 months old and the presence of trees on farm 1198 (marginal effects)
*****************----------------------------------
eststo clear
	*** Stunting
	eststo: xi: probit stunting share_treesonfarm $controls   cpexp30  i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)
	margins , dydx(share_treesonfarm) atmeans

	eststo: xi: probit stunting share_fruit $controls   cpexp30  i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)
	margins , dydx(share_fruit) atmeans

	*** Under_Weight
	eststo: xi: probit under_weight share_treesonfarm $controls cpexp30 i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)
	margins , dydx(share_treesonfarm) atmeans

	eststo: xi: probit under_weight share_fruit ch_age_m $controls cpexp30 i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)
	margins , dydx(share_fruit) atmeans

	*** Wasting
	eststo: xi: probit wasting share_treesonfarm $controls cpexp30 i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)
	margins , dydx(share_treesonfarm) atmeans

	eststo: xi: probit wasting share_fruit $controls cpexp30 i.stratum i.intef_fe i.ssa_aez09_x, cl(hhid)
	margins , dydx(share_fruit) atmeans

	*** Stunting
esttab using "$tables/table_6.csv", fragment star(* 0.1 ** 0.05 *** 0.01)   nonumber nomtitles replace brackets mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" ) label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N r2, fmt(%9.0f  %9.3f) labels("Observations" "R-squared" ))  indicate( "Strata FE= _Istr*" "Agroecological FE= _Issa*" "Moth Interview FE=_Iint*") keep(share_treesonfarm share_fruit $controls cpexp30) order(share_treesonfarm share_fruit $controls cpexp30)

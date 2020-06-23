******
** UGANDA PAPER
** Miller et al 2020
**
******


global ds"/Users/juan-carlosm/Dropbox/Documents/Projects_papers/2020/Miller_etal_2020/TreesOnFarm_Uganda/"


**** Baseline Information 2005-06
	u "$ds/00_RawData/2005-06/Household/GSEC1.dta", clear

		keep HHID comm region 
		renvars comm region, prefix("w0_")

	***** 2009-10 Information
	lab def hh_st 1 "Non-Mover Original HH Tracked in 2009/1" 2 "Mover Original HH Tracked in 2009/10" 3 "Drop Out"

	merge 1:1 HHID using "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/00_RawData/2009-10/Household/GSEC1.dta", nogen keep(matched master)
	rename h1aq4b  parish_name
	rename h1aq4 parish_id
	replace hh_status=3 if hh_status==.
	gen hh_st=hh_status
	lab val hh_st hh_st
	renvars parish_id parish_name hh_st stratum, prefix("w1_")
	keep HHID w0_* w1_*

	**** 2010-11 Information
	merge 1:1 HHID using "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/00_RawData/2010-11/Household/GSEC1.dta", nogen keep(matched master)
	rename h1aq4b  parish_name
	replace hh_status=3 if hh_status==.
	gen hh_st=hh_status
	lab val hh_st hh_st
	renvars parish_name hh_st stratum, prefix("w2_")
	keep HHID w0_* w1_* w2_*
	
	****** 2011-12
	merge 1:1 HHID using "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/00_RawData/2011-12/Household/GSEC1.dta", keep(matched master)
	gen hh_status=(w2_parish_name== h1aq4 & _merge==3)
	replace hh_status=3 if _merge==1
	replace hh_status=2 if _merge==3 & hh_status==0
	rename h1aq4  parish_name

	gen hh_st=hh_status
	lab val hh_st hh_st
	renvars parish_name hh_st , prefix("w3_")
	keep HHID w0_* w1_* w2_* w3_*


	******* 2013-2014
	preserve 
		u "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/00_RawData/2013-14/GSEC1.dta", clear
		drop if HHID_old==.
		drop HHID
		tostring HHID_old, gen(HHID) force
		duplicates drop HHID, force

		tempfile w4
		save `w4',replace
	restore

	merge 1:n HHID using `w4', keep(matched master)

	gen parish_name=upper(h1aq4b)

	gen hh_status=(w2_parish_name==parish_name & _merge==3)
	replace hh_status=3 if _merge==1
	replace hh_status=2 if _merge==3 & hh_status==0

	gen hh_st=hh_status
	lab val hh_st hh_st
	renvars parish_name hh_st , prefix("w4_")
	keep HHID w0_* w1_* w2_* w3_* w4_*

	**** Migration
	gen migra=(w1_hh_st==2|w2_hh_st==2|w3_hh_st==2)
	rename HHID hhid

save "/Users/juancarlosmunoz/Box Sync/Uganda_LSMS/05_Analysis/Panel_Structure_Data_Uganda.dta", replace


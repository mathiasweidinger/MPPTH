/*******************************************************************************
Topic: Heckman Selection Correction in Panel Data, using fixed effects

Author: Mathias Weidinger, UNU-MERIT

Last edited: 2021-08-26

Notes: 	The data for this final analysis in my MPP thesis was first compiled in
		R. Since the appropriate estimation framework is not readily available
		in R, I chose to go to Stata, which has the comment XTHECKMANFE.
*******************************************************************************/

* Insall packages 
* ssc install XTHECKMANFE
* ssc install ftools
* ssc install ppml

clear all
cls

* Change directory to your laptop

cd "C:\Users\mathias\ROOT\EDU\UM\MA_MPP\MTH4911\MPP_Thesis\code\analysis"

* Open log file
capture log close
log using "LOG_xtheckmanfe.log", replace

* Make sure your do-file runs without stops
set more off

* Open data from the Input folder 
use "..\..\outputs\NEW_PANEL_final.dta", clear

* Generate unit identifier
gen str_hhid = string(int(hhid),"%07.0f") // %02.0f because 'county' is two digits
gen str_indiv = string(int(indiv),"%02.0f")
egen uniqueid = concat(str_hhid str_indiv)
destring uniqueid, replace

* Generate selection variable

gen selection = childworkchores
replace selection = 1 if childworkchores > 0 & childworkchores !=.

* rescale and clean variables

replace pre = pre/100
gen pre_squared = pre^2
replace age = . if age > 100
gen age_squared = age^2
gen log_totcons = log(totcons)

* label variables
lab var childworkchores "child labour (hrs/week)"
lab var pre "precipitation (by 100 mls)"
lab var pre_squared "precipitation squared"
lab var tmp "mean temperature (°C)"
lab var tmx "maximum temperature (°C)"
lab var tmn "minimum temperature (°C)"
lab var wet "wet days"
lab var age "age"
lab var age_squared "age squared"
lab var log_totcons "log of total household consumption"
lab var MPI "MPI"
lab var sector "sector (urban = 1, rural = 2)"

* create cubic b-splines for climate vars
* ssc install bspline
bspline, x(wet) knots(32.43 44.28 60.14) power(3) gen(bs_wet)
bspline, x(pre) knots(4.438 5.869 8.818) power(3) gen(bs_pre)
bspline, x(tmp) knots(26.24 27.1 28.04) power(3) gen(bs_tmp)
bspline, x(tmx) knots(33.4 34.8 37.5) power(3) gen(bs_tmx)
bspline, x(tmn) knots(18.4 21.6 22.4) power(3) gen(bs_tmn)
*set analysis variables
global Y childworkchores
global C pre pre_squared tmp tmx tmn
global B bs_pre* bs_tmp* bs_tmx* bs_tmn*
global X age age_squared i.sex MPI log_totcons i.sector
global T i.zone i.t // time and geography dummies

* declare panel data
xtset uniqueid t
* Run pooled OLS
reg $Y $C $X $T, vce(cl uniqueid)
est save pool,replace
* Run standard linear FE
xtreg $Y $C $X $T, fe vce(cl uniqueid)
est save base,replace
* Run level xtheckmanfe with panel-bootstrap
xtheckmanfe $Y $C $X, select(selection = $C $X hhsize) reps(200) seed(123)
est save level,replace

predict levelmod
lab var levelmod "predicted child labour (weekly hrs)"

twoway (fpfitci levelmod pre), legend(off) ///
ytitle("predicted weekly hrs") yla(10(5)30) saving(gr_pre, replace)

twoway (fpfitci levelmod tmp), legend(off) ///
ytitle("predicted weekly hrs") yla(10(5)30) saving(gr_tmp, replace)

twoway (fpfitci levelmod tmx), legend(off) ///
ytitle("predicted weekly hrs") yla(10(5)30) saving(gr_tmx, replace)

twoway (fpfitci levelmod tmn), legend(off) ///
ytitle("predicted weekly hrs") yla(10(5)30) saving(gr_tmn, replace)

gr combine gr_pre.gph gr_tmp.gph gr_tmx.gph gr_tmn.gph, saving(combined_effects)

margins, at(pre=(1(1)12))
marginsplot, yla(-20(20)60) saving(me_pre)
margins, at(tmp=(22(1)33))
marginsplot, yla(-20(20)60) saving(me_tmp)
margins, at(tmx=(30(1)43))
marginsplot, yla(-20(20)60) saving(me_tmx)
margins, at(tmn=(13(1)24))
marginsplot, yla(-20(20)60) saving(me_tmn)

gr combine me_pre.gph me_tmp.gph me_tmx.gph me_tmn.gph, saving(marginal_effects)


* Run spline xtheckmanfe with panel-bootstrap
xtheckmanfe $Y $B $X, select(selection = $B $X hhsize) reps(200) seed(123)
est save splines,replace
/*
* THE SAME MODEL, but with FLEXCURVE...
flexcurv, x(pre) power(3) refpts(2.752 4.438 5.869 8.818 10.984) gen(fc_pre)
flexcurv, x(tmp) power(3) refpts(25.1 26.24 27.1 28.04 31.16) gen(fc_tmp)
flexcurv, x(tmx) power(3) refpts(32.2 33.4 34.8 37.5 41.1) gen(fc_tmx)
flexcurv, x(tmn) power(3) refpts(14 18.4 21.6 22.4 23) gen(fc_tmn)

global F fc_pre* fc_tmp* fc_tmx* fc_tmn*
xtset uniqueid t
xtheckmanfe $Y $F $X, select(selection = $F $X hhsize) reps(200) seed(123)
est save flexcurve
*/
*esttab pool base level splines using table.rtf, replace
capture log close
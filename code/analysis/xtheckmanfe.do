
/*******************************************************************************
Topic: Heckman Selection Correction in Panel Data, using fixed effects

Author: Mathias Weidinger, UNU-MERIT

Last edited: 2021-06-27

Notes: 	The data for this final analysis in my MPP thesis was first compiled in
		R, but due to its capabilities with advanced econometric models, I
		chose to swap over to Stata, were packages for XTHECKMANFE exist.
*******************************************************************************/

* Insall packages 
* ssc install XTHECKMANFE
* ssc install ftools
* ssc install ppml

* Change directory to your laptop

cd "C:\Users\mathias\ROOT\EDU\UM\MA_MPP\MTH4911\MPP_Thesis\code\analysis"

* Open log file
capture log close
log using "LOG_xtheckmanfe.log", replace

* Make sure your do-file runs without stops
set more off

* Open data from the Input folder 
use "NGA_panel.dta", clear

* Generate unit identifier
gen str_hhid = string(int(hhid),"%07.0f") // %02.0f because 'country' is two digits
gen str_indiv = string(int(indiv),"%02.0f")
egen uniqueid = concat(str_hhid str_indiv)
gen age_squared = age^2
destring uniqueid, replace

* Generate selection variable

gen selection = childworkchores
replace selection = 1 if childworkchores > 0 & childworkchores !=.

* declare panel data
xtset uniqueid t

* THIS DOES NOT CONVERGE WITH XTHECKMANFE

*set analysis variables
global DV childworkchores
global meandev pre*_md tmp*_md tmx*_md tmn*_md
global IV tmx* tmp* tmn* wet*
global control age age_squared sex MPI totcons sector

* Run xtheckman with RE
xtheckman $DV $IV $control, select(selection = $IV $control)

* Run PPML
ppml $DV $IV $control

* Run xtheckman with FE
probit selection $IV $control

* Run standard linear FE
xtreg $DV $IV $control, fe

* create means


xtheckmanfe $DV $IV $control, select(selection = $IV $control)


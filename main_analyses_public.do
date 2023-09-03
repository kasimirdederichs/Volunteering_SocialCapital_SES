*Do - file for fixed effects models with cluster-robust standard errors:
clear all
set seed 19960909
global data "Y:/Documents/VOL_SC/03_data"
global outputs "Y:/Documents/VOL_SC/05_outputs"

*********************
****MAIN ANALYSIS****
*********************

*OLS:
use "${data}/analysis_data_ols.dta", clear

*models:
reg diversity active_6 if wave==6, robust
regsave using "${data}/models_stata/model0.dta", pval ci addlabel(model , M0) replace
est sto m0

reg diversity active_6 i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6, robust
regsave using "${data}/models_stata/model1.dta", pval ci addlabel(model , M1) replace
est sto m1

reg diversity i_start_tc i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & active_6==0, robust
regsave using "${data}/models_stata/model2.dta", pval ci addlabel(model , M2) replace
est sto m2

*selection hypothesis also holds with respect to ureach but not aprestige (if I control for volearlier6). 
reg ureach i_start_tc i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & active_6==0, robust
reg aprestige i_start_tc i.f_isei_n isced3_4 isced5_6 age woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & active_6==0, robust

esttab m0 m1 m2, b(3) se(3) wide
esttab m0 m1 m2 using "${outputs}/for_export/tableS2a.rtf", replace b(3) se(3) wide nogaps mtitle("Model 0" "Model 1" "Model 2") nonumbers title("Full OLS Models")

*Robustness check: differentiate by three broad types of organizations:
*Model 0:
reg diversity active_6 if wave==6 & (broadvol=="NA" | broadvol=="Sport"), robust
est sto m0_sport
reg diversity active_6 if wave==6 & (broadvol=="NA" | broadvol=="Social"), robust
est sto m0_social
reg diversity active_6 if wave==6 & (broadvol=="NA" | broadvol=="Other"), robust
est sto m0_other
*Model 1:
reg diversity active_6 i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & (broadvol=="NA" | broadvol=="Sport"), robust
est sto m1_sport
reg diversity active_6 i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & (broadvol=="NA" | broadvol=="Social"), robust
est sto m1_social
reg diversity active_6 i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & (broadvol=="NA" | broadvol=="Other"), robust
est sto m1_other
*Model 2:
reg diversity i_start_tc i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & active_6==0 & (broadvol=="NA" | broadvol=="Sport"), robust
est sto m2_sport
reg diversity i_start_tc i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & active_6==0 & (broadvol=="NA" | broadvol=="Social"), robust
est sto m2_social
reg diversity i_start_tc i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & active_6==0 & (broadvol=="NA" | broadvol=="Other"), robust
est sto m2_other

*results by type of organization:
esttab m0_sport m1_sport m2_sport, wide b(3) se(3) //sport
esttab m0_social m1_social m2_social, wide b(3) se(3) //social
esttab m0_other m1_other m2_other, wide b(3) se(3) //other

*Robustness check: models by intensity of involvement

*Lower intensity:
reg diversity active_6 if wave==6 & i_start_intensity_2catc!=2, robust
est sto l0
reg diversity active_6 i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & i_start_intensity_2catc!=2, robust
est sto l1
reg diversity i_start_tc i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & active_6==0  & i_start_intensity_2catc!=2, robust
est sto l2
*Higher intensity:
reg diversity active_6 if wave==6 & i_start_intensity_2catc!=1, robust
est sto h0
reg diversity active_6 i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & i_start_intensity_2catc!=1, robust
est sto h1
reg diversity i_start_tc i.f_isei_n isced3_4 isced5_6 agem woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree i.volearlier6 east populationdensity if wave==6 & active_6==0  & i_start_intensity_2catc!=1, robust
est sto h2

esttab l0 h0 l1 h1 l2 h2, wide b(3) se(3)

*Fixed effects:
use "${data}/analysis_data_fe.dta", clear
*declare panel structure
xtset ID_t wave

gen lreach_i = (-1)*lreach //variable that corrects for the dirction of the effects.
*center (i.e., demean) variables for later fixed effects analyses
by ID_t: center diversity ureach lreach lreach_i i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem populationdensity east 

*models:
*3 Joining -> SC changes
xtreg diversity c.i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight], fe robust
regsave using "${data}/models_stata/model3.dta", pval ci addlabel(model , M3) replace
est sto m3
*4 Joining -> SC changes
xtreg diversity c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight], fe robust
regsave using "${data}/models_stata/model4.dta", pval ci addlabel(model , M4) replace
est sto m4
eststo margin: margins, dydx(c.i_start_tv) at(f_isei_n = (1 2 3)) noestimcheck post //store marginal effects by SES
regsave using "${data}/models_stata/model4_margins.dta", pval ci addlabel(model, M4) replace
*5 Joining -> Ureach changes
xtreg ureach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight], fe robust
regsave using "${data}/models_stata/model5.dta", pval ci addlabel(model , M5) replace
est sto m5
*6 Joining -> Lreach changes
xtreg lreach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight], fe robust
regsave using "${data}/models_stata/model6.dta", pval ci addlabel(model , M6) replace
est sto m6
*7 Joining -> Aprestige changes
xtreg aprestige i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight], fe robust
regsave using "${data}/models_stata/model7.dta", pval ci addlabel(model , M7) replace
est sto m7
*8 Joining -> Ureach changes, by ISEI quartiles
xtreg ureach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight], fe robust
regsave using "${data}/models_stata/model8.dta", pval ci addlabel(model , M8) replace
est sto m8
eststo margin: margins, dydx(c.i_start_tv) at(f_isei_n = (1 2 3)) noestimcheck post //store marginal effects by SES
regsave using "${data}/models_stata/model8_margins.dta", pval ci addlabel(model, M8) replace
*9 Joining -> Lreach changes, by ISEI quartiles
xtreg lreach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight], fe robust
regsave using "${data}/models_stata/model9.dta", pval ci addlabel(model , M9) replace
est sto m9
eststo margin: margins, dydx(c.i_start_tv) at(f_isei_n = (1 2 3)) noestimcheck post //store marginal effects by SES
regsave using "${data}/models_stata/model9_margins.dta", pval ci addlabel(model, M9) replace
*10 Joining -> Aprestige changes, by ISEI quartiles
xtreg aprestige c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight], fe robust
regsave using "${data}/models_stata/model10.dta", pval ci addlabel(model , M10) replace
est sto m10
eststo margin: margins, dydx(c.i_start_tv) at(f_isei_n = (1 2 3)) noestimcheck post //store marginal effects by SES
regsave using "${data}/models_stata/model10_margins.dta", pval ci addlabel(model, M10) replace 

esttab m3 m4 m5 m6 m7 m8 m9 m10, wide b(3) se(3)
esttab m3 m4 m5 m6 m7 m8 m9 m10 using "${outputs}/for_export/tableS2bc.rtf", replace b(3) se(3) wide nogaps mtitle("Model 3" "Model 4" "Model 5" "Model 6" "Model 7" "Model 8" "Model 9" "Model 10") nonumbers title("Full FE Models")

esttab m3 m4 m5 m6 using "${outputs}/for_export/tableS2b.rtf", replace b(3) se(3) wide nogaps mtitle("Model 3" "Model 4" "Model 5" "Model 6" ) nonumbers title("Full FE Models")

esttab m7 m8 m9 m10 using "${outputs}/for_export/tableS2c.rtf", replace b(3) se(3) wide nogaps mtitle("Model 7" "Model 8" "Model 9" "Model 10") nonumbers title("Full FE Models")


*Marginal effects for those models with interactions (save in separate files):
xtreg diversity c.i_start_tv##c.indiversity start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem [pweight=st_weight], fe robust
eststo margin: margins, dydx(i_start_tv) at(c.indiversity = (1 (1) 13)) noestimcheck post
regsave using "${data}/models_stata/main_margins_m4_results.dta", pval ci addlabel(model, M4) replace

xtreg ureach i_start_tv##i.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem [pweight=st_weight], fe robust
eststo margin: margins, dydx(i_start_tv) at(f_isei_n = (1 2 3)) noestimcheck post
regsave using "${data}/models_stata/model8_margins.dta", pval ci addlabel(model, M8) replace

xtreg lreach i_start_tv##i.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem [pweight=st_weight], fe robust
eststo margin: margins, dydx(i_start_tv) at(f_isei_n = (1 2 3)) noestimcheck post
regsave using "${data}/models_stata/model9_margins.dta", pval ci addlabel(model, M9) replace

xtreg aprestige i_start_tv##i.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem [pweight=st_weight], fe robust
eststo margin: margins, dydx(i_start_tv) at(f_isei_n = (1 2 3)) noestimcheck post
regsave using "${data}/models_stata/model10_margins.dta", pval ci addlabel(model, M10) replace

*Robustness check: differentiate by three broad types of organizations:
*Model 3: Diversity
xtreg diversity c.i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Sport", fe robust
est sto m3_sport
xtreg diversity c.i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Social", fe robust
est sto m3_social
xtreg diversity c.i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Other", fe robust
est sto m3_other
*Model 4: Diversity + Interaction
xtreg diversity c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Sport", fe robust
est sto m4_sport
xtreg diversity c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Social", fe robust
est sto m4_social
xtreg diversity c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Other", fe robust
est sto m4_other
*Model 5: Ureach
xtreg ureach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Sport", fe robust
est sto m5_sport
xtreg ureach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Social", fe robust
est sto m5_social
xtreg ureach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Other", fe robust
est sto m5_other
*Model 6: Lreach
xtreg lreach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Sport", fe robust
est sto m6_sport
xtreg lreach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Social", fe robust
est sto m6_social
xtreg lreach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Other", fe robust
est sto m6_other
*Model 7: Aprestige
xtreg aprestige i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Sport", fe robust
est sto m7_sport
xtreg aprestige i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Social", fe robust
est sto m7_social
xtreg aprestige i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Other", fe robust
est sto m7_other
*Model 8: Upper reach + Interaction
xtreg ureach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Sport", fe robust
est sto m8_sport
xtreg ureach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Social", fe robust
est sto m8_social
xtreg ureach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Other", fe robust
est sto m8_other
*Model 9: Lower reach + Interaction
xtreg lreach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Sport", fe robust
est sto m9_sport
xtreg lreach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Social", fe robust
est sto m9_social
xtreg lreach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Other", fe robust
est sto m9_other
*Model 10: Average status + Interaction
xtreg aprestige c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Sport", fe robust
est sto m10_sport
xtreg aprestige c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Social", fe robust
est sto m10_social
xtreg aprestige c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if broadvol=="NA" | broadvol=="Other", fe robust
est sto m10_other

*N of each group:
tab broadvol

esttab m3_sport m4_sport m5_sport m6_sport m7_sport m8_sport m9_sport m10_sport, wide b(3) se(3) //sport
esttab m3_social m4_social m5_social m6_social m7_social m8_social m9_social m10_social, wide b(3) se(3) //social
esttab m3_other m4_other m5_other m6_other m7_other m8_other m9_other m10_other, wide b(3) se(3) //other

*ROBUSTNESS CHECK: by intensity of voluntary involvement
//1=low intensity, 2=high intensity
*models low intensity:
*3 Joining -> SC changes
xtreg diversity c.i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=2, fe robust
est sto l3
*4 Joining -> SC changes
xtreg diversity c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=2, fe robust
est sto l4
*5 Joining -> Ureach changes
xtreg ureach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=2, fe robust
est sto l5
*6 Joining -> Lreach changes
xtreg lreach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=2, fe robust
est sto l6
*7 Joining -> Aprestige changes
xtreg aprestige i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=2, fe robust
est sto l7
*8 Joining -> Ureach changes, by ISEI quartiles
xtreg ureach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=2, fe robust
est sto l8
*9 Joining -> Lreach changes, by ISEI quartiles
xtreg lreach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=2, fe robust
est sto l9
*10 Joining -> Aprestige changes, by ISEI quartiles
xtreg aprestige c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=2, fe robust
est sto l10 

esttab l3 l4 l5 l6 l7 l8 l9 l10, wide b(3) se(3)

*models high intensity:
*3 Joining -> SC changes
xtreg diversity c.i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=1, fe robust
est sto h3
*4 Joining -> SC changes
xtreg diversity c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=1, fe robust
est sto h4
*5 Joining -> Ureach changes
xtreg ureach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=1, fe robust
est sto h5
*6 Joining -> Lreach changes
xtreg lreach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=1, fe robust
est sto h6
*7 Joining -> Aprestige changes
xtreg aprestige i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=1, fe robust
est sto h7
*8 Joining -> Ureach changes, by ISEI quartiles
xtreg ureach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=1, fe robust
est sto h8
*9 Joining -> Lreach changes, by ISEI quartiles
xtreg lreach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=1, fe robust
est sto h9
*10 Joining -> Aprestige changes, by ISEI quartiles
xtreg aprestige c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity [pweight=st_weight] if i_start_intensity_2catc!=1, fe robust
est sto h10 

esttab h3 h4 h5 h6 h7 h8 h9 h10, wide b(3) se(3)

*Get N of respondents who get involved at different intensities:
tab i_start_intensity_2catc


*ROBUSTNESS CHECK: UNWEIGHTED RESULTS:

*models:
*3 Joining -> SC changes
xtreg diversity c.i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity, fe robust
regsave using "${data}/models_stata/uwmodel3.dta", pval ci addlabel(model , uwM3) replace
est sto uwm3
*4 Joining -> SC changes
xtreg diversity c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity, fe robust
regsave using "${data}/models_stata/uwmodel4.dta", pval ci addlabel(model , uwM4) replace
est sto uwm4
eststo margin: margins, dydx(c.i_start_tv) at(f_isei_n = (1 2 3)) noestimcheck post //store marginal effects by SES
regsave using "${data}/models_stata/uwmodel4_margins.dta", pval ci addlabel(model, uwM4) replace
*5 Joining -> Ureach changes
xtreg ureach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity, fe robust
regsave using "${data}/models_stata/uwmodel5.dta", pval ci addlabel(model , uwM5) replace
est sto uwm5
*6 Joining -> Lreach changes
xtreg lreach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity, fe robust
regsave using "${data}/models_stata/uwmodel6.dta", pval ci addlabel(model , uwM6) replace
est sto uwm6
*7 Joining -> Aprestige changes
xtreg aprestige i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity, fe robust
regsave using "${data}/models_stata/uwmodel7.dta", pval ci addlabel(model , uwM7) replace
est sto uwm7
*8 Joining -> Ureach changes, by ISEI quartiles
xtreg ureach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity, fe robust
regsave using "${data}/models_stata/uwmodel8.dta", pval ci addlabel(model , uwM8) replace
est sto uwm8
eststo margin: margins, dydx(c.i_start_tv) at(f_isei_n = (1 2 3)) noestimcheck post //store marginal effects by SES
regsave using "${data}/models_stata/uwmodel8_margins.dta", pval ci addlabel(model, uwM8) replace
*9 Joining -> Lreach changes, by ISEI quartiles
xtreg lreach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity, fe robust
regsave using "${data}/models_stata/uwmodel9.dta", pval ci addlabel(model , uwM9) replace
est sto uwm9
eststo margin: margins, dydx(c.i_start_tv) at(f_isei_n = (1 2 3)) noestimcheck post //store marginal effects by SES
regsave using "${data}/models_stata/uwmodel9_margins.dta", pval ci addlabel(model, uwM9) replace
*10 Joining -> Aprestige changes, by ISEI quartiles
xtreg aprestige c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem east populationdensity, fe robust
regsave using "${data}/models_stata/uwmodel10.dta", pval ci addlabel(model , swM10) replace
est sto uwm10
eststo margin: margins, dydx(c.i_start_tv) at(f_isei_n = (1 2 3)) noestimcheck post //store marginal effects by SES
regsave using "${data}/models_stata/uwmodel10_margins.dta", pval ci addlabel(model, uwM10) replace 

est tab uwm3 uwm4 uwm5 uwm6 uwm7 uwm8 uwm9 uwm10, star
esttab uwm3 uwm4 uwm5 uwm6 uwm7 uwm8 uwm9 uwm10 using "${outputs}/for_export/uwtableS2bc.rtf", replace b(3) se(3) wide nogaps mtitle("Model 3" "Model 4" "Model 5" "Model 6" "Model 7" "Model 8" "Model 9" "Model 10") nonumbers title("Full FE Models, unweighted")

esttab uwm3 uwm4 uwm5 uwm6 using "${outputs}/for_export/uwtableS2b.rtf", replace b(3) se(3) wide nogaps mtitle("Model 3" "Model 4" "Model 5" "Model 6" ) nonumbers title("Full FE Models, unweighted")

esttab uwm7 uwm8 uwm9 uwm10 using "${outputs}/for_export/uwtableS2c.rtf", replace b(3) se(3) wide nogaps mtitle("Model 7" "Model 8" "Model 9" "Model 10") nonumbers title("Full FE Models, unweighted")


***********************
***ROBUSTNESS-CHECKS***
***********************

*ROBUSTNESS-CHECK: two-way FE models:
*3 Joining -> SC changes
xtreg diversity c.i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem i.wave [pweight=st_weight], fe robust
est sto m3_2fe
*4 Joining -> SC changes
xtreg diversity c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem i.wave [pweight=st_weight], fe robust
est sto m4_2fe
*5 Joining -> Ureach changes
xtreg ureach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem i.wave [pweight=st_weight], fe robust
est sto m5_2fe
*6 Joining -> Lreach changes
xtreg lreach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem i.wave [pweight=st_weight], fe robust
est sto m6_2fe
*7 Joining -> Aprestige changes
xtreg aprestige i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem i.wave [pweight=st_weight], fe robust
est sto m7_2fe
*8 Joining -> Ureach changes, by ISEI quartiles
xtreg ureach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem i.wave [pweight=st_weight], fe robust
est sto m8_2fe
*9 Joining -> Lreach changes, by ISEI quartiles
xtreg lreach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem i.wave [pweight=st_weight], fe robust
est sto m9_2fe
*10 Joining -> Aprestige changes, by ISEI quartiles
xtreg aprestige c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem i.wave [pweight=st_weight], fe robust
est sto m10_2fe

est tab m3_2fe m4_2fe m5_2fe m6_2fe m7_2fe m8_2fe m9_2fe m10_2fe, star
esttab m3_2fe m4_2fe m5_2fe m6_2fe m7_2fe m8_2fe m9_2fe m10_2fe using "${outputs}/for_export/tableS2bc.rtf", replace b(3) se(3) wide nogaps mtitle("Model 3" "Model 4" "Model 5" "Model 6" "Model 7" "Model 8" "Model 9" "Model 10") nonumbers title("Full FE Models, two-way FE")

*ROBUSTNESS-CHECK: By gender:
foreach g in 0 1 {
*3 Joining -> SC changes
xtreg diversity c.i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem if woman==`g' [pweight=st_weight], fe robust
est sto m3_woman_`g'
*4 Joining -> SC changes
xtreg diversity c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem if woman==`g' [pweight=st_weight], fe robust
est sto m4_woman_`g'
*5 Joining -> Ureach changes
xtreg ureach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem if woman==`g' [pweight=st_weight], fe robust
est sto m5_woman_`g'
*6 Joining -> Lreach changes
xtreg lreach i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem if woman==`g' [pweight=st_weight], fe robust
est sto m6_woman_`g'
*7 Joining -> Aprestige changes
xtreg aprestige i_start_tv start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem if woman==`g' [pweight=st_weight], fe robust
est sto m7_woman_`g'
*8 Joining -> Ureach changes, by ISEI quartiles
xtreg ureach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem if woman==`g' [pweight=st_weight], fe robust
est sto m8_woman_`g'
*9 Joining -> Lreach changes, by ISEI quartiles
xtreg lreach c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem if woman==`g' [pweight=st_weight], fe robust
est sto m9_woman_`g'
*10 Joining -> Aprestige changes, by ISEI quartiles
xtreg aprestige c.i_start_tv##ib3.f_isei_n start_emp610 end_emp610 new_partner610 quit_partner610 move610 childbirth610 agem if woman==`g' [pweight=st_weight], fe robust
est sto m10_woman_`g'

esttab m3_woman_`g' m4_woman_`g' m5_woman_`g' m6_woman_`g' m7_woman_`g' m8_woman_`g' m9_woman_`g' m10_woman_`g'
}
esttab m3_woman_0 m4_woman_0 m5_woman_0 m6_woman_0 m7_woman_0 m8_woman_0 m9_woman_0 m10_woman_0 using "${outputs}/for_export/tableS2bc_men.rtf", replace b(3) se(3) wide nogaps mtitle("Model 3" "Model 4" "Model 5" "Model 6" "Model 7" "Model 8" "Model 9" "Model 10") nonumbers title("Full FE Models, two-way FE")
esttab m3_woman_1 m4_woman_1 m5_woman_1 m6_woman_1 m7_woman_1 m8_woman_1 m9_woman_1 m10_woman_1 using "${outputs}/for_export/tableS2bc_women.rtf", replace b(3) se(3) wide nogaps mtitle("Model 3" "Model 4" "Model 5" "Model 6" "Model 7" "Model 8" "Model 9" "Model 10") nonumbers title("Full FE Models, two-way FE")


***************************
*Are effects on lreach and ureach sign. different?
*test difference of coefficients - separate models:
qui reg c_ureach c_i_start_tv c_start_emp610 c_end_emp610 c_new_partner610 c_quit_partner610 c_move610 c_childbirth610 c_agem populationdensity east[aweight=st_weight], nocons dof(3894)
est sto regureach1 
qui reg c_lreach_i c_i_start_tv c_start_emp610 c_end_emp610 c_new_partner610 c_quit_partner610 c_move610 c_childbirth610 c_agem populationdensity east[aweight=st_weight], nocons dof(3894)
est sto reglreach1

est tab regureach1 reglreach1, se

suest regureach1 reglreach1
test [regureach1_mean]c_i_start_tv = [reglreach1_mean]c_i_start_tv



*putting all model-tables for main analysis together for export to R.

*main models:
use "${data}/models_stata/model0.dta", clear
append using "${data}/models_stata/model1.dta"
append using "${data}/models_stata/model2.dta"
append using "${data}/models_stata/model3.dta"
append using "${data}/models_stata/model4.dta"
append using "${data}/models_stata/model5.dta"
append using "${data}/models_stata/model6.dta"
append using "${data}/models_stata/model7.dta"
append using "${data}/models_stata/model8.dta"
append using "${data}/models_stata/model9.dta"
append using "${data}/models_stata/model10.dta"

replace var = subinstr(var, "#", "X", .) //replace # with X for handling variable in R.
save "${data}/models_stata/main_regression_results.dta", replace



*****************************************************************
***SIMULATION: WHAT WOULD HAPPEN IF NOBODY WOULD GET INVOLVED?***
*****************************************************************
*Use wide data, only those who were not active at t1:
use "${data}/wide_data_imp_hyp.dta", clear
keep if active_6==0

*Create some dummy variables:
gen i_start_cf = 0
gen d_volearlier6_1 = volearlier6
replace d_volearlier6_1 = 0 if volearlier6==2
gen d_volearlier6_2 = volearlier6
replace d_volearlier6_2 = 0 if volearlier6==1

*Generate hypothetical distribution of Social capital at t1:
foreach k in diversity ureach aprestige {
*run OLS regression (with interaction effects to account for heterogeneous returns to volunteering, predict values, and extract residuals:
reg `k'_10 c.i_start##c.d_misei_n c.i_start##c.d_lisei_n `k'_6 age woman migback_6 religiosity b5extra b5openn b5neuro b5consc b5agree d_volearlier6_1 d_volearlier6_2 east_6 populationdensity_6 [pw=s_weight], robust
predict `k'_10_pred
gen `k'_10_res = `k'_10 - `k'_10_pred
*generate counterfactual values for diversity_10 by plugging in the counterfactual volunteering variable in the previous regression equation. 
gen `k'_10_cf = _b[_cons] + i_start_cf * _b[i_start] + ///
	d_misei_n * _b[d_misei_n] + ///
	i_start_cf * d_misei_n * _b[i_start#d_misei_n] + ///
	d_lisei_n * _b[d_lisei_n] + ///
	i_start_cf * d_lisei_n * _b[i_start#d_lisei_n] + ///
	`k'_6 * _b[`k'_6] + ///
	age * _b[age] + woman * _b[woman] + migback_6 * _b[migback_6] + ///
	religiosity * _b[religiosity] + b5extra * _b[b5extra] + ///
	b5openn * _b[b5openn] + b5neuro * _b[b5neuro] + b5consc * _b[b5consc] + ///
	b5agree * _b[b5agree] + ///
	d_volearlier6_1 * _b[d_volearlier6_1] + ///
	d_volearlier6_2 * _b[d_volearlier6_2] + ///
	east_6 * _b[east_6] + ///
	populationdensity_6 * _b[populationdensity_6] + ///
	`k'_10_res
}
*show mean values in observed and counterfactual distribution:
tabstat diversity_10 diversity_10_cf ureach_10 ureach_10_cf aprestige_10 aprestige_10_cf, by(f_isei_n)

*extract means for observed and counterfactual distribution:
foreach k in diversity ureach aprestige {
mean `k'_10 `k'_10_cf [pw=s_weight], over(f_isei_n)
mat means_`k' = r(table)'
mat list means_`k'
putexcel set ${data}/means_`k', replace
putexcel C1 = mat(means_`k'), colnames
putexcel A1 = "SES"
putexcel A2 = "Low"
putexcel A3 = "Medium"
putexcel A4 = "High"
putexcel A5 = "Low"
putexcel A6 = "Medium"
putexcel A7 = "High"
putexcel B1 = "Scenario"
putexcel B2:B4 = "Observed"
putexcel B5:B7 = "Counterfactual"
}

*estimate SES-inequalities in observed and counterfactual scenario: (high vs low and high vs medium SES)
foreach k in diversity ureach aprestige {
*actually observed scenario:
reg `k'_10 d_misei_n d_lisei_n [iw=s_weight]
mat `k'_act = r(table)' //extract coefficients and confidence intervals for plot
est sto `k'_m1 //store results for suest testing
*counterfactual scenario:
reg `k'_10_cf d_misei_n d_lisei_n [iw=s_weight]
mat `k'_cf = r(table)'
est sto `k'_m2
*Comparing the two:
suest `k'_m1 `k'_m2
test [`k'_m1_mean]d_misei_n = [`k'_m2_mean]d_misei_n
test [`k'_m1_mean]d_lisei_n = [`k'_m2_mean]d_lisei_n
}
//suest section reveals that inequalities in diversity do not change meaningfully across scenarios but that inequalities in ureach and aprestige are significantly smaller in a world with (vis-a-vis) a world without volunteering. 

*Overview of inequalities in observed and counterfactual scenario wrt to all three SC indicators:	
esttab diversity_m1 diversity_m2 ureach_m1 ureach_m2 aprestige_m1 aprestige_m2

*export inequality coefficients:
putexcel set ${data}/simulation_results_2022_11_15, replace
putexcel A1 = "Outcome"
putexcel A2:A7 = "Social Capital"
putexcel A8:A13 = "Upper Reach"
putexcel A14:A19 = "Average Prestige"
putexcel B1 = "Scenario"
putexcel B2:B4 = "Observed"
putexcel B5:B7 = "Counterfactual"
putexcel B8:B10 = "Observed"
putexcel B11:B13 = "Counterfactual"
putexcel B14:B16 = "Observed"
putexcel B17:B19 = "Counterfactual"
putexcel C1 = "Coefficient_name"
putexcel C2 = "Medium"
putexcel C3 = "Low"
putexcel C4 = "Constant"
putexcel C5 = "Medium"
putexcel C6 = "Low"
putexcel C7 = "Constant"
putexcel C8 = "Medium"
putexcel C9 = "Low"
putexcel C10 = "Constant"
putexcel C11 = "Medium"
putexcel C12 = "Low"
putexcel C13 = "Constant"
putexcel C14 = "Medium"
putexcel C15 = "Low"
putexcel C16 = "Constant"
putexcel C17 = "Medium"
putexcel C18 = "Low"
putexcel C19 = "Constant"
putexcel D1 = "Coefficient_value"
putexcel D2 = mat(diversity_act)
putexcel D5 = mat(diversity_cf)
putexcel D8 = mat(ureach_act)
putexcel D11 = mat(ureach_cf)
putexcel D14 = mat(aprestige_act)
putexcel D17 = mat(aprestige_cf)
putexcel E1 = "SE"
putexcel F1 = "tvalue"
putexcel G1 = "pvalue"
putexcel H1 = "LowerCI"
putexcel I1 = "UpperCI"
putexcel J1 = "N"
putexcel K1 = "crit_val"
putexcel L1 = "fin"

*estimate SES-inequalities in observed and counterfactual scenario: (medium vs low and medium vs high SES)
foreach k in diversity ureach aprestige {
*actually observed scenario:
reg `k'_10 d_hisei_n d_lisei_n [iw=s_weight]
mat `k'_mref_act = r(table)' //extract coefficients and confidence intervals for plot
est sto `k'_m1_mref //store results for suest testing
*counterfactual scenario:
reg `k'_10_cf d_hisei_n d_lisei_n [iw=s_weight]
mat `k'_mref_cf = r(table)'
est sto `k'_m2_mref
*Comparing the two:
suest `k'_m1_mref `k'_m2_mref
test [`k'_m1_mref_mean]d_hisei_n = [`k'_m2_mref_mean]d_hisei_n
test [`k'_m1_mref_mean]d_lisei_n = [`k'_m2_mref_mean]d_lisei_n
}

*export inequality coefficients:
putexcel set ${data}/simulation_results_mref, replace
putexcel A1 = "Outcome"
putexcel A2:A7 = "Social Capital"
putexcel A8:A13 = "Upper Reach"
putexcel A14:A19 = "Average Prestige"
putexcel B1 = "Scenario"
putexcel B2:B4 = "Observed"
putexcel B5:B7 = "Counterfactual"
putexcel B8:B10 = "Observed"
putexcel B11:B13 = "Counterfactual"
putexcel B14:B16 = "Observed"
putexcel B17:B19 = "Counterfactual"
putexcel C1 = "Coefficient_name"
putexcel C2 = "High"
putexcel C3 = "Low"
putexcel C4 = "Constant"
putexcel C5 = "High"
putexcel C6 = "Low"
putexcel C7 = "Constant"
putexcel C8 = "High"
putexcel C9 = "Low"
putexcel C10 = "Constant"
putexcel C11 = "High"
putexcel C12 = "Low"
putexcel C13 = "Constant"
putexcel C14 = "High"
putexcel C15 = "Low"
putexcel C16 = "Constant"
putexcel C17 = "High"
putexcel C18 = "Low"
putexcel C19 = "Constant"
putexcel D1 = "Coefficient_value"
putexcel D2 = mat(diversity_mref_act)
putexcel D5 = mat(diversity_mref_cf)
putexcel D8 = mat(ureach_mref_act)
putexcel D11 = mat(ureach_mref_cf)
putexcel D14 = mat(aprestige_mref_act)
putexcel D17 = mat(aprestige_mref_cf)
putexcel E1 = "SE"
putexcel F1 = "tvalue"
putexcel G1 = "pvalue"
putexcel H1 = "LowerCI"
putexcel I1 = "UpperCI"
putexcel J1 = "N"
putexcel K1 = "crit_val"
putexcel L1 = "fin"


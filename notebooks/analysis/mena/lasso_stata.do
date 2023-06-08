/*Lasso regression for vulnerability index indicators */

 import delimited "C:\Users\wb569257\OneDrive - WBG\WoE\Vulnerability\data\Vul_Indicators_ADM2_std.csv", clear
 
 global hazards pm25_2019 quake_2020 flood_2020  loss_2020 precip_2020 temp_2020 drought_2020 
 
foreach x of varlist pm25_2019  temp_2020 drought_2020 vulpopsh_2016_2020 exposure_2016_2020 rwi_mean {
	replace `x'="." if `x'=="NA"
	destring `x', replace
}


tabstat exposure_2016_2020  econ_2016_2020 vulpopsh_2016_2020

label var pm25_2019 "PM 2.5"
label var quake_2020 "Earthquake risk"
label var flood_2020 "Flood Risk"
label var loss_2020  "Forest Loss"
label var temp_2020  "Temperature variation"
label var precip_2020 "Precipitation Variation"
label var drought_2020 "Drought Risk"

/*Split data*/

splitsample , generate(sample) split(.25 .25 .25 .25) rseed(64242)

tab id_adm, gen(admin1_)

local y vulpopsh_2016_2020

gen `y'_lasso=.


foreach k of numlist 1/4 {
quietly: reg `y' $hazards admin1_* if sample!=`k'


quietly: lasso linear `y' $hazards admin1_* if sample!=`k', nolog rseed(12345)
estimates store cv
predict `y'_lasso_`k' if sample==`k', xb
replace `y'_lasso=`y'_lasso_`k' if sample==`k'

drop `y'_*_`k'
}


gen MSE_lasso=(`y'-`y'_lasso)^2


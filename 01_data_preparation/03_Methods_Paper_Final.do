* Methods Paper - DHS data 2015 and up *
* Under 6 months and at 6 months / under 183 days and at 183 days

* Set this to the folder where you cloned the repository, e.g.
* global root "C:/Users/yourname/Documents/DHS_Methods_EBF"
global root "."


use "$root/data/restricted/DHS_harmonized.dta", clear

*Only include survey 2015 and onward as they have the agedays variable 
drop if survey_year<2015
*drop all surveys after 2015 that still do not have the agedays variable or no BF data or missing food/liquid from the whole survey
drop if survey=="Afghanistan 2015" | survey=="India 2015" | survey=="Indonesia 2017" | survey=="Myanmar 2015" | survey=="Senegal 2015" | survey=="Senegal 2016" | survey=="Colombia 2015" | survey=="Philippines 2017"

keep food liquid BF prelacteal v000 v001 v002 v003 v005 v024 v025 v023 v021 v101 v106 v190 v012 v501 m4 b4 agedays agemonths EBF_DHS EBF_DHS_p EBF_strict country_name survey_year survey_code survey edu

drop if missing(edu)

* Generate new agemonths and agedays variables for non-linear model 
gen agemonths_sq = agemonths^2
gen agemonths_cub = agemonths^3
gen agemonths_quar = agemonths^4

gen agedays_sq = agedays^2
gen agedays_cub = agedays^3
gen agedays_quar = agedays^4

*EBF definitions used: EBF_DHS, EBF_DHS_p and EBF_strict (created in 02_DHS-data-research.do)

*Use this dataset for MI in R
save "$root/data/restricted/DHS_methods_analysis.dta", replace 

**********************************************************************************************************************************************

* DHS data 2000 and up *

use "$root/data/restricted/DHS_harmonized.dta", clear

*Only include survey 2000 and onward as they have the agedays variable 
drop if survey_year<2000
*drop all surveys after 2000 that do not have BF data or missing food/liquid/prelacteal from the whole survey
drop if survey=="Colombia 2000" | survey=="Colombia 2015" | survey=="Philippines 2013" | survey=="Philippines 2017" | survey=="Bangladesh 2000" | survey=="Bangladesh 2004" | survey=="Ethiopia 2000" | survey=="Gabon 2000" | survey=="Haiti 2000" |survey=="India 2000" | survey=="Jordan 2009" | survey=="Namibia 2000" | survey=="Rwanda 2000" | survey=="Rwanda 2007" | survey=="Vietnam 2002"

keep food liquid BF prelacteal v000 v001 v002 v003 v005 v024 v025 v023 v021 v101 v106 v190 v012 v501 m4 b4 agedays agemonths EBF_DHS EBF_DHS_p EBF_strict country_name survey_year survey_code survey edu

drop if missing(edu)

* Generate new agemonths and agedays variables for non-linear model 
gen agemonths_sq = agemonths^2
gen agemonths_cub = agemonths^3
gen agemonths_quar = agemonths^4

gen agedays_sq = agedays^2
gen agedays_cub = agedays^3
gen agedays_quar = agedays^4

*Use this dataset for MI in R
save "$root/data/restricted/DHS_methods_analysis_2000-2022.dta", replace 
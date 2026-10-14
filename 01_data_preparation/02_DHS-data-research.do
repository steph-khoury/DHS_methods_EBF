*original dataset
* Set this to the folder where you cloned the repository, e.g.
* global root "C:/Users/yourname/Documents/DHS_Methods_EBF"
global root "."

* Input: appended DHS Children's Recode (KR) files (restricted, see data/README.md)
				
use "$root/data/restricted/wf_breastfeeding_dhs_22Jun2026.dta", clear

********************************************
**alive children					
*dropping children who died
drop if b5==0

*dropping children who do not live with their mothers and the last born child
keep if b9 == 0
keep if _n == 1 | caseid != caseid[_n-1]	

**age
*generating age in months where you subtract the birthdate from the date of visit
gen agemonths =.
replace agemonths= v008-b3
*generating age in days (only from survey years 2015 and up)
gen agedays =.
replace agedays= v008a-b18
*age at 6 months is equal to 183 days after birth

*NOTE: Survey_year was incorretly inputted in the DTA file for Ethiopia 2016 --> It was inputted it as 2008 but there is no 2008
replace survey_year=2016 if survey_year==2008 & country_name=="Ethiopia"
**survey: country_name and year 
gen survey = country_name + " " + string(survey_year)
tab survey

********************************************
*education (v106 and s105) 
gen edu=.
replace edu=0 if v106==0
replace edu=1 if v106==1
replace edu=2 if v106==2
replace edu=3 if v106==3
replace edu=. if v106==4
replace edu=. if v106==6
replace edu=. if v106==8
replace edu=. if v106==9 

replace edu=0 if country_name== "Yemen, Rep." & s105_string== "no education"
replace edu=1 if country_name== "Yemen, Rep." & s105_string== "primary"
replace edu=1 if country_name== "Yemen, Rep." & s105_string== "fundamental (preparatory, unified)"
replace edu=2 if country_name== "Yemen, Rep." & s105_string== "secondary"
replace edu=2 if country_name== "Yemen, Rep." & s105_string== "diploma before secondary"
replace edu=3 if country_name== "Yemen, Rep." & s105_string== "university/higher"
replace edu=3 if country_name== "Yemen, Rep." & s105_string== "diploma after secondary"

label define edu 0 "No education" 1 "Primary" 2 "Secondary" 3 "Higher" 


********************************************
*creating seperate variables for the variables that have the same question code

**s435a-n
gen s435a_plainwater_turkey=.
replace s435a_plainwater_turkey=0 if s435a_question=="plain water" & s435a_string=="no"
replace s435a_plainwater_turkey=1 if s435a_question=="plain water" & s435a_string=="yes"

gen s435b_sugarwater_turkey=.
replace s435b_sugarwater_turkey=0 if s435b_question=="sugar water" & s435b_string=="no"
replace s435b_sugarwater_turkey=1 if s435b_question=="sugar water" & s435b_string=="yes"

gen s435c_juice_turkey=.
replace s435c_juice_turkey=0 if s435c_question=="juice" & s435c_string=="no"
replace s435c_juice_turkey=1 if s435c_question=="juice" & s435c_string=="yes"

gen s435d_tea_turkey=.
replace s435d_tea_turkey=0 if s435d_question=="tea" & s435d_string=="no"
replace s435d_tea_turkey=1 if s435d_question=="tea" & s435d_string=="yes"

gen s435e_babyformula_turkey=.
replace s435e_babyformula_turkey=0 if s435e_question=="baby formula" & s435e_string=="no"
replace s435e_babyformula_turkey=1 if s435e_question=="baby formula" & s435e_string=="yes"

gen s435f_yogurt_turkey=.
replace s435f_yogurt_turkey=0 if s435f_question=="yogurt" & s435f_string=="no"
replace s435f_yogurt_turkey=1 if s435f_question=="yogurt" & s435f_string=="yes"

gen s435g_pudding_turkey=.
replace s435g_pudding_turkey=0 if s435g_question=="pudding" & s435g_string=="no"
replace s435g_pudding_turkey=1 if s435g_question=="pudding" & s435g_string=="yes"

gen s435h_juiceofmeal_turkey=.
replace s435h_juiceofmeal_turkey=0 if s435h_question=="juice of a cooked meal" & s435h_string=="no"
replace s435h_juiceofmeal_turkey=1 if s435h_question=="juice of a cooked meal" & s435h_string=="yes"

gen s435i_turkishdelight_turkey=.
replace s435i_turkishdelight_turkey=0 if s435i_question=="turkish delight" & s435i_string=="no"
replace s435i_turkishdelight_turkey=1 if s435i_question=="turkish delight" & s435i_string=="yes"

gen s435j_boxedmilk_turkey=.
replace s435j_boxedmilk_turkey=0 if s435j_question=="bottles/boxed milk" & s435j_string=="no"
replace s435j_boxedmilk_turkey=1 if s435j_question=="bottles/boxed milk" & s435j_string=="yes"

gen s435k_milk_turkey=.
replace s435k_milk_turkey=0 if s435k_question=="fresh milk" & s435k_string=="no"
replace s435k_milk_turkey=1 if s435k_question=="fresh milk" & s435k_string=="yes"

gen s435l_soup_turkey=.
replace s435l_soup_turkey=0 if s435l_question=="soup" & s435l_string=="no"
replace s435l_soup_turkey=1 if s435l_question=="soup" & s435l_string=="yes"

gen s435m_otherliquids_turkey=.
replace s435m_otherliquids_turkey=0 if s435m_question=="other liquids" & s435m_string=="no"
replace s435m_otherliquids_turkey=1 if s435m_question=="other liquids" & s435m_string=="yes"

gen s435n_othersolids_turkey=.
replace s435n_othersolids_turkey=0 if s435n_question=="other solid, semi-solid food" & s435n_string=="no"
replace s435n_othersolids_turkey=1 if s435n_question=="other solid, semi-solid food" & s435n_string=="yes"

**s449a
gen s449a_sugarwater_chad=.
replace s449a_sugarwater_chad=0 if s449a_question=="liquids/water, sugar water" & s449a_string=="no"
replace s449a_sugarwater_chad=1 if s449a_question=="liquids/water, sugar water" & s449a_string=="yes"

gen s449a_water_turkey=.
replace s449a_water_turkey=0 if s449a_question=="food 24 hours: water" & s449a_string=="no"
replace s449a_water_turkey=1 if s449a_question=="food 24 hours: water" & s449a_string=="yes"

gen s449a_water_bangladesh=.
replace s449a_water_bangladesh=0 if survey=="Bangladesh 2004" & s449a1_string=="no"
replace s449a_water_bangladesh=1 if survey=="Bangladesh 2004" & s449a1_string=="yes"

**s449b
gen s449b_babyformula_chad=.
replace s449b_babyformula_chad=0 if s449b_question=="liquids/baby formula" & s449b_string=="no"
replace s449b_babyformula_chad=1 if s449b_question=="liquids/baby formula" & s449b_string=="yes"

gen s449b_bottledmilk_turkey=.
replace s449b_bottledmilk_turkey=0 if s449b_question=="food 24 hours: bottled milk" & s449b_string=="no"
replace s449b_bottledmilk_turkey=1 if s449b_question=="food 24 hours: bottled milk" & s449b_string=="yes"

gen s449b_sugarwater_bangladesh=.
replace s449b_sugarwater_bangladesh=0 if survey=="Bangladesh 2004" & s449b1_string=="no"
replace s449b_sugarwater_bangladesh=1 if survey=="Bangladesh 2004" & s449b1_string=="yes"

**s449c
gen s449c_othermilk_chad=.
replace s449c_othermilk_chad=0 if s449c_question=="liquids/other milk" & s449c_string=="no"
replace s449c_othermilk_chad=1 if s449c_question=="liquids/other milk" & s449c_string=="yes"

gen s449c_yogurt_turkey=.
replace s449c_yogurt_turkey=0 if s449c_question=="food 24 hours: yogurt" & s449c_string=="no"
replace s449c_yogurt_turkey=1 if s449c_question=="food 24 hours: yogurt" & s449c_string=="yes"

gen s449c_babyformula_bangladesh=.
replace s449c_babyformula_bangladesh=0 if survey=="Bangladesh 2004" & s449c1_string=="yes"
replace s449c_babyformula_bangladesh=1 if survey=="Bangladesh 2004" & s449c1_string=="yes"

**s449d
gen s449d_juice_chad=.
replace s449d_juice_chad=0 if s449d_question=="liquids/fruit juice" & s449d_string=="no"
replace s449d_juice_chad=1 if s449d_question=="liquids/fruit juice" & s449d_string=="yes"

gen s449d_cheese_turkey=.
replace s449d_cheese_turkey=0 if s449d_question=="food 24 hours: cheese" & s449d_string=="no"
replace s449d_cheese_turkey=1 if s449d_question=="food 24 hours: cheese" & s449d_string=="yes"

gen s449d_milk_bangladesh=.
replace s449d_milk_bangladesh=0 if survey=="Bangladesh 2004" & s449d1_string=="no"
replace s449d_milk_bangladesh=1 if survey=="Bangladesh 2004" & s449d1_string=="yes"

**s449e
gen s449e_otherliquids_chad=.
replace s449e_otherliquids_chad=0 if s449e_question=="liquids/other liquids" & s449e_string=="no"
replace s449e_otherliquids_chad=1 if s449e_question=="liquids/other liquids" & s449e_string=="yes"

gen s449e_eggs_turkey=.
replace s449e_eggs_turkey=0 if s449e_question=="food 24 hours: egg" & s449e_string=="no"
replace s449e_eggs_turkey=1 if s449e_question=="food 24 hours: egg" & s449e_string=="yes"

gen s449e_otherliquids_bangladesh=.
replace s449e_otherliquids_bangladesh=0 if survey=="Bangladesh 2004" & s449e1_string=="no"
replace s449e_otherliquids_bangladesh=1 if survey=="Bangladesh 2004" & s449e1_string=="yes"

**s449f
gen s449f_cereals_chad=.
replace s449f_cereals_chad=0 if s449f_question=="solid food/cereals" & s449f_string=="no"
replace s449f_cereals_chad=1 if s449f_question=="solid food/cereals" & s449f_string=="yes"

gen s449f_meat_turkey=.
replace s449f_meat_turkey=0 if s449f_question=="food 24 hours: meat" & s449f_string=="no"
replace s449f_meat_turkey=1 if s449f_question=="food 24 hours: meat" & s449f_string=="yes"

gen s449f_banana_bangladesh=.
replace s449f_banana_bangladesh=0 if survey=="Bangladesh 2004" & s449f1_string=="no"
replace s449f_banana_bangladesh=1 if survey=="Bangladesh 2004"  & s449f1_string=="yes"

**s449g
gen s449g_tubers_chad=.
replace s449g_tubers_chad=0 if s449g_question=="solid food/tubers" & s449g_string=="no"
replace s449g_tubers_chad=1 if s449g_question=="solid food/tubers" & s449g_string=="yes"

gen s449g_chicken_turkey=.
replace s449g_chicken_turkey=0 if s449g_question=="food 24 hours: chicken" & s449g_string=="no"
replace s449g_chicken_turkey=1 if s449g_question=="food 24 hours: chicken" & s449g_string=="yes"

gen s449g_greens_bangladesh=.
replace s449g_greens_bangladesh=0 if survey=="Bangladesh 2004" & s449g1_string=="no"
replace s449g_greens_bangladesh=1 if survey=="Bangladesh 2004" & s449g1_string=="yes"

**s449h
gen s449h_greens_chad=.
replace s449h_greens_chad=0 if s449h_question=="solid food/green leafy vegetables" & s449h_string=="no"
replace s449h_greens_chad=1 if s449h_question=="solid food/green leafy vegetables" & s449h_string=="yes"

gen s449h_fish_turkey=.
replace s449h_fish_turkey=0 if s449h_question=="food 24 hours: fish" & s449h_string=="no"
replace s449h_fish_turkey=1 if s449h_question=="food 24 hours: fish" & s449h_string=="yes"

gen s449h_rice_bangladesh=.
replace s449h_rice_bangladesh=0 if survey=="Bangladesh 2004" & s449h1_string=="no"
replace s449h_rice_bangladesh=1 if survey=="Bangladesh 2004" & s449h1_string=="yes"

**s449i
gen s449i_carrots_chad=.
replace s449i_carrots_chad=0 if s449i_question=="solid food/carrots" & s449i_string=="no"
replace s449i_carrots_chad=1 if s449i_question=="solid food/carrots" & s449i_string=="yes"

gen s449i_legumes_turkey=.
replace s449i_legumes_turkey=0 if s449i_question=="food 24 hours: dry legumes" & s449i_string=="no"
replace s449i_legumes_turkey=1 if s449i_question=="food 24 hours: dry legumes" & s449i_string=="yes"

gen s449i_meat_bangladesh=.
replace s449i_meat_bangladesh=0 if survey=="Bangladesh 2004" & s449i1_string=="no"
replace s449i_meat_bangladesh=1 if survey=="Bangladesh 2004" & s449i1_string=="yes"

**s449j
gen s449j_oranges_chad=.
replace s449j_oranges_chad=0 if survey=="Chad 2004" & s449j_string=="no"
replace s449j_oranges_chad=1 if survey=="Chad 2004" & s449j_string=="yes"

gen s449j_fruitveggie_turkey=.
replace s449j_fruitveggie_turkey=0 if s449j_question=="food 24 hours: vegetables/fruits" & s449j_string=="no"
replace s449j_fruitveggie_turkey=1 if s449j_question=="food 24 hours: vegetables/fruits" & s449j_string=="yes"

gen s449j_dal_bangladesh=.
replace s449j_dal_bangladesh=0 if survey=="Bangladesh 2004" & s449j1_string=="no"
replace s449j_dal_bangladesh=1 if survey=="Bangladesh 2004" & s449j1_string=="yes"

**s449k
gen s449k_otherfruits_chad=.
replace s449k_otherfruits_chad=0 if s449k_question=="solid food/other fruits" & s449k_string=="no"
replace s449k_otherfruits_chad=1 if s449k_question=="solid food/other fruits" & s449k_string=="yes"

gen s449k_bread_turkey=.
replace s449k_bread_turkey=0 if s449k_question=="food 24 hours: bread" & s449k_string=="no"
replace s449k_bread_turkey=1 if s449k_question=="food 24 hours: bread" & s449k_string=="yes"

gen s449k_other_bangladesh=.
replace s449k_other_bangladesh=0 if survey=="Bangladesh 2004" & s449k1_string=="no"
replace s449k_other_bangladesh=1 if survey=="Bangladesh 2004" & s449k1_string=="yes"

**s449l
gen s449l_dairy_chad=.
replace s449l_dairy_chad=0 if s449l_question=="solid food/dairy products" & s449l_string=="no"
replace s449l_dairy_chad=1 if s449l_question=="solid food/dairy products" & s449l_string=="yes"

gen s449l_cereals_turkey=.
replace s449l_cereals_turkey=0 if s449l_question=="food 24 hours: cereals or grains" & s449l_string=="no"
replace s449l_cereals_turkey=1 if s449l_question=="food 24 hours: cereals or grains" & s449l_string=="yes"

**s449m
gen s449m_meat_chad=.
replace s449m_meat_chad=0 if survey=="Chad 2004" & s449m_string=="no"
replace s449m_meat_chad=1 if survey=="Chad 2004" & s449m_string=="yes"

gen s449m_babyformula_turkey=.
replace s449m_babyformula_turkey=0 if s449m_question=="food 24 hours: baby formula" & s449m_string=="no"
replace s449m_babyformula_turkey=1 if s449m_question=="food 24 hours: baby formula" & s449m_string=="yes"

**s449n
gen s449n_othersolid_chad=.
replace s449n_othersolid_chad=0 if survey=="Chad 2004" & s449n_string=="no"
replace s449n_othersolid_chad=1 if survey=="Chad 2004" & s449n_string=="yes"

gen s449n_juiceofmeal_turkey=.
replace s449n_juiceofmeal_turkey=0 if s449n_question=="food 24 hours: juice of cooked meal" & s449n_string=="no"
replace s449n_juiceofmeal_turkey=1 if s449n_question=="food 24 hours: juice of cooked meal" & s449n_string=="yes"

**s449o
gen s449o_soup_turkey=.
replace s449o_soup_turkey=0 if s449o_question=="food 24 hours: soup" & s449o_string=="no"
replace s449o_soup_turkey=1 if s449o_question=="food 24 hours: soup" & s449o_string=="yes"

**s449p
gen s449p_junkfood_turkey=.
replace s449p_junkfood_turkey=0 if s449p_question=="food 24 hours: junk foods" & s449p_string=="no"
replace s449p_junkfood_turkey=1 if s449p_question=="food 24 hours: junk foods" & s449p_string=="yes"

**s449q
gen s449q_beverage_turkey=.
replace s449q_beverage_turkey=0 if s449q_question=="food 24 hours: beverage" & s449q_string=="no"
replace s449q_beverage_turkey=1 if s449q_question=="food 24 hours: beverage" & s449q_string=="yes"

**s469a
gen s469a_plainwater=.
replace s469a_plainwater=0 if s469a_question=="child drank plain water" & s469a_string=="no"
replace s469a_plainwater=1 if s469a_question=="child drank plain water" & s469a_string=="yes"

gen s469a_monthsBF_bangladesh = cond(country_name == "Bangladesh" & s469a >= 0 & s469a <= 35, s469a, .)

gen s469a_firstnonBF_tanzania=.
replace s469a_firstnonBF_tanzania=0 if s469a_question=="age first time fed something other than breast milk (in month)" & s469a_string=="not yet started given anything"
replace s469a_firstnonBF_tanzania=1 if s469a_question=="age first time fed something other than breast milk (in month)" & s469a_string=="less than 1 month"

**s469b
gen s469b_drank_formula_armenia=.
replace s469b_drank_formula_armenia=0 if s469b_question=="child drank commercially produced infant formula" & s469b_string=="no"
replace s469b_drank_formula_armenia=1 if s469b_question=="child drank commercially produced infant formula" & s469b_string=="yes"

gen s469b_ate_babyfood_ethiopia=.
replace s469b_ate_babyfood_ethiopia=0 if s469b_question=="child ate commercially fortified baby food" & s469b_string=="no"
replace s469b_ate_babyfood_ethiopia=1 if s469b_question=="child ate commercially fortified baby food" & s469b_string=="yes"

**s469c
gen s469c_drank_milk_armenia=.
replace s469c_drank_milk_armenia=0 if s469c_question=="child drank milk" & s469c_string=="no"
replace s469c_drank_milk_armenia=1 if s469c_question=="child drank milk" & s469c_string=="yes"

gen s469c_drank_milk_ethiopia=.
replace s469c_drank_milk_ethiopia=0 if country_name=="Ethiopia" & s469c_string=="no"
replace s469c_drank_milk_ethiopia=1 if country_name=="Ethiopia" & s469c_string=="yes"

**s469d
gen s469d_drank_narine_armenia=.
replace s469d_drank_narine_armenia=0 if s469d_question=="child drank narine" & s469d_string=="no"
replace s469d_drank_narine_armenia=1 if s469d_question=="child drank narine" & s469d_string=="yes"

gen s469d_drank_sugary_ethiopia=.
replace s469d_drank_sugary_ethiopia=0 if country_name=="Ethiopia" & s469d_string=="no"
replace s469d_drank_sugary_ethiopia=1 if country_name=="Ethiopia" & s469d_string=="yes"

**s469e
gen s469e_drank_juice_armenia=.
replace s469e_drank_juice_armenia=0 if s469e_question=="child drank juice" & s469e_string=="no"
replace s469e_drank_juice_armenia=1 if s469e_question=="child drank juice" & s469e_string=="yes"

gen s469e_drank_tea_ethiopia=.
replace s469e_drank_tea_ethiopia=0 if country_name=="Ethiopia" & s469e_string=="no"
replace s469e_drank_tea_ethiopia=1 if country_name=="Ethiopia" & s469e_string=="yes"

**s469f
gen s469f_drank_tea_armenia=.
replace s469f_drank_tea_armenia=0 if country_name=="Armenia" & s469f_string=="no"
replace s469f_drank_tea_armenia=1 if country_name=="Armenia" & s469f_string=="yes"

gen s469f_drank_other_ethiopia=.
replace s469f_drank_other_ethiopia=0 if country_name=="Ethiopia" & s469f_string=="no"
replace s469f_drank_other_ethiopia=1 if country_name=="Ethiopia" & s469f_string=="yes"

**s469g
gen s469g_drank_other_armenia=.
replace s469g_drank_other_armenia=0 if country_name=="Armenia" & s469g_string=="no"
replace s469g_drank_other_armenia=1 if country_name=="Armenia" & s469g_string=="yes"

**s470
gen s470_yesterday_foodwater_bolivia=.
replace s470_yesterday_foodwater_bolivia=0 if country_name=="Bolivia" & s470_string=="no"
replace s470_yesterday_foodwater_bolivia=1 if country_name=="Bolivia" & s470_string=="yes"

**s470ca
gen s470ca_ate_babyfood_armenia=.
replace s470ca_ate_babyfood_armenia=0 if country_name=="Armenia" & s470ca_string=="no"
replace s470ca_ate_babyfood_armenia=1 if country_name=="Armenia" & s470ca_string=="yes"

**s470cb
gen s470cb_ate_bread_armenia=.
replace s470cb_ate_bread_armenia=0 if country_name=="Armenia" & s470cb_string=="no"
replace s470cb_ate_bread_armenia=1 if country_name=="Armenia" & s470cb_string=="yes"

**s470cc
gen s470cc_white_potato_armenia=.
replace s470cc_white_potato_armenia=0 if country_name=="Armenia" & s470cc_string=="no"
replace s470cc_white_potato_armenia=1 if country_name=="Armenia" & s470cc_string=="yes"

**s470cd
gen s470cd_pumpkin_armenia=.
replace s470cd_pumpkin_armenia=0 if country_name=="Armenia" & s470cd_string=="no"
replace s470cd_pumpkin_armenia=1 if country_name=="Armenia" & s470cd_string=="yes"

**s470ce
gen s470ce_greens_armenia=.
replace s470ce_greens_armenia=0 if country_name=="Armenia" & s470ce_string=="no"
replace s470ce_greens_armenia=1 if country_name=="Armenia" & s470ce_string=="yes"

**s470cf
gen s470cf_mango_armenia=.
replace s470cf_mango_armenia=0 if country_name=="Armenia" & s470cf_string=="no"
replace s470cf_mango_armenia=1 if country_name=="Armenia" & s470cf_string=="yes"

** s470cg
gen s470cg_other_fruits_armenia=.
replace s470cg_other_fruits_armenia=0 if country_name=="Armenia" & s470cg_string=="no"
replace s470cg_other_fruits_armenia=1 if country_name=="Armenia" & s470cg_string=="yes"

**s470ch
gen s470ch_liver_armenia=.
replace s470ch_liver_armenia=0 if country_name=="Armenia" & s470ch_string=="no"
replace s470ch_liver_armenia=1 if country_name=="Armenia" & s470ch_string=="yes"

**s470ci
gen s470ci_beef_armenia=.
replace s470ci_beef_armenia=0 if country_name=="Armenia" & s470ci_string=="no"
replace s470ci_beef_armenia=1 if country_name=="Armenia" & s470ci_string=="yes"

**s470cj
gen s470cj_chicken_armenia=.
replace s470cj_chicken_armenia=0 if country_name=="Armenia" & s470cj_string=="no"
replace s470cj_chicken_armenia=1 if country_name=="Armenia" & s470cj_string=="yes"

**s470ck
gen s470ck_eggs_armenia=.
replace s470ck_eggs_armenia=0 if country_name=="Armenia" & s470ck_string=="no"
replace s470ck_eggs_armenia=1 if country_name=="Armenia" & s470ck_string=="yes"

**s470cl
gen s470cl_fish_armenia=.
replace s470cl_fish_armenia=0 if country_name=="Armenia" & s470cl_string=="no"
replace s470cl_fish_armenia=1 if country_name=="Armenia" & s470cl_string=="yes"

**s470cm
gen s470cm_beans_armenia=.
replace s470cm_beans_armenia=0 if country_name=="Armenia" & s470cm_string=="no"
replace s470cm_beans_armenia=1 if country_name=="Armenia" & s470cm_string=="yes"

**s470cn
gen s470cn_nuts_armenia=.
replace s470cn_nuts_armenia=0 if country_name=="Armenia" & s470cn_string=="no"
replace s470cn_nuts_armenia=1 if country_name=="Armenia" & s470cn_string=="yes"

**s470co
gen s470co_cheese_armenia=.
replace s470co_cheese_armenia=0 if country_name=="Armenia" & s470co_string=="no"
replace s470co_cheese_armenia=1 if country_name=="Armenia" & s470co_string=="yes"

**s470cp
gen s470cp_oil_armenia=.
replace s470cp_oil_armenia=0 if country_name=="Armenia" & s470cp_string=="no"
replace s470cp_oil_armenia=1 if country_name=="Armenia" & s470cp_string=="yes"

**s470cq
gen s470cq_othersolid_armenia=.
replace s470cq_othersolid_armenia=0 if country_name=="Armenia" & s470cq_string=="no"
replace s470cq_othersolid_armenia=1 if country_name=="Armenia" & s470cq_string=="yes"

**s471
gen s471_age_liquidfood_bolivia=.
replace s471_age_liquidfood_bolivia = cond(country_name == "Bolivia" & s471 >= 0 & s471 <= 99, s471, .)
replace s471_age_liquidfood_bolivia=100 if country_name=="Bolivia" & s471_string=="aun no recibe"
replace s471_age_liquidfood_bolivia=. if s471==99
label define s471_age_liquidfood_bolivia 100 "No"
label value s471_age_liquidfood_bolivia s471_age_liquidfood_bolivia

**s472
gen s472_times_food24_armenia=.
replace s472_times_food24_armenia = cond(country_name == "Armenia" & s472 >= 0 & s472 <= 7, s472, .)
replace s472_times_food24_armenia=0 if s472_times_food24_armenia==0
replace s472_times_food24_armenia=1 if s472_times_food24_armenia>0 & s472_times_food24_armenia<=7

gen s472_times_food24_ethiopia=.
replace s472_times_food24_ethiopia = cond(country_name == "Ethiopia" & survey_year== 2005 & s472 >= 0 & s472 <= 7, s472, .)
replace s472_times_food24_ethiopia=0 if s472_times_food24_ethiopia==0
replace s472_times_food24_ethiopia=1 if s472_times_food24_ethiopia>0 & s472_times_food24_ethiopia<=7

**s472a
gen s472a_egg_turkey=.
replace s472a_egg_turkey=0 if s472a_question=="food 24 hours: egg" & s472a_string=="no"
replace s472a_egg_turkey=1 if s472a_question=="food 24 hours: egg" & s472a_string=="yes"

gen s472a_carrots_chad=.
replace s472a_carrots_chad=0 if s472a_question=="food: carrots" & s472a_string=="no"
replace s472a_carrots_chad=1 if s472a_question=="food: carrots" & s472a_string=="carrots"

**s472b
gen s472b_cheese_turkey=.
replace s472b_cheese_turkey=0 if s472b_question=="food 24 hours: cheese" & s472b_string=="no"
replace s472b_cheese_turkey=1 if s472b_question=="food 24 hours: cheese" & s472b_string=="yes"

gen s472b_sorrel_chad=.
replace s472b_sorrel_chad=0 if s472b_question=="food: sorrel" & s472b_string=="no"
replace s472b_sorrel_chad=1 if s472b_question=="food: sorrel" & s472b_string=="sorrel"

**s472c
gen s472c_yogurt_turkey=.
replace s472c_yogurt_turkey=0 if s472c_question=="food 24 hours: yogurt" & s472c_string=="no"
replace s472c_yogurt_turkey=1 if s472c_question=="food 24 hours: yogurt" & s472c_string=="yes"

gen s472c_cabbage_chad=.
replace s472c_cabbage_chad=0 if s472c_question=="food: cabbage" & s472c_string=="no"
replace s472c_cabbage_chad=1 if s472c_question=="food: cabbage" & s472c_string=="cabbage"

**s472d
gen s472d_meat_turkey=.
replace s472d_meat_turkey=0 if s472d_question=="food 24 hours: meat" & s472d_string=="no"
replace s472d_meat_turkey=1 if s472d_question=="food 24 hours: meat" & s472d_string=="yes"

gen s472d_cassava_chad=.
replace s472d_cassava_chad=0 if s472d_question=="food: cassava leaves" & s472d_string=="no"
replace s472d_cassava_chad=1 if s472d_question=="food: cassava leaves" & s472d_string=="cassava leaves"

**s472e
gen s472e_chicken_turkey=.
replace s472e_chicken_turkey=0 if s472e_question=="food 24 hours: chicken" & s472e_string=="no"
replace s472e_chicken_turkey=1 if s472e_question=="food 24 hours: chicken" & s472e_string=="yes"

gen s472e_mango_chad=.
replace s472e_mango_chad=0 if s472e_question=="food: mangoes" & s472e_string=="no"
replace s472e_mango_chad=1 if s472e_question=="food: mangoes" & s472e_string=="mangoes"

**s472f
gen s472f_fish_turkey=.
replace s472f_fish_turkey=0 if s472f_question=="food 24 hours: fish" & s472f_string=="no"
replace s472f_fish_turkey=1 if s472f_question=="food 24 hours: fish" & s472f_string=="yes"

gen s472f_papaya_chad=.
replace s472f_papaya_chad=0 if s472f_question=="food: papayas" & s472f_string=="no"
replace s472f_papaya_chad=1 if s472f_question=="food: papayas" & s472f_string=="papayas"

**s472g
gen s472g_fruitsveggie_turkey=.
replace s472g_fruitsveggie_turkey=0 if s472g_question=="food 24 hours: vegetables/fruits" & s472g_string=="no"
replace s472g_fruitsveggie_turkey=1 if s472g_question=="food 24 hours: vegetables/fruits" & s472g_string=="yes"

gen s472g_melons_chad=.
replace s472g_melons_chad=0 if s472g_question=="food: melons" & s472g_string=="no"
replace s472g_melons_chad=1 if s472g_question=="food: melons" & s472g_string=="melons"

**s472h
gen s472h_water_turkey=.
replace s472h_water_turkey=0 if s472h_question=="food 24 hours: plain water" & s472h_string=="no"
replace s472h_water_turkey=1 if s472h_question=="food 24 hours: plain water" & s472h_string=="yes"

gen s472h_meat_chad=.
replace s472h_meat_chad=0 if s472h_question=="food: meat" & s472h_string=="no"
replace s472h_meat_chad=1 if s472h_question=="food: meat" & s472h_string=="meat"

**s472i
gen s472i_babyformula_turkey=.
replace s472i_babyformula_turkey=0 if s472i_question=="food 24 hours: baby formula" & s472i_string=="no"
replace s472i_babyformula_turkey=1 if s472i_question=="food 24 hours: baby formula" & s472i_string=="yes"

gen s472i_liver_chad=.
replace s472i_liver_chad=0 if s472i_question=="food: liver" & s472i_string=="no"
replace s472i_liver_chad=1 if s472i_question=="food: liver" & s472i_string=="liver"

**s472j
gen s472j_bottledmilk_turkey=.
replace s472j_bottledmilk_turkey=0 if s472j_question=="food 24 hours: bottled milk" & s472j_string=="no"
replace s472j_bottledmilk_turkey=1 if s472j_question=="food 24 hours: bottled milk" & s472j_string=="yes"

gen s472j_fish_chad=.
replace s472j_fish_chad=0 if s472j_question=="food: fish" & s472j_string=="no"
replace s472j_fish_chad=1 if s472j_question=="food: fish" & s472j_string=="fish"

**s472k
gen s472k_cereal_turkey=.
replace s472k_cereal_turkey=0 if s472k_question=="food 24 hours: cereals or grains" & s472k_string=="no"
replace s472k_cereal_turkey=1 if s472k_question=="food 24 hours: cereals or grains" & s472k_string=="yes"

gen s472k_egg_chad=.
replace s472k_egg_chad=0 if s472k_question=="food: eggs" & s472k_string=="no"
replace s472k_egg_chad=1 if s472k_question=="food: eggs" & s472k_string=="eggs"

**s472l
gen s472l_juiceofmeal_turkey=.
replace s472l_juiceofmeal_turkey=0 if s472l_question=="food 24 hours: soup/juice of cooked meal" & s472l_string=="no"
replace s472l_juiceofmeal_turkey=1 if s472l_question=="food 24 hours: soup/juice of cooked meal" & s472l_string=="yes"

**s472m
gen s472m_bread_turkey=.
replace s472m_bread_turkey=0 if s472m_question=="food 24 hours: bread" & s472m_string=="no"
replace s472m_bread_turkey=1 if s472m_question=="food 24 hours: bread" & s472m_string=="yes"

**s567a
gen s567a_plainwater_moldova=.
replace s567a_plainwater_moldova=0 if country_name=="Moldova" & s567a_string=="no"
replace s567a_plainwater_moldova=1 if country_name=="Moldova" & s567a_string=="yes"

**s567b
gen s567b_infantformula_moldova=.
replace s567b_infantformula_moldova=0 if country_name=="Moldova" & s567b_string=="no"
replace s567b_infantformula_moldova=1 if country_name=="Moldova" & s567b_string=="yes"

**s567c
gen s567c_babyfood_moldova=.
replace s567c_babyfood_moldova=0 if country_name=="Moldova" & s567c_string=="no"
replace s567c_babyfood_moldova=1 if country_name=="Moldova" & s567c_string=="yes"

**s567d
gen s567d_porridge_moldova=.
replace s567d_porridge_moldova=0 if country_name=="Moldova" & s567d_string=="no"
replace s567d_porridge_moldova=1 if country_name=="Moldova" & s567d_string=="yes"

**s567e
gen s567e_otherfoodliquid_moldova=.
replace s567e_otherfoodliquid_moldova=0 if country_name=="Moldova" & s567e_string=="no"
replace s567e_otherfoodliquid_moldova=1 if country_name=="Moldova" & s567e_string=="yes"

**s568ca
gen s568ca_drankmilk_moldova=.
replace s568ca_drankmilk_moldova=0 if country_name=="Moldova" & s568ca_string=="no"
replace s568ca_drankmilk_moldova=1 if country_name=="Moldova" & s568ca_string=="yes"

**s568cb
gen s568cb_drankteacoffee_moldova=.
replace s568cb_drankteacoffee_moldova=0 if country_name=="Moldova" & s568cb_string=="no"
replace s568cb_drankteacoffee_moldova=1 if country_name=="Moldova" & s568cb_string=="yes"

**s568cc
gen s568cc_sugarydrinks_moldova=.
replace s568cc_sugarydrinks_moldova=0 if country_name=="Moldova" & s568cc_string=="no"
replace s568cc_sugarydrinks_moldova=1 if country_name=="Moldova" & s568cc_string=="yes"

**s568cd
gen s568cd_otherliquids_moldova=.
replace s568cd_otherliquids_moldova=0 if country_name=="Moldova" & s568cd_string=="no"
replace s568cd_otherliquids_moldova=1 if country_name=="Moldova" & s568cd_string=="yes"

**s568ce
gen s568ce_atebread_moldova=.
replace s568ce_atebread_moldova=0 if country_name=="Moldova" & s568ce_string=="no"
replace s568ce_atebread_moldova=1 if country_name=="Moldova" & s568ce_string=="yes"

**s568cf
gen s568cf_atepumpkin_moldova=.
replace s568cf_atepumpkin_moldova=0 if country_name=="Moldova" & s568cf_string=="no"
replace s568cf_atepumpkin_moldova=1 if country_name=="Moldova" & s568cf_string=="yes"

**s568cg
gen s568cg_atepotato_moldova=.
replace s568cg_atepotato_moldova=0 if country_name=="Moldova" & s568cg_string=="no"
replace s568cg_atepotato_moldova=1 if country_name=="Moldova" & s568cg_string=="yes"

**s568ch
gen s568ch_ategreens_moldova=.
replace s568ch_ategreens_moldova=0 if country_name=="Moldova" & s568ch_string=="no"
replace s568ch_ategreens_moldova=1 if country_name=="Moldova" & s568ch_string=="yes"

**s568ci
gen s568ci_atepeaches_moldova=.
replace s568ci_atepeaches_moldova=0 if country_name=="Moldova" & s568ci_string=="no"
replace s568ci_atepeaches_moldova=1 if country_name=="Moldova" & s568ci_string=="yes"

**s568cj
gen s568cj_atefruitsveggies_moldova=.
replace s568cj_atefruitsveggies_moldova=0 if country_name=="Moldova" & s568cj_string=="no"
replace s568cj_atefruitsveggies_moldova=1 if country_name=="Moldova" & s568cj_string=="yes"

**s568ck
gen s568ck_ateliver_moldova=.
replace s568ck_ateliver_moldova=0 if country_name=="Moldova" & s568ck_string=="no"
replace s568ck_ateliver_moldova=1 if country_name=="Moldova" & s568ck_string=="yes"

**s568cl
gen s568cl_atebeef_moldova=.
replace s568cl_atebeef_moldova=0 if country_name=="Moldova" & s568cl_string=="no"
replace s568cl_atebeef_moldova=1 if country_name=="Moldova" & s568cl_string=="yes"

**s568cm
gen s568cm_atechicken_moldova=.
replace s568cm_atechicken_moldova=0 if country_name=="Moldova" & s568cm_string=="no"
replace s568cm_atechicken_moldova=1 if country_name=="Moldova" & s568cm_string=="yes"

**s568cn
gen s568cn_ateeggs_moldova=.
replace s568cn_ateeggs_moldova=0 if country_name=="Moldova" & s568cn_string=="no"
replace s568cn_ateeggs_moldova=1 if country_name=="Moldova" & s568cn_string=="yes"

**s568co
gen s568co_atefish_moldova=.
replace s568co_atefish_moldova=0 if country_name=="Moldova" & s568co_string=="no"
replace s568co_atefish_moldova=1 if country_name=="Moldova" & s568co_string=="yes"

**s568cp
gen s568cp_atebeans_moldova=.
replace s568cp_atebeans_moldova=0 if country_name=="Moldova" & s568cp_string=="no"
replace s568cp_atebeans_moldova=1 if country_name=="Moldova" & s568cp_string=="yes"

**s568cq
gen s568cq_atenuts_moldova=.
replace s568cq_atenuts_moldova=0 if country_name=="Moldova" & s568cq_string=="no"
replace s568cq_atenuts_moldova=1 if country_name=="Moldova" & s568cq_string=="yes"

**s568cr
gen s568cr_atedairy_moldova=.
replace s568cr_atedairy_moldova=0 if country_name=="Moldova" & s568cr_string=="no"
replace s568cr_atedairy_moldova=1 if country_name=="Moldova" & s568cr_string=="yes"

**s568cs
gen s568cs_ateoil_moldova=.
replace s568cs_ateoil_moldova=0 if country_name=="Moldova" & s568cs_string=="no"
replace s568cs_ateoil_moldova=1 if country_name=="Moldova" & s568cs_string=="yes"

**s568ct
gen s568ct_atesugary_moldova=.
replace s568ct_atesugary_moldova=0 if country_name=="Moldova" & s568ct_string=="no"
replace s568ct_atesugary_moldova=1 if country_name=="Moldova" & s568ct_string=="yes"

**s568cu
gen s568cu_othersolids_moldova=.
replace s568cu_othersolids_moldova=0 if country_name=="Moldova" & s568cu_string=="no"
replace s568cu_othersolids_moldova=1 if country_name=="Moldova" & s568cu_string=="yes"

**s570
gen s570_times_food24_moldova=.
replace s570_times_food24_moldova= cond(country_name == "Moldova" & survey_year== 2005 & s570 >= 0 & s570 <= 7, s570, .)
replace s570_times_food24_moldova=0 if s570_times_food24_moldova==0
replace s570_times_food24_moldova=1 if s570_times_food24_moldova>0 & s570_times_food24_moldova<=7

**v469i
gen v469i_otherliquids_cameroon=.
replace v469i_otherliquids_cameroon= cond(country_name == "Cameroon" & survey_year== 2004 & v469i >= 0 & v469i <= 7, v469i, .)
replace v469i_otherliquids_cameroon=0 if v469i_otherliquids_cameroon==0
replace v469i_otherliquids_cameroon=1 if v469i_otherliquids_cameroon>0 & v469i_otherliquids_cameroon<=7

gen v469i_sugarwater_colombia=.
replace v469i_sugarwater_colombia= cond(country_name == "Colombia" & survey_year== 2000 & v469i >= 0 & v469i <= 7, v469i, .)
replace v469i_sugarwater_colombia=0 if v469i_sugarwater_colombia==0
replace v469i_sugarwater_colombia=1 if v469i_sugarwater_colombia>0 & v469i_sugarwater_colombia<=7

gen v469i_otherliquids_colombia=.
replace v469i_otherliquids_colombia= cond(country_name == "Colombia" & survey_year== 2004 & v469i >= 0 & v469i <= 7, v469i, .)
replace v469i_otherliquids_colombia=0 if v469i_otherliquids_colombia==0
replace v469i_otherliquids_colombia=1 if v469i_otherliquids_colombia>0 & v469i_otherliquids_colombia<=7

gen v469i_ricewater_madagascar=.
replace v469i_ricewater_madagascar= cond(country_name == "Madagascar" & survey_year== 2003 & v469i >= 0 & v469i <= 7, v469i, .)
replace v469i_ricewater_madagascar=0 if v469i_ricewater_madagascar==0
replace v469i_ricewater_madagascar=1 if v469i_ricewater_madagascar>0 & v469i_ricewater_madagascar<=7

gen v469i_thobwa_malawi=.
replace v469i_thobwa_malawi= cond(country_name == "Malawi" & survey_year== 2000 & v469i >= 0 & v469i <= 7, v469i, .)
replace v469i_thobwa_malawi=0 if v469i_thobwa_malawi==0
replace v469i_thobwa_malawi=1 if v469i_thobwa_malawi>0 & v469i_thobwa_malawi<=7

gen v469i_milk_namibia=.
replace v469i_milk_namibia= cond(country_name == "Namibia" & survey_year== 2000 & v469i >= 0 & v469i <= 7, v469i, .)
replace v469i_milk_namibia=0 if v469i_milk_namibia==0
replace v469i_milk_namibia=1 if v469i_milk_namibia>0 & v469i_milk_namibia<=7

gen v469i_coffee_philippines=.
replace v469i_coffee_philippines= cond(country_name == "Philippines" & survey_year== 2003 & v469i >= 0 & v469i <= 7, v469i, .)
replace v469i_coffee_philippines=0 if v469i_coffee_philippines==0
replace v469i_coffee_philippines=1 if v469i_coffee_philippines>0 & v469i_coffee_philippines<=7

gen v469i_soup_rwanda=.
replace v469i_soup_rwanda= cond(country_name == "Rwanda" & survey_year== 2005 & v469i >= 0 & v469i <= 7, v469i, .)
replace v469i_soup_rwanda=0 if v469i_soup_rwanda==0
replace v469i_soup_rwanda=1 if v469i_soup_rwanda>0 & v469i_soup_rwanda<=7 

**v469j
gen v469j_otherliquids_cameroon=.
replace v469j_otherliquids_cameroon= cond(country_name == "Cameroon" & survey_year== 2004 & v469j >= 0 & v469j <= 7, v469j, .)
replace v469j_otherliquids_cameroon=0 if v469j_otherliquids_cameroon==0
replace v469j_otherliquids_cameroon=1 if v469j_otherliquids_cameroon>0 & v469j_otherliquids_cameroon<=7

gen v469j_juice_namibia=.
replace v469j_juice_namibia= cond(country_name == "Namibia" & survey_year== 2000 & v469j >= 0 & v469j <= 7, v469j, .)
replace v469j_juice_namibia=0 if v469j_juice_namibia==0
replace v469j_juice_namibia=1 if v469j_juice_namibia>0 & v469j_juice_namibia<=7

gen v469j_otherliquids_rwanda=.
replace v469j_otherliquids_rwanda= cond(country_name == "Rwanda" & survey_year== 2005 & v469j >= 0 & v469j <= 7, v469j, .)
replace v469j_otherliquids_rwanda=0 if v469j_otherliquids_rwanda==0
replace v469j_otherliquids_rwanda=1 if v469j_otherliquids_rwanda>0 & v469j_otherliquids_rwanda<=7 

**s492b_a-g
*s492b_a
replace s492b_a=0 if country_name=="Congo, Rep." & s492b_a==0
replace s492b_a=1 if country_name=="Congo, Rep." & s492b_a > 0 & s492b_a<=7
*s492b_b
replace s492b_b=0 if country_name=="Congo, Rep." & s492b_b==0
replace s492b_b=1 if country_name=="Congo, Rep." & s492b_b > 0 & s492b_b<=7
*s492b_c
replace s492b_c=0 if country_name=="Congo, Rep." & s492b_c==0
replace s492b_c=1 if country_name=="Congo, Rep." & s492b_c > 0 & s492b_c<=7
*s492b_d
replace s492b_d=0 if country_name=="Congo, Rep." & s492b_d==0
replace s492b_d=1 if country_name=="Congo, Rep." & s492b_d > 0 & s492b_d<=7
*s492b_e
replace s492b_e=0 if country_name=="Congo, Rep." & s492b_e==0
replace s492b_e=1 if country_name=="Congo, Rep." & s492b_e > 0 & s492b_e<=7
*s492b_f
replace s492b_f=0 if country_name=="Congo, Rep." & s492b_f==0
replace s492b_f=1 if country_name=="Congo, Rep." & s492b_f > 0 & s492b_f<=7
*s492b_g
replace s492b_g=0 if country_name=="Congo, Rep." & s492b_g==0
replace s492b_g=1 if country_name=="Congo, Rep." & s492b_g > 0 & s492b_g<=7

**s493b_a-k
*s493b_a
replace s493b_a=0 if country_name=="Congo, Rep." & s493b_a==0
replace s493b_a=1 if country_name=="Congo, Rep." & s493b_a > 0 & s493b_a<=7
*s493b_b
replace s493b_b=0 if country_name=="Congo, Rep." & s493b_b==0
replace s493b_b=1 if country_name=="Congo, Rep." & s493b_b > 0 & s493b_b<=7
*s493b_c
replace s493b_c=0 if country_name=="Congo, Rep." & s493b_c==0
replace s493b_c=1 if country_name=="Congo, Rep." & s493b_c > 0 & s493b_c<=7
*s493b_d
replace s493b_d=0 if country_name=="Congo, Rep." & s493b_d==0
replace s493b_d=1 if country_name=="Congo, Rep." & s493b_d > 0 & s493b_d<=7
*s493b_e
replace s493b_e=0 if country_name=="Congo, Rep." & s493b_e==0
replace s493b_e=1 if country_name=="Congo, Rep." & s493b_e > 0 & s493b_e<=7
*s493b_f
replace s493b_f=0 if country_name=="Congo, Rep." & s493b_f==0
replace s493b_f=1 if country_name=="Congo, Rep." & s493b_f > 0 & s493b_f<=7
*s493b_g
replace s493b_g=0 if country_name=="Congo, Rep." & s493b_g==0
replace s493b_g=1 if country_name=="Congo, Rep." & s493b_g > 0 & s493b_g<=7
*s493b_h
replace s493b_h=0 if country_name=="Congo, Rep." & s493b_h==0
replace s493b_h=1 if country_name=="Congo, Rep." & s493b_h > 0 & s493b_h<=7
*s493b_i
replace s493b_i=0 if country_name=="Congo, Rep." & s493b_i==0
replace s493b_i=1 if country_name=="Congo, Rep." & s493b_i > 0 & s493b_i<=7
*s493b_j
replace s493b_j=0 if country_name=="Congo, Rep." & s493b_j==0
replace s493b_j=1 if country_name=="Congo, Rep." & s493b_j > 0 & s493b_j<=7
*s493b_k
replace s493b_k=0 if country_name=="Congo, Rep." & s493b_k==0
replace s493b_k=1 if country_name=="Congo, Rep." & s493b_k > 0 & s493b_k<=7

**s486a-d
*s498a
gen s498a_water_pakistan=.
replace s498a_water_pakistan=0 if country_name=="Pakistan" & s498a_string=="no"
replace s498a_water_pakistan=1 if country_name=="Pakistan" & s498a_string=="yes"

*s498b
gen s498b_milk_pakistan=.
replace s498b_milk_pakistan=0 if country_name=="Pakistan" & s498b_string=="no"
replace s498b_milk_pakistan=1 if country_name=="Pakistan" & s498b_string=="yes"

*s498c
gen s498c_juice_pakistan=.
replace s498c_juice_pakistan=0 if country_name=="Pakistan" & s498c_string=="no"
replace s498c_juice_pakistan=1 if country_name=="Pakistan" & s498c_string=="yes"

*s498d
gen s498d_food_pakistan=.
replace s498d_food_pakistan=0 if country_name=="Pakistan" & s498d_string=="no"
replace s498d_food_pakistan=1 if country_name=="Pakistan" & s498d_string=="yes"

**s1203a-d
*s1203a
gen s1203a_water_egypt=.
replace s1203a_water_egypt=0 if country_name=="Egypt, Arab Rep." & s1203a_string=="no"
replace s1203a_water_egypt=1 if country_name=="Egypt, Arab Rep." & s1203a_string=="yes"

*s1203b
gen s1203b_formula_egypt=.
replace s1203b_formula_egypt=0 if country_name=="Egypt, Arab Rep." & s1203b_string=="no"
replace s1203b_formula_egypt=1 if country_name=="Egypt, Arab Rep." & s1203b_string=="yes"

*s1203c
gen s1203c_babyfood_egypt=.
replace s1203c_babyfood_egypt=0 if country_name=="Egypt, Arab Rep." & s1203c_string=="no"
replace s1203c_babyfood_egypt=1 if country_name=="Egypt, Arab Rep." & s1203c_string=="yes"

*s1203d
gen s1203d_porridge_egypt=.
replace s1203d_porridge_egypt=0 if country_name=="Egypt, Arab Rep." & s1203d_string=="no"
replace s1203d_porridge_egypt=1 if country_name=="Egypt, Arab Rep." & s1203d_string=="yes"

**s1206
gen s1206_food_egypt=.
replace s1206_food_egypt=0 if country_name=="Egypt, Arab Rep." & s1206==0
replace s1206_food_egypt=1 if country_name=="Egypt, Arab Rep." & s1206 > 0 & s1206<=7


**m4
replace m4=. if m4==99 
label define m4 93 "Ever breastfed, not currently breastfeeding" 94 "Never breastfed" 95 "Still breastfeeding" 96 "Breastfed until died" 97 "Inconsistent" 98 "Dont know" 
label value m4 m4
*I dropped m4_question and m4_string because it was not needed

**m5
replace m5=. if m5==99 
label define m5 93 "Ever breastfed, not currently breastfeeding" 94 "Never breastfed" 95 "Still breastfeeding" 96 "Breastfed until died" 97 "Inconsistent" 98 "Dont know" 
label value m5 m5
drop m5_question 
drop m5_string
*I dropped m5_question and m5_string because it was not needed (they all asked the same question)

**m39
drop m39_question 
drop m39_string
*I dropped m39_question and m39_string bc it was not needed (they all asked the same question)
gen m39_food24=.
replace m39_food24=0 if m39==0
replace m39_food24=1 if m39>0 & m39<=7

*m55 = gave anything other than breastmilk in the first 3 days 
**m5a-z = prelacteal feedings
drop m55a_question 
drop m55a_string
drop m55b_question 
drop m55b_string
drop m55c_question 
drop m55c_string
drop m55d_question 
drop m55d_string
drop m55e_question 
drop m55e_string
drop m55f_question 
drop m55f_string
drop m55g_question 
drop m55g_string
drop m55h_question 
drop m55h_string
drop m55i_question 
drop m55i_string
drop m55j_question 
drop m55j_string
drop m55k_question 
drop m55k_string
drop m55l_question 
drop m55l_string
drop m55m_question 
drop m55m_string
drop m55n_question 
drop m55n_string
drop m55o_question 
drop m55o_string
drop m55z_question
drop m55z_string
drop m55x_question
drop m55x_string 
*I dropped m55a-i, m55z, m55x bc all asked the same question
*I dropped m55j-n question and string bc ask country specific questions but I did not adjust this bc if they answer yes for any then they did not EBF
*I dropped m55o_question, m55o_string bc no country answered this question

**v469a-z - change from continuous to binary
*v469a
gen v469a_yesno=.
replace v469a_yesno=0 if v469a==0
replace v469a_yesno=1 if v469a > 0 & v469a <= 7

*v469b
gen v469b_yesno=.
replace v469b_yesno=0 if v469b==0
replace v469b_yesno=1 if v469b > 0 & v469b <= 7

*v469c
gen v469c_yesno=.
replace v469c_yesno=0 if v469c==0
replace v469c_yesno=1 if v469c > 0 & v469c <= 7

*v469d
gen v469d_yesno=.
replace v469d_yesno=0 if v469d==0
replace v469d_yesno=1 if v469d > 0 & v469d <= 7

**v469e
gen v469e_yesno=.
replace v469e_yesno=0 if v469e==0
replace v469e_yesno=1 if v469e > 0 & v469e <= 7

**v469f
gen v469f_yesno=.
replace v469f_yesno=0 if v469f==0
replace v469f_yesno=1 if v469f > 0 & v469f <= 7

*v469g
gen v469g_yesno=.
replace v469g_yesno=0 if v469g==0
replace v469g_yesno=1 if v469g > 0 & v469g <= 7

*v469h
gen v469h_yesno=.
replace v469h_yesno=0 if v469h==0
replace v469h_yesno=1 if v469h > 0 & v469h <= 7

*v469i - recoded above (country specific)

*v469j - recoded above (country specific)

*v469k - does not exist

*v469l
gen v469l_yesno=.
replace v469l_yesno=0 if v469l==0
replace v469l_yesno=1 if v469l > 0 & v469l <= 7

*v469m
gen v469m_yesno=.
replace v469m_yesno=0 if v469m==0
replace v469m_yesno=1 if v469m > 0 & v469m <= 7

*v469n
gen v469n_yesno=.
replace v469n_yesno=0 if v469n==0
replace v469n_yesno=1 if v469n > 0 & v469n <= 7

*v469o
gen v469o_yesno=.
replace v469o_yesno=0 if v469o==0
replace v469o_yesno=1 if v469o > 0 & v469o <= 7

*v469p
gen v469p_yesno=.
replace v469p_yesno=0 if v469p==0
replace v469p_yesno=1 if v469p > 0 & v469p <= 7

*v469q
gen v469q_yesno=.
replace v469q_yesno=0 if v469q==0
replace v469q_yesno=1 if v469q > 0 & v469q <= 7

*v469r
gen v469r_yesno=.
replace v469r_yesno=0 if v469r==0
replace v469r_yesno=1 if v469r > 0 & v469r <= 7

*v469s
gen v469s_yesno=.
replace v469s_yesno=0 if v469s==0
replace v469s_yesno=1 if v469s > 0 & v469s <= 7

*v469t
gen v469t_yesno=.
replace v469t_yesno=0 if v469t==0
replace v469t_yesno=1 if v469t > 0 & v469t <= 7

*v469u
gen v469u_yesno=.
replace v469u_yesno=0 if v469u==0
replace v469u_yesno=1 if v469u > 0 & v469u <= 7

*v469v
gen v469v_yesno=.
replace v469v_yesno=0 if v469v==0
replace v469v_yesno=1 if v469v > 0 & v469v <= 7

*v469w
gen v469w_yesno=.
replace v469w_yesno=0 if v469w==0
replace v469w_yesno=1 if v469w > 0 & v469w <= 7

**v469x
gen v469x_yesno=.
replace v469x_yesno=0 if v469x==0
replace v469x_yesno=1 if v469x > 0 & v469x <= 7

*v469y
gen v469y_yesno=.
replace v469y_yesno=0 if v469y==0
replace v469y_yesno=1 if v469y > 0 & v469y <= 7

*v469z
gen v469z_yesno=.
replace v469z_yesno=0 if v469z==0
replace v469z_yesno=1 if v469z > 0 & v469z <= 7

*v501
replace v501=2 if survey=="Trinidad and Tobago 1987" & v501==6
replace v501=3 if survey=="Trinidad and Tobago 1987" & v501==7
replace v501=. if v501==9

********************************************
*labeling variables

label variable v409a "gave child sugar water"

label variable v412 "gave child fresh milk"

label variable v412b "gave child other porridge/gruel"

label variable v414 "gave child solid or mushy food"

label variable v414a "gave child cs foods"

label variable v414b "gave child cs foods"

label variable v414c "gave child cs foods"

label variable v414d "gave child cs foods"

label variable v414q "gave child oil, fats, butter, products made of them"

label variable v414r "gave child chocolates, sweets, candies, pastries, etc"

label variable v414t "gave child cs foods"

label variable v414u "gave child cs foods"

label variable v414w "gave child cs foods"

label variable v437 "respondent's weight in kilograms (1 decimal)"

label variable v438 "respondent's height in centimeters (1 decimal)"

label variable v445 "body mass index"

label variable v446 "rohrer's index"

label variable v447a "women's age in years (from household questionnaire)"

label variable b19 "current age of child in months"

label variable m27 "flag for breastfeeding"

label variable m35 "number of times breastfed during previous night"

label variable m36 "number of times breastfed during previous day"

label variable m55k "first 3 days, given country specific"

label variable m55l "first 3 days, given country specific"

label variable m55m "first 3 days, given country specific"

label variable m55n "first 3 days, given country specific"

label variable m55o "first 3 days, given country specific"

label variable h57 "received rotavirus 1"

label variable h57d "rotavirus 1 day"

label variable h57m "rotavirus 1 month"

label variable h57y "rotavirus 1 year"

label variable h80a "in the last 7 days given: local name for multiple micronutrient powder"

label variable h80b "in the last 7 days given: local name ready to use therapeutic food"

label variable h80c "in the last 7 days given: local name for ready to use supplemental food"

label variable h80d "in the last 7 days given: cs"

label variable h80e "in the last 7 days given: cs"

label variable h80f "in the last 7 days given: cs"

label variable h80g "in the last 7 days given: cs"

label variable hw1 "child's age in months"

label variable hw2 "child's weight in kilograms (1 decimal)"

label variable hw3 "child's height in centimeters (1 decimal)"

label variable hw5 "height/age standard deviation"

label variable hw8 "weight/age standard deviation"

label variable hw11 "weight/height standard deviation"

label variable hw13 "result of measurement - height/weight"

label variable hw15 "height: lying or standing"

label variable hw17 "date measured (day)"

label variable hw18 "date measured (month)"

label variable hw19 "date measured (year)"

label variable b17 "day of birth"

label variable b18 "century day code of birth (cdc)"

label variable v453 "hemoglobin level (g/dl - 1 decimal)"

label variable v439 "height/age percentile"

label variable v410a "gave child tea or coffee"

label variable v408 "number of times breastfed during day"

label variable v407 "number of times breastfed during night"

label variable v104 "years lived in place of residence"

label variable hw4 "height/age percentile"

label variable hw6 "height/age percent of ref. median"

label variable hw7 "weight/age percentile"

label variable hw9 "weight/age percent of ref. median"

label variable hw10 "weight/height percentile"

label variable hw12 "weight/height percent of ref. median"

label variable hw16 "day of birth of child"

label variable childhood_res "childhood place of residence"

label variable v414a "gave child banana, papaya, mango"

label variable v414b "gave child dal"

label variable v414c "gave child green leafy vegitables "

label variable v414d "gave child other solid or semi-solid foods  "

label variable v414t "gave child chips, crisps, french fries, fried dough, instant noodles"

label variable v414u "gave child chicken, duck, turkey, pigeons"

label variable v414w "gave child fortified baby food"

********************************************
*RECODING / COMBINING OVERLAPPING VARIABLES
*variables that ask the same thing
*set all to missing first
* 0=no 1=yes

**water
*check for overlaps per variable overlap
*generate first so all is missing
gen overlap=.
*replace if both variables are not missing 
replace overlap=1 if v469a_yesno!=. & v409!=. & s469a_plainwater!=. & s567a_plainwater_moldova!=. & s435a_plainwater_turkey!=. & s449a_water_turkey!=. & s472h_water_turkey!=. & s492b_a!=. & s498a_water_pakistan!=. & s1203a_water_egypt!=. & s449a_water_bangladesh!=.
*check which country and then survey year
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen water=.
*replaces the missing with 1 if either of the varibales are 1
replace water=0 if v469a_yesno==0 | v409==0 | s469a_plainwater==0 | s567a_plainwater_moldova==0 | s435a_plainwater_turkey==0 | s449a_water_turkey==0 | s472h_water_turkey==0 | s492b_a==0 | s498a_water_pakistan==0 | s1203a_water_egypt==0 | s449a_water_bangladesh==0
replace water=1 if v469a_yesno==1 | v409==1 | s469a_plainwater==1 | s567a_plainwater_moldova==1 | s435a_plainwater_turkey==1 | s449a_water_turkey==1 | s472h_water_turkey==1 | s492b_a==1 | s498a_water_pakistan==1 | s1203a_water_egypt==1 | s449a_water_bangladesh==1


**sugar water
gen overlap=.
replace overlap=1 if v469i_sugarwater_colombia!=. & v469i_thobwa_malawi!=. & v469b_yesno!=. & v409a!=. & s469d_drank_sugary_ethiopia!=. & s568cc_sugarydrinks_moldova!=. & s435b_sugarwater_turkey!=. & s449a_sugarwater_chad!=. & s492b_e!=. & s449b_sugarwater_bangladesh!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen sugar_water=.
replace sugar_water=0 if v469i_sugarwater_colombia==0 | v469i_thobwa_malawi==0 |v469b_yesno==0 |v409a==0 | s469d_drank_sugary_ethiopia==0 | s568cc_sugarydrinks_moldova==0 | s435b_sugarwater_turkey==0 & s449a_sugarwater_chad==0 | s492b_e==0 | s449b_sugarwater_bangladesh==0
replace sugar_water=1 if v469i_sugarwater_colombia==1 | v469i_thobwa_malawi==1 |v469b_yesno==1 | v409a==1 | s469d_drank_sugary_ethiopia==1 | s568cc_sugarydrinks_moldova==1 | s435b_sugarwater_turkey==1 & s449a_sugarwater_chad==1 | s492b_e==1 | s449b_sugarwater_bangladesh==1

**juice
gen overlap=.
replace overlap=1 if v469j_juice_namibia!=. & v469c_yesno!=. & v410!=. & s469e_drank_juice_armenia!=. & s435c_juice_turkey!=. & s449d_juice_chad!=. & s449q_beverage_turkey!=. & s492b_b!=. & s498c_juice_pakistan!=. & s492b_d!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

generate juice=.
replace juice=0 if v469j_juice_namibia==0 | v469c_yesno==0 | v410==0 | s469e_drank_juice_armenia==0 | s435c_juice_turkey==0 | s449d_juice_chad==0 | s449q_beverage_turkey==0 | s492b_b==0 | s498c_juice_pakistan==0 | s492b_d==0 
replace juice=1 if v469j_juice_namibia==1 | v469c_yesno==1 | v410==1 | s469e_drank_juice_armenia==1 | s435c_juice_turkey==1 | s449d_juice_chad==1 | s449q_beverage_turkey==1 | s492b_b==1 | s498c_juice_pakistan==1 | s492b_d==1 

**tea/coffee
gen overlap=.
replace overlap=1 if v469i_coffee_philippines!=. & v469d_yesno!=. & v410a!=. & s469e_drank_tea_ethiopia!=. & s568cb_drankteacoffee_moldova!=. & s469f_drank_tea_armenia!=. & s435d_tea_turkey!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

generate tea_coffee=.
replace tea_coffee=0 if v469i_coffee_philippines==0 | v469d==0 | v410a==0 | s469e_drank_tea_ethiopia==0 | s568cb_drankteacoffee_moldova==0 | s469f_drank_tea_armenia==0 | s435d_tea_turkey==0
replace tea_coffee=1 if v469i_coffee_philippines==1 | v469d_yesno==1 | v410a==1 | s469e_drank_tea_ethiopia==1 | s568cb_drankteacoffee_moldova==1 | s469f_drank_tea_armenia==1 | s435d_tea_turkey==1

**milk
gen overlap=.
replace overlap=1 if v469i_milk_namibia!=. & v469h_yesno!=. & v469g_yesno!=. & v411!=. & v412!=. & s568ca_drankmilk_moldova!=. & s469c_drank_milk_ethiopia!=. & v469e_yesno!=. & s469c_drank_milk_armenia!=. & s435j_boxedmilk_turkey!=. & s435k_milk_turkey!=. & s449b_bottledmilk_turkey!=. & s449c_othermilk_chad!=. & s498b_milk_pakistan!=. & s472j_bottledmilk_turkey!=. & s492b_c!=. & s449d_milk_bangladesh!=. 
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen milk=.
replace milk=0 if v469i_milk_namibia==0 | v469h_yesno==0 | v469g_yesno==0 | v411==0 | v412==0 | s568ca_drankmilk_moldova==0 | s469c_drank_milk_ethiopia==0 | v469e_yesno==0 | s469c_drank_milk_armenia==0 | s435j_boxedmilk_turkey==0 | s435k_milk_turkey==0 | s449b_bottledmilk_turkey==0 | s449c_othermilk_chad==0 | s498b_milk_pakistan==0 | s472j_bottledmilk_turkey==0 | s492b_c==0 | s449d_milk_bangladesh==0 
replace milk=1 if v469i_milk_namibia==1 | v469h_yesno==1 | v469g_yesno==1 | v411==1 | v412==1 | s568ca_drankmilk_moldova==1 | s469c_drank_milk_ethiopia==1 | v469e_yesno==1 | s469c_drank_milk_armenia==1 | s435j_boxedmilk_turkey==1 | s435k_milk_turkey==1 | s449b_bottledmilk_turkey==1 | s449c_othermilk_chad==1 | s498b_milk_pakistan==1 | s472j_bottledmilk_turkey==1 | s492b_c==1 | s449d_milk_bangladesh==1 

**formula
gen overlap=.
replace overlap=1 if v411a!=. & s469b_drank_formula_armenia!=. & s567b_infantformula_moldova!=. & v469f_yesno!=. & s435e_babyformula_turkey!=. & s449b_babyformula_chad!=. & s1203b_formula_egypt!=. & s449m_babyformula_turkey!=. & s449c_babyformula_bangladesh!=. & s472i_babyformula_turkey!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen formula=.
replace formula=0 if v411a==0 | s469b_drank_formula_armenia==0 | s567b_infantformula_moldova==0 | v469f_yesno==0 | s435e_babyformula_turkey==0 | s449b_babyformula_chad==0 | s1203b_formula_egypt==0 | s449m_babyformula_turkey==0 | s449c_babyformula_bangladesh==0 | s472i_babyformula_turkey==0
replace formula=1 if v411a==1 | s469b_drank_formula_armenia==1 | s567b_infantformula_moldova==1 | v469f_yesno==1 | s435e_babyformula_turkey==1 | s449b_babyformula_chad==1 | s1203b_formula_egypt==1 | s449m_babyformula_turkey==1 | s449c_babyformula_bangladesh==1 | s472i_babyformula_turkey==1

**baby food
gen overlap=.
replace overlap=1 if v412a!=. & s470ca_ate_babyfood_armenia!=. & v414w!=. & s567c_babyfood_moldova!=. & s469b_ate_babyfood_ethiopia!=. & s493b_a!=. & s1203c_babyfood_egypt!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen baby_food=.
replace baby_food=0 if v412a==0 | s470ca_ate_babyfood_armenia==0 | v414w==0 & s567c_babyfood_moldova==0 & s469b_ate_babyfood_ethiopia==0 | s493b_a==0 | s1203c_babyfood_egypt==0
replace baby_food=1 if v412a==1 | s470ca_ate_babyfood_armenia==1 | v414w==1 & s567c_babyfood_moldova==1 & s469b_ate_babyfood_ethiopia==1 | s493b_a==1 | s1203c_babyfood_egypt==1

**narine 
gen overlap=.
replace overlap=1 if s469d_drank_narine_armenia!=. & v469i_ricewater_madagascar!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen narine=.
replace narine=0 if s469d_drank_narine_armenia==0 | v469i_ricewater_madagascar==0
replace narine=1 if s469d_drank_narine_armenia==1 | v469i_ricewater_madagascar==1

**soup
gen overlap=.
replace overlap=1 if v469i_soup_rwanda!=. & v412c!=. & s492b_f!=. & s449o_soup_turkey!=. & s435l_soup_turkey!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen soup=.
replace soup=0 if v469i_soup_rwanda==0 | v412c==0 | s492b_f==0 | s449o_soup_turkey==0 | s435l_soup_turkey==0 
replace soup=1 if v469i_soup_rwanda==1 | v412c==1 | s492b_f==1 | s449o_soup_turkey==1 | s435l_soup_turkey==1 

**other liquids
gen overlap=.
replace overlap=1 if v469l_yesno!=. & v413!=. & s469g_drank_other_armenia!=. & s568cd_otherliquids_moldova!=. & s469f_drank_other_ethiopia!=. & v469j_otherliquids_cameroon!=. & v469i_otherliquids_cameroon!=. &s435m_otherliquids_turkey!=. & s449e_otherliquids_bangladesh!=. & s449e_otherliquids_chad!=. & s492b_g!=. & v469j_otherliquids_rwanda!=. & v469i_otherliquids_colombia!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen other_liquids=.
replace other_liquids=0 if v469l_yesno==0 |v413==0 | s469g_drank_other_armenia==0 | s568cd_otherliquids_moldova==0 | s469f_drank_other_ethiopia==0 | v469j_otherliquids_cameroon==0 | v469i_otherliquids_cameroon==0 |s435m_otherliquids_turkey==0 | s449e_otherliquids_bangladesh==0 | s449e_otherliquids_chad==0 | s492b_g==1 | v469j_otherliquids_rwanda==0 | v469i_otherliquids_colombia==0
replace other_liquids=1 if v469l_yesno==1 | v413==1 | s469g_drank_other_armenia==1 | s568cd_otherliquids_moldova==1 | s469f_drank_other_ethiopia==1 | v469j_otherliquids_cameroon==1 | v469i_otherliquids_cameroon==1 | s435m_otherliquids_turkey==1 | s449e_otherliquids_bangladesh==1 | s449e_otherliquids_chad==1 | s492b_g==1 | v469j_otherliquids_rwanda==1 | v469i_otherliquids_colombia==1

**bread
gen overlap=.
replace overlap=1 if v414e!=. & s470cb_ate_bread_armenia!=. & s568ce_atebread_moldova!=. & s449k_bread_turkey!=. & s472m_bread_turkey!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen bread=.
replace bread=0 if v414e==0 | s470cb_ate_bread_armenia==0 | s568ce_atebread_moldova==0 | s449k_bread_turkey==0 | s472m_bread_turkey==0
replace bread=1 if v414e==1 | s470cb_ate_bread_armenia==1 | s568ce_atebread_moldova==1 | s449k_bread_turkey==1 | s472m_bread_turkey==1

**potatoes
gen overlap=.
replace overlap=1 if v469r_yesno!=. & v469z_yesno!=. & v414f!=. & s470cc_white_potato_armenia!=. & s568cg_atepotato_moldova!=. & s449g_tubers_chad!=. & s472d_cassava_chad!=. & s493b_b!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen potatoes=.
replace bread=0 if v469r_yesno==0 | v469z_yesno==0 | v414f==0 | s470cc_white_potato_armenia==0 |s568cg_atepotato_moldova==0 | s449g_tubers_chad==0 |s472d_cassava_chad==0 | s493b_b==0
replace potatoes=1 if v469r_yesno==1 | v469z_yesno==1 | v414f==1 | s470cc_white_potato_armenia==1 |s568cg_atepotato_moldova==1 | s449g_tubers_chad==1 |s472d_cassava_chad==1 | s493b_b==1

**eggs
gen overlap=.
replace overlap=1 if v469s_yesno!=. & v414g!=. & s470ck_eggs_armenia!=. & s568cn_ateeggs_moldova!=. & s449e_eggs_turkey!=. & s472a_egg_turkey!=. & s472k_egg_chad!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen eggs=.
replace eggs=0 if v469s_yesno==0 | v414g==0 | s470ck_eggs_armenia==0 | s568cn_ateeggs_moldova==0 | s449e_eggs_turkey==0 | s472a_egg_turkey==0 | s472k_egg_chad==0
replace eggs=1 if v469s_yesno==1 | v414g==1 | s470ck_eggs_armenia==1 | s568cn_ateeggs_moldova==1 | s449e_eggs_turkey==1 | s472a_egg_turkey==1 | s472k_egg_chad==1

**beef
gen overlap=.
replace overlap=1 if v469t_yesno!=. & v469v_yesno!=. & v414h!=. & s470ci_beef_armenia!=. & s568cl_atebeef_moldova!=. & s449f_meat_turkey!=. & s449m_meat_chad!=. & s472d_meat_turkey!=. & s472h_meat_chad!=. & s493b_f!=. & s449i_meat_bangladesh!=. 
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen beef=.
replace beef=0 if v469t_yesno==0 | v469v_yesno==0 | v414h==0 | s470ci_beef_armenia==0 | s568cl_atebeef_moldova==0 | s449f_meat_turkey==0 | s449m_meat_chad==0 | s472d_meat_turkey==0 | s472h_meat_chad==0 | s493b_f==0 | s449i_meat_bangladesh==0  
replace beef=1 if v469t_yesno==1 | v469v_yesno==1 | v414h==1 | s470ci_beef_armenia==1 | s568cl_atebeef_moldova==1 | s449f_meat_turkey==1 | s449m_meat_chad==1 | s472d_meat_turkey==1 | s472h_meat_chad==1 | s493b_f==1 | s449i_meat_bangladesh==1  

**chicken
gen overlap=.
replace overlap=1 if v414u!=. & s470cj_chicken_armenia!=. & s568cm_atechicken_moldova!=. & s449g_chicken_turkey!=. & s472e_chicken_turkey!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen chicken=.
replace chicken=0 if v414u==0 | s470cj_chicken_armenia==0 | s568cm_atechicken_moldova==0 | s449g_chicken_turkey==0 | s472e_chicken_turke==0
replace chicken=1 if v414u==1 | s470cj_chicken_armenia==1 | s568cm_atechicken_moldova==1 | s449g_chicken_turkey==1 | s472e_chicken_turke==1

**pumpkin
gen overlap=.
replace overlap=1 if v469m_yesno!=. & v414i!=. & s470cd_pumpkin_armenia!=. & s568cf_atepumpkin_moldova!=. & s449i_carrots_chad!=. & s472a_carrots_chad!=. 
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen pumpkin=.
replace pumpkin=0 if v469m_yesno==0 | v414i==0 |s470cd_pumpkin_armenia==0 | s568cf_atepumpkin_moldova==0 | s449i_carrots_chad==0 | s472a_carrots_chad==0
replace pumpkin=1 if v469m_yesno==1 | v414i==1 |s470cd_pumpkin_armenia==1 | s568cf_atepumpkin_moldova==1 | s449i_carrots_chad==1 | s472a_carrots_chad==1 

**greens
gen overlap=.
replace overlap=1 if v469n_yesno!=. & v414j!=. & s470ce_greens_armenia!=. & v414c!=. & s568ch_ategreens_moldova!=. & s472c_cabbage_chad!=. & s449h_greens_chad!=. & s493b_c!=. & s449g_greens_bangladesh!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen greens=.
replace greens=0 if v469n_yesno==0 | v414j==0 |s470ce_greens_armenia==0 | v414c==0 | s568ch_ategreens_moldova==0 | s472c_cabbage_chad==0 | s449h_greens_chad==0 | s493b_c==0 | s449g_greens_bangladesh==0
replace greens=1 if v469n_yesno==1 | v414j==1 |s470ce_greens_armenia==1 | v414c==1 | s568ch_ategreens_moldova==1 | s472c_cabbage_chad==1 | s449h_greens_chad==1 | s493b_c==1 | s449g_greens_bangladesh==1

**mangoes
gen overlap=.
replace overlap=1 if v469o_yesno!=. & v414k!=. & s470cf_mango_armenia!=. & v414a!=. & s568ci_atepeaches_moldova!=. & s449j_oranges_chad!=. & s472e_mango_chad!=. & s472f_papaya_chad!=. & s493b_d!=. & s449f_banana_bangladesh!=. 
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen mangoes=.
replace mangoes=0 if v469o_yesno==0 | v414k==0 | s470cf_mango_armenia==0 | v414a==0 | s568ci_atepeaches_moldova==0 | s449j_oranges_chad==0 | s472e_mango_chad==0 |s472f_papaya_chad==0 | s493b_d==0 | s449f_banana_bangladesh==0
replace mangoes=1 if v469o_yesno==1 | v414k==1 | s470cf_mango_armenia==1 | v414a==1 | s568ci_atepeaches_moldova==1 | s449j_oranges_chad==1 | s472e_mango_chad==1 |s472f_papaya_chad==1 | s493b_d==1 | s449f_banana_bangladesh==1

**other fruits
gen overlap=.
replace overlap=1 if v469u_yesno!=. & v414l!=. & s470cg_other_fruits_armenia!=. & s568cj_atefruitsveggies_moldova!=. & s449j_fruitveggie_turkey!=. & s449k_otherfruits_chad!=. & s472g_fruitsveggie_turkey!=. & s472g_melons_chad!=. & s493b_e!=. 
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen other_fruits=.
replace other_fruits=0 if v469u_yesno==0 | v414l==0 | s470cg_other_fruits_armenia==0 | s568cj_atefruitsveggies_moldova==0 | s449j_fruitveggie_turkey==0 | s449k_otherfruits_chad==0 | s472g_fruitsveggie_turkey==0 | s472g_melons_chad==0 | s493b_e==0 
replace other_fruits=1 if v469u_yesno==1 | v414l==1 | s470cg_other_fruits_armenia==1 | s568cj_atefruitsveggies_moldova==1 | s449j_fruitveggie_turkey==1 | s449k_otherfruits_chad==1 | s472g_fruitsveggie_turkey==1 | s472g_melons_chad==1 | s493b_e==1 

**liver
gen overlap=.
replace overlap=1 if v414m!=. & s470ch_liver_armenia!=. & s568ck_ateliver_moldova!=. & s472i_liver_chad!=. 
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen liver=.
replace liver=0 if v414m==0 | s470ch_liver_armenia==0 | s568ck_ateliver_moldova==0 | s472i_liver_chad==0
replace liver=1 if v414m==1 | s470ch_liver_armenia==1 | s568ck_ateliver_moldova==1 | s472i_liver_chad==1

**fish
gen overlap=.
replace overlap=1 if v414n!=. & s470cl_fish_armenia!=. & s568co_atefish_moldova!=. & s449h_fish_turkey!=. & s472f_fish_turkey!=. & s472j_fish_chad!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen fish=.
replace fish=0 if v414n==0 | s470cl_fish_armenia==0 | s568co_atefish_moldova==0 | s449h_fish_turkey==0 | s472f_fish_turkey==0 | s472j_fish_chad==0
replace fish=1 if v414n==1 | s470cl_fish_armenia==1 | s568co_atefish_moldova==1 | s449h_fish_turkey==1 | s472f_fish_turkey==1 | s472j_fish_chad==1

**beans
gen overlap=.
replace overlap=1 if v469w_yesno!=. & v414o!=. & s470cm_beans_armenia!=. & s568cp_atebeans_moldova!=. & s449i_legumes_turkey!=. & s493b_h!=. 
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen beans=.
replace beans=0 if v469w_yesno==0 | v414o==0 | s470cm_beans_armenia==0 | s568cp_atebeans_moldova==0 | s449i_legumes_turkey==0 | s493b_h==0
replace beans=1 if v469w_yesno==1 | v414o==1 | s470cm_beans_armenia==1 | s568cp_atebeans_moldova==1 | s449i_legumes_turkey==1 | s493b_h==1

**nuts
gen overlap=.
replace overlap=1 if s470cn_nuts_armenia!=. & s568cq_atenuts_moldova!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen nuts=.
replace nuts=0 if s470cn_nuts_armenia==0 | s568cq_atenuts_moldova==0
replace nuts=1 if s470cn_nuts_armenia==1 | s568cq_atenuts_moldova==1

**dairy
gen overlap=.
replace overlap=1 if v469x_yesno!=. & v414p!=. & s470co_cheese_armenia!=. & v414v!=. & s568cr_atedairy_moldova!=. & s435f_yogurt_turkey!=. & s449c_yogurt_turkey!=. & s449d_cheese_turkey!=. & s449l_dairy_chad!=. & s472b_cheese_turkey!=. & s472c_yogurt_turkey!=. & s493b_i!=. 
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen dairy=.
replace dairy=0 if v469x_yesno==0 | v414p==0 | s470co_cheese_armenia==0 | v414v==0 | s568cr_atedairy_moldova==0 | s435f_yogurt_turkey==0 | s449c_yogurt_turkey==0 | s449d_cheese_turkey==0 | s449l_dairy_chad==0 | s472b_cheese_turkey==0 | s472c_yogurt_turkey==0 | s493b_i==0 
replace dairy=1 if v469x_yesno==1 | v414p==1 | s470co_cheese_armenia==1 | v414v==1 | s568cr_atedairy_moldova==1 | s435f_yogurt_turkey==1 | s449c_yogurt_turkey==1 | s449d_cheese_turkey==1 | s449l_dairy_chad==1 | s472b_cheese_turkey==1 | s472c_yogurt_turkey==1 | s493b_i==1 

**oil
gen overlap=.
replace overlap=1 if v469y_yesno!=. & v414q!=. & s470cp_oil_armenia!=. & s568cs_ateoil_moldova!=. 
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen oil=.
replace oil=0 if v414q==0 | s470cp_oil_armenia==0 | s568cs_ateoil_moldova==0
replace oil=1 if v414q==1 | s470cp_oil_armenia==1 | s568cs_ateoil_moldova==1

**other food
gen overlap=.
replace overlap=1 if v469p_yesno!=. & s570_times_food24_moldova!=. & s472_times_food24_armenia!=. & v414s!=. & s470cq_othersolid_armenia!=. & v414d!=. & s568cu_othersolids_moldova!=. & v414!=. & s435n_othersolids_turkey!=. & s449n_othersolid_chad!=. & s498d_food_pakistan!=. & s449k_other_bangladesh!=. & s472_times_food24_ethiopia!=. & m39a!=. & m39!=. & s493b_k!=. & s1206_food_egypt!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen other_food=.
replace other_food=0 if v469p_yesno==0 | s570_times_food24_moldova==0 | s472_times_food24_armenia==0 | v414s==0 | s470cq_othersolid_armenia==0 | v414d==0 | s568cu_othersolids_moldova==0 | v414==0 | s435n_othersolids_turkey==0 | s449n_othersolid_chad==0 | s498d_food_pakistan==1 | s449k_other_bangladesh==0 | s472_times_food24_ethiopia==0 | m39a==0 | m39==0 | s493b_k==0 | s1206_food_egypt==0
replace other_food=1 if v469p_yesno==1 | s570_times_food24_moldova==1 | s472_times_food24_armenia==1 | v414s==1 | s470cq_othersolid_armenia==1 | v414d==1 | s568cu_othersolids_moldova==1 | v414==1 | s435n_othersolids_turkey==1 | s449n_othersolid_chad==1 | s498d_food_pakistan==1 | s449k_other_bangladesh==1 | s472_times_food24_ethiopia==1 | m39a==1 | m39==1 | s493b_k==1 | s1206_food_egypt==1

**daal
gen overlap=.
replace overlap=1 if v414b!=. & s449j_dal_bangladesh!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen daal=.
replace daal=0 if v414b==0 | s449j_dal_bangladesh==1
replace daal=1 if v414b==1 | s449j_dal_bangladesh==1

**fried (no need to check for overlap, only one variable)
gen fried=.
replace fried=0 if v414t==0
replace fried=1 if v414t==1

**porridge
gen overlap=.
replace overlap=1 if v412b!=. & s567d_porridge_moldova!=. & s435g_pudding_turkey!=. & s1203d_porridge_egypt!=. & s449h_rice_bangladesh!=. 
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen porridge=.
replace porridge=0 if v412b==0 | s567d_porridge_moldova==0 | s435g_pudding_turkey==0 | s1203d_porridge_egypt==0 | s449h_rice_bangladesh==0
replace porridge=1 if v412b==1 | s567d_porridge_moldova==1 | s435g_pudding_turkey==1 | s1203d_porridge_egypt==1 | s449h_rice_bangladesh==1 

**sweets
gen overlap=.
replace overlap=1 if v414r!=. & s568ct_atesugary_moldova!=. & s435i_turkishdelight_turkey!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen sweets=.
replace sweets=0 if v414r==0 | s568ct_atesugary_moldova==0 | s435i_turkishdelight_turkey==0
replace sweets=1 if v414r==1 | s568ct_atesugary_moldova==1 | s435i_turkishdelight_turkey==1


**mealjuice
gen overlap=.
replace overlap=1 if s435h_juiceofmeal_turkey!=. & s449n_juiceofmeal_turkey!=. & s472l_juiceofmeal_turkey!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen mealjuice=.
replace mealjuice=0 if s435h_juiceofmeal_turkey==0 | s449n_juiceofmeal_turkey==0 | s472l_juiceofmeal_turkey==0
replace mealjuice=1 if s435h_juiceofmeal_turkey==1 | s449n_juiceofmeal_turkey==1 | s472l_juiceofmeal_turkey==1

**cereal
gen overlap=.
replace overlap=1 if v469q_yesno!=. & s449f_cereals_chad!=. & s449l_cereals_turkey!=. & s472k_cereal_turkey!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen cereal=.
replace cereal=0 if v469q_yesno==0 | s449f_cereals_chad==0 | s449l_cereals_turkey==0 | s472k_cereal_turkey==0
replace cereal=1 if v469q_yesno==1 | s449f_cereals_chad==1 | s449l_cereals_turkey==1 | s472k_cereal_turkey==1

**junk (no need to check for overlap, only one variable)
gen junk=.
replace junk=0 if s449p_junkfood_turkey==0 
replace junk=1 if s449p_junkfood_turkey==1 


**sorrel (no need to check for overlap, only one variable)
gen sorrel=.
replace sorrel=0 if s472b_sorrel_chad==0
replace sorrel=1 if s472b_sorrel_chad==1

**insects (no need to check for overlap, only one variable)
gen insects=.
replace insects=0 if s493b_g==0
replace insects=1 if s493b_g==1

**other liquid and food - did not use this variable 
gen overlap=.
replace overlap=1 if s470_yesterday_foodwater_bolivia!=. & s567e_otherfoodliquid_moldova!=.
tab country_name if overlap==1
tab survey_year if overlap==1
drop overlap
*No overlap, proceed as normal 

gen other_liquid_food=.
replace other_liquid_food=0 if s470_yesterday_foodwater_bolivia==0 | s567e_otherfoodliquid_moldova==0
replace other_liquid_food=1 if s470_yesterday_foodwater_bolivia==1 | s567e_otherfoodliquid_moldova==1

**prelacteal variables
gen prelacteal_m55=.
replace prelacteal_m55=0 if m55==0
replace prelacteal_m55=1 if m55==1

gen p_milk=.
replace p_milk=0 if m55a==0
replace p_milk=1 if m55a==1

gen p_water=.
replace p_water=0 if m55b==0
replace p_water=1 if m55b==1

gen p_sugarwater=.
replace p_sugarwater=0 if m55c==0
replace p_sugarwater=1 if m55c==1

gen p_gripewater=.
replace p_gripewater=0 if m55d==0
replace p_gripewater=1 if m55d==1

gen p_sugar_salt_solution=.
replace p_sugar_salt_solution=0 if m55e==0
replace p_sugar_salt_solution=1 if m55e==1

gen p_juice=.
replace p_juice=0 if m55f==0
replace p_juice=1 if m55f==1

gen p_formula=.
replace p_formula=0 if m55g==0
replace p_formula=1 if m55g==1

gen p_tea=.
replace p_tea=0 if m55h==0
replace p_tea=1 if m55h==1

gen p_honey=.
replace p_honey=0 if m55i==0
replace p_honey=1 if m55i==1

gen p_coffee=.
replace p_coffee=0 if m55j==0
replace p_coffee=1 if m55j==1

gen p_other=.
replace p_other=0 if m55x==0
replace p_other=1 if m55x==1

gen p_nothing=.
replace p_nothing=0 if m55z==0
replace p_nothing=1 if m55z==1

* country specific prelacteal foods that are not specified
gen p_m55k=.
replace p_m55k=0 if m55k==0
replace p_m55k=1 if m55k==1

gen p_m55l=.
replace p_m55l=0 if m55l==0
replace p_m55l=1 if m55l==1

gen p_m55m=.
replace p_m55m=0 if m55m==0
replace p_m55m=1 if m55m==1

gen p_m55n=.
replace p_m55n=0 if m55n==0
replace p_m55n=1 if m55n==1

gen p_m55o=.
replace p_m55o=0 if m55o==0
replace p_m55o=1 if m55o==1

**combining all liquid variables
gen liquid=.
replace liquid=0 if water==0 | sugar_water==0 | juice==0 | tea_coffee==0 | milk==0 | formula==0 | baby_food==0 | other_liquids==0 | soup==0 | mealjuice==0 | narine==0
replace liquid=1 if water==1 | sugar_water==1 | juice==1 | tea_coffee==1| milk==1 | formula==1 | baby_food==1 | other_liquids==1 | soup==1 | mealjuice==1 | narine==1
replace liquid=. if water==. & sugar_water==. & juice==. & tea_coffee==. & milk==. & formula==. & baby_food==. & other_liquids==. & soup==. & mealjuice==. & narine==.
* 259,718=No liquids given; 1,539,897=Yes liquids given; 1,024,443=missing 

*checking my results by using another code
egen liquid_sum= rowtotal(water sugar_water juice tea_coffee milk formula baby_food other_liquids soup mealjuice narine), missing

gen liquid1=.
replace liquid1=0 if liquid_sum==0
replace liquid1=1 if liquid_sum>0
replace liquid1=. if liquid_sum==.
* Same results were found

**combining all food variables
gen food=.
replace food=0 if bread==0 | potatoes==0 | eggs==0 | beef==0 | chicken==0 | pumpkin==0 | greens==0 | mangoes==0 | other_fruit==0 | liver==0 | fish==0 | beans==0 | nuts==0 | dairy==0 | oil==0 | other_food==0 | porridge==0 | sweets==0 | cereal==0 | daal==0 | junk==0 | sorrel==0 | insects==0 | fried==0
replace food=1 if bread==1 | potatoes==1 | eggs==1 | beef==1 | chicken==1 | pumpkin==1 | greens==1 | mangoes==1 | other_fruit==1 | liver==1 | fish==1 | beans==1 | nuts==1 | dairy==1 | oil==1 | other_food==1 | porridge==1 | sweets==1 | cereal==1 | daal==1 | junk==1 | sorrel==1 | insects==1 | fried==1
replace food=. if bread==. & potatoes==. & eggs==. & beef==. & chicken==. & pumpkin==. & greens==. & mangoes==. & other_fruit==. & liver==. & fish==. & beans==. & nuts==. & dairy==. & oil==. & other_food==. & porridge==. & sweets==. & cereal==. & daal==. & junk==. & sorrel==. & insects==. & fried==.
* 663,965=No liquids given; 1,196,346=Yes liquids given; 963,747=missing 

*checking my results by using another code
egen food_sum= rowtotal(bread potatoes eggs beef chicken pumpkin greens mangoes other_fruit liver fish beans nuts dairy oil other_food porridge sweets cereal daal junk sorrel insects fried), missing

gen food1=.
replace food1=0 if food_sum==0
replace food1=1 if food_sum>0
replace food1=. if food_sum==.
* Same results were found

**combining all prelacteal feeding
*p_nothing (or m55z) is asking if you gave your child nothing - so it is coded as 1 if they are not prelacteal feeding and vice versa
gen prelacteal=.
replace prelacteal=0 if prelacteal_m55==0
replace prelacteal=0 if p_nothing==1
replace prelacteal=0 if p_milk==0 | p_water==0 | p_sugarwater==0 | p_gripewater==0 | p_sugar_salt_solution==0 | p_juice==0 | p_formula==0 | p_tea==0 | p_honey==0 | p_coffee==0 | p_other==0 | p_m55k==0| p_m55l==0 | p_m55m==0 | p_m55n==0 | p_m55o==0
replace prelacteal=1 if p_nothing==0
replace prelacteal=1 if prelacteal_m55==1
replace prelacteal=1 if p_milk==1 | p_water==1 | p_sugarwater==1 | p_gripewater==1 | p_sugar_salt_solution==1 | p_juice==1 | p_formula==1 | p_tea==1 | p_honey==1 | p_coffee==1 | p_other==1 | p_m55k==1 | p_m55l==1 | p_m55m==1 | p_m55n==1 | p_m55o==1 
replace prelacteal=. if prelacteal_m55==. & p_milk==. & p_water==. & p_sugarwater==. & p_gripewater==. & p_sugar_salt_solution==. & p_juice==. & p_formula==. & p_tea==. & p_honey==. & p_coffee==. & p_other==. & p_m55k==. & p_m55l==. & p_m55m==. & p_m55n==. & p_m55o==. & p_nothing==.
* 1,070,087=No prelacteal feeding; 523,154=Yes prelacteal feeding; 1,230,817=missing

*checking my results by using another code (I removed the p_nothing as it will not work in the sum coding)
gen prelacteal1=.
replace prelacteal1=0 if prelacteal_m55==0 | p_milk==0 | p_water==0 | p_sugarwater==0 | p_gripewater==0 | p_sugar_salt_solution==0 | p_juice==0 | p_formula==0 | p_tea==0 | p_honey==0 | p_coffee==0 | p_other==0 | p_m55k==0| p_m55l==0 | p_m55m==0 | p_m55n==0 | p_m55o==0 
replace prelacteal1=1 if prelacteal_m55==1 | p_milk==1 | p_water==1 | p_sugarwater==1 | p_gripewater==1 | p_sugar_salt_solution==1 | p_juice==1 | p_formula==1 | p_tea==1 | p_honey==1 | p_coffee==1 | p_other==1 | p_m55k==1 | p_m55l==1 | p_m55m==1 | p_m55n==1 | p_m55o==1
replace prelacteal1=. if prelacteal_m55==. & p_milk==. & p_water==. & p_sugarwater==. & p_gripewater==. & p_sugar_salt_solution==. & p_juice==. & p_formula==. & p_tea==. & p_honey==. & p_coffee==. & p_other==. & p_m55k==. & p_m55l==. & p_m55m==. & p_m55n==. & p_m55o==. 

egen prelacteal_sum= rowtotal(prelacteal_m55 p_milk p_water p_sugarwater p_gripewater p_sugar_salt_solution p_juice p_formula p_tea p_honey p_coffee p_other p_m55k  p_m55l p_m55m p_m55n p_m55o), missing

gen prelacteal2=.
replace prelacteal2=0 if prelacteal_sum==0
replace prelacteal2=1 if prelacteal_sum>0
replace prelacteal2=. if prelacteal_sum==.
* Same results were found as prelacteal1

********************************

*BREASTFEEDING VARIABLES
*NOTE: Colombia 2015 -- no breastfeeding questions were asked

**V404 = currently breastfeeding variable but it is a mother variable and does not take into account when a mother had 2 children in the last 2 years; M4 takes into consideration each individual child-- DHS uses M4=95 as the BF variable
gen BF= m4==95


********************************
* EBF variable

*EBF_DHS: if they answer yes to food or liquid then no EBF. If both are missing they are still categorized as EBF (DHS definition)
gen EBF_DHS=1
replace EBF_DHS=0 if BF==0 
replace EBF_DHS=. if BF==.
replace EBF_DHS=0 if food==1 
replace EBF_DHS=0 if liquid==1

************************************************

**** for weighted measurements:  [aweight= v005]

************************************************


***********************************************************************************************************

save "$root/data/restricted/DHS_harmonized.dta", replace 

************************************************
*Note: the cleaned version of the dta file is saved as DHS_harmonized.dta - will use this dta for analysis

use "$root/data/restricted/DHS_harmonized.dta", clear

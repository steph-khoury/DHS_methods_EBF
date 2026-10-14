
** Version notes

// 22 June 2026: added additional variable (education)

* Child demographics: age, order, lives with parent, birth order

capture gen bord=.
capture gen b0=.
capture gen b1=.
capture gen b2=.
capture gen b3=.
capture gen b4=.
capture gen b5=.
capture gen b6=.
capture gen b7=.
capture gen b8=.
capture gen b9=.
capture gen b11=.

*immunization

capture gen h1=.
capture gen h2=.
capture gen h3=.
capture gen h4=.
capture gen h5=.
capture gen h6=.
capture gen h7=.
capture gen h8=.
capture gen h9=.

* diarrhea h11

capture gen h11 = .

*fever h22

capture gen h22=.

* cough

capture gen h31=.
capture gen h31a=.
capture gen h31b =.

*height and weight

forvalues i=1/20 {
	capture gen hw`i'=.
	}


* antibiotic treatment

cap gen h15 = . // given AB pill or syrup for diarrhea
cap gen h15b = . // given AB injection for diarrhea
cap gen h37i = . // AB for fever or cough - var set 1
cap gen h37j = .  // AB injection
cap gen ml13i = . // AB for fever - var set 
cap gen ml13j = . // AB injection

* treatment seeking

cap gen h12y= .
cap gen h12z = .
cap gen h32y = .
cap gen h32z = .

** food intake 

foreach x in s469a s469b s469c s469d s469e s469f s469g s470ca s470cb s470cc s470cd s470ce s470cf s470cg s470ch s470ci s470cj s470ck s470cl s470cm s470cn s470co s470cp s470cq s472 {
	cap gen `x'=.
}

* Survey characteristics: survey, cluster, year, day , age of child

*v000 v001 v003 v005 v008 v007 v021 v022 v023

cap gen v021=.
cap gen v022=.
cap gen v023 =.
cap gen b17=.
cap gen b18=.
cap gen v008a=.



* Respondent characteristics

*age: v012
*urban: v025 v102
*region: v024 v101
*birth5y: v208
* children born: v201
capture gen v201 = .
*height 
capture gen v438=.
capture gen v439 = .

capture gen v024=.
capture gen v025=.
capture decode v025, gen(urban1)
capture gen urban1=""


capture gen v101=.
capture gen v102=.
capture decode v102, gen(urban2)
capture gen urban2=""

capture decode v024, gen(region_1)
capture decode v101, gen(region_2)
capture gen region_1=""
capture gen region_2=""

capture gen v208=.

* Childhood residence

capture decode v103, gen(childhood_res)
capture gen childhood_res=""


*education v106 v133 s105
*married v501
* partner_educ v701

capture gen v501=.
capture gen v701=.
capture gen s105=.



* sex_head 
capture gen v151=.

* hh_members
capture gen v136 = .

* children under 5
capture gen v137 = .

* pregnancy stuff

capture gen v213=.
capture gen v214=.
capture gen v225=.
capture gen v228=.
capture gen v231=.


* other

capture gen v005=.
capture gen v023 =.
capture gen v024=.
capture gen v205=.
capture gen v206=.

* wealth index

capture gen v190 = .

* Migration

capture gen v104 = .
capture gen v105 = .


* Breastfeeding detail, maternal height

capture gen m4=.
capture gen m5=.
capture gen v401=.
capture gen v404=.
capture gen v405=.
capture gen v409=.
capture gen v426=.
capture gen v437=.
capture gen v438=.
capture gen v439=.
capture gen v440=.
capture gen v445=.
capture gen v446=.
capture gen v447a=.
capture gen v453=.
capture gen v463a=.
capture gen v464 =.

** additional breastfeeding variables 


cap gen v204 = .
cap gen v207 = .
cap gen v216 = .
cap gen v222 = .
cap gen v407 = .
cap gen v408 = .
cap gen v409a = .
cap gen v410 = .
cap gen v410a = .
cap gen v411 = .
cap gen v411a = .
cap gen v412 = .
cap gen v412a = .
cap gen v412b = .
cap gen v412c = .
cap gen v413 = .
cap gen v414 = .
cap gen v414a=.
cap gen v414b=.
cap gen v414c=.
cap gen v414d=.
cap gen v414e = .
cap gen v414f = .
cap gen v414g = .
cap gen v414h = .
cap gen v414i = .
cap gen v414j = .
cap gen v414k = .
cap gen v414l = .
cap gen v414m = .
cap gen v414n = .
cap gen v414o = .
cap gen v414p = .
cap gen v414q = .
cap gen v414r = .
cap gen v414s = .
cap gen v414t =.
cap gen v414u = .
cap gen v414v = .
cap gen v414w = .
cap gen v415 = .
cap gen v469a = .
cap gen v469b = .
cap gen v469c = .
cap gen v469d = .
cap gen v469e = .
cap gen v469f = .
cap gen v469g = .
cap gen v469h = .
cap gen v469i = .
cap gen v469j = .
cap gen v469k = .
cap gen v469l = .
cap gen v469m = .
cap gen v469n = .
cap gen v469o = .
cap gen v469p = .
cap gen v469q = .
cap gen v469r = .
cap gen v469s = .
cap gen v469t = .
cap gen v469u = .
cap gen v469v = .
cap gen v469w = .
cap gen v469x = .
cap gen v469y = .
cap gen v469z = .
cap gen v469xx = .
cap gen v469xy = .
cap gen v469xz = .
cap gen b19 = .
cap gen m27 = .
cap gen m21 = .
cap gen m22 = .
cap gen m34 = .
cap gen m35 = .
cap gen m36 = .
cap gen m38 = .
cap gen m39a = .
cap gen m39 = .
cap gen m55 = .
cap gen m55a = .
cap gen m55b = .
cap gen m55c = .
cap gen m55d = .
cap gen m55e = .
cap gen m55f = .
cap gen m55g = .
cap gen m55h = .
cap gen m55i = .
cap gen m55j = .
cap gen m55k = .
cap gen m55l = .
cap gen m55m = .
cap gen m55n = .
cap gen m55o = .
cap gen m55x = .
cap gen m55z = .
cap gen h18a = .
cap gen h57 = .
cap gen h57d = .
cap gen h57m = .
cap gen h57y = .
cap gen h80a = .
cap gen h80b = .
cap gen h80c = .
cap gen h80d = .
cap gen h80e = .
cap gen h80f = .
cap gen h80g = .
cap gen s429 = .
cap gen s430 = .
cap gen s470 = .
cap gen s471 = .

cap gen	s567a	=.
cap gen	s567b	=.
cap gen	s567c	=.
cap gen	s567d	=.
cap gen	s567e	=.
cap gen	s568ca	=.
cap gen	s568cb	=.
cap gen	s568cc	=.
cap gen	s568cd	=.
cap gen	s568ce	=.
cap gen	s568cf	=.
cap gen	s568cg	=.
cap gen	s568ch	=.
cap gen	s568ci	=.
cap gen	s568cj	=.
cap gen	s568ck	=.
cap gen	s568cl	=.
cap gen	s568cm	=.
cap gen	s568cn	=.
cap gen	s568co	=.
cap gen	s568cp	=.
cap gen	s568cq	=.
cap gen	s568cr	=.
cap gen	s568cs	=.
cap gen	s568ct	=.
cap gen	s568cu	=.
cap gen	s570	=.

cap gen s435a =.
cap gen s435b =.
cap gen s435c =.
cap gen s435d =.
cap gen s435e =.
cap gen s435f =.
cap gen s435g =.
cap gen s435h =.
cap gen s435i =.
cap gen s435j =.
cap gen s435k =.
cap gen s435l =.
cap gen s435m =.
cap gen s435n =.

cap gen s449a =.
cap gen s449b =.

*recoding s449a and s449b
cap destring s449a, replace force
cap destring s449b, replace force

cap gen s449c =.
cap gen s449d =.

*recoding s449c and s449d
cap destring s449c, replace force
cap destring s449d, replace force

cap gen s449e =.
cap gen s449f =.
cap gen s449g =.
cap gen s449h =.
cap gen s449i =.
cap gen s449j =.
cap gen s449k =.
cap gen s449l =.
cap gen s449m =.
cap gen s449n =.
cap gen s449o =.
cap gen s449p =.
cap gen s449q =.

cap gen s449a1 =.
cap gen s449b1 =.
cap gen s449c1 =.
cap gen s449d1 =.
cap gen s449e1 =.
cap gen s449f1 =.
cap gen s449g1 =.
cap gen s449h1 =.
cap gen s449i1 =.
cap gen s449j1 =.
cap gen s449k1 =.

cap gen s472a =.
cap gen s472b =.
cap gen s472c =.
cap gen s472d =.
cap gen s472e =.
cap gen s472f =.
cap gen s472g =.
cap gen s472h =.
cap gen s472i =.
cap gen s472j =.
cap gen s472k =.
cap gen s472l =.
cap gen s472m =.

cap gen s492a_a =.
cap gen s492a_b =.
cap gen s492a_c =.
cap gen s492a_d =.
cap gen s492a_e =.
cap gen s492a_f =.
cap gen s492a_g =.
cap gen s492b_a =.
cap gen s492b_b =.
cap gen s492b_c =.
cap gen s492b_d =.
cap gen s492b_e =.
cap gen s492b_f =.
cap gen s492b_g =.

cap gen s493a_a =.
cap gen s493a_b =.
cap gen s493a_c =.
cap gen s493a_d =.
cap gen s493a_e =.
cap gen s493a_f =.
cap gen s493a_g =.
cap gen s493a_h =.
cap gen s493a_i =.
cap gen s493a_j =.
cap gen s493a_k =.
cap gen s493b_a =.
cap gen s493b_b =.
cap gen s493b_c =.
cap gen s493b_d =.
cap gen s493b_e =.
cap gen s493b_f =.
cap gen s493b_g =.
cap gen s493b_h =.
cap gen s493b_i =.
cap gen s493b_j =.
cap gen s493b_k =.
cap gen s105 =.

cap gen s498a =.
cap gen s498b =.
cap gen s498c =.
cap gen s498d =.

cap gen s1203a =.
cap gen s1203b =.
cap gen s1203c =.
cap gen s1203d =.
cap gen s1206 =.

**************************************************
** strings for non-standard variables 
***************************************************

# delimit;
foreach x of varlist 
s105
s435a
s435b
s435c
s435d
s435e
s435f
s435g
s435h
s435i
s435j
s435k
s435l
s435m
s435n
s449a
s449b
s449c
s449d
s449e
s449f
s449g
s449h
s449i
s449j
s449k
s449l
s449m
s449n
s449o
s449p
s449q
s449a1
s449b1
s449c1
s449d1
s449e1
s449f1
s449g1
s449h1
s449i1
s449j1
s449k1
s469a
s469b
s469c
s469d
s469e
s469f
s469g
s470
s470ca
s470cb
s470cc
s470cd
s470ce
s470cf
s470cg
s470ch
s470ci
s470cj
s470ck
s470cl
s470cm
s470cn
s470co
s470cp
s470cq
s471
s472
s472a
s472b
s472c
s472d
s472e
s472f
s472g
s472h
s472i
s472j
s472k
s472l
s472m
s492a_a
s492a_b
s492a_c
s492a_d
s492a_e
s492a_f
s492a_g
s492b_a
s492b_b
s492b_c
s492b_d
s492b_e
s492b_f
s492b_g
s493a_a
s493a_b
s493a_c
s493a_d
s493a_e
s493a_f
s493a_g
s493a_h
s493a_i
s493a_j
s493a_k
s493b_a
s493b_b
s493b_c
s493b_d
s493b_e
s493b_f
s493b_g
s493b_h
s493b_i
s493b_j
s493b_k
s498a
s498b
s498c
s498d
s567a
s567b
s567c
s567d
s567e
s568ca
s568cb
s568cc
s568cd
s568ce
s568cf
s568cg
s568ch
s568ci
s568cj
s568ck
s568cl
s568cm
s568cn
s568co
s568cp
s568cq
s568cr
s568cs
s568ct
s568cu
s570
s1203a
s1203b
s1203c
s1203d
s1206
m4
m5
m39
m39a
m55
m55a
m55b
m55c
m55d
m55e
m55f
m55g
m55h
m55i
m55j
m55k
m55l
m55m
m55n
m55o
m55x
m55z

{; 
*di "Variable is `x'";
cap gen `x' = .   ; 
local xlab: variable label `x';
gen `x'_question = "`xlab'";
cap decode `x', gen(`x'_string);
cap gen `x'_string ="";
};

#delimit cr;


* Identifiers
capture gen caseid=""
capture gen v000=""
capture gen v001  = .
capture gen v002  = .
capture gen v003  = .
capture gen b16 =.


# delimit;

keep 
caseid v000 v001 v002 v003  v021 v022 v023
bord b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b11 b16 b17 b18
h1 h2 h3 h4 h5 h6 h7 h8 h9
h11 h22 h31 h31a h31b
hw1 hw2 hw3 hw4 hw5 hw6 hw7 hw8 hw9 hw10 hw11 hw12 hw13 hw14 hw15 hw15 hw16 hw17 hw18 hw19 hw20
v000 v001 v002 v003 v005 v008 v008a v007
v012 v023 v024 v025 v101 v104 childhood_res
region_1 region_2 urban1 urban2
v106 v133 v501 v701 v201 v205 v206 v438 v439 s105 
v190
v213 v214 v225 v228 v231
 v151 v136 v137 v208
m4 m5 
v401 v404 v405 v409 v426 v437 v438 v439 v440 v445 v446 v447a v453 v463a v464
h15 h15b h37i h37j ml13i ml13j
h12y h12z h32y h32z
s435a s435b s435c s435d s435e s435f s435g s435h s435i s435j s435k s435l s435m s435n
s449a s449b s449c s449d s449e s449f s449g s449h s449i s449j s449k s449l s449m s449n s449o s449p s449q s449a1 s449b1 s449c1 s449d1 s449e1 s449f1 s449g1 s449h1 s449i1 s449j1 s449k1
s470 s470_question s470_st
s469a* s469b* s469c s469d s469e s469f s469g s470ca s470cb s470cc s470cd s470ce s470cf s470cg s470ch s470ci s470cj s470ck s470cl s470cm s470cn s470co s470cp s470cq s472
s472a s472b s472c s472d s472e s472f s472g s472h s472i s472j s472k s472l s472m
s492a_a s492a_b s492a_c s492a_d s492a_e s492a_f s492a_g s492b_a s492b_b s492b_c s492b_d s492b_e s492b_f s492b_g 
s493a_a s493a_b s493a_c s493a_d s493a_e s493a_f s493a_g s493a_h s493a_i s493a_j s493a_k s493b_a s493b_b s493b_c s493b_d s493b_e s493b_f s493b_g s493b_h s493b_i s493b_j s493b_k 
s498a s498b s498c s498d 
s1203a s1203b s1203c s1203d s1206
v204	v207	v216	v222	v407	v408	v409a	v410	v410a	v411	v411a	v412	v412a	v412b	v412c	v413	v414	v414a v414b v414c v414d	v414e	v414f	v414g	v414h	v414i	v414j	v414k	v414l	v414m	v414n	v414o	v414p	v414q	v414r	v414s	v414t v414u	v414v	v414w	v415	v469a v469b v469c v469d v469e	v469f v469g v469h v469i v469j v469k v469l v469m v469n v469o v469p v469q v469r v469s v469t v469u v469v v469w v469x v469y v469z v469xx v469xy v469xz	b19	m27	m21	m22	m34	m35	m36	m38	m39a	m39	m55	m55a	m55b	m55c	m55d	m55e	m55f	m55g	m55h	m55i	m55j	m55k	m55l	m55m	m55n	m55o	m55x	m55z	h18a	h57	h57d	h57m	h57y	h80a	h80b	h80c	h80d	h80e	h80f	h80g	s429	s430	s470	s471
s567a	s567b	s567c	s567d	s567e	s568ca	s568cb	s568cc	s568cd	s568ce	s568cf	s568cg	s568ch	s568ci	s568cj	s568ck	s568cl	s568cm	s568cn	s568co	s568cp	s568cq	s568cr	s568cs	s568ct	s568cu	s570
*_string *_question

;



# delimit cr;






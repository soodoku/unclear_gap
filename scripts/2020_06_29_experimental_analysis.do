
** Set WD
cd "~/Dropbox/partisan_gaps/data/"

** Load data
insheet using "merged_survey_ip_06_29_2020_final.csv", clear names


* Drop assholes who didn't finish
drop if sincerity == "NA"

**renaming some vars
rename wedliketounderstandhowyouthinkva self_econ
rename v31 self_unemployment
rename v32 self_inflation

************
**RECODING**
************

*7-point party ID
gen pid7 = .
replace pid7 = 1 if pid_dem == "1"
replace pid7 = 2 if pid_dem == "2"
replace pid7 = 3 if pid_ind == "2"
replace pid7 = 4 if pid_ind == "3"
replace pid7 = 5 if pid_ind == "1"
replace pid7 = 6 if pid_rep == "2"
replace pid7 = 7 if pid_rep == "1"	
label define pid7_lbl 1 "Strong Democrat" 2 "Weak Democrat" 3 "Leaning Democrat" 4 "Independent" 5 "Leaning Republican" 6 "Weak Republican" 7 "Strong Republican", replace
label values pid7 pid7_lbl

*3-point party ID
recode pid7 (1/3=1)(4=2)(5/7=3), gen(pid3)
label define pid3_lbl 1 "Democratic" 2 "Independent" 3 "Republican", replace
label values pid3 pid3_lbl

*Democratic dummy for comparing Dems and Reps
gen dem_rep = .
replace dem_rep = 1 if pid3 == 1
replace dem_rep = 0 if pid3 == 3
label define dem_rep_lbl 0 "Republican" 1 "Democrat", replace
label values dem_rep dem_rep_lbl

***********************
**CODING FOR TROLLING**
***********************
	
*low-incidence screeners
gen troll_sleep=1 if sleep=="1"
replace troll_sleep=0 if sleep=="0"
tab troll_sleep

gen troll_prosthetic=1 if prosthetic=="TRUE"
replace troll_prosthetic=0 if prosthetic=="FALSE"
tab troll_prosthetic

gen troll_blind=1 if blind=="TRUE"
replace troll_blind=0 if blind=="FALSE"
tab troll_blind

gen troll_deaf=1 if deaf=="TRUE"
replace troll_deaf=0 if deaf=="FALSE"
tab troll_deaf

gen troll_gang=1 if gang_resp=="TRUE"
replace troll_gang=0 if gang_resp=="FALSE"
tab troll_gang

gen troll_famgang=1 if gang_fam=="TRUE"
replace troll_famgang=0 if gang_fam=="FALSE"
tab troll_famgang

egen troll_index = rowtotal(troll_sleep troll_prosthetic troll_blind troll_deaf troll_gang troll_famgang)
gen troll = 0
replace troll = 1 if troll_index > 1
	tab troll
*30.07 % of the sample is marked as a troll

*how many people self-report as providing insincere responses?
destring sincerity, replace
tab sincerity
recode sincerity (1/3 = 0) (4/5 = 1), gen(insincere)
tab insincere
*14.37% admit to responding non-seriously 

*correlation between answering sincerely and troll index
corr troll_index sincerity
tab insincere troll, col chi

***********************
**CODING FOR BAD IPs**
***********************

tab blacklisted
*6.72% show up on a blacklist

tab missing_ip
*no missing IPs

tab duplicated
*5.39% are duplicated

tab foreign_ip
*1.13% are foreign IPs
gen foreign_ip_corrected=1 if foreign_ip=="TRUE"
replace foreign_ip_corrected=0 if foreign_ip=="FALSE"
tab foreign_ip_corrected

**********
***DATE***
**********
		
gen date_ok=0
replace date_ok=1 if date=="06 29 2020"|date=="06.29.2020"|date=="06\29\2020"|date=="06\30\2020" ///
	|date=="6/29/20"|date=="6/29/2020."|date=="6/29/20209"|date=="6/29/2020`"|date=="60/29/2020" ///
	|date=="ju/26/2020"|date=="june/29/20"|date=="o6/29/2020" | date=="16/29/2020"
tab date_ok
*so 24.82% of respondents entered the date incorrectly

*okay, what about those who wrote DD/MM/YYYY
gen date_poss_foreign = 0
replace date_poss_foreign = 1 if date == "20/06/2020" | date == "28.06.2020" | date == "28/06/2020" | date == "28/6/2020" ///
	|date == " 29 06 2020" | date ==  "29-06-2020" | date == "29-Jun-20" | date == "29.06 2020" | date == "29.06.2020" ///
	|date == "29/06/2020"| date == "29/6/2020" | date == " 29/6/2020." | date == "29\06\2020" | date == "29|06|2020" ///
	|date == "30/06/2020"
tab date_poss_foreign 
*geez, 20.03% of the sample is possibly comprised of foreigners

*what's the correlation between this and foreign IPs?
corr date_poss_foreign foreign_ip_corrected

*okay, how many people wrote a nonsensical response to the date question?
gen inattentive = 1
replace inattentive = 0 if date_ok==1|date_poss_foreign==1
tab inattentive
*4.79%

* Plotting time to completion against writing date/foreign IP
twoway kdensity duration if date_ok == 1 || kdensity duration if date_ok == 0, ///
	ytitle("Density") xt("Survey duration, in seconds") /// 
	xline(900, lc(black) lp(dot)) ///
	text(0.00195 1600 "Target time = 15 minutes") ///
	legend(label(1 "Date formatted MM/DD/YYYY") label(2 "Date formatted otherwise"))
ksmirnov durationinseconds, by(date_ok)

************************************************************
***CREATING UPPER AND LOWER BOUND ESTIMATES OF BAD ACTORS***
************************************************************

*creating a variable that combines trolling and weird IPs ("lower bound")
gen combined_troll_1=.
replace combined_troll_1 = 1 if funny_ip == "TRUE" | troll == 1
replace combined_troll_1 = 0 if funny_ip == "FALSE" & troll == 0
tab combined_troll_1
*okay, so 37.99% of data is suspicious using this measure

*creating a variable that combines trolling, weird IP, and weirdly written date 
gen combined_troll_2=.
replace combined_troll_2=1 if funny_ip=="TRUE" |troll==1|date_poss_foreign==1
replace combined_troll_2=0 if funny_ip=="FALSE" & troll==0 & date_poss_foreign==0
tab combined_troll_2
*44.64% of respondents are suspicious using this measure

*creating a variable that combines trolling, weird IP, weirdly written date, or inattentive (nonsensical date write-in)
gen combined_troll_3=.
replace combined_troll_3=1 if funny_ip=="TRUE" |troll==1|date_poss_foreign==1|inattentive==1
replace combined_troll_3=0 if funny_ip=="FALSE"  & troll==0 & date_poss_foreign==0 & inattentive==0
tab combined_troll_3
*46.17% marked as suspicious here


************************
**EXPERIMENTAL RESULTS**
************************

*recoding DVs

gen gop_unemploy=1 if gop_unemployment=="1"
replace gop_unemploy=.5 if gop_unemployment=="2"
replace gop_unemploy=0 if gop_unemployment=="3"
tab gop_unemploy

gen gop_inflate=1 if gop_inflation=="1"
replace gop_inflate=.5 if gop_inflation=="2"
replace gop_inflate=0 if gop_inflation=="3"
tab gop_inflate

gen obama_unemploy=1 if obama_unemployment=="1"
replace obama_unemploy=.5 if obama_unemployment=="2"
replace obama_unemploy=0 if obama_unemployment=="3"
tab obama_unemploy

gen obama_inflate=1 if obama_inflation=="1"
replace obama_inflate=.5 if obama_inflation=="2"
replace obama_inflate=0 if obama_inflation=="3"
tab obama_inflate


*creating a collapsed unemployment DV
gen unemploy = gop_unemploy
replace unemploy = obama_unemploy if unemploy==.
tab unemploy

*creating a collapsed inflation DV
gen inflation = gop_inflate
replace inflation = obama_inflate if inflation==.
tab inflation

*generating treatment variable
gen dem_treat=1 if randomization_1 == "obama"
replace dem_treat=0 if randomization_1 == "congress"
tab dem_treat

*creating a party x treatment variable 
*1 = if you got the out-party treatment, 0 = in-party treatment
gen out_party_treat=.
replace out_party_treat = 1 if dem_rep == 1 & dem_treat == 0
replace out_party_treat = 1 if dem_rep == 0 & dem_treat == 1
replace out_party_treat = 0 if dem_rep == 0 & dem_treat == 0
replace out_party_treat = 0 if dem_rep == 1 & dem_treat == 1
tab out_party_treat

*effects among non-flagged respondents
reg unemploy out_party_treat if combined_troll_1 == 0
est store nontroll1
reg inflation out_party_treat if combined_troll_1 == 0
est store nontroll2
outreg2 [nontroll*]  using "nontroll_results", tex replace cttop(full) dec(3)

*interactive effects with trolling indicator
reg unemploy i.out_party_treat##i.combined_troll_1
est store interact1
reg inflation i.out_party_treat##i.combined_troll_1
est store interact2
outreg2 [interact*]  using "exp_eff_interact", tex replace cttop(full) dec(3)

*effects among all flagged respondents
reg unemploy out_party_treat if combined_troll_1 == 1
est store flagged1
reg inflation out_party_treat if combined_troll_1 == 1
est store flagged2
outreg2 [flagged*] using "all_flagged_resp_results", tex replace cttop(full) dec(3)

*effects among flagged IPs only
reg unemploy out_party_treat if funny_ip =="TRUE"
est store funny1
reg inflation out_party_treat if funny_ip =="TRUE"
est store funny2
outreg2 [funny*] using "funny_ip_results", tex replace cttop(full) dec(3)

*effects among trolls only
reg unemploy out_party_treat if troll==1
est store troll1
reg inflation out_party_treat if troll==1
est store troll2
outreg2 [troll*] using "troll_results", tex replace cttop(full) dec(3)

*effects among those with 1k+ HITs
reg unemploy out_party_treat if hits=="4"
est store hits1
reg inflation out_party_treat if hits=="4"
est store hits2
outreg2 [hits*] using "high_hits_results", tex replace cttop(full) dec(3)

*********************************
***GENERATING TABLES FOR P_GAP***
*********************************

*okay, what are the effects among the full sample?

*effects among non-flagged respondents
reg unemploy out_party_treat if combined_troll_1 == 0
est store nontroll1
reg inflation out_party_treat if combined_troll_1 == 0
est store nontroll2
outreg2 [nontroll*]  using "nontroll_results", tex replace cttop(full) dec(3)

*other models for ITT in the appendix
reg unemploy out_party_treat
est store interact1
reg inflation out_party_treat
est store interact2
reg unemploy i.out_party_treat##i.combined_troll
est store interact3
reg inflation i.out_party_treat##i.combined_troll 
est store interact4
outreg2 [interact*] using "full_sample_2", tex replace cttop(full) dec(3)


************************************************
***CREATING DISRIBUTION GRAPHS FOR EXPEIRMENT***
************************************************

grstyle init
grstyle set plain, nogrid noextend

byhist unemploy if combined_troll_1==0, by(out_party_treat) discrete percent tw(ylab(0(20)80))

byhist inflation if combined_troll_1==0, by(out_party_treat) discrete percent tw(ylab(0(20)80))

cd "~/Dropbox/partisan_gaps/figs/"

grc1leg unemploy_nontrolls.gph inflation_notrolls.gph, scheme(lean2)

*are these two samples independent from one another? 
ranksum unemploy if combined_troll_1==0, by(out_party_treat)
sum unemploy if combined_troll_1==0, by(out_party_treat)

*getting medians to report
sum unemploy if combined_troll_1==0 & out_party_treat==1, d
*median = 0.50, sd= .300797
sum unemploy if combined_troll_1==0 & out_party_treat==0, d
*median = 1.0, sd= .2876815

sum inflation if combined_troll_1==0 & out_party_treat==1, d
*median = 0.50, sd=.3041086
sum inflation if combined_troll_1==0 & out_party_treat==0, d
*median=1.0, sd=.3077154


***********************************************************
***OKAY, WHAT ABOUT ATTENUATION EFFECTS DUE TO TROLLING?***
***********************************************************

*getting non-suspicious beta and se
reg unemploy out_party_treat if combined_troll_1 == 0 
reg inflation out_party_treat if combined_troll_1 == 0
*n=861 

*getting suspicious beta and se
reg unemploy out_party_treat if combined_troll_1 == 1 
reg inflation out_party_treat if combined_troll_1 == 1
*n=564

*full sample beta and se
reg unemploy out_party_treat
reg inflation out_party_treat
*n=1425

insheet using "turk_06_29_2020/june_2020_attenuation_fx.csv", clear names

gen diff = nonsusp_beta - susp_beta
gen se_diff = sqrt(((nonsusp_se ^ 2) / 861) + (susp_se ^ 2) / 564)
gen weight = 1 / se_diff
reg diff [aw = weight]
	
*average treatment effect in the non-troll group
gen weight_nonsusp = 1 / nonsusp_se
reg nonsusp_beta [aw = weight_nonsusp]
	* -.108 
		
*getting an attenuation effect, weighted by the inverse of the estimated SE of the differences
gen attn = nonsusp_beta - full_beta
gen se_attn = sqrt(((nonsusp_se ^ 2) / 861) + (full_se ^ 2) / 1425)
gen attn_wt = 1 / se_attn
reg attn [aw = attn_wt] 
	*-.0298358

*putting it in percentage point terms. we observe treatment effects that are...
gen attn_pct = full_beta / nonsusp_beta
reg attn_pct [aw = attn_wt]
	* .7165279 what they would be without suspicious responses
*in other words, our treatment effects are attenuated by...
	display 1 -   .7165279
	* = .2834721 or 28.3% 

*alternative test: ttests 
ttest gop_unemploy, by(dem_rep)
ttest obama_unemploy, by(dem_rep)
ttest gop_inflate, by(dem_rep)
ttest obama_inflate, by(dem_rep)

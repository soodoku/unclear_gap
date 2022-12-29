
** Set WD
cd "~/Dropbox/partisan_gaps/data/"
cd "~/Dropbox/turk/data/"

** Load data
insheet using "turk_study_june_2020/2020_06_29.csv", clear names
insheet using "turk_06_29_2020/merged_survey_ip_06_29_2020_final.csv", clear names


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
destring sleep, replace
recode sleep (1=1)(2/4=0)(5=1), gen(troll_sleep)

gen troll_prosthetic=1 if prosthetic=="1"
replace troll_prosthetic=0 if prosthetic=="2"
tab troll_prosthetic

destring blind, replace
recode blind (1=1)(2=0), gen(troll_blind)

destring deaf, replace
recode deaf (1=1)(2=0), gen(troll_deaf)

gen troll_gang=1 if gang=="1"
replace troll_gang=0 if gang=="2"
tab troll_gang

gen troll_famgang=1 if family_gang=="1"
replace troll_famgang=0 if family_gang=="2"
tab troll_famgang

egen troll_index = rowtotal(troll_sleep troll_prosthetic troll_blind troll_deaf troll_gang troll_famgang)
gen troll = 0
replace troll = 1 if troll_index > 1
	tab troll
*30.07 % of the sample is marked as a troll

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
* 2.99% are foreign IPs
gen foreign_ip_corrected=1 if foreign_ip=="TRUE"
replace foreign_ip_corrected=0 if foreign_ip=="FALSE"
tab foreign_ip_corrected

tab funny_ip
* 13.91% come from suspicious IPs

gen suspicious_ip=1 if funny_ip=="TRUE"
replace suspicious_ip=0 if funny_ip=="FALSE"
tab suspicious_ip

**********
***DATE***
**********
		
gen date_ok=0
replace date_ok=1 if date=="06 29 2020"|date=="06.29.2020"|date=="06\29\2020"|date=="06\30\2020" ///
	|date=="6/29/20"|date=="6/29/2020."|date=="6/29/20209"|date=="6/29/2020`"|date=="60/29/2020" ///
	|date=="ju/26/2020"|date=="june/29/20"|date=="o6/29/2020" | date=="16/29/2020"
tab date_ok
*so 24.82% of respondents entered the date incorrectly

*what about the strict definition of writing the date DD/MM/YYYY (incorrectly)?
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

*creating a variable that combines trolling and weird IPs ("lower bound")
gen combined_troll_1=.
replace combined_troll_1 = 1 if suspicious_ip == 1 | troll == 1
replace combined_troll_1 = 0 if suspicious_ip == 0 & troll == 0
tab combined_troll_1
*okay, so 39.06% of data is suspicious using this measure

*creating a variable that combines trolling, weird IP, and weirdly written date 
gen combined_troll_2=.
replace combined_troll_2=1 if suspicious_ip==1 |troll==1|date_poss_foreign==1
replace combined_troll_2=0 if suspicious_ip==0 & troll==0 & date_poss_foreign==0
tab combined_troll_2
*45.44% of respondents are suspicious using this measure

*creating a variable that combines trolling, weird IP, weirdly written date, and inattentiveness on date question




***************
***SINCERITY****
****************

recode sincerity (1/2 = 0) (3/5 = 1), gen(insincere)
tab insincere

*survey sincerity -- rescaling 0-1
replace sincerity = (sincerity - 1) / 4
		
*what's the relationship between trolling measures and sincerity?
corr troll_2 sincerity
**correlation = .359

************************
**EXPERIMENTAL RESULTS**
************************

*recoding DVs
recode gop_unemployment (1=1)(2=0.5)(3=0)
recode gop_inflation (1=1)(2=0.5)(3=0)
recode obama_unemployment (1=1)(2=0.5)(3=0)
recode obama_inflation (1=1)(2=0.5)(3=0)

*creating a collapsed unemployment DV
gen unemploy = gop_unemployment
replace unemploy = obama_unemployment if unemploy==.
tab unemploy

*creating a collapsed inflation DV
gen inflation = gop_inflation
replace inflation = obama_inflation if inflation==.
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

*okay, what are the effects among the full sample?
reg unemploy out_party_treat

reg inflation out_party_treat

*what about effects just among non-trolls?
reg unemploy out_party_treat if troll_2 == 0
est store notroll1
reg inflation out_party_treat if troll_2 == 0
est store notroll2
outreg2 [notroll*]  using "exp_eff_notroll", tex replace cttop(full) dec(3)

*interactive effects with trolling indicator
reg unemploy i.out_party_treat##i.troll_2
est store interact1
reg inflation i.out_party_treat##i.troll_2
est store interact2
outreg2 [interact*]  using "exp_eff_interact", tex replace cttop(full) dec(3)

***OKAY, WHAT ABOUT ATTENUATION EFFECTS DUE TO TROLLING?***

*getting non-suspicious beta and se
reg unemploy out_party_treat if troll_2 == 0 
reg inflation out_party_treat if troll_2 == 0
*n=818 

*getting suspicious beta and se
reg unemploy out_party_treat if troll_2 == 1 
reg inflation out_party_treat if troll_2 == 1
*n=607

*full sample beta and se
reg unemploy out_party_treat
reg inflation out_party_treat
*n=1425

insheet using "/Users/carrieroush/Dropbox/partisan_gaps/tabs/pgap_attenuation_fx.csv", clear names

gen diff = nonsusp_beta - susp_beta
gen se_diff = sqrt(((nonsusp_se ^ 2) / 818) + (susp_se ^ 2) / 607)
gen weight = 1 / se_diff
reg diff [aw = weight]
	
*average treatment effect in the non-troll group
gen weight_nonsusp = 1 / nonsusp_se
reg nonsusp_beta [aw = weight_nonsusp]
	* -.128 
		
*getting an attenuation effect, weighted by the inverse of the estimated SE of the differences
gen attn = nonsusp_beta - full_beta
gen se_attn = sqrt(((nonsusp_se ^ 2) / 818) + (full_se ^ 2) / 1425)
gen attn_wt = 1 / se_attn
reg attn [aw = attn_wt] 
	*-.0316586

*putting it in percentage point terms. we observe treatment effects that are...
gen attn_pct = full_beta / nonsusp_beta
reg attn_pct [aw = attn_wt]
	* .7523311 what they would be without suspicious responses
*in other words, our treatment effects are attenuated by...
	display 1 -   .7523311 
	* = .2476689, or 24.8%

*alternative test: ttests 
ttest gop_unemployment, by(dem_rep)
ttest obama_unemployment, by(dem_rep)
ttest gop_inflation, by(dem_rep)
ttest obama_inflation, by(dem_rep)


*******************************************
**VARIABLE RECODES FOR DESCRIPTIVE ANALYSES
*******************************************

recode self_econ (1=1)(4=0.5)(5=0)
recode self_unemployment (1=1)(4=0.5)(5=0)
recode self_inflation (1=1)(4=0.5)(5=0)

rename reps_economynonamegotbetter reps_econ_better
rename reps_economynonamestayedaboutthe reps_econ_same
rename reps_economynonamegotworse reps_econ_worse
	
rename reps_unemploymentnonamegotbetter reps_unemployment_better
rename reps_unemploymentnonamestayedabo reps_unemployment_same
rename reps_unemploymentnonamegotworse reps_unemployment_worse
	
rename reps_inflationnonamegotbetter reps_inflation_better
rename reps_inflationnonamestayedaboutt reps_inflation_same
rename reps_inflationnonamegotworse reps_inflation_worse
	
rename dems_economynonamegotbetter dems_econ_better
rename dems_economynonamestayedaboutthe dems_econ_same
rename dems_economynonamegotworse dems_econ_worse
	
rename dems_unemploymentnonamegotbetter dems_unemployment_better
rename dems_unemploymentnonamestayedabo dems_unemployment_same
rename dems_unemploymentnonamegotworse dems_unemployment_worse
	
rename dems_inflationnonamegotbetter dems_inflation_better
rename dems_inflationnonamestayedaboutt dems_inflation_same
rename dems_inflationnonamegotworse dems_inflation_worse

******************************************
**SELF PERCEPTIONS - NON-SUS RESPONDENTS**
******************************************

*Overall economy
	*Full sample: 
	tab self_econ if troll_2==0
		*44.75% got better, 34.46% stayed about the same, 20.79% got worse
	*Republicans:
	tab self_econ if dem_rep == 0 & troll_2==0
		*62.13% got better, 27.47% stayed about the same, 10.40% got worse
	*Democrats: 
	tab self_econ if dem_rep==1 & troll_2==0
		*31.60% got better, 38.83% stayed about the same, 29.57% got worse
		
*Unemployment
	*Full sample
	tab self_unemployment if troll_2==0
		*43.05% got better, 30.17% stayed about the same, 26.78% got worse
	*Republicans
	tab self_unemployment if dem_rep==0 & troll_2==0
		*54.93% got better, 25.33% stayed about the same, 19.73% got worse
	tab self_unemployment if dem_rep==1 & troll_2==0
		*33.63% got better, 34.31% stayed about the same, 32.05% got worse
		
*Inflation 
	*Full sample
	tab self_inflation if troll_2==0
		*17.74% got better, 55.25% stayed about the same, 27.01% got worse
	*Republicans
	tab self_inflation if dem_rep==0 & troll_2==0
		*26.13% got better, 57.87% stayed about the same, 16.00% got worse
	*Democrats
	tab self_inflation if dem_rep==1 & troll_2==0
		*11.51% got better, 52.37% stayed about the same, 36.12% got worse
		
			
**************************************************
**SECOND ORDER PERCEPTIONS - NON SUS RESPONDENTS**
**************************************************

*HOW WOULD REPUBLICANS INTERPRET...

*Overall economy
	*Full sample
	sum reps_econ_better if troll_2==0
	sum reps_econ_same if troll_2==0
	sum reps_econ_worse if troll_2==0
	*59.22% got better, 23.76% stayed about the same, 17.02% got worse
	
	*Republicans' perceptions of themselves 
	sum reps_econ_better if dem_rep==0 & troll_2==0
	sum reps_econ_same if dem_rep==0 & troll_2==0
	sum reps_econ_worse if dem_rep==0 & troll_2==0
	*58.10% got better, 25.07% stayed about the same, 15.84% got worse
	
	*Democrats' perceptions of Republicans
	sum reps_econ_better if dem_rep==1 & troll_2==0
	sum reps_econ_same if dem_rep==1 & troll_2==0
	sum reps_econ_worse if dem_rep==1 & troll_2==0
	*59.50% got better, 22.40% stayed about the same, 18.08% got worse
	
	*Are there statistically significant differences between how Republicans and Democrats view Republicans?
	ttest reps_econ_better if troll_2==0, by(dem_rep)
		*diff (Rep - Dem) = -.42, t = -0.2
	ttest reps_econ_same if troll_2==0, by(dem_rep)
		*diff (Rep - Dem) = 2.67, t = 2.2
	ttest reps_econ_worse if troll_2==0, by(dem_rep)
		*diff (Rep - Dem) = -2.25, t = -1.62
		
*Unemployment
	*Full sample
	sum reps_unemployment_better if troll_2==0
	sum reps_unemployment_same if troll_2==0
	sum reps_unemployment_worse if troll_2==0
	*58.85% got better, 23.40% stayed about the same, 17.75% got worse

	*Republicans' perceptions of themselves
	sum reps_unemployment_better if dem_rep==0 & troll_2==0
	sum reps_unemployment_same if dem_rep==0 & troll_2==0
	sum reps_unemployment_worse if dem_rep==0 & troll_2==0
	*58.56% got better, 25.28% stayed about the same, 17.06% got worse
	
	*Democrats' perceptions of Republicans
	sum reps_unemployment_better if dem_rep==1 & troll_2==0
	sum reps_unemployment_same if dem_rep==1 & troll_2==0
	sum reps_unemployment_worse if dem_rep==1 & troll_2==0
	*59.42% got better, 21.83% stayed about the same, 18.76% got worse

*Inflation
	*Full sample
	sum reps_inflation_better if troll_2==0
	sum reps_inflation_same if troll_2==0
	sum reps_inflation_worse if troll_2==0
	*46.12% got better, 34.98% stayed about the same, 18.90% got worse
	
	*Republicans' perceptions of themselves
	sum reps_inflation_better if dem_rep==0 & troll_2==0
	sum reps_inflation_same if dem_rep==0 & troll_2==0
	sum reps_inflation_worse if dem_rep==0 & troll_2==0
	*46.63% got better, 35.54% stayed about the same, 17.83% got worse
	
	*Democrats' perceptions of Republicans
	sum reps_inflation_better if dem_rep==1 & troll_2==0
	sum reps_inflation_same if dem_rep==1 & troll_2==0
	sum reps_inflation_worse if dem_rep==1 & troll_2==0
	*45.55% got better, 34.83% stayed about the same, 19.63% got worse
	

*HOW WOULD DEMOCRATS INTERPRET...

*Overall economy
	*Full sample
	sum dems_econ_better if troll_2==0
	sum dems_econ_same if troll_2==0
	sum dems_econ_worse if troll_2==0
	*35.00% got better, 32.02% stayed about the same, 32.85% got worse
	
	*Democrats' perceptions of themselves
	sum dems_econ_better if dem_rep==1 & troll_2==0
	sum dems_econ_same if dem_rep==1 & troll_2==0
	sum dems_econ_worse if dem_rep==1 & troll_2==0
	*35.48% got better, 33.09% stayed about the same, 31.52% got worse
	
	*Republicans' perceptions of Democrats
	sum dems_econ_better if dem_rep==0 & troll_2==0
	sum dems_econ_same if dem_rep==0 & troll_2==0
	sum dems_econ_worse if dem_rep==0 & troll_2==0
	*35.11% got better, 31.04% stayed about the same, 33.42% got worse
	
*Unemployment
	*Full sample
	sum dems_unemployment_better if troll_2==0
	sum dems_unemployment_same if troll_2==0
	sum dems_unemployment_worse if troll_2==0
	*36.99% got better, 32.81% stayed about the same, 30.20% got worse
	
	*Democrats' perceptions of themselves
	sum dems_unemployment_better if dem_rep==1 & troll_2==0
	sum dems_unemployment_same if dem_rep==1 & troll_2==0
	sum dems_unemployment_worse if dem_rep==1 & troll_2==0
	*37.91% got better, 33.02% stayed about the same, 29.07% got worse
	
	*Republicans' perceptions of Democrats
	sum dems_unemployment_better if dem_rep==0 & troll_2==0
	sum dems_unemployment_same if dem_rep==0 & troll_2==0
	sum dems_unemployment_worse if dem_rep==0 & troll_2==0
	*36.64% got better, 32.27% stayed about the same, 31.09% got worse

*Inflation 
	*Full sample
	sum dems_inflation_better if troll_2==0
	sum dems_inflation_same if troll_2==0
	sum dems_inflation_worse if troll_2==0
	*29.10% got better, 36.63% stayed about the same, 34.27% got worse
	
	*Democrats' perceptions of themselves
	sum dems_inflation_better if dem_rep==1 & troll_2==0
	sum dems_inflation_same if dem_rep==1 & troll_2==0
	sum dems_inflation_worse if dem_rep==1 & troll_2==0
	*28.33% got better, 38.74% stayed about the same, 32.93% got worse
	
	*Republicans' perceptions of Democrats
	sum dems_inflation_better if dem_rep==0 & troll_2==0
	sum dems_inflation_same if dem_rep==0 & troll_2==0
	sum dems_inflation_worse if dem_rep==0 & troll_2==0
	*30.8% got better, 34.34% stayed about the same, 34.86% got worse



*****************************************************
**SELF PERCEPTIONS AMONG FULL SAMPLE (INC. TROLLS) **
*****************************************************
	
*Overall economy
	*Full sample: 
	tab self_econ
		*41.2% got better, 40.59% stayed about the same, 18.23% got worse
	*Republicans:
	tab self_econ if dem_rep == 0
		*Better = 50.21%, about the same = 38.08%, worse = 11.71%
	*Democrats: 
	tab self_econ if dem_rep==1
		*Better = 33.38%, about the same = 42.46%, worse = 24.16%
		
*Unemployment
	*Full sample
	tab self_unemployment
		*36.9% got better, 33.9% stayed about the same, 29.14% got worse 
	*Republicans
	tab self_unemployment if dem_rep==0
		*Got better = 44.15%, about the same = 30.32%, worse = 25.53%
	tab self_unemployment if dem_rep==1
		*Better = 30.03%, about the same = 37.71%m worse = 32.26%
		
*Inflation 
	*Full sample
	tab self_inflation
		*Better = 22.2%, about the same = 52.2%, got worse = 25.6%
	*Republicans
	tab self_inflation if dem_rep==0
		*Better = 28.21%, about the same = 52.75%, worse = 19.04%
	*Democrats
	tab self_inflation if dem_rep==1
		*Better = 17.18%, about the same = 51.26%, worse = 31.56%
	
*************************************************************
**SECOND ORDER PERCEPTIONS AMONG FULL SAMPLE (INC. TROLLS) **
*************************************************************

*HOW WOULD REPUBLICANS INTERPRET...

*Overall economy
	*Full sample
	sum reps_econ_better
	sum reps_econ_same
	sum reps_econ_worse
	*51.6% got better, 26.8% stayed about the same, 21.7% got worse
	
	*Republicans' perceptions of themselves
	sum reps_econ_better if dem_rep==0
	sum reps_econ_same if dem_rep==0
	sum reps_econ_worse if dem_rep==0
	*50.3% got better, 28.16% stayed about the same, 21.5% got worse
	
	*Democrats' perceptions of Republicans
	sum reps_econ_better if dem_rep==1
	sum reps_econ_same if dem_rep==1
	sum reps_econ_worse if dem_rep==1
	*52.19% got better, 25.47% stayed about the same, 22.3% got worse
	
	*Are there statistically significant differences between how Republicans and Democrats view Republicans?
	ttest reps_econ_better, by(dem_rep)
		*Democrats higher than Republicans by 1.86, not statistically significant
	ttest reps_econ_same, by(dem_rep)
		*Republicans higher than Democrats by 2.69, statistically sig at 95%
	ttest reps_econ_worse, by(dem_rep)
		*Democrats higher by 0.84, not statistically sig

*Unemployment
	*Full sample
	sum reps_unemployment_better
	sum reps_unemployment_same
	sum reps_unemployment_worse
	*51.2% got better, 26.04% stayed about the same, 22.7% got worse

	*Republicans' perceptions of themselves
	sum reps_unemployment_better if dem_rep==0
	sum reps_unemployment_same if dem_rep==0
	sum reps_unemployment_worse if dem_rep==0
	*50.4% got better, 27.6% about the same, 21.9% got worse
	
	*Democrats' perceptions of Republicans
	sum reps_unemployment_better if dem_rep==1
	sum reps_unemployment_same if dem_rep==1
	sum reps_unemployment_worse if dem_rep==1
	*51.6% got better, 24.6% stayed about the same, 23.8% got worse

*Inflation
	*Full sample
	sum reps_inflation_better
	sum reps_inflation_same
	sum reps_inflation_worse
	*43.76% got better, 33.10% about the same, 18.71% got worse
	
	*Republicans' perceptions of themselves
	sum reps_inflation_better if dem_rep==0
	sum reps_inflation_same if dem_rep==0
	sum reps_inflation_worse if dem_rep==0
	*44.22% got better, 32.8% about the same, 22.97% got worse
	
	*Democrats' perceptions of Republicans
	sum reps_inflation_better if dem_rep==1
	sum reps_inflation_same if dem_rep==1
	sum reps_inflation_worse if dem_rep==1
	*42.98% got better, 33.36% got worse, 23.65% got worse
	

*HOW WOULD DEMOCRATS INTERPRET...

*Overall economy
	*Full sample
	sum dems_econ_better
	sum dems_econ_same
	sum dems_econ_worse
	*Got better = 36.5%, about the same = 31.76%, worse = 31.24%
	
	*Democrats' perceptions of themselves
	sum dems_econ_better if dem_rep==1
	sum dems_econ_same if dem_rep==1
	sum dems_econ_worse if dem_rep==1
	*Got better = 37.89%, 32.21 about the same, 29.76% got worse
	
	*Republicans' perceptions of Democrats
	sum dems_econ_better if dem_rep==0
	sum dems_econ_same if dem_rep==0
	sum dems_econ_worse if dem_rep==0
	*Got better = 35.69%, 31.36 about the same, 32.12% got worse
	
*Unemployment
	*Full sample
	sum dems_unemployment_better
	sum dems_unemployment_same
	sum dems_unemployment_worse
	*37.63% got better, 32.17% about the same, 30.20% got worse
	
	*Democrats' perceptions of themselves
	sum dems_unemployment_better if dem_rep==1
	sum dems_unemployment_same if dem_rep==1
	sum dems_unemployment_worse if dem_rep==1
	*38.86% got better, 32.06% about the same, 29.10% got worse
	
	*Republicans' perceptions of Democrats
	sum dems_unemployment_better if dem_rep==0
	sum dems_unemployment_same if dem_rep==0
	sum dems_unemployment_worse if dem_rep==0
	*36.76% got better, 32.08% about the same, 31.16% got worse

*Inflation 
	*Full sample
	sum dems_inflation_better
	sum dems_inflation_same
	sum dems_inflation_worse
	*32.76% got better, 34.90% about the same, 32.34% worse
	
	*Democrats' perceptions of themselves
	sum dems_inflation_better if dem_rep==1
	sum dems_inflation_same if dem_rep==1
	sum dems_inflation_worse if dem_rep==1
	*32.62% got better, 36.21% got worse, 31.17% got worse
	
	*Republicans' perceptions of Democrats
	sum dems_inflation_better if dem_rep==0
	sum dems_inflation_same if dem_rep==0
	sum dems_inflation_worse if dem_rep==0
	*33.61% got better, 33.58% about the same, 32.81% got worse

	



** Testing perceptions of partisans' beliefs against reality
	* TL;DR: People (on MTurk, at least) are SHOCKINGLY accurate about this
	
	* First, let's get perceptions. What was the average perception of the % of Republicans who thought...
		
		*The economy got better? 51.5%
		sum reps_econ_better
		*The economy stayed the same? 26.8%
		sum reps_econ_same
		*The economy got worse? 21.7%
		sum reps_econ_worse
		
		*Unemployment got better? 51.2%
		sum reps_unemployment_better
		*Unemployment stayed the same? 26.0%
		sum reps_unemployment_same
		*Unemployment got worse? 22.8%
		sum reps_unemployment_worse
		
		*Inflation got better? 43.7%
		sum reps_inflation_better
		*Inflation stayed the same? 33.1%
		sum reps_inflation_same
		*Inflation got worse? 23.2%
		sum reps_inflation_worse
		
	* And what are the perceptions of the % of Democrats who thought...
	

		

		
	* How about a formal test, with partisanship accounted for:
		*Reps/economy
		ttest reps_econ_better == 50.4
		ttest reps_econ_better == 50.4 if pid3 == 1
		ttest reps_econ_better == 50.4 if pid3 == 3
		ttest reps_econ_better, by(dem_rep)
		
		ttest reps_econ_worse == 11.7
		ttest reps_econ_worse == 11.7 if pid3 == 1
		ttest reps_econ_worse == 11.7 if pid3 == 3
		ttest reps_econ_worse, by(dem_rep)
		
		ttest reps_unemployment_better == 
		ttest reps_unemployment_better == if pid3 == 1
		ttest reps_unemployment_better == if pid3 == 3
		ttest reps_unemployment_better, by(dem_rep)
		
		ttest reps_unemployment_worse == 
		ttest reps_unemployment_worse == if pid3 == 1
		ttest reps_unemployment_worse == if pid3 == 3
		ttest reps_unemployment_worse, by(dem_rep)
		
		ttest reps_inflation_better == 
		ttest reps_inflation_better == if pid3 == 1
		ttest reps_inflation_better == if pid3 == 3
		ttest reps_inflation_better, by(dem_rep)
		
		ttest reps_inflation_worse == 
		ttest reps_inflation_worse == if pid3 == 1
		ttest reps_inflation_worse == if pid3 == 3
		ttest reps_inflation_worse, by(dem_rep)
		
		*Dems/economy
		ttest dems_econ_better == 33.3
		ttest dems_econ_better == 33.3 if pid3 == 1
		ttest dems_econ_better == 33.3 if pid3 == 3
		ttest dems_econ_better, by(dem_rep)
		
		ttest dems_econ_worse == 24.3
		ttest dems_econ_worse == 24.3 if pid3 == 1
		ttest dems_econ_worse == 24.3 if pid3 == 3
		ttest dems_econ_worse, by(dem_rep)
		
		ttest dems_unemployment_better == 
		ttest dems_unemployment_better == if pid3 == 1
		ttest dems_unemployment_better == if pid3 == 3
		ttest dems_unemployment_better, by(dem_rep)
		
		ttest dems_unemployment_worse == 
		ttest dems_unemployment_worse == if pid3 == 1
		ttest dems_unemployment_worse == if pid3 == 3
		ttest dems_unemployment_worse, by(dem_rep)
		
		ttest dems_inflation_better == 
		ttest dems_inflation_better == if pid3 == 1
		ttest dems_inflation_better == if pid3 == 3
		ttest dems_inflation_better, by(dem_rep)
		
		ttest dems_inflation_worse == 
		ttest dems_inflation_worse == if pid3 == 1
		ttest dems_inflation_worse == if pid3 == 3
		ttest dems_inflation_worse, by(dem_rep)
		
		
** Trump video study
sum trump_masks
bys pid7: reg trump_masks

ttest trump_masks, by(dem_rep)
ttest trump_masks if trump_masks < 100, by(dem_rep)
ttest trump_masks if trump_masks < 10, by(dem_rep)

	* Testing for corr with trolling
	reg trump_masks sincerity
	reg trump_masks troll
	reg trump_masks troll_index
	
	reg trump_masks sincerity if trump_masks < 100
	reg trump_masks troll if trump_masks < 100
	reg trump_masks troll_index if trump_masks < 100



********************************************************************************
******* Knowledge Seeking and Anonymity in Digital Work Settings ***************
******* by Mickeler, M., Khashabi, P., Kleine, M., and Kretschmer, T. **********
******* Published at the Strategic Management Journal **************************
******* Do-file for laboratory experiment **************************************
******* 17/04/2023 *************************************************************
******* corresponding dta-file: SMJ_Seeking_and_Anonymity_Lab.dta **************
********************************************************************************

***************************************************************************
******** Explanation of used variables ************************************
***************************************************************************

*** Variable name [explanation; outcome range]

*** Age [age; 18-63]
*** difficulty [question difficulty; 1 "easy", 2 "moderate", 3 "tough"]
*** Exp [experience participation in experiments; 0 "never before", 1 "1-2 times before",3 "3-5 times before, 6 "6 times and more"]
*** Gender [gender; 1 "female, 2 "Male"] 
*** German [German mother tongue; 0 "no", 1 "yes"]
*** Payoff1perquestion [per-question payoff in EUR for stage 1 of the experiment; -0.1 - 1.25]
*** SeekNum [total number of knowledge seeking per participant; 0 - 15]
*** Seeking [knowledge seeking per question; 0 "no", 1 "yes"]
*** SocImage [social image concerns; 1 "low social image concern", 2 "high social image concern"]
*** TT [treatment; 1 "FULL COSTS", 2 "SOCIAL COSTS", 3 "NO COSTS"]
*** uid [unique identifier for each participant; unique number per participant]


***************************************************************************
******** Section: Results *************************************************
***************************************************************************

************************************************************************
******* Sub section: Main treatments ***********************************
************************************************************************

*********************************************************************
***** Table 2. Summary statistics of variables (lab experiment) *****
*********************************************************************

tabstat Seeking Age Gender German Exp, by (TT) stat(N mean sd min max) columns(statistics)


*********************************************************************
***** Figure 1. Average seeking behavior across the categories. *****  
***** The illustrated confidence intervals are calculated at*********
***** 95% level. ****************************************************
*********************************************************************
cibar Seeking, over(difficulty) level(95) barc(gs1 gs4 gs12) ciopts(ylabel(0 (.1) .6))

***** T-tests seeking behavior across categories ********************

** Easy vs. Moderate
ttest Seeking if (difficulty==1|difficulty==2), by (difficulty)

** Moderate vs. Tough
ttest Seeking if (difficulty==2|difficulty==3), by (difficulty)


*********************************************************************
***** Figure 2. Average seeking behavior across the three main ****** 
***** treatment groups for the lab experiment. The illustrated ******
***** confidence intervals are calculated at 95% level. *************
*********************************************************************

cibar Seeking, over(TT) level(95) barc(gs1 gs4 gs12) ciopts(ylabel(0 (.1) .6))


***** T-tests seeking behavior across treatments ********************

** FULLCOSTS vs. SOCIAL COSTS
ttest Seeking if (TT==1|TT==2), by (TT)


** FULLCOSTS vs. NOCOSTS
ttest Seeking if (TT==1|TT==3), by (TT)

** SOCIALCOSTS vs. NOCOSTS
ttest Seeking if (TT==2|TT==3), by (TT)


*********************************************************************
***** Table 3. Odds ratio for the main treatment effects (Lab *******
***** Experiment) ***************************************************
*********************************************************************

eststo clear
eststo:logistic Seeking i.b1.TT , or cl(uid)
esttab using T3.rtf, eform replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo:logistic Seeking i.b1.TT Gender, cl(uid)
esttab using T3.rtf, eform replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo:logistic Seeking i.b1.TT Gender Age, cl(uid)
esttab using T3.rtf, eform replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo:logistic Seeking i.b1.TT Gender Age German, cl(uid)
esttab using T3.rtf, eform replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo:logistic Seeking i.b1.TT Gender Age German i.Exp, cl(uid)
esttab using T3.rtf, eform replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap

*********************************************************************
***** Table C1. Logit estimations for the main treatment ************ 
***** effects (Lab Experiment) **************************************
*********************************************************************

eststo clear
eststo:logit Seeking i.b1.TT , cl(uid)
esttab using TCC1.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo:logit Seeking i.b1.TT Gender, cl(uid)
esttab using TC1.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo:logit Seeking i.b1.TT Gender Age, cl(uid)
esttab using TC1.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo:logit Seeking i.b1.TT Gender Age German, cl(uid)
esttab using TC1.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo:logit Seeking i.b1.TT Gender Age German i.Exp, cl(uid)
esttab using TC1.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap


** Treatment differences in stage-one payoffs
preserve
collapse Payoff1perquestion TT, by(uid)

reg Payoff1perquestion i.b1.TT, robust
reg Payoff1perquestion i.b2.TT, robust

restore



************************************************************************
******* Sub section: Heterogeneous effects *****************************
************************************************************************

*********************************************************************
***** Figure 3. Average seeking behavior for low/high image concern *
***** participants across the three main treatment groups in the ****
***** lab experiment. The illustrated confidence intervals are ******
***** calculated at 95% level. **************************************
*********************************************************************

cibar Seeking, over(TT SocImage) level(95) barc(gs1 gs6 gs12) ciopts(ylabel(0 (.1) .6))


*********************************************************************
***** Table 4. Odds ratio for the main treatment effects across *****
***** image concern and gender subsamples (Lab Experiment) **********
*********************************************************************

eststo clear
eststo: logistic Seeking i.b1.TT Gender Age German i.Exp if SocImage==2 , cl(uid)
esttab using T4.rtf, eform replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap

eststo: logistic Seeking i.b1.TT Gender Age German i.Exp if SocImage==1 , cl(uid)
esttab using T4.rtf, eform replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap


*Gender split sample
eststo: logistic Seeking i.b1.TT Age German i.Exp if Gender==2 , cl(uid)
esttab using T4.rtf, eform replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap

eststo: logistic Seeking i.b1.TT Age German i.Exp if Gender==1 , cl(uid)
esttab using T4.rtf, eform replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap

*********************************************************************
***** Table C2. Logit estimations for our split sample analyses *****
*********************************************************************

*Social image split sample
eststo clear
eststo: logit Seeking i.b1.TT Gender Age German i.Exp if SocImage==2 , cl(uid)
esttab using TC2.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap

eststo: logit Seeking i.b1.TT Gender Age German i.Exp if SocImage==1 , cl(uid)
esttab using TC2.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
*Gender Split sample
eststo:logit Seeking i.b1.TT Age German i.Exp if Gender==2 , cl(uid)
esttab using TC2.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo:logit Seeking i.b1.TT Age German i.Exp if Gender==1 , cl(uid)
esttab using TC2.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap


*********************************************************************
***** Figure 4. Average seeking behavior for female/male ************
***** participants across the three main treatment groups in the ****
***** lab experiment. The illustrated confidence intervals are ******
***** calculated at 95% level. **************************************
*********************************************************************

cibar Seeking, over(TT Gender) level(95) barc(gs1 gs6 gs12) ciopts(ylabel(0 (.1) .6))


************************************************************************
******* Sub section: Alternative specifications - **********************
******* individual-level seeking behavior ******************************
************************************************************************

**** Mann-Whitney-U-Tests *******************************************

preserve
collapse TT Seeking, by(uid)

** SOCIALCOSTS vs. FULLCOSTS

ranksum Seeking if (TT==1|TT==2), by(TT)


** NOCOSTS vs. SOCIALCOSTS

ranksum Seeking if (TT==2|TT==3), by(TT)

restore

**** Poisson and negative binomial estimations **********************


** Mean and standard deviation of individual number of seeking 

sum SeekNum

*********************************************************************
***** Table C3. Replication of Table 3 with Poisson specification *** 
*********************************************************************


preserve
collapse SeekNum TT Gender Age German Exp, by(uid)

eststo clear
eststo: poisson SeekNum i.b1.TT , robust
esttab using TC3.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo: poisson SeekNum i.b1.TT Gender, robust
esttab using TC3.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo: poisson SeekNum i.b1.TT Gender	 Age, robust
esttab using TC3.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo: poisson SeekNum i.b1.TT Gender	 Age German, robust
esttab using TC3.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo: poisson SeekNum i.b1.TT Gender	 Age German i.Exp, robust
esttab using TC3.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap



*********************************************************************
***** Table C4. Replication of Table 3 with Negative Binomial *******
***** specification ************************************************* 
*********************************************************************


eststo clear
eststo: nbreg SeekNum i.b1.TT , robust
esttab using TC4.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo: nbreg SeekNum i.b1.TT Gender, robust
esttab using TC4.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo: nbreg SeekNum i.b1.TT Gender Age, robust
esttab using TC4.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo: nbreg SeekNum i.b1.TT Gender Age German, robust
esttab using TC4.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap
eststo: nbreg SeekNum i.b1.TT Gender Age German i.Exp, robust
esttab using TC4.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap


restore









********************************************************************************
******* Knowledge Seeking and Anonymity in Digital Work Settings ***************
******* by Mickeler, M., Khashabi, P., Kleine, M., and Kretschmer, T. **********
******* Published at the Strategic Management Journal **************************
******* Do-file for survey experiment ******************************************
******* 17/04/2023 *************************************************************
******* corresponding dta-file: SMJ_Seeking_and_Anonymity_Online_Exp.dta *******
********************************************************************************


***************************************************************************
******** Explanation of used variables ************************************
***************************************************************************

*** Variable name [explanation; outcome range]

*** age (age; age categories ranging from 18-73]
*** gender [gender; 1 "Female" 2 "Male" 3 Non-binary 4 "Prefere not to say"]
*** industry [industry of organization; categories like "Finance or Insurance" or "Wholesale trade"]
*** yearsEmployed [years of total employment; 1-50, >50] 
*** role [role in organization; categories like "Consultant" or "Upper management"]
*** Seekingbetweensubject [likelihood of seeking, data for between-subject analysis (first scenario presented to participant); 0 - 100 "extremely unlikely - extremely likely"]
*** Seeking0 [likelihood of seeking for treatment 0 "FULL COSTS", 0 - 100 "extremely unlikely - extremely likely"]
*** Seeking1 [likelihood of seeking for treatment 1 "SOCIAL COSTS", 0 - 100 "extremely unlikely - extremely likely"]
*** Seeking2 [likelihood of seeking for treatment 2 "NOCOSTS", 0 - 100 "extremely unlikely - extremely likely"]
*** SocialImage [social image concerns; 0-100 "not important at all - extremely important"]
*** Treat [treatment if data shaped long; 0 "FULL COSTS", 1 "SOCIAL COSTS", 2 "NOCOSTS"]
*** TreatBetween [treatment, data for between-subject analysis (first scenario presented to participant); 0 "FULL COSTS", 1 "SOCIAL COSTS", 2 "NOCOSTS"]
*** uid [unique identifier for each participant; unique number per participant]


*********************************************************************
***** Figure 5. Average seeking likelihood across the three *********
***** main treatment groups in the survey experiment. ***************  
***** The illustrated confidence intervals are calculated at ********
***** 95% level. ****************************************************
*********************************************************************

*** "within-subject"-data and analyses

reshape long Seeking, i(uid) j(Treat)
xtset uid Treat

cibar Seeking, over(Treat) level(95) barc(gs1 gs4 gs12) ciopts(ylabel(0 (20) 80))

***** T-tests seeking behavior across categories ********************

** FULLCOSTS vs. SOCIAL COSTS
ttest Seeking if (Treat==0|Treat==1), by(Treat)

** FULLCOSTS vs. NO COSTS
ttest Seeking if (Treat==0|Treat==2), by(Treat)

** SOCIAL COSTS vs. NO COSTS
ttest Seeking if (Treat==1|Treat==2), by(Treat)




*********************************************************************
***** Table 5. Fixed-effects and OLS estimations for the main *******
***** treatment groups in the survey experiment. ********************  
*********************************************************************

*** Fixed effects estimation (within-subject)
eststo clear
eststo: xtreg Seeking  i.Treat, fe cl(uid)
esttab using T5.rtf,  replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap


*** OLS regression for between-subject estimation (between-subject)

reshape wide

eststo: reg Seekingbetweensubject i.i.TreatBetween i.age i.gender i.yearsEmployed i.role i.industry, robust
esttab using T5.rtf, replace  stats(N  N_clust chi2, fmt(%9.0f %9.0g 3)) cells(b( fmt(3)) se(par fmt(3)) p(par(`"["' `"]"') fmt(3)))   nogap


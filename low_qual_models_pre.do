#delimit ;
set more off;
set seed 1244329;
clear;
/******************************************************************************************************
Program: low_qual_models.do
Created by: Brendan Rabideau
Created on: 4/4/16
Purpose: To run a two part model on the Optum beneficiary level data to calculate marginal effects
		 on low value procedure costs
	  
Input: beneyear_cdhp_did_saf_match
Output: 

Notes: 
Updates: 11-04-16 BR: Updated to add pweight to the twopm two account for the 1:N treat:control exact match 
		 11-09-16 BR: Added in cost variables for procs that are more or less sensitive to patient preferences
		 11-29-16 BR: Added new dependent variables (low value/all imaging and lab procs). Put the program
					  in a loop. The outcomes per loop are simple and unadjusted TPM with low-value costs,
					  total costs, lv:total cost ratio, less sensitive costs, and more sensitive costs.
******************************************************************************************************/
global varlist non_inpatient imaging laboratory;

foreach depvar in $varlist {;
	use "/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Data/CDHP/beneyear_cdhp_did_saf_match_pre.dta",clear;
	describe, simple;
	drop product2 non_inpatient_ratio;

	gen post_period=(year==2012); /*Create a variable for pre and post time period for the dif-in-dif*/
	gen postXtreatment=post_period*treatment;

	tab post_period;
	tab treatment;
	tab postXtreatment;

	/*Subset the data*/
	*sample 100000,count;

	/*Clean up dependent vars for the 2 part model. Missing values are coverage-only benes and should be set to 0*/
	gen `depvar'_lv_cost=1 if cst_lv_`depvar'>0 & cst_lv_`depvar'!=.;
	replace `depvar'_lv_cost=0 if cst_lv_`depvar'==0;
	tab `depvar'_lv_cost;
	
	gen missing_cst_`depvar'=0;
	replace missing_cst_`depvar'=1 if cst_`depvar'==.;
	tab2 cov_only missing_cst_`depvar';
		
	replace cst_less_sensitive=0 if cst_less_sensitive<0 | cst_less_sensitive==.;
	replace cst_more_sensitive=0 if cst_more_sensitive<0 | cst_more_sensitive==.;

	replace cst_lv_`depvar'=0 if cst_lv_`depvar'<0 | cst_lv_`depvar'==.;
	replace cst_`depvar'=0 if cst_`depvar'<0 | cst_`depvar'==.;


	gen `depvar'_ratio=(cst_lv_`depvar'/cst_`depvar')*10000 if cst_`depvar'!=0 & cst_`depvar'!=.;
	replace `depvar'_ratio=0 if cst_`depvar'==. | cst_`depvar'==0;
	
	gen less_sensitive_ratio=(cst_less_sensitive/cst_non_inpatient)*10000 if cst_non_inpatient!=0 & cst_non_inpatient!=.;
	replace less_sensitive_ratio=0 if cst_non_inpatient==. | cst_non_inpatient==0;
	
	gen more_sensitive_ratio=(cst_more_sensitive/cst_non_inpatient)*10000 if cst_non_inpatient!=0 & cst_non_inpatient!=.;
	replace more_sensitive_ratio=0 if cst_non_inpatient==. | cst_non_inpatient==0;

	sort treatment;
	by treatment: summarize cst_lv_`depvar' if year==2012;
	by treatment: summarize cst_`depvar' treatment if year==2012;
	by treatment: summarize `depvar'_ratio treatment if year==2012;

	by treatment: summarize cst_lv_`depvar' treatment if year==2013;
	by treatment: summarize cst_`depvar' treatment if year==2013;
	by treatment: summarize `depvar'_ratio treatment if year==2013;

	summarize cst_lv_`depvar',detail;
	summarize cst_`depvar',detail;
	summarize `depvar'_ratio,detail;
	tab cov_only;
	tab2 cov_only treatment;
	summarize cst_`depvar' if cov_only==1, detail;

	/*Formats were stripped in stat transfer - Manually apply the value labels*/
	replace d_education_level_code="High School Diploma or Lower" if d_education_level_code=="A";
	replace d_education_level_code="High School Diploma or Lower" if d_education_level_code=="B";
	replace d_education_level_code="Less than Bachelor Degree" if d_education_level_code=="C";
	replace d_education_level_code="Bachelor Degree Plus" if d_education_level_code=="D";
	replace d_education_level_code="Unknown" if d_education_level_code=="U";

	replace d_household_income_range_code="Unknown" if d_household_income_range_code=="0";
	replace d_household_income_range_code="<$40K" if d_household_income_range_code=="1";
	replace d_household_income_range_code="$40K-$49K" if d_household_income_range_code=="2";
	replace d_household_income_range_code="$50K-$59K" if d_household_income_range_code=="3";
	replace d_household_income_range_code="$60K-$74K" if d_household_income_range_code=="4";
	replace d_household_income_range_code="$75K-$99K" if d_household_income_range_code=="5";
	replace d_household_income_range_code="$100K+" if d_household_income_range_code=="6";

	replace d_race_code="Asian" if d_race_code=="A";
	replace d_race_code="Black" if d_race_code=="B";
	replace d_race_code="Hispanic" if d_race_code=="H";
	replace d_race_code="Unknown" if d_race_code=="U";
	replace d_race_code="White" if d_race_code=="W";

	replace gdr_cd="Male" if gdr_cd=="M";
	replace gdr_cd="Female" if gdr_cd=="F";

	replace product = "Point of Service or Other" if product=="POS";
	replace product = "Exclusive Provider Organization" if product=="EPO";
	replace product = "Health Maintenance Organization" if product=="HMO";
	replace product = "Preferred Provider Organization or Indemnity" if product=="PPO";
	replace product = "Point of Service or Other" if product=="OTH";
	replace product = "Preferred Provider Organization or Indemnity" if product=="IND";

	replace age_cat="18-34"	if age>=18 & age<35;
	replace age_cat="35-49"	if age>=35 & age<50;
	replace age_cat="50-64"	if age>=50 & age<65;

	/*Models require numeric vars - Use encode to make the following string vars numeric*/
	foreach var in d_household_income_range_code d_race_code gdr_cd product age_cat division {;
		tab `var';
		encode `var',generate(`var'2);
	};

	/*Get some summary stats*/	
	sort d_household_income_range_code2;
	by d_household_income_range_code2: summarize `depvar'_ratio,detail;
		
	sort d_race_code2;
	by d_race_code2: summarize `depvar'_ratio,detail;
		
	sort gdr_cd2;
	by gdr_cd2: summarize `depvar'_ratio,detail;
		
	sort product2;
	by product2: summarize `depvar'_ratio,detail;
		
	sort age_cat2;
	by age_cat2: summarize `depvar'_ratio,detail;
		
	sort division2;
	by division2: summarize `depvar'_ratio,detail;
		
		
	/*Look at the distributions of components of the dependent variable. See what happens to `depvar'_ratio as cst_`depvar' approaches 0*/
	summarize cst_lv_`depvar',detail;
	summarize cst_`depvar',detail;
	summarize cst_lv_`depvar' if cst_lv_`depvar'!=0,detail;
	summarize cst_`depvar' if cov_only!=1,detail;
	summarize cst_`depvar' if cov_only==1,detail;
	tab cov_only;

	if 0==1 {;
		twoway lfit `depvar'_ratio cst_`depvar',range(1 21621.11);
		graph export cost_graph.pdf,replace;

		regress `depvar'_ratio cst_`depvar' if cst_`depvar'<=21621.11 & cov_only!=1;
		predict p_`depvar'_ratio;
		egen cst_lv_at_0 = mean(p_`depvar'_ratio) if cst_`depvar'==0 & cov_only!=1; 
		egen cst_lv_at_1 = mean(p_`depvar'_ratio) if cst_`depvar'<=45 & cov_only!=1; /*1st percentile cost*/
		summarize cst_lv_at_0,detail;
		summarize cst_lv_at_1,detail;
		global impute_`depvar'_ratio = r(mean);
		replace `depvar'_ratio=/*${impute_`depvar'_ratio}*/0 if cst_`depvar'==0 | cst_`depvar'==.;

		egen all_cost_group = cut(cst_`depvar') if cst_`depvar'>0 & cst_`depvar'!=., group(10) label;
		egen `depvar'_ratio_group=mean(`depvar'_ratio), by(all_cost_group);
		tab2 all_cost_group `depvar'_ratio_group;
		bysort all_cost_group : gen ok=(_n==1);
		graph twoway line `depvar'_ratio_group all_cost_group if ok==1;
		graph export cost_graph1.pdf,replace;

		/*Boxcox test to determine the identity link*/
		boxcox `depvar'_ratio division2 d_household_income_range_code2 d_race_code2 gdr_cd2 product2 age_cat2 if `depvar'_ratio>0 & `depvar'_ratio!=.;

		/*Park Test. The coefficient on xbetahat at the end indicates what family should be used based.*/
		glm `depvar'_ratio division2 d_household_income_range_code2 d_race_code2 gdr_cd2 product2 age_cat2, family(gamma)link(log);
		predict xbetahat, xb;
		summarize xbetahat;
		gen exp_xbetahat=exp(xbetahat)*(-1);
		summarize exp_xbetahat;
		summarize `depvar'_ratio;
		gen test=`depvar'_ratio+exp_xbetahat;
		gen rawresid = `depvar'_ratio + exp_xbetahat;
		summarize rawresid;
		gen rawvar = rawresid^2;
		glm rawvar xbetahat, f(gamma)link(log);
	};


	/*Run the two part models*/
	/*ssc install outreg;
	ssc install estout;*/
	
	summarize `depvar'_ratio, detail;
	summarize cst_lv_`depvar',detail;
	summarize cst_`depvar',detail;
	
		/*Unadjusted OLS Regressions*/
			xi: regress `depvar'_ratio i.treatment i.post_period i.postXtreatment [pweight=c_weight];
			estadd margins, dydx(*) post;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: regress cst_lv_`depvar' i.treatment i.post_period i.postXtreatment [pweight=c_weight];
			estadd margins, dydx(*) post;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: regress cst_`depvar' i.treatment i.post_period i.postXtreatment [pweight=c_weight];
			estadd margins, dydx(*) post;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: regress cst_less_sensitive i.treatment i.post_period i.postXtreatment [pweight=c_weight];
			estadd margins, dydx(*) post;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: regress cst_more_sensitive i.treatment i.post_period i.postXtreatment [pweight=c_weight];
			estadd margins, dydx(*) post;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 

		/*Unadjusted Two Part Models*/
			xi: twopm `depvar'_ratio i.treatment i.post_period i.postXtreatment [pweight=c_weight], first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_all_prop, title(`depvar'_ratio) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: twopm cst_lv_`depvar' i.treatment i.post_period i.postXtreatment [pweight=c_weight], first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_all_lv, title(cst_lv) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: twopm cst_`depvar' i.treatment i.post_period i.postXtreatment [pweight=c_weight], first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_all_tot, title(cst_tot) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: twopm cst_less_sensitive i.treatment i.post_period i.postXtreatment [pweight=c_weight], first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_l_sens, title(l_sens) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: twopm cst_more_sensitive i.treatment i.post_period i.postXtreatment [pweight=c_weight], first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_m_sens, title(m_sens) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 

		/*Adjusted Two Part Models*/
			xi: twopm `depvar'_ratio ib1.division2 ib1.d_household_income_range_code2 ib5.d_race_code2 ib1.gdr_cd2 ib2.product2 ib1.age_cat2 ib1.charlson_count i.treatment i.post_period  i.postXtreatment [pweight=c_weight], 
				first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_all_prop_adj, title(`depvar'_ratio) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: twopm cst_lv_`depvar' ib1.division2 ib1.d_household_income_range_code2 ib5.d_race_code2 ib1.gdr_cd2 ib2.product2 ib1.age_cat2 ib1.charlson_count i.treatment i.post_period i.postXtreatment [pweight=c_weight], 
				first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_all_lv_adj, title(cst_lv) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: twopm cst_`depvar' ib1.division2 ib1.d_household_income_range_code2 ib5.d_race_code2 ib1.gdr_cd2 ib2.product2 ib1.age_cat2 ib1.charlson_count i.treatment i.post_period  i.postXtreatment [pweight=c_weight], 
				first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_all_tot_adj, title(cst_tot) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: twopm cst_less_sensitive ib1.division2 ib1.d_household_income_range_code2 ib5.d_race_code2 ib1.gdr_cd2 ib2.product2 ib1.age_cat2 ib1.charlson_count i.treatment i.post_period  i.postXtreatment [pweight=c_weight], 
				first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_l_sens_adj, title(l_sens) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: twopm cst_more_sensitive ib1.division2 ib1.d_household_income_range_code2 ib5.d_race_code2 ib1.gdr_cd2 ib2.product2 ib1.age_cat2 ib1.charlson_count i.treatment i.post_period  i.postXtreatment [pweight=c_weight], 
				first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_m_sens_adj, title(m_sens) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: twopm less_sensitive_ratio ib1.division2 ib1.d_household_income_range_code2 ib5.d_race_code2 ib1.gdr_cd2 ib2.product2 ib1.age_cat2 ib1.charlson_count i.treatment i.post_period  i.postXtreatment [pweight=c_weight], 
				first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_l_sens_prop_adj, title(l_sens_ratio) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 
			
			xi: twopm more_sensitive_ratio ib1.division2 ib1.d_household_income_range_code2 ib5.d_race_code2 ib1.gdr_cd2 ib2.product2 ib1.age_cat2 ib1.charlson_count i.treatment i.post_period  i.postXtreatment [pweight=c_weight], 
				first(probit) second(glm,family(gamma) link(log));
			estadd margins, dydx(*) post;
			estimates store tpm_m_sens_prop_adj, title(m_sens_ratio) ;
			lincom (_b[_Ipost_peri_1] + _b[_IpostXtrea_1]); 




		/*Output to Excel*/
		esttab tpm_all_prop_adj tpm_all_lv_adj tpm_all_tot_adj tpm_l_sens_adj tpm_m_sens_adj tpm_l_sens_prop_adj tpm_m_sens_prop_adj  using "/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Output/cdhp_adj_`depvar'_pre.xls", 
			cells((margins_b(star fmt(4)) p(fmt(%4.3f)) ci_l(fmt(a3)) ci_u(fmt(a3))) margins_se(par(`"("' `")"') fmt(4))) /*These are parentheses that excel can see. otherwise you can just specify par*/
			legend label varlabels(_cons Constant) mtitles("LV `depvar' Cost Ratio" "LV `depvar' Costs" "All `depvar' Costs")
			extracols(5)
			stats(N, fmt(0 3) labels(`"Observations"')) replace;
			
		esttab tpm_all_prop tpm_all_lv tpm_all_tot tpm_l_sens tpm_m_sens  using "/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Output/cdhp_`depvar'_pre.xls", 
			cells((margins_b(star fmt(4)) p(fmt(%4.3f)) ci_l(fmt(a3)) ci_u(fmt(a3))) margins_se(par(`"("' `")"') fmt(4))) /*These are parentheses that excel can see. otherwise you can just specify par*/
			legend label varlabels(_cons Constant) mtitles("LV `depvar' Cost Ratio" "LV `depvar' Costs" "All `depvar' Costs")
			extracols(5)
			stats(N, fmt(0 3) labels(`"Observations"')) replace;

};
/*	Sample esttab formatted output from http://www.jwe.cc/2012/03/stata-latex-tables-estout/

	esttab A B C using table4.tex, replace f
	label booktabs b(3) p(3) eqlabels(none) alignment(S S) collabels("\multicolumn{1}{c}{$\beta$ / SE}" "\multicolumn{1}{c}{Mfx}") 
	drop(_cons spouse*  ) 
	star(* 0.10 ** 0.05 *** 0.01) 
	cells("b(fmt(3)star) margins_b(star)" "se(fmt(3)par)") 
	refcat(age18 "\emph{Age}" male "\emph{Demographics}" educationage "\emph{Education}" employeddummy "\emph{Employment}" oowner "\emph{Housing}" hhincome_thou "\emph{Household Finances}" reduntant "\emph{Income and Expenditure Risk}" literacyscore "\emph{Behavioural Characteristics}", nolabel) ///
	stats(N, fmt(0 3) labels(`"Observations"'))
 */ 
 
		regress cst_less_sensitive i.treatment i.post_period  i.postXtreatment [pweight=c_weight];
		regress less_sensitive_ratio i.treatment i.post_period  i.postXtreatment [pweight=c_weight];
		
		regress cst_more_sensitive i.treatment i.post_period  i.postXtreatment [pweight=c_weight];
		regress more_sensitive_ratio i.treatment i.post_period  i.postXtreatment [pweight=c_weight];
			
		regress cst_less_sensitive ib1.division2 ib1.d_household_income_range_code2 ib5.d_race_code2 ib1.gdr_cd2 ib2.product2 ib1.age_cat2 ib1.charlson_count i.treatment i.post_period  i.postXtreatment [pweight=c_weight];
		regress less_sensitive_ratio ib1.division2 ib1.d_household_income_range_code2 ib5.d_race_code2 ib1.gdr_cd2 ib2.product2 ib1.age_cat2 ib1.charlson_count i.treatment i.post_period  i.postXtreatment [pweight=c_weight];
		
		regress cst_more_sensitive ib1.division2 ib1.d_household_income_range_code2 ib5.d_race_code2 ib1.gdr_cd2 ib2.product2 ib1.age_cat2 ib1.charlson_count i.treatment i.post_period  i.postXtreatment [pweight=c_weight];
		regress more_sensitive_ratio ib1.division2 ib1.d_household_income_range_code2 ib5.d_race_code2 ib1.gdr_cd2 ib2.product2 ib1.age_cat2 ib1.charlson_count i.treatment i.post_period  i.postXtreatment [pweight=c_weight];
 
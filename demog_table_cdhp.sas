%include "/sch-projects/dua-data-projects/OPTUM/rabideau/Programs/gen_summary.sas";
%include "/sch-projects/dua-data-projects/OPTUM/rabideau/Programs/propensity_matching.sas";
libname fmt "/sch-projects/dua-data-projects/OPTUM/rabideau/Documentation";
libname in "/sch-projects/dua-data-projects/OPTUM/rabideau/Data";
%let out = /schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Output;
%let cost_var = cst_lv_s; /*Specify the cost variable of interest. Total sensitive low value is cst_lv, total specific low value is cst_lv_s*/

/**************************************************************
 DATA PREP
**************************************************************/
data optum_demog (keep=Patid Fst_dt gdr_cd age age_cat product cdhp lv D_EDUCATION_LEVEL_CODE D_RACE_CODE 
					   D_HOUSEHOLD_INCOME_RANGE_CODE division cdhp_ind &cost_var. all_costs rename=(cdhp=cdhp_cd));
	set in.low_value_beneficiary (where=(year=2013 & elig_2013=1));
	if 18<=age<=34 then age_cat='18-34';
	if 35<=age<=49 then age_cat='35-49';
	if 50<=age<=64 then age_cat='50-64';
	if upcase(D_EDUCATION_LEVEL_CODE)='B' then D_EDUCATION_LEVEL_CODE='A'; /*We're combining A and B into 'High School or Less'*/
	if upcase(product)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product)='OTH' then product='POS'; /*Combine POS and Other*/
	cdhp_ind=(cdhp~='3');
run;

proc sort data=optum_demog; by Patid Fst_dt; run;

data optum_demog;
	length group $12;
	set optum_demog;
	retain group;
	by Patid;
	if first.Patid then group="No_Low_Value";
	if lv=1 then group="Low_Value";
	if last.Patid then output;
run;

/**************************************************************
 PROPENSITY SCORE MATCHING
**************************************************************/
/*Set up the dataset for the propensity score macro*/
PROC LOGISTIC DATA = optum_demog descend;
class gdr_cd age_cat product D_EDUCATION_LEVEL_CODE D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division;
MODEL cdhp_ind = gdr_cd age_cat product D_EDUCATION_LEVEL_CODE D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division
/ SELECTION = STEPWISE RISKLIMITS LACKFIT RSQUARE PARMLABEL;
 OUTPUT OUT=optum_demog_prop prob=prob ;
RUN;

%OneToManyMTCH (
 Lib=work, 					/* Library Name */
 Dataset=optum_demog_prop,	/* Data set of all patients */
 depend=cdhp_ind, 			/* Dependent variable that indicates Case or Control, Code 1 for Cases, 0 for Controls */
 SiteN=, 					/* Site/Hospital ID */
 PatientN=Patid, 			/* Patient ID */
 matches=optum_demog_match,	/* Output data set of matched pairs */
 NoContrls=1); 				/* Number of controls to match to each case */

 /**************************************************************
 COARSE EXACT MATCHING
**************************************************************/
 data optum_demog_cem;
 	set optum_demog;
run;
 /*%CEM (
	lib = work, 
	data = optum_demog_cem, 
	id = Patid, 
	treat = chdp_ind, 
	del_miss = 1,
 	match_type = ONE, 
	method = Sturges, 
	path_graph = /schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Output,
	report = on
      );*/

/**************************************************************
 INVERSE PROBABILITY WEIGHTING
**************************************************************/
PROC LOGISTIC DATA = optum_demog descend;
class gdr_cd age_cat product D_EDUCATION_LEVEL_CODE D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division;
MODEL cdhp_ind = gdr_cd age_cat product D_EDUCATION_LEVEL_CODE D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division
/ SELECTION = STEPWISE RISKLIMITS LACKFIT RSQUARE PARMLABEL;
 OUTPUT OUT=optum_demog_wt prob=prob ;
RUN;

data optum_demog_wt;
	set optum_demog_wt;
	if cdhp_ind=1 then weight=1/(prob);
	if cdhp_ind=0 then weight=1/(1-prob);
run;

/**************************************************************
 GENERATE SUMMARY 
**************************************************************/

 /*Combine the different datasets and make a var to indicate which dataset each obs comes from for the summary*/
 data optum_demog;
 	length cdhp_var $30;
 	set optum_demog (in=a)
		optum_demog_match (in=b)
		optum_demog_wt (in=c);
	if a & cdhp_ind=1 then cdhp_var="Original_CDHP";
	else if a & cdhp_ind=0 then cdhp_var="Original_Non_CDHP";
	else if b & cdhp_ind=1 then cdhp_var="Prop_Matched_CDHP";
	else if b & cdhp_ind=0 then cdhp_var="Prop_Matched_Non_CDHP";
	else if c & cdhp_ind=1 then cdhp_var="Prop_Weighted_CDHP";
	else if c & cdhp_ind=0 then cdhp_var="Prop_Weighted_Non_CDHP";
	if a | b then weight=1; /*Set weights to 1 if it's not part of the weighted dataset */
run;

proc freq data=optum_demog;
	tables cdhp_var;
run;

%summary(ds=optum_demog,cutoff=100,byvar=cdhp_var,subset=,formats=Y,weight=Y,weightvar=weight);

/**************************************************************
 FORMATTING
**************************************************************/
proc format;
	value $D_EDUCATION_LEVEL_CODE
	'A'=	'High School Diploma or Lower'/*'Less than 12th Grade'*/ /*We're combining A and B into 'High School or Less'*/
	/*'B'=	'High School Diploma'*/
	'C'=	'Less than Bachelor Degree'
	'D'=	'Bachelor Degree Plus'
	'U'=	'Unknown';

	value $D_HOUSEHOLD_INCOME_RANGE_CODE
	'0'=	'Unknown'
	'1'=	'<$40K'
	'2'=	'$40K-$49K'
	'3'=	'$50K-$59K'
	'4'=	'$60K-$74K'
	'5'=	'$75K-$99K'
	'6'=	'$100K+';

	value $D_RACE_CODE
	'A'=	'Asian'
	'B'=	'Black'
	'H'=	'Hispanic'
	'U'=	'Unknown'
	'W'=	'White';

	value $gdr_cd
	'M'=	'Male'
	'F'=	'Female';
	/*'U'=	'U=UNKNOWN';*/

	value $cdhp
	'1'=	'HRA'
	'2'=	'HSA'
	'3'=	'No HRA / HSA';

	value $product
	'POS'=	'Point of Service or Other' /*Combine POS and Other*/
	'EPO'=	'Exclusive Provider Organization'
	'HMO'=	'Health Maintenance Organization'
	'PPO'=	'Preferred Provider Organization or Indemnity'/*'PPO=PREFERRED PROVIDER ORGANIZATION'*/ /*Combine PPO and Indemnity*/
	/*'OTH'=	'Other'*/
	/*'IND'=	'IND=INDEMNITY'*/;
run;

/**************************************************************
 CALCULATE COSTS BY VARIABLE
**************************************************************/

/*This macro determines which dataset to read in for analysis - original, propensity matched, or propensity weighted*/
%macro propensity(ds,name);
	data optum_demog&ds.;
		set optum_demog&ds.;
		if 18<=age<=34 then age_cat='18-34';
		if 35<=age<=49 then age_cat='35-49';
		if 50<=age<=64 then age_cat='50-64';
		if upcase(D_EDUCATION_LEVEL_CODE)='B' then D_EDUCATION_LEVEL_CODE='A';
		if upcase(product)='IND' then product='PPO';
		if upcase(product)='OTH' then product='POS';
		if all_costs~=. & all_costs~=0 then lv_cost_ratio = &cost_var./all_costs;
	run;

	/*This macro makes the ttest output a single line dataset with means and ttest*/
	%macro ttest(byvar);

		proc sort data=optum_demog&ds.; by &byvar.; run;

		ods listing close; /*For now do not print output to listing - .lst file is too cluttered*/
		ods output  "Statistics" = stats
		 			"T-Tests" = ttests;
		proc ttest data=optum_demog&ds.;
			class cdhp_ind;
			by &byvar.;
			var &cost_var. all_costs lv_cost_ratio;
		run;
		ods output close;
		ods listing;

		/*proc print data=stats; run;	You can include these if somethings going wrong - for now suppress output
		proc print data=ttests; run;*/

		data stats (keep = &byvar. variable class cdhp non_cdhp);
			set stats(rename=(mean=avg));
			if trim(left(class)) = '0' then non_cdhp = avg;
			if trim(left(class)) = '1' then cdhp = avg; 
		run;

		 data non_cdhp(drop = class cdhp)
		 	  cdhp (drop = class non_cdhp);
		 	set stats;
		 	if trim(left(class)) = '0' then output non_cdhp;
		 	if trim(left(class)) = '1' then output cdhp;
		 run;

		 proc sort data = non_cdhp; by &byvar. variable; run;
		 proc sort data = cdhp; by &byvar. variable; run;

		 data stats_final;
		 	merge non_cdhp
			      cdhp;
		 	by &byvar. variable;
		 run;

		 /*proc print data=stats_final; run;*/

		 data ttest (keep=&byvar. variable probt rename=(probt=Significance));
		 	set ttests;
			if trim(left(Method))="Pooled";
		run;

		/*proc print data=ttest; run;*/
		proc sort data=ttest; by &byvar. variable; run;

		data t_&byvar.;
			length variable $ 50;
			merge stats_final
				  ttest;
			by &byvar. variable;
		run;

		/*proc print data=t_&byvar.; run;*/

		data a_&byvar. (rename=(cdhp=&cost_var._chdp non_cdhp=&cost_var._non_cdhp Significance=&cost_var._Significance))
		     b_&byvar. (rename=(cdhp=all_costs_chdp non_cdhp=all_costs_non_cdhp Significance=all_costs_Significance))
		     c_&byvar. (rename=(cdhp=lv_cost_ratio_chdp non_cdhp=lv_cost_ratio_non_cdhp Significance=lv_cost_ratio_Significance));
			set t_&byvar.;
			if trim(left(variable))="&cost_var." then output a_&byvar.;
			else if trim(left(variable))="all_costs" then output b_&byvar.;
			else if trim(left(variable))="lv_cost_ratio" then output c_&byvar.;
		run;
		
		proc sort data=a_&byvar.; by &byvar.; run;
		proc sort data=b_&byvar.; by &byvar.; run;
		proc sort data=c_&byvar.; by &byvar.; run;

		data f_&byvar. (drop=Variable &byvar. rename=(Var=Variable));
			length Var $32;
			merge a_&byvar.
			      b_&byvar.
			      c_&byvar.;
			by &byvar.;
			format &byvar. $&byvar..;
			Value=VVALUE(&byvar.);
			Var="&byvar.";
		run;
		
		proc print data=f_&byvar.; run;
	%mend;
	%ttest(byvar=D_HOUSEHOLD_INCOME_RANGE_CODE);
	%ttest(byvar=D_RACE_CODE);
	%ttest(byvar=D_EDUCATION_LEVEL_CODE);
	%ttest(byvar=gdr_cd);
	%ttest(byvar=product);
	%ttest(byvar=age_cat);
	%ttest(byvar=division);

	data ttests_&name.;
		length Value $50;
		set f_:;
	run;
%mend;
%propensity(ds=,name=orignal);
%propensity(ds=_match,name=match);
%propensity(ds=_wt,name=wt);


/*Output the relevant datasets*/
ods tagsets.excelxp file="&out./cdhp_demog.xml" style=sansPrinter;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Demographics by CHDP" frozen_headers='yes');
proc print data=bycategorical; run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Costs by CDHP-Orignal" frozen_headers='yes');
proc print data=ttests_orignal; run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Costs by CDHP-Prop Matched" frozen_headers='yes');
proc print data=ttests_match; run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Costs by CDHP-Prop Weighted" frozen_headers='yes');
proc print data=ttests_wt; run;
ods tagsets.excelxp close;


proc datasets library=work kill;
run;
quit;







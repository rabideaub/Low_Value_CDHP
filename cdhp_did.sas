libname fmt "/sch-projects/dua-data-projects/OPTUM/rabideau/Documentation";
libname in "/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Data/CDHP";
libname raw "/sch-data-library/dua-data/OPTUM/Original_data/Data";

%include "/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Programs/charlson_ruleout.sas";
%include "/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Programs/charlson_calc.sas";

%let out = /schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Output;
%let model_vars=D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division age_cat gdr_cd product;

/**************************************************************
 DIFFERENCE-IN-DIFFERENCES ANALYSIS
**************************************************************/
%macro get_means(var);
	data optum_demog;
		set in.beneyear_cdhp_did_saf_match; /*This file is output from demog_table_did.sas*/

		if cst_lv_&var.<0 | cst_lv_&var.=. then cst_lv_&var.=0 ;
		if cst_&var.<0 | cst_&var.=. then cst_&var.=0 ;
		if cst_&var.~=0 & cst_&var.~=. then &var._ratio=(cst_lv_&var./cst_&var.)*10000 ;
		if cst_&var.=. | cst_&var.=0 then &var._ratio=0 ;
		post=(year=2013);
	run;

	title "TTest for total &var. in pre-period";
	proc ttest data=optum_demog;
		class treatment;
		var cst_&var.;
		where post=0;
		weight c_weight;
	run;

	title "TTest for total &var. in post-period";
	proc ttest data=optum_demog;
		class treatment;
		var cst_&var.;
		where post=1;
		weight c_weight;
	run;

	title "TTest for LV &var. in pre-period";
	proc ttest data=optum_demog;
		class treatment;
		var cst_lv_&var.;
		where post=0;
		weight c_weight;
	run;

	title "TTest for LV &var. in post-period";
	proc ttest data=optum_demog;
		class treatment;
		var cst_lv_&var.;
		where post=1;
		weight c_weight;
	run;

	title "TTest for &var. ratio in pre-period";
	proc ttest data=optum_demog;
		class treatment;
		var &var._ratio;
		where post=0;
		weight c_weight;
	run;

	title "TTest for &var. ratio in post-period";
	proc ttest data=optum_demog;
		class treatment;
		var &var._ratio;
		where post=1;
		weight c_weight;
	run;

	proc freq data=optum_demog;
		tables year*treatment post treatment treatment*post;
	run;

	proc means data=optum_demog;
		class post treatment;
		var cst_lv_&var. cst_&var. &var._ratio cst_less_sensitive cst_more_sensitive;
		weight c_weight;
		output out=did;
	run;

	data did (keep=post treatment cst_lv_&var. cst_&var. &var._ratio);
		set did;
		if _TYPE_=3 & trim(left(_STAT_))="MEAN";
	run;

	proc print data=did; run;

	proc sort data=did; by treatment; run;

	proc transpose data=did out=cst_lv_&var.;
		by treatment;
		id post;
		var cst_lv_&var.;
	run;
	proc transpose data=did out=cst_&var.;
		by treatment;
		id post;
		var cst_&var.;
	run;
	proc transpose data=did out=&var._ratio;
		by treatment;
		id post;
		var &var._ratio;
	run;

	data cst_lv_&var.;
		length treat $10;
		set cst_lv_&var. (drop=_NAME_ rename=(_0=_2012 _1=_2013));
		if treatment=0 then treat="Non-CDHP";
		else if treatment=1 then treat="CDHP";
		Difference=_2013-_2012;
		drop treatment;
	run;
	data cst_&var.;
		length treat $10;
		set cst_&var. (drop=_NAME_ rename=(_0=_2012 _1=_2013));
		if treatment=0 then treat="Non-CDHP";
		else if treatment=1 then treat="CDHP";
		Difference=_2013-_2012;
		drop treatment;
	run;
	data &var._ratio;
		length treat $10;
		set &var._ratio (drop=_NAME_ rename=(_0=_2012 _1=_2013));
		if treatment=0 then treat="Non-CDHP";
		else if treatment=1 then treat="CDHP";
		Difference=_2013-_2012;
		drop treatment;
	run;

	proc print data=cst_lv_&var.; run;
	proc print data=cst_&var.; run;
	proc print data=&var._ratio; run;
%mend;
*%get_means(var=non_inpatient);
*%get_means(var=imaging);
*%get_means(var=laboratory);


	data optum_demog;
		set in.beneyear_cdhp_did_saf_match; /*This file is output from demog_table_did.sas*/

		if cst_less_sensitive<0 | cst_less_sensitive=. then cst_sensitive=0 ;
		if cst_non_inpatient~=0 & cst_less_sensitive~=. then less_sensitive_ratio=(cst_less_sensitive/cst_non_inpatient)*10000 ;
		if cst_less_sensitive=. | cst_non_inpatient=0 then less_sensitive_ratio=0 ;

		if cst_more_sensitive<0 | cst_more_sensitive=. then cst_sensitive=0 ;
		if cst_non_inpatient~=0 & cst_more_sensitive~=. then more_sensitive_ratio=(cst_more_sensitive/cst_non_inpatient)*10000 ;
		if cst_more_sensitive=. | cst_non_inpatient=0 then more_sensitive_ratio=0 ;

		post=(year=2013);
	run;

title "TTest for total less sensitive in pre-period";
	proc ttest data=optum_demog;
		class treatment;
		var cst_less_sensitive;
		where post=0;
		weight c_weight;
	run;

	title "TTest for total less_sensitive in post-period";
	proc ttest data=optum_demog;
		class treatment;
		var cst_less_sensitive;
		where post=1;
		weight c_weight;
	run;

	title "TTest for less_sensitive ratio in pre-period";
	proc ttest data=optum_demog;
		class treatment;
		var less_sensitive_ratio;
		where post=0;
		weight c_weight;
	run;

	title "TTest for less_sensitive ratio in post-period";
	proc ttest data=optum_demog;
		class treatment;
		var less_sensitive_ratio;
		where post=1;
		weight c_weight;
	run;





title "TTest for total more sensitive in pre-period";
	proc ttest data=optum_demog;
		class treatment;
		var cst_more_sensitive;
		where post=0;
		weight c_weight;
	run;

	title "TTest for total more_sensitive in post-period";
	proc ttest data=optum_demog;
		class treatment;
		var cst_more_sensitive;
		where post=1;
		weight c_weight;
	run;

	title "TTest for more_sensitive ratio in pre-period";
	proc ttest data=optum_demog;
		class treatment;
		var more_sensitive_ratio;
		where post=0;
		weight c_weight;
	run;

	title "TTest for more_sensitive ratio in post-period";
	proc ttest data=optum_demog;
		class treatment;
		var more_sensitive_ratio;
		where post=1;
		weight c_weight;
	run;

%macro out1;
/*********************
PRINT CHECK
*********************/
/*Output a sample of each of the datasets to an excel workbook*/
ods tagsets.excelxp file="&out./cdhp_means.xml" style=sansPrinter;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="cst_lv_non_inpatient" frozen_headers='yes');
proc print data=cst_lv_non_inpatient;
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="cst_non_inpatient" frozen_headers='yes');
proc print data=cst_non_inpatient;
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="non_inpatient_ratio" frozen_headers='yes');
proc print data=non_inpatient_ratio;
run;

ods tagsets.excelxp options(absolute_column_width='20' sheet_name="cst_lv_imaging" frozen_headers='yes');
proc print data=cst_lv_imaging;
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="cst_imaging" frozen_headers='yes');
proc print data=cst_imaging;
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="imaging_ratio" frozen_headers='yes');
proc print data=imaging_ratio;
run;

ods tagsets.excelxp options(absolute_column_width='20' sheet_name="cst_lv_laboratory" frozen_headers='yes');
proc print data=cst_lv_laboratory;
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="cst_laboratory" frozen_headers='yes');
proc print data=cst_laboratory;
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="laboratory_ratio" frozen_headers='yes');
proc print data=laboratory_ratio;
run;
ods tagsets.excelxp close;
/*********************
CHECK END
*********************/
%macro;



%macro out;
/*Unadjusted DiD*/
proc glm data=optum_demog;
	ods output ParameterEstimates = parm_cst_lv_non_inpatient;
	model cst_lv_non_inpatient = treatment post treatment*post;
run;
	ods output close;

proc glm data=optum_demog;
ods output ParameterEstimates = parm_cst_non_inpatient;
	model cst_non_inpatient = treatment post treatment*post;
run;
ods output close;

proc glm data=optum_demog;
ods output ParameterEstimates = parm_non_inpatient_ratio;
	model non_inpatient_ratio = treatment post treatment*post;
run;
ods output close;

proc glm data=optum_demog;
ods output ParameterEstimates = parm_non_inpatient_ratio;
	model cst_less_sensitive = treatment post treatment*post;
run;
ods output close;

proc glm data=optum_demog;
ods output ParameterEstimates = parm_non_inpatient_ratio;
	model cst_more_sensitive = treatment post treatment*post;
run;
ods output close;

/*Adjusted DiD*/
proc glm data=optum_demog;
ods output ParameterEstimates = parm_cst_lv_non_inpatient2;
	class &model_vars.;
	model cst_lv_non_inpatient = treatment post treatment*post charlson_count &model_vars. / solution;
run;
ods output close;

proc glm data=optum_demog;
ods output ParameterEstimates = parm_cst_non_inpatient2;
	class &model_vars.;
	model cst_non_inpatient = treatment post treatment*post charlson_count &model_vars. / solution;
run;
ods output close;

proc glm data=optum_demog;
ods output ParameterEstimates = parm_non_inpatient_ratio2;
	class &model_vars.;
	model non_inpatient_ratio = treatment post treatment*post charlson_count &model_vars. / solution;
run;
ods output close;

data output_table (keep=Dependent parameter Value estimate stderr probt rename=(Dependent=Outcome parameter=Covariate Estimate=Coefficient Probt=Significance));
	length dependent $32 parameter value $100;
	set parm_:;
	Value=trim(left(substr(parameter,(index(trim(left(paramater)),' ')+1))));
	parameter=substr(parameter,1,(index(parameter,' ')-1));
run;

proc glm data=optum_demog;
ods output ParameterEstimates = parm_non_inpatient_ratio2;
	class &model_vars.;
	model cst_less_sensitive = treatment post treatment*post charlson_count &model_vars. / solution;
run;
ods output close;

data output_table (keep=Dependent parameter Value estimate stderr probt rename=(Dependent=Outcome parameter=Covariate Estimate=Coefficient Probt=Significance));
	length dependent $32 parameter value $100;
	set parm_:;
	Value=trim(left(substr(parameter,(index(trim(left(paramater)),' ')+1))));
	parameter=substr(parameter,1,(index(parameter,' ')-1));
run;

proc glm data=optum_demog;
ods output ParameterEstimates = parm_non_inpatient_ratio2;
	class &model_vars.;
	model cst_more_sensitive = treatment post treatment*post charlson_count &model_vars. / solution;
run;
ods output close;

data output_table (keep=Dependent parameter Value estimate stderr probt rename=(Dependent=Outcome parameter=Covariate Estimate=Coefficient Probt=Significance));
	length dependent $32 parameter value $100;
	set parm_:;
	Value=trim(left(substr(parameter,(index(trim(left(paramater)),' ')+1))));
	parameter=substr(parameter,1,(index(parameter,' ')-1));
run;



%macro out;
/*Quantile Regression*/

/*QR does not work at the median if the depvar is zero-inflated past the median. Check depvars here*/
proc univariate data=optum_demog;
	class treatment;
	var cst_lv_non_inpatient cst_non_inpatient non_inpatient_ratio tot_lv_s tot_musculo_s tot_procs;
run;

/*Unadjusted DiD*/
proc quantreg data=optum_demog;
	ods output ParameterEstimates = quant_cst_lv_non_inpatient;
	model cst_lv_non_inpatient = treatment post treatment*post/ quantile=.5;
	where tot_lv_s>0;
run;
	ods output close;

title "Look at QR ODS Output";
proc print data=quant_cst_lv_non_inpatient; run;
title;

proc quantreg data=optum_demog;
ods output ParameterEstimates = quant_cst_non_inpatient;
	model cst_non_inpatient = treatment post treatment*post/ quantile=.5;
run;
ods output close;

proc quantreg data=optum_demog;
ods output ParameterEstimates = quant_non_inpatient_ratio;
	model non_inpatient_ratio = treatment post treatment*post/ quantile=.5;
	where tot_lv_s>0;
run;
ods output close;

proc quantreg data=optum_demog;
ods output ParameterEstimates = quant_tot_lv_s;
	model tot_lv_s = treatment post treatment*post/ quantile=.5;
	where tot_lv_s>0;
run;
ods output close;

proc quantreg data=optum_demog;
ods output ParameterEstimates = quant_tot_musculo_s;
	model tot_musculo_s = treatment post treatment*post/ quantile=.5;
	where tot_musculo_s>0;
run;
ods output close;

proc quantreg data=optum_demog;
ods output ParameterEstimates = quant_tot_procs;
	model tot_procs = treatment post treatment*post/ quantile=.5;
run;
ods output close;



/*Adjusted DiD*/
proc quantreg data=optum_demog;
ods output ParameterEstimates = quant_cst_lv_non_inpatient2;
	class &model_vars.;
	model cst_lv_non_inpatient = treatment post treatment*post charlson_count &model_vars. / quantile=.5;
	where tot_lv_s>0;
run;
ods output close;

title "Look at QR ODS Output2";
proc print data=quant_cst_lv_non_inpatient2; run;
title;

proc quantreg data=optum_demog;
ods output ParameterEstimates = quant_cst_non_inpatient2;
	class &model_vars.;
	model cst_non_inpatient = treatment post treatment*post charlson_count &model_vars. / quantile=.5;
run;
ods output close;

proc quantreg data=optum_demog;
ods output ParameterEstimates = quant_non_inpatient_ratio2;
	class &model_vars.;
	model non_inpatient_ratio = treatment post treatment*post charlson_count &model_vars. / quantile=.5;
	where tot_lv_s>0;
run;
ods output close;

proc quantreg data=optum_demog;
ods output ParameterEstimates = quant_tot_lv_s2;
	class &model_vars.;
	model tot_lv_s = treatment post treatment*post charlson_count &model_vars. / quantile=.5;
run;
ods output close;

proc quantreg data=optum_demog;
ods output ParameterEstimates = quant_tot_musculo_s2;
	class &model_vars.;
	model tot_musculo_s = treatment post treatment*post charlson_count &model_vars. / quantile=.5;
run;
ods output close;

proc quantreg data=optum_demog;
ods output ParameterEstimates = quant_tot_procs2;
	class &model_vars.;
	model tot_procs = treatment post treatment*post charlson_count &model_vars. / quantile=.5;
run;
ods output close;

data output_table_quant (keep=Dependent parameter level1 estimate stderr probt 
						 rename=(Dependent=Outcome parameter=Covariate level1=Value Estimate=Coefficient Probt=Significance));
	length dependent $32 level1 $100;
	set quant_:;
run;
%mend;

/*********************
PRINT CHECK
*********************/
/*Output a sample of each of the datasets to an excel workbook*/
ods tagsets.excelxp file="&out./cdhp_did2_no_ro.xml" style=sansPrinter;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Parameters" frozen_headers='yes');
proc print data=output_table;
run;
/*
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="QR Parameters" frozen_headers='yes');
proc print data=output_table_quant;
run;*/
ods tagsets.excelxp close;
/*********************
CHECK END
*********************/
%mend;

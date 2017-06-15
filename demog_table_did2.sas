%include "/sch-projects/dua-data-projects/OPTUM/rabideau/Programs/gen_summary.sas";
%include "/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Programs/charlson_ruleout.sas";
%include "/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Programs/charlson_calc.sas";
%include "/sch-projects/dua-data-projects/OPTUM/rabideau/Programs/CDHP/propensity_matching.sas";
libname fmt "/sch-projects/dua-data-projects/OPTUM/rabideau/Documentation";
libname in "/sch-projects/dua-data-projects/OPTUM/rabideau/Data/CDHP";
libname in2 "/sch-projects/dua-data-projects/OPTUM/rabideau/Data";
libname raw "/sch-data-library/dua-data/OPTUM/Original_data/Data";
%let out = /schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Output;

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
 DATA PREP
**************************************************************/

/*Test to see if people are in all 3 years of data*/
proc sort data=in.low_value_beneficiary out=test; by patid year; run;
data test;
	set test;
	by patid;
	retain count;
	if first.patid then count=0;
	count+1;
run;

proc freq data=test;
	tables year count;
run;

data optum_demog (keep=Patid Fst_dt gdr_cd age age_cat product cdhp lv D_EDUCATION_LEVEL_CODE D_RACE_CODE 
					   D_HOUSEHOLD_INCOME_RANGE_CODE division cdhp_ind all_costs year cov_only
					   cst_neuro_s cst_diagnostic_s cst_preoperative_s cst_musculo_s cst_cardio_s cst_lv_s cst_imaging_op
					   cst_non_inpatient_op cst_laboratory_op cst_lv_imaging cst_lv_non_inpatient cst_lv_laboratory
					   cst_less_sensitive cst_more_sensitive
					   rename=(cdhp=cdhp_cd cst_imaging_op=cst_imaging cst_non_inpatient_op=cst_non_inpatient cst_laboratory_op=cst_laboratory));

	/*Benes must be enrolled for 2 years, since we want the 18-64 population they must not be 17 in 2012 or 65 in 2013*/
	set in.low_value_beneficiary (where=((year=2012 & 18<=age<=63) | (year=2013 & 19<=age<=64))); 

	if 18<=age<=34 then age_cat='18-34';
	if 35<=age<=49 then age_cat='35-49';
	if 50<=age<=64 then age_cat='50-64';
	if upcase(D_EDUCATION_LEVEL_CODE)='B' then D_EDUCATION_LEVEL_CODE='A'; /*We're combining A and B into 'High School or Less'*/
	if upcase(product)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product)='OTH' then product='POS'; /*Combine POS and Other*/
	cdhp_ind=(cdhp~='3');
	*if upcase(product) in('PPO','POS');

	if gdr_cd~="" & age_cat~="" & product~="" & D_RACE_CODE~="" & D_HOUSEHOLD_INCOME_RANGE_CODE~="" & division~="" & division~="UNKNOWN";
run;

proc freq data=optum_demog;
	tables year product*year;
run;

proc means data=optum_demog;
	var cst_non_inpatient cst_lv_non_inpatient cst_imaging cst_lv_imaging cst_laboratory cst_lv_laboratory 
		cst_less_sensitive cst_more_sensitive;
run;

proc freq data=optum_demog;
	tables cov_only;
run;

/********************************************************************************************
READ IN AND COLLAPSE THE MEMBERSHIP FILE - CREATE MONTHLY ELIGIBILITY FLAGS
********************************************************************************************/
/*Select Difference-in-Differences sample. Non-CDHP in 2011 and 2012 for all, non-CDHP in 2013 for control, CDHP 2013 for treatment*/

/*Capture all eligible beneficiaries for each year - even those without claims*/
/*Benes have multiple observations for different enrollment periods. Dynamically flatten with arrays by patid*/
proc sort data=raw.ses_mbr_detail out=elig; by patid eligeff; run;

data elig;
	set elig;
	if patid ~in('000000000','0','');
	retain num;
	by patid;
	if first.patid then num=0;
	num+1;
run;

proc print data=elig (obs=20);
run;

proc freq data=elig;
	tables num / out=count_ds;
run;

proc print data=count_ds;
run;

/*Store the maximum number of obs a single bene has*/
data _null_;
	set count_ds end=last;
	if last then call symput('N',trim(left(num)));
run;

%put &N.;

/*Make an eligibility start and end date for each period of enrollment that a bene has*/
data elig_array;
	set elig;
	retain eligeff1-eligeff&N. eligend1-eligend&N. cdhp1-cdhp&N. product1-product&N.;
	array eligb {&N.} eligeff1-eligeff&N.;
	array elige {&N.} eligend1-eligend&N.;
	array cdhp_cd {&N.} cdhp1-cdhp&N.;
	array prod{&N.} $ product1-product&N.;
	by patid;
	if first.patid then do;
		do i=1 to &N.;
			eligb[i]=.;
			elige[i]=.;
			cdhp_cd[i]='';
			prod[i]='';
		end;
	end;

	/*Collapse all of the eligibility periods into a single observation*/
	do i=1 to &N.;
		if i=num then do;
			eligb[num]=eligeff;
			elige[num]=eligend;
			cdhp_cd[num]=put(cdhp,1.);
			prod[num]=product;
		end;
	end;

	drop i;
	if last.patid then output;
	format eligeff: eligend: MMDDYY8.;
run;

proc print data=elig_array (obs=20);
	where num>1;
run;

data elig_flags;
	set elig_array;
	array month2011{12} Y2011_1-Y2011_12;
	array month2012{12} Y2012_1-Y2012_12;
	array month2013{12} Y2013_1-Y2013_12;

	array product2011{12} $ p2011_1-p2011_12;
	array product2012{12} $ p2012_1-p2012_12;
	array product2013{12} $ p2013_1-p2013_12;

	array cdhp_cd{&N.} $ cdhp1-cdhp&N.;
	array prod{&N.} $ product1-product&N.;

		/*Make Y2012_1 to Y2012_12 flags for each month indicating which CDHP code is in effect each month (if any)*/
		%macro loop;
			%do yr=2011 %to 2013;
				%do j=1 %to &N.;
					do i=0 to 11;
						z=i+1;
						date=intnx('month',"01jan&yr."d,i);
						if eligeff&j.~=. & ((eligeff&j.<=date<=eligend&j.) | (eligeff&j.<=date & eligend=.)) 
						then month&yr.[z]=cdhp_cd[&j.];

						if eligeff&j.~=. & ((eligeff&j.<=date<=eligend&j.) | (eligeff&j.<=date & eligend=.)) 
						then product&yr.[z]=prod[&j.];
					end;
				%end;
			%end;
		%mend;
		%loop;

	/*Identify the product people had at the end of each year or the end of their eligibility period for that year*/
	%macro loop_product;
		%do i=1 %to 12;
			if p2011_&i.~='' then product_2011=p2011_&i.;
			if p2012_&i.~='' then product_2012=p2012_&i.;
			if p2013_&i.~='' then product_2013=p2013_&i.;
		%end;
	%mend;
	%loop_product;

	/*Flag people who are in a non-CDHP in all of 2012 and identify those who stay non-CDHP and those who switch to CDHP for all 2013*/
	control_2011=0;
	control_2012=0;
	control_2013=0;
	treat_2013=0;

	cov_2011=0;
	cov_2012=0;
	cov_2013=0;
	do j=1 to 12;
		/*Define Sample*/
		if month2011[j]=3 then control_2011+1;
		if month2012[j]=3 then control_2012+1;
		if month2013[j]=3 then control_2013+1;
		if month2013[j]=1 | month2013[j]=2 then treat_2013+1;

		/*Make yearly eligibility flags - this is kind of redundant but helps make this code work with some legacy code*/
		if month2011[j]~=. then cov_2011+1;
		if month2012[j]~=. then cov_2012+1;
		if month2013[j]~=. then cov_2013+1;
	end;
	if control_2011=12 & control_2012=12 & control_2013=12 then treatment=0;
	if control_2011=12 & control_2012=12 & treat_2013=12 then treatment=1;

	if cov_2011=12 then elig_2011=1;
	if cov_2012=12 then elig_2012=1;
	if cov_2013=12 then elig_2013=1;
	drop i j;
run;

proc print data=elig_flags (obs=20);
	where eligeff2~=.;
run;

proc freq data=elig_flags;
	table product_2011 product_2012 product_2013 treatment control_2012 control_2013 treat_2013;
run;

data elig2012 elig2013;
	set elig_flags;
	output elig2012;
	output elig2013;
run;

/*We merge eligibility by Patid and Year to account for those with no medical claims so we have an observation each year if a patient is only eligible*/
data elig;
	set elig2012 (in=a)
		elig2013 (in=b);
	if a then year=2012;
	if b then year=2013;
run;

proc freq data=elig;
	tables gdr_cd division product;
run;

proc print data=elig (obs=20); run;

/*Re-merge the SES information on here so that patients with no medical claims can still have SES info, otherwise it's all missing*/
data ses;
	set raw.ses_ses;
run;

proc sort data=ses; by patid; run;
proc sort data=elig; by patid year; run;

/*Format the data appropriately. This seems a little redundant here, but it covers people who have no medical claims, just coverage*/
data elig;
	merge elig (in=a)
		  ses (in=b);
	by patid;
	if a;
	/*Do some data prep*/
	age=year-yrdob;
	if 18<=age<=34 then age_cat='18-34';
	if 35<=age<=49 then age_cat='35-49';
	if 50<=age<=64 then age_cat='50-64';
	if upcase(D_EDUCATION_LEVEL_CODE)='B' then D_EDUCATION_LEVEL_CODE='A'; /*We're combining A and B into 'High School or Less'*/
	if year=2011 then product=product_2011;
	else if year=2012 then product=product_2012;
	else if year=2013 then product=product_2013;
	if upcase(product)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product)='OTH' then product='POS'; /*Combine POS and Other*/
	if upcase(product_2012)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product_2012)='OTH' then product='POS'; /*Combine POS and Other*/
	if upcase(product_2013)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product_2013)='OTH' then product='POS'; /*Combine POS and Other*/

	/*Only keep bene's that are in PPOs or POS for all years*/
	if product_2011 in ('PPO','POS') & product_2012 in('PPO','POS') & product_2013 in('PPO','POS'); /*Only keep bene's that are in PPOs or POS for all years*/
	if (year=2012 & 18<=age<=63) | (year=2013 & 19<=age<=64);
	if treatment~=.;
run;

/*Keep only observations that are in the DiD sample. Make separate 2012 and 2013 files as well as pooled*/
proc freq data=elig;
	tables gdr_cd age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division treatment year treatment*year / missing;
run;

proc sort data=elig nodupkey; by patid year; run;
proc sort data=optum_demog; by patid year; run;

data optum_demog;
	merge optum_demog (in=a)
		  elig (in=b);
	by patid year;
	if b; /*Keep people with coverage even if they have no medical records*/
	if ~a & b then cov_only=1;

	if year=2011 then product=product_2011;
	else if year=2012 then product=product_2012;
	else if year=2013 then product=product_2013;
	if upcase(product)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product)='OTH' then product='POS'; /*Combine POS and Other*/
	if upcase(product_2012)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product_2012)='OTH' then product='POS'; /*Combine POS and Other*/
	if upcase(product_2013)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product_2013)='OTH' then product='POS'; /*Combine POS and Other*/
	if product_2011 in ('PPO','POS') & product_2012 in('PPO','POS') & product_2013 in('PPO','POS'); /*Only keep bene's that are in PPOs or POS for all years*/
	if cst_non_inpatient~=0 & cst_non_inpatient~=. then non_inpatient_ratio=(cst_lv_non_inpatient/cst_non_inpatient)*10000 ;
	if cst_non_inpatient=. | cst_non_inpatient=0 then non_inpatient_ratio=0 ;
	if gdr_cd~="" & age_cat~="" & product~="" & D_RACE_CODE~="" & D_HOUSEHOLD_INCOME_RANGE_CODE~="" & division~="" & division~="UNKNOWN";
run;

title "Spending Differences Before Matching";
proc ttest data=optum_demog;
	class treatment;
	var cst_non_inpatient cst_lv_non_inpatient non_inpatient_ratio;
	where year=2012;
run;
title;

proc freq data=optum_demog;
	tables product_2011 product_2012 product_2013 year product treatment*product cov_only;
run;

proc freq data=optum_demog;
	tables gdr_cd age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division treatment year treatment*year / missing;
run;

proc univariate data=optum_demog;
	var cst_non_inpatient cst_lv_non_inpatient cst_less_sensitive cst_more_sensitive;
run;

title "Check Analytic File";
proc print data=optum_demog (obs=10); run;
title;

data optum_demog2012;
	merge optum_demog (in=a)
		  elig (in=b);
	by patid year;
	if b & year=2012;
	if ~a & b then cov_only=1;

	if year=2011 then product=product_2011;
	else if year=2012 then product=product_2012;
	else if year=2013 then product=product_2013;
	if upcase(product)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product)='OTH' then product='POS'; /*Combine POS and Other*/
	if upcase(product_2012)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product_2012)='OTH' then product='POS'; /*Combine POS and Other*/
	if upcase(product_2013)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product_2013)='OTH' then product='POS'; /*Combine POS and Other*/
	if product_2011 in ('PPO','POS') & product_2012 in('PPO','POS') & product_2013 in('PPO','POS'); /*Only keep bene's that are in PPOs or POS for all years*/
	if gdr_cd~="" & age_cat~="" & product~="" & D_RACE_CODE~="" & D_HOUSEHOLD_INCOME_RANGE_CODE~="" & division~="" & division~="UNKNOWN";
run;

data optum_demog2013;
	merge optum_demog (in=a)
		  elig (in=b);
	by patid year;
	if b & year=2013;
	if ~a & b then cov_only=1;

	if year=2011 then product=product_2011;
	else if year=2012 then product=product_2012;
	else if year=2013 then product=product_2013;
	if upcase(product)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product)='OTH' then product='POS'; /*Combine POS and Other*/
	if upcase(product_2012)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product_2012)='OTH' then product='POS'; /*Combine POS and Other*/
	if upcase(product_2013)='IND' then product='PPO'; /*Combine PPO and Indemnity*/
	if upcase(product_2013)='OTH' then product='POS'; /*Combine POS and Other*/
	if product_2011 in ('PPO','POS') & product_2012 in('PPO','POS') & product_2013 in('PPO','POS'); /*Only keep bene's that are in PPOs or POS for all years*/
	if gdr_cd~="" & age_cat~="" & product~="" & D_RACE_CODE~="" & D_HOUSEHOLD_INCOME_RANGE_CODE~="" & division~="" & division~="UNKNOWN";
run;


proc freq data=optum_demog;
	tables treatment;
	where cst_lv_s=. | cst_lv_s=0;
run;

proc freq data=optum_demog;
	tables year treatment*cov_only product*treatment / missing;
run;


/**************************************************************
 ADD ON CHARLSON COMORBIDITY SCORES
**************************************************************/
%macro cci;
	%do yr=2011 %to 2011;
		data claims_&yr.;
			set in2.ses_m2011_2013 (where=(ses_m_year=&yr.)); /*Only count comorbidities that happen in 2011*/

			/*Add Charlson Variables*/
			if pos="23" then claim_type='M'; /*If inpatient, set claim_type to 'M', for MedPAR. This value is used by the Charlson Macro*/
			status='I';
			length=(Lst_Dt - Fst_Dt)+1;
			hcpcs='';
		run;

		proc sort data=claims_&yr.; by patid Fst_dt; run;

		/*We must use the ruleout macro because most of our claims are physician and outpatient - they use misleading ruleout diagnoses*/
		*%RULEOUT (SETIN=claims_&yr.,
						PATID=patid,
						CLMDTE=Fst_Dt,
						START="01jan&yr."d,
						FINISH="31dec&yr."d,
						DXVARSTR=diag1-diag5,
						NDXVAR=5,
						HCPCS=hcpcs,
						FILETYPE=claim_type);

		/*The output of ruleout is clmrecs, so input that into the actual comorbidity macro below. If not using rulouts,
		  input the claims_&yr. dataset. The output dataset is 'comorb'*/
		%COMORB  (SETIN=/*CLMRECS*/claims_&yr.,
						PATID=patid,
						IDXPRI=status,
						DAYS=length,
						DXVARSTR=diag1-diag5,
						NDXVAR=5,
						SXVARSTR=proc1-proc3,
						NSXVAR=3,
						HCPCS=hcpcs,
						FILETYPE=claim_type);

		data cci_&yr.;
			set comorb;
		run;
	%end;
%mend;
%cci;

data cci;
	set cci_2011 (in=a);
	charlson_count=sum(of CVPRIO01-CVPRIO18,of CVINDX01-CVINDX18);
run;

proc freq data=cci;
	tables charlson_count;
run;

proc print data=cci (obs=10); 
	var CVPRIO01-CVPRIO18 CVINDX01-CVINDX18 charlson_count;
run;

proc sort data=cci; by patid;
proc sort data=optum_demog; by patid; 
proc sort data=optum_demog2012; by patid;
proc sort data=optum_demog2013; by patid;

data optum_demog;
	merge optum_demog (in=a)
		  cci (keep=patid charlson_count in=b);
	by patid;
	if a;

	if charlson_count>3 then charlson_count=3; /*Cap the Charlson count*/
	if charlson_count=. then charlson_count=0;
run;

data optum_demog2012;
	merge optum_demog2012 (in=a)
		  cci (keep=patid charlson_count in=b);
	by patid;
	if a;

	if charlson_count>3 then charlson_count=3; /*Cap the Charlson count*/
	if charlson_count=. then charlson_count=0;
run;

data optum_demog2013;
	merge optum_demog2013 (in=a)
		  cci (keep=patid charlson_count in=b);
	by patid;
	if a;

	if charlson_count>3 then charlson_count=3; /*Cap the Charlson count*/
	if charlson_count=. then charlson_count=0;
run;

proc sort data=optum_demog; by Patid Fst_dt; run;

proc contents data=optum_demog; run;

proc freq data=optum_demog;
	tables year charlson_count*year;
run;

proc print data=optum_demog (obs=50);
	var patid year charlson_count;
	where year=2012;
run;

proc print data=optum_demog (obs=50);
	var patid year charlson_count;
	where year=2013;
run;

proc means data=optum_demog;
	var cst_non_inpatient cst_lv_non_inpatient cst_less_sensitive cst_more_sensitive;
run;

proc freq data=optum_demog;
	tables cov_only;
run;


/**************************************************************
 PROPENSITY SCORE MATCHING
**************************************************************/
%macro prop_match;
	/*Set up the dataset for the propensity score macro*/
	PROC LOGISTIC DATA = optum_demog descend;
	class gdr_cd age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division charlson_count;
	MODEL treatment = gdr_cd age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division charlson_count
	/ SELECTION = STEPWISE RISKLIMITS LACKFIT RSQUARE PARMLABEL;
	 OUTPUT OUT=optum_demog_prop prob=prob ;
	RUN;

	%OneToManyMTCH (
	 Lib=work, 					/* Library Name */
	 Dataset=optum_demog_prop,	/* Data set of all patients */
	 depend=treatment, 			/* Dependent variable that indicates Case or Control, Code 1 for Cases, 0 for Controls */
	 SiteN=, 					/* Site/Hospital ID */
	 PatientN=Patid, 			/* Patient ID */
	 matches=optum_demog_prop,	/* Output data set of matched pairs */
	 NoContrls=1); 				/* Number of controls to match to each case */

	PROC LOGISTIC DATA = optum_demog2012 descend;
	class gdr_cd age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division charlson_count;
	MODEL treatment = gdr_cd age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division charlson_count
	/ SELECTION = STEPWISE RISKLIMITS LACKFIT RSQUARE PARMLABEL;
	 OUTPUT OUT=optum_demog2012_match prob=prob ;
	RUN;

	%OneToManyMTCH (
	 Lib=work, 					/* Library Name */
	 Dataset=optum_demog2012_match,	/* Data set of all patients */
	 depend=treatment, 			/* Dependent variable that indicates Case or Control, Code 1 for Cases, 0 for Controls */
	 SiteN=, 					/* Site/Hospital ID */
	 PatientN=Patid, 			/* Patient ID */
	 matches=optum_demog2012_match,	/* Output data set of matched pairs */
	 NoContrls=1); 				/* Number of controls to match to each case */


	PROC LOGISTIC DATA = optum_demog2013 descend;
	class gdr_cd age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division charlson_count;
	MODEL treatment = gdr_cd age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division charlson_count
	/ SELECTION = STEPWISE RISKLIMITS LACKFIT RSQUARE PARMLABEL;
	 OUTPUT OUT=optum_demog2013_match prob=prob ;
	RUN;

	%OneToManyMTCH (
	 Lib=work, 					/* Library Name */
	 Dataset=optum_demog2013_match,	/* Data set of all patients */
	 depend=treatment, 			/* Dependent variable that indicates Case or Control, Code 1 for Cases, 0 for Controls */
	 SiteN=, 					/* Site/Hospital ID */
	 PatientN=Patid, 			/* Patient ID */
	 matches=optum_demog2013_match,	/* Output data set of matched pairs */
	 NoContrls=1); 				/* Number of controls to match to each case */
 %mend;
 *%prop_match;











/*Perform an exact match on observables*/
 data treat (keep=gdr_cd age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division charlson_count patid  rename=(patid=t_patid))
	 control (keep=gdr_cd age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division charlson_count patid  rename=(patid=c_patid));
	set optum_demog (where=(year=2012));
	if treatment=1 then output treat;
	else if treatment=0 then output control;
run;

data treat;
	set treat;
	if gdr_cd~="" & age_cat~="" & product~="" & D_RACE_CODE~="" & D_HOUSEHOLD_INCOME_RANGE_CODE~="" & division~="" & division~="UNKNOWN";
run;

data control;
	set control;
	if gdr_cd~="" & age_cat~="" & product~="" & D_RACE_CODE~="" & D_HOUSEHOLD_INCOME_RANGE_CODE~="" & division~="" & division~="UNKNOWN";
run;

proc sql;
	create table controls_id
	as select t.t_patid, c.c_patid
	from treat t
	inner join control c
	on (t.gdr_cd=c.gdr_cd and 
	   t.age_cat=c.age_cat and
	   t.product=c.product and
	   t.D_RACE_CODE=c.D_RACE_CODE and
	   t.D_HOUSEHOLD_INCOME_RANGE_CODE=c.D_HOUSEHOLD_INCOME_RANGE_CODE and
	   t.division=c.division and
	   t.charlson_count=c.charlson_count);
quit;

proc sort data=controls_id nodupkey; by t_patid c_patid; run;
proc sort data=controls_id; by t_patid; run;

proc print data=controls_id (obs=100); run;

%macro out; /*This works for 1:1 matching, but see above for 1:n matching with weights*/
	/* extract random sample of from each group */
	proc surveyselect data=controls_id
	                    out=controls_id
	                    n=1
	                    seed=12345; /* specify seed to enable results to be reproduced */
	strata t_patid; /* set grouping variable */
	run;
%mend;

proc print data=controls_id (obs=100); run;

data t_id (keep=t_patid c_patid) c_id (keep=t_patid c_patid);
	set controls_id;
	if t_patid~="" & c_patid~="";
	output t_id;
	output c_id;
run;

/*Add weights*/
proc sort data=c_id; by t_patid; run;
proc sort data=t_id; by c_patid; run;
data t_id;
	set t_id;
	by c_patid;
	if ~first.c_patid then dup_control=1; /*Instead of deleting observations (including treatment obs) that have merged onto dup controls, just flag them*/
run;

proc freq data=t_id;
	tables dup_control / missing;
run;

proc sort data=t_id; by t_patid; run;

data c_id;
	set c_id;
	retain n_controls;
	by t_patid;
	if first.t_patid then n_controls=0;
	if c_patid~="" then n_controls+1;
run;

data t_id (keep=t_patid n_controls); 
	set t_id;
	retain n_controls;
	by t_patid;
	if first.t_patid then n_controls=0;
	if c_patid~='' & dup_control~=1 then n_controls+1;
	if last.t_patid then output;
run;

proc univariate data=t_id;
	var n_controls;
run;

/*Add weights as the inverse of the number of controls matched to a particular treatment in a 1:N match*/
proc sort data=c_id; by t_patid descending n_controls; run;

data c_id (drop=t_patid n_controls);
	set c_id;
	retain match_weight t_weight;
	by t_patid;
	if first.t_patid then do;
		match_weight=1/n_controls;
		t_weight=1;
	end;
run;

proc sort data=c_id; by c_patid; run;

/*The control group above is duplicated by c_patid, and the inverse weight on each c_patid should be summed up with its dups so as not to underweight.
  Collapse down to the unique c_patid level here while preserving the weights by summing them up within a c_patid*/
data c_id;
	set c_id;
	by c_patid;
	retain c_weight;
	if first.c_patid then c_weight=match_weight;
	else c_weight=c_weight+match_weight;
	if last.c_patid then output;
run;

/*Use c_weight if weighting controls to treatment (weights <1), t_weight if weighting treatment to controls (weights>1 but lots of 0's)*/
data t_id;
	set t_id;
	c_weight=1;
	t_weight=n_controls;
run;
	
proc sort data=t_id nodupkey; by t_patid; run;
proc sort data=c_id nodupkey; by c_patid; run;
proc sort data=optum_demog; by patid; run;
proc sort data=optum_demog2012; by patid; run;
proc sort data=optum_demog2013; by patid; run;

data optum_demog_match;
	merge optum_demog (in=a)
		  c_id (in=c keep=c_patid c_weight t_weight rename=(c_patid=patid))
		  t_id (in=t keep=t_patid c_weight t_weight rename=(t_patid=patid));
	by patid;
	if t | c ; /*Keep all treatment IDs that found a match, and their corresponding control IDs*/
run;

data optum_demog2012_match;
	merge optum_demog2012 (in=a)
		  c_id (in=c keep=c_patid c_weight t_weight rename=(c_patid=patid))
		  t_id (in=t keep=t_patid c_weight t_weight rename=(t_patid=patid));
	by patid;
	if t | c ; /*Keep all treatment IDs that found a match, and their corresponding control IDs*/
run;

data optum_demog2013_match;
	merge optum_demog2013 (in=a)
		  c_id (in=c keep=c_patid c_weight t_weight rename=(c_patid=patid))
		  t_id (in=t keep=t_patid c_weight t_weight rename=(t_patid=patid));
	by patid;
	if t | c ; /*Keep all treatment IDs that found a match, and their corresponding control IDs*/
run;

proc freq data=optum_demog_match;
	tables year treatment treatment*year;
run;

title "Spending Differences After Matching, No Weighting";
proc ttest data=optum_demog_match;
	class treatment;
	var cst_non_inpatient cst_lv_non_inpatient non_inpatient_ratio;
	where year=2012;
run;
title;

title "Spending Differences After Matching, With Weighting";
proc ttest data=optum_demog_match;
	class treatment;
	var cst_non_inpatient cst_lv_non_inpatient non_inpatient_ratio;
	where year=2012;
	weight c_weight;
run;
title;

title "Charlson by Treatment After Matching";
proc freq data=optum_demog_match;
	tables charlson_count*treatment / missing;
	where year=2012;
run;

title "Charlson by Treatment Before Matching 2012";
proc freq data=optum_demog2012;
	tables charlson_count*treatment / missing;
run;

title "Charlson by Treatment After Matching 2012";
proc freq data=optum_demog2012_match;
	tables charlson_count*treatment / missing;
run;
title;


 /***********************************************************************************************************************/

/*Output an intermediate dataset to use for future analysis*/
data in.beneyear_cdhp_did_saf_match;
	set optum_demog_match;
run;

proc freq data=in.beneyear_cdhp_did_saf_match;
	tables gdr_cd age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division charlson_count / missing;
run;

proc export data=in.beneyear_cdhp_did_saf_match
			outfile = "/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Data/CDHP/beneyear_cdhp_did_saf_match.dta" replace;
run;


/**************************************************************
 GENERATE DEMOGRAPHIC AND DESCRIPTIVE STATISTICS
**************************************************************/
*%summary(ds=optum_demog,cutoff=100,byvar=treatment,subset=,formats=Y,weight=N,weightvar=);

%macro freqs(byvar,var);
	ods output ChiSq = chi
			   CrossTabFreqs = xtab;
	proc freq data=optum_demog2012_match;
		tables &byvar.*&var. / chisq;
		weight c_weight;
	run;
	ods output close;

	proc freq data=optum_demog2013_match;
		tables &byvar.*&var. / chisq;
		weight c_weight;
	run;

	proc print data=xtab; run;
	proc print data=chi; run;

	data _NULL_;
		set chi;
		if trim(left(statistic))="Chi-Square" then call symput("chisq",prob);
	run;

	data &byvar. (keep=&byvar. &var. rowpercent rename=(&var.=value1 rowpercent=pct_treat))
		 non_&byvar. (keep=&byvar. &var. rowpercent rename=(&var.=value1 rowpercent=pct_control));
		set xtab (where=(rowpercent~=.));
		if &byvar.=1 then output &byvar.;
		else if &byvar.=0 then output non_&byvar.;
	run;

	proc sort data=&byvar.; by value1; run;
	proc sort data=non_&byvar.; by value1; run;

	data z_&var. (keep=value pct_treat pct_control);
		length value $100;
		merge &byvar.
			  non_&byvar.;
		by value1;
		format value1 $&var..;
		value=vvalue(value1);
	run;

	data z_&var.;
		length variable $32;
		set z_&var.;
		if _n_=1 then do; 
			variable="&var.";
			Significance=&chisq.;
		end;
	run;

	proc print data=z_&var.; run;
%mend;
%freqs(byvar=treatment, var=D_HOUSEHOLD_INCOME_RANGE_CODE);
%freqs(byvar=treatment, var=D_RACE_CODE);
%freqs(byvar=treatment, var=gdr_cd);
%freqs(byvar=treatment, var=product);
%freqs(byvar=treatment, var=age_cat);
%freqs(byvar=treatment, var=division);
%freqs(byvar=treatment, var=charlson_count);

data freqs_all;
	set z_:;
run;

proc print data= freqs_all; run;



/**************************************************************
 CALCULATE COSTS BY VARIABLE
**************************************************************/

/*This macro determines which dataset to read in for analysis - original, propensity matched, or propensity weighted*/
%macro costs(ds,name,cost_var);
	data optum_demog&ds.;
		set optum_demog&ds.;
		if 18<=age<=34 then age_cat='18-34';
		if 35<=age<=49 then age_cat='35-49';
		if 50<=age<=64 then age_cat='50-64';
		if upcase(D_EDUCATION_LEVEL_CODE)='B' then D_EDUCATION_LEVEL_CODE='A';
		if upcase(product)='IND' then product='PPO';
		if upcase(product)='OTH' then product='POS';
		if cst_&cost_var.~=. & cst_&cost_var.~=0 then ratio_&cost_var. = (cst_lv_&cost_var./cst_&cost_var.)*10000;
	run;

	/*This macro makes the ttest output a single line dataset with means and ttest*/
	%macro ttest(byvar);

		proc sort data=optum_demog&ds.; by &byvar.; run;

		ods listing close; /*For now do not print output to listing - .lst file is too cluttered*/
		ods output  Statistics = stats
		 			TTests = ttests
					Equality = variances;
		proc ttest data=optum_demog&ds.;
			class treatment;
			by &byvar.;
			var cst_lv_&cost_var. cst_&cost_var. ratio_&cost_var. cst_less_sensitive cst_more_sensitive;
			weight c_weight;
		run;
		ods output close;
		ods listing;

		 proc print data=variances; run;

		/*proc print data=stats; run;	You can include these if somethings going wrong - for now suppress output
		proc print data=ttests; run;*/

		data stats (keep = &byvar. variable class treatment non_treatment);
			set stats(rename=(mean=avg));
			if trim(left(class)) = '0' then non_treatment = avg;
			if trim(left(class)) = '1' then treatment = avg; 
		run;

		 data non_treatment(drop = class treatment)
		 	  treatment (drop = class non_treatment);
		 	set stats;
		 	if trim(left(class)) = '0' then output non_treatment;
		 	if trim(left(class)) = '1' then output treatment;
		 run;

		 proc sort data = non_treatment; by &byvar. variable; run;
		 proc sort data = treatment; by &byvar. variable; run;

		 data stats_final;
		 	merge non_treatment
			      treatment;
		 	by &byvar. variable;
		 run;

		 /*proc print data=stats_final; run;*/
		
		 proc print data=ttests; run;

		 /*If the variances are unequal, use the satterthwaite p-val, otherwise use the pooled p-val*/
		 proc sort data=ttests; by &byvar. variable; run;
		 proc sort data=variances; by &byvar. variable; run;

		 data ttest (keep=&byvar. variable probt rename=(probt=Significance));
		 	merge ttests
				  variances (keep=&byvar. Variable ProbF);
			by &byvar. Variable;
			if /*trim(left(ProbF))="<.0001" |*/ (ProbF*1)<.05 then do;
				if trim(left(Method))="Satterthwaite";
			end;
			else do;
				if trim(left(Method))="Pooled";
			end;
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

		data a_&byvar. (rename=(treatment=&cost_var._treat non_treatment=&cost_var._non_treat Significance=&cost_var._Sig))
		     b_&byvar. (rename=(treatment=all_costs_treat non_treatment=all_costs_non_treat Significance=all_costs_Sig))
		     c_&byvar. (rename=(treatment=lv_cost_ratio_treat non_treatment=lv_cost_ratio_non_treat Significance=lv_cost_ratio_Sig))
			 d_&byvar. (rename=(treatment=less_sensitive_treat non_treatment=less_sensitive_non_treat Significance=less_sensitive_Sig))
			 e_&byvar. (rename=(treatment=more_sensitive_treat non_treatment=more_sensitive_non_treat Significance=more_sensitive_Sig));
			set t_&byvar.;
			if trim(left(variable))="cst_lv_&cost_var." then output a_&byvar.;
			else if trim(left(variable))="cst_&cost_var." then output b_&byvar.;
			else if trim(left(variable))="ratio_&cost_var." then output c_&byvar.;
			else if trim(left(variable))="cst_less_sensitive" then output d_&byvar.;
			else if trim(left(variable))="cst_more_sensitive" then output e_&byvar.;
		run;
		
		proc sort data=a_&byvar.; by &byvar.; run;
		proc sort data=b_&byvar.; by &byvar.; run;
		proc sort data=c_&byvar.; by &byvar.; run;

		data f_&byvar. (drop=Variable &byvar. rename=(Var=Variable));
			length Var $32;
			merge a_&byvar.
			      b_&byvar.
			      c_&byvar.
				  d_&byvar.
				  e_&byvar.;
			by &byvar.;
			format &byvar. $&byvar..;
			Value=VVALUE(&byvar.);
			Var="&byvar.";
		run;
		
		proc print data=f_&byvar.; run;
	%mend;
	%ttest(byvar=D_HOUSEHOLD_INCOME_RANGE_CODE);
	%ttest(byvar=D_RACE_CODE);
	%ttest(byvar=gdr_cd);
	%ttest(byvar=product);
	%ttest(byvar=age_cat);
	%ttest(byvar=division);
	%ttest(byvar=charlson_count);

	data ttests_&cost_var._&name.;
		length Value $50;
		set f_:;
	run;

	/*Unstratified costs*/
	proc ttest data=optum_demog&ds.;
		class treatment;
		var cst_lv_&cost_var. cst_&cost_var. ratio_&cost_var. cst_less_sensitive cst_more_sensitive;
	run;
%mend;
%costs(ds=2012_match,name=2012,cost_var=non_inpatient);
%costs(ds=2013_match,name=2013,cost_var=non_inpatient);

%costs(ds=2012_match,name=2012,cost_var=imaging);
%costs(ds=2013_match,name=2013,cost_var=imaging);

%costs(ds=2012_match,name=2012,cost_var=laboratory);
%costs(ds=2013_match,name=2013,cost_var=laboratory);


/*Output the relevant datasets*/
ods tagsets.excelxp file="&out./cdhp_did_demog_noro_match_nw.xml" style=sansPrinter;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Demographics by CHDP" frozen_headers='yes');
proc print data=freqs_all; run;

ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Non-Inpatient Costs - 2012" frozen_headers='yes');
proc print data=ttests_non_inpatient_2012; run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Non-Inpatient Costs - 2013" frozen_headers='yes');
proc print data=ttests_non_inpatient_2013; run;

ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Imaging Costs - 2012" frozen_headers='yes');
proc print data=ttests_imaging_2012; run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Imaging Costs - 2013" frozen_headers='yes');
proc print data=ttests_imaging_2013; run;

ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Laboratory Costs - 2012" frozen_headers='yes');
proc print data=ttests_laboratory_2012; run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Laboratory Costs - 2013" frozen_headers='yes');
proc print data=ttests_laboratory_2013; run;

ods tagsets.excelxp close;



proc datasets library=work kill;
run;
quit;







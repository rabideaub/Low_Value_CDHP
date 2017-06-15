libname dat "/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Data/CDHP";
libname raw "/sch-data-library/dua-data/OPTUM/Original_data/Data";

data all_procs;
	set dat.low_value_procs (keep=patid fst_dt std_cost non_inpatient lv_non_inpatient age product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE gdr_cd division); 
	month=month(Fst_dt);
	year=year(Fst_dt);
	yearmonth=trim(left(year)) || "_" || trim(left(month));
run;

proc sort data=all_procs; by Patid yearmonth; run;

/*Collapse down to the patient-month level*/
data month_procs;
	set all_procs;
	retain m_cst_non_inpatient m_cst_lv_non_inpatient;
	by Patid yearmonth;
	if first.yearmonth then do;
		m_cst_non_inpatient=0;
		m_cst_lv_non_inpatient=0;
	end;
	if non_inpatient>=1 then m_cst_non_inpatient=sum(m_cst_non_inpatient,std_cost);
	if lv_non_inpatient>=1 then m_cst_lv_non_inpatient=sum(m_cst_lv_non_inpatient,std_cost);
	if last.yearmonth then output;
run;

proc print data=month_procs (obs=100);
	var patid year month yearmonth;
run;

proc univariate data=month_procs;
	var m_cst_non_inpatient m_cst_lv_non_inpatient;
run;

/********************************************************************************************
READ IN AND COLLAPSE THE MEMBERSHIP FILE - CREATE MONTHLY ELIGIBILITY FLAGS
********************************************************************************************/
/*Select Difference-in-Differences sample. Non-CDHP in 2011 and 2012 for all, non-CDHP in 2013 for control, CDHP 2013 for treatment*/

/*Capture all eligible beneficiaries for each year - even those without claims*/
/*Benes have multiple observations for different enrollment periods. Dynamically flatten with arrays by patid*/


data elig;
	set raw.ses_mbr_detail;
run; 

proc sort data=elig; by patid eligeff; run;

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

/*Take the patient level eligibility file and create monthly level file. This is so that when we merge onto the medical table, we can have 0s
  for months when patients were eligible but did not incur any costs*/
data elig2011_1 elig2011_2 elig2011_3 elig2011_4 elig2011_5 elig2011_6 elig2011_7 elig2011_8 elig2011_9 elig2011_10 elig2011_11 elig2011_12
	 elig2012_1 elig2012_2 elig2012_3 elig2012_4 elig2012_5 elig2012_6 elig2012_7 elig2012_8 elig2012_9 elig2012_10 elig2012_11 elig2012_12
	 elig2013_1 elig2013_2 elig2013_3 elig2013_4 elig2013_5 elig2013_6 elig2013_7 elig2013_8 elig2013_9 elig2013_10 elig2013_11 elig2013_12;
	set elig_flags (keep=patid yrdob treatment product:);
	%macro loop;
		%do i=1 %to 12;
			output elig2011_&i.;
			output elig2012_&i.;
			output elig2013_&i.;
		%end;
	%mend;
	%loop;
run;

/*We merge eligibility by Patid and Year to account for those with no medical claims so we have an observation each year if a patient is only eligible*/
data elig;
	set elig2012_1 (in=a) elig2012_2 (in=b)	elig2012_3 (in=c) elig2012_4 (in=d)	elig2012_5 (in=e) elig2012_6 (in=f)	elig2012_7 (in=g)
		elig2012_8 (in=h) elig2012_9 (in=i)	elig2012_10 (in=j) elig2012_11 (in=k) elig2012_12 (in=l)
		elig2013_1 (in=m) elig2013_2 (in=n)	elig2013_3 (in=o) elig2013_4 (in=p)	elig2013_5 (in=q) elig2013_6 (in=r)	elig2013_7 (in=s)
		elig2013_8 (in=t) elig2013_9 (in=u)	elig2013_10 (in=v) elig2013_11 (in=w) elig2013_12 (in=x)
		elig2011_1 (in=aa) elig2011_2 (in=bb)	elig2011_3 (in=cc) elig2011_4 (in=dd)	elig2011_5 (in=ee) elig2011_6 (in=ff)	elig2011_7 (in=gg)
		elig2011_8 (in=hh) elig2011_9 (in=ii)	elig2011_10 (in=jj) elig2011_11 (in=kk) elig2011_12 (in=ll);

	/*Added in 2011 as an after-thought, which is why things are a little weird*/
	if aa then do; year=2011; month=1; end;
	if bb then do; year=2011; month=2; end;
	if cc then do; year=2011; month=3; end;
	if dd then do; year=2011; month=4; end;
	if ee then do; year=2011; month=5; end;
	if ff then do; year=2011; month=6; end;
	if gg then do; year=2011; month=7; end;
	if hh then do; year=2011; month=8; end;
	if ii then do; year=2011; month=9; end;
	if jj then do; year=2011; month=10; end;
	if kk then do; year=2011; month=11; end;
	if ll then do; year=2011; month=12; end;

	if a then do; year=2012; month=1; end;
	if b then do; year=2012; month=2; end;
	if c then do; year=2012; month=3; end;
	if d then do; year=2012; month=4; end;
	if e then do; year=2012; month=5; end;
	if f then do; year=2012; month=6; end;
	if g then do; year=2012; month=7; end;
	if h then do; year=2012; month=8; end;
	if i then do; year=2012; month=9; end;
	if j then do; year=2012; month=10; end;
	if k then do; year=2012; month=11; end;
	if l then do; year=2012; month=12; end;

	if m then do; year=2013; month=1; end;
	if n then do; year=2013; month=2; end;
	if o then do; year=2013; month=3; end;
	if p then do; year=2013; month=4; end;
	if q then do; year=2013; month=5; end;
	if r then do; year=2013; month=6; end;
	if s then do; year=2013; month=7; end;
	if t then do; year=2013; month=8; end;
	if u then do; year=2013; month=9; end;
	if v then do; year=2013; month=10; end;
	if w then do; year=2013; month=11; end;
	if x then do; year=2013; month=12; end;
run;

proc print data=elig (obs=20); run;

/*Re-merge the SES information on here so that patients with no medical claims can still have SES info, otherwise it's all missing*/
data ses;
	set raw.ses_ses;
run;

proc sort data=ses; by patid; run;
proc sort data=elig; by patid year month; run;

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
	if (year=2011 & 17<=age<=62) | (year=2012 & 18<=age<=63) | (year=2013 & 19<=age<=64);
	if treatment~=.;
	yearmonth=trim(left(year)) || "_" || trim(left(month));
run;

proc print data=elig (obs=100);
	var patid year month;
run;

/*Keep only observations that are in the DiD sample. Make separate 2012 and 2013 files as well as pooled*/
proc freq data=elig;
	tables age_cat product D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE treatment year treatment*year / missing;
run;






proc sort data=elig nodupkey; by patid year month; run;
proc sort data=month_procs; by patid year month; run;

data month_procs_elig;
	merge month_procs (in=a)
		  elig (in=b);
	by patid year month;
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

	qtr=0;
	qtr1=0;
	qtr2=0;
	qtr3=0;
	qtr4=0;
	if 1<=month<=3 then do; qtr1=1; qtr=1; end;
	if 4<=month<=6 then do; qtr2=1; qtr=2; end;
	if 7<=month<=9 then do; qtr3=1; qtr=3; end;
	if 10<=month<=12 then do; qtr4=1; qtr=4; end;
run;

proc print data=month_procs_elig (obs=100);
	var patid year month cov_only;
run;

/*This is the main SAF for our analysis. Use it here to keep only the beneficiaries who will be in our sample*/
data matched_finder;
	set dat.beneyear_cdhp_did_saf_match (keep=patid charlson_count c_weight);
run;

proc sort data=matched_finder nodupkey; by patid; run;
proc sort data=month_procs_elig; by patid; run;

data month_procs_elig;
	merge month_procs_elig (in=a)
		  matched_finder (in=b);
	by patid;
	if a & b;
	if m_cst_non_inpatient<0 | m_cst_non_inpatient=. then m_cst_non_inpatient=0;
	if m_cst_lv_non_inpatient<0 | m_cst_lv_non_inpatient=. then m_cst_lv_non_inpatient=0;

	if 1<=month<=3 then do; qtr1=1; qtr=1; end;
	if 4<=month<=6 then do; qtr2=1; qtr=2; end;
	if 7<=month<=9 then do; qtr3=1; qtr=3; end;
	if 10<=month<=12 then do; qtr4=1; qtr=4; end;

	if year=2011 then time_trend=month;
	if year=2012 then time_trend=month+12;
	if year=2012 then time_trend2=month;
run;

proc sort data=month_procs_elig; by patid year month; run;

proc print data=month_procs_elig (obs=100);
	var patid year month cov_only;
run;

/*We should have equal number of 2012 and 2013 within a treatment, and we should have 12x more obs than the SAF, which is about 22k treat, 730k control*/
proc freq data=month_procs_elig;
	tables treatment month year treatment*year treatment*month treatment*yearmonth / missing;
run;





/*Break it down to the quarter level to test*/
proc sort data=month_procs_elig; by patid year qtr; run;

data qtr_procs_elig;
	set month_procs_elig;
	retain q_cst_non_inpatient q_cst_lv_non_inpatient;
	by patid year qtr;
	if first.qtr then do;
		q_cst_non_inpatient=0;
		q_cst_lv_non_inpatient=0;
	end;
	q_cst_non_inpatient=sum(q_cst_non_inpatient,m_cst_non_inpatient);
	q_cst_lv_non_inpatient=sum(q_cst_lv_non_inpatient,m_cst_lv_non_inpatient);
	yearqtr=trim(left(year)) || "_" || trim(left(qtr));
	if last.qtr then output;
run;

data qtr_procs_elig;
	set qtr_procs_elig;
	if q_cst_non_inpatient<0 | q_cst_non_inpatient=. then q_cst_non_inpatient=0;
	if q_cst_lv_non_inpatient<0 | q_cst_lv_non_inpatient=. then q_cst_lv_non_inpatient=0;
run;

proc univariate data=month_procs_elig;
	var m_cst_non_inpatient;
run;
proc univariate data=qtr_procs_elig;
	var q_cst_non_inpatient;
run;

proc print data=month_procs_elig (obs=120);
	var patid year month treatment qtr: m_cst_non_inpatient m_cst_lv_non_inpatient;
run;
proc print data=qtr_procs_elig (obs=60);
	var patid year treatment qtr: yearqtr q_cst_non_inpatient q_cst_lv_non_inpatient;
run;


/*Check average spending per yearqtr between CDHP and non-CDHP*/
proc sort data=month_procs_elig; by treatment; run;

title "Total Outpatient Spending";
proc means data=month_procs_elig;
	class yearmonth;
	by treatment;
	var m_cst_non_inpatient; 
	output out=cst;
run;

proc print data=cst; run;

proc glm data=month_procs_elig;
ods output ParameterEstimates = tot_2012;
	class D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division age_cat gdr_cd product charlson_count treatment qtr4;
	model m_cst_non_inpatient=D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division age_cat gdr_cd product charlson_count treatment treatment*qtr4 / solution;
	where year=2012;
	weight c_weight;
run;
ods output close;

proc glm data=month_procs_elig;
ods output ParameterEstimates = tot_2013;
	class D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division age_cat gdr_cd product charlson_count treatment qtr4;
	model m_cst_non_inpatient=D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division age_cat gdr_cd product charlson_count treatment treatment*qtr1 / solution;
	where year=2013;
	weight c_weight;
run;
ods output close;
title;

/*Check average spending per yearqtr between CDHP and non-CDHP*/
title "Low Value Outpatient Spending";
proc means data=month_procs_elig;
	class yearmonth;
	by treatment;
	var m_cst_lv_non_inpatient; 
	output out=cst;
run;

proc print data=cst; run;

proc glm data=month_procs_elig;
ods output ParameterEstimates = lv_2012;
	class D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division age_cat gdr_cd product charlson_count treatment qtr1;
	model m_cst_lv_non_inpatient=D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division age_cat gdr_cd product charlson_count treatment treatment*qtr4 / solution;
	where year=2012;
	weight c_weight;
run;
ods output close;

proc glm data=month_procs_elig;
ods output ParameterEstimates = lv_2013;
	class D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division age_cat gdr_cd product charlson_count treatment qtr1;
	model m_cst_lv_non_inpatient=D_RACE_CODE D_HOUSEHOLD_INCOME_RANGE_CODE division age_cat gdr_cd product charlson_count treatment treatment*qtr1 / solution;
	where year=2013;
	weight c_weight;
run;
ods output close;
title;

title "Test Pre-Period Linear Time Trend. Total Spending, 2011 and 2012";
proc glm data=month_procs_elig;
	class time_trend treatment;
	model m_cst_non_inpatient=time_trend treatment time_trend*treatment / solution;
	where (year=2011 | (year=2012 & month<12));
	weight c_weight;
run;
title;

title "Test Pre-Period Linear Time Trend. Total Spending, Only 2012";
proc glm data=month_procs_elig;
	class time_trend2 treatment;
	model m_cst_non_inpatient=time_trend2 treatment time_trend2*treatment / solution;
	where year=2012 & month<12;
	weight c_weight;
run;
title;

title "Test Pre-Period Linear Time Trend. Low Value Spending";
proc glm data=month_procs_elig;
	class time_trend2 treatment;
	model m_cst_lv_non_inpatient=time_trend2 treatment time_trend2*treatment / solution;
	where year=2012 & month<12; /*No 2011 low value spending because there isn't a 1 year lookback*/
	weight c_weight;
run;
title;



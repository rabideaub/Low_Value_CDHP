#delimit ;
set more off;
clear;

use "/schaeffer-a/sch-projects/dua-data-projects/OPTUM/rabideau/Data/CDHP/costs_collapsed_by_month.dta";

gen time_trend=month if year==2011;
replace time_trend=month+12 if year==2012;
gen time_trend2=month if year==2012;

tab time_trend;
tab time_trend2;

summarize tot if treat==1;
summarize tot if treat==0;
summarize lv if treat==1;
summarize lv if treat==0;

gen timetreat = time_trend*treat;

regress tot time_trend treat timetreat if (year==2011 | (year==2012 & month<12));
regress lv time_trend2 treat timetreat if (year==2012 & month<12);

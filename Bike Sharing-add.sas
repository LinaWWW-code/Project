*Import data file;
proc import datafile = "day-revised.csv" out=day replace;
delimiter = ',';
getnames = yes;
run;

proc print;
run;

data day;
set day;
*create dummy variables;
* Season Winter = 0;
ds1 = (season = 1);
ds2 = (season = 2);
ds3 = (season = 3);

*Weekday: sunday = 0;
dw1 = (weekday = 1);
dw2 = (weekday = 2);
dw3 = (weekday = 3);
dw4 = (weekday = 4);
dw5 = (weekday = 5);
dw6 = (weekday = 6);
*Weathersit 4 = 0;
dwe1 = (weathersit = 1);
dwe2 = (weathersit = 2);
dwe3 = (weathersit = 3);

RUN;

PROC PRINT;
RUN;

*Histogram with 5-number summary of cnt;
TITLE "Histogram with 5-number summary - cnt";
PROC UNIVARIATE normal;
VAR cnt;
histogram/ normal(mu = est sigma = est);
inset min mean Q1 Q2 Q3 max Range stddev/
      header = 'Overall Statistics'
	  pos = tm;
RUN;


title "Scatterplot Matrix";
proc sgscatter data=day;
matrix cnt ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed;
run;

title "Scatterplot without dummy variables";
proc sgscatter data=day;
matrix cnt temp atemp hum windspeed;
run;


title "Boxplot for cnt by yr";
proc sort data=day;
by yr;
proc boxplot data=day;
plot cnt*yr;
run;

title "Boxplot for cnt by season";
proc sort data=day;
by season;
proc boxplot data=day;
plot cnt*season;
run;

title "Boxplot for cnt by workingday";
proc sort data=day;
by workingday;
proc boxplot data=day;
plot cnt*workingday;
run;

title "Correlation";
proc corr data = day;
var cnt temp atemp hum windspeed;
run;


*fit a full model with all predictors;
PROC REG DATA = day;
TITLE "FULL MODEL";
model cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed/ vif influence r stb;
plot student.*(ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed predicted.);
plot npp.*student.;
RUN;

*creates a training and testing dataset;
title "Test and Train sets for cnt";
proc surveyselect data = day out = newDay seed = 49587
samprate = 0.7 outall; 
run;

PROC PRINT;
RUN;

*create new variable new_cnt = cnt for training set, = NA for testing set;
data newDay;
set newDay;
if selected then new_cnt = cnt;
run;

proc print data = newDay;
run;

* use training set to come-up with new model;
title "model with training set";
proc reg data = newDay;
model new_cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed/vif tol stb;
RUN;


*Model Selection;
* adjrsq;
proc reg data=newDay;
model new_cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed/selection=adjrsq;
run;

* forward; 
proc reg data=newDay;
model new_cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed/selection=forward;
run;

*proc reg data=newDay;
*model new_cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed/selection=stepwise;
*run;


*model selection -backward;
*proc reg data=newDay;
*model new_cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed/selection=backward;
*run;




* Test the performance of 2 models -- compute regression for traning set and predicted value for test sets;
title "validation - Test set";
proc reg data = newDay;
*model 1 -- adjrsq;
model new_cnt = ds1 ds2 ds3 yr dw1 dw2 dw6 workingday dwe2 dwe3 temp hum windspeed;
output out = outm1 (where = (new_cnt = .)) p = yhat;
*model 2 -- forward;
model new_cnt = ds1 ds2 ds3 yr holiday dw3 dw4 dw5 dw6 workingday dwe2 dwe3 temp atemp hum windspeed;
output out = outm2(where = (new_cnt = .)) p = yhat;
run;

proc print;
run;



*****************
/* Analysis of predictions on testing set for model M1
Create new dataset outm1_test that contains prediction for model M1 and 
   the difference between observed and predicted values*/;
data outm1_sum;
set outm1;
d=cnt-yhat; *d is the difference between observed and predicted values in training set;
absd=abs(d);
run;
/* Computes predictive statistics: root mean square error (rmse), 
mean absolute error (mae) and mean absolute percentage error (MAPE) for model M1*/
proc summary data=outm1_sum;
var d absd;
output out=outm1_stats std(d)=rmse mean(absd)=mae;
run;
proc print data=outm1_stats;
title 'Validation  statistics for Model 1';
run;
*computes correlation of observed and predicted values in test set for model M1;
proc corr data=outm1;
var cnt yhat;
run;

/* Analysis of predictions on testing set for model M2
Create new dataset outm2_test that contains prediction for model M2 and 
   the difference between observed and predicted values*/;
data outm2_sum;
set outm2;
d=cnt-yhat; *d is the difference between observed and predicted values in training set;
absd=abs(d);
run;
/* Computes predictive statistics: root mean square error (rmse), 
mean absolute error (mae) and mean absolute percentage error (MAPE) for model M2*/
proc summary data=outm2_sum;
var d absd;
output out=outm2_stats std(d)=rmse mean(absd)=mae;
run;
proc print data=outm2_stats;
title 'Validation  statistics for Model 2';
run;
*computes correlation of observed and predicted values in test set for model M2;
proc corr data=outm2;
var cnt yhat;
run;


/**************************************************
K-FOLD CROSS VALIDATION
**************************************************/

* Compute 5-fold crossvalidation;
/* Apply 5-fold cross validation with backward model selection 
using prediction res#idual sum of squares as criterion for removing variables
(step=cv)*/
title "5-fold crossvalidation + 25% testing set-- backward" ;
proc glmselect data=day
	plots=(asePlot Criteria);
	*partition defines a test set (25% of data) to validate model on new data;
	partition fraction(test=0.25);
	* selection=stepwise uses stepwise selection method;
	* stop=cv: minimizes prediction residual sum of squares for variable selection;
	model cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed/
		selection=backward(stop=cv) cvMethod=split(5) cvDetails=all;
run;


title "5-fold crossvalidation + 25% testing set-- stepwise" ;
proc glmselect data=day
	plots=(asePlot Criteria);
	*partition defines a test set (25% of data) to validate model on new data;
	partition fraction(test=0.25);
	* selection=stepwise uses stepwise selection method;
	* stop=cv: minimizes prediction residual sum of squares for variable selection;
	model cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed/
		selection=stepwise(stop=cv) cvMethod=split(5) cvDetails=all;
run;


************************** New model with new interaction variable atemp * windspeed;
title "data w/ interaction terms";
data day_interaction;
set newDay;
windspeed_atemp=windspeed*atemp;
run;

proc print data=day_interaction;
run;

title "interaction variable";
proc reg data=day_interaction;
model new_cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed windspeed_atemp/vif stb influence r;
plot student.*(ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed windspeed_atemp predicted.);
plot npp.*student.;
RUN;

*proc reg data=day_interaction;
*model new_cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed windspeed_atemp/selection=adjrsq;
*run;


proc reg data=day_interaction;
model new_cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed windspeed_atemp/selection=forward;
run;

proc reg data=day_interaction;
model new_cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed windspeed_atemp/selection=stepwise;
run;

*proc reg data=day_interaction;
*model new_cnt = ds1 ds2 ds3 yr holiday dw1 dw2 dw3 dw4 dw5 dw6 workingday dwe1 dwe2 dwe3 temp atemp hum windspeed windspeed_atemp/selection=backward;
*run;

* Test the performance of 2 models -- compute regression for traning set and predicted value for test sets;
title "validation - Test set";
proc reg data = day_interaction;
*model 1 -- adjrsq;
model new_cnt = ds1 ds2 ds3 yr dw1 dw2 dw6 workingday dwe2 dwe3 temp hum windspeed windspeed_atemp;
output out = outm1 (where = (new_cnt = .)) p = yhat;
*model 2 -- forward;
model new_cnt = ds1 ds2 ds3 yr holiday dw3 dw4 dw5 dw6 workingday dwe2 dwe3 temp atemp hum windspeed windspeed_atemp;
output out = outm2(where = (new_cnt = .)) p = yhat;
run;

proc print;
run;

******************fitted a final model using subset of predictors from adjsq;
title "Fitted final model";
proc reg data = day;
*model -- adjrsq;
model cnt = ds1 ds2 ds3 yr dw1 dw2 dw6 workingday dwe2 dwe3 temp hum windspeed/vif stb influence r;
plot student.*(ds1 ds2 ds3 yr dw1 dw2 dw6 workingday dwe2 dwe3 temp hum windspeed predicted.);
plot npp.*student.;
run; 

*remove outliers and influential points;
data Day_2;
set day;
if _n_ in (442, 554, 668, 669, 692, 694) then delete;
run;

proc reg data = Day_2;
model cnt = ds1 ds2 ds3 yr dw1 dw2 dw6 workingday dwe2 dwe3 temp hum windspeed/vif stb influence r;
run; 

data Day_2;
set Day_2;
if _n_ in (204, 499, 553, 689) then delete;
run;

proc reg data = Day_2;
model cnt = ds1 ds2 ds3 yr dw1 dw2 dw6 workingday dwe2 dwe3 temp hum windspeed/stb influence r;
run; 

data Day_2;
set Day_2;
if _n_ in (238) then delete;
run;

proc reg data = Day_2;
model cnt = ds1 ds2 ds3 yr dw1 dw2 dw6 workingday dwe2 dwe3 temp hum windspeed/stb influence r;
run; 

*remove non-significant variable;
title "Final model";
proc reg data = Day_2;
model cnt = ds1 ds2 ds3 yr dw1 dw6 workingday dwe2 dwe3 temp hum windspeed/vif stb influence r;
plot student.*(ds1 ds2 ds3 yr dw1 dw6 workingday dwe2 dwe3 temp hum windspeed predicted.);
plot npp.*student.;
run; 

****************final model code*****************;
proc import datafile = "day-revised.csv" out=day replace;
delimiter = ',';
getnames = yes;
run;

data day;
set day;
*create dummy variables;
* Season Winter = 0;
ds1 = (season = 1);
ds2 = (season = 2);
ds3 = (season = 3);

*Weekday: sunday = 0;
dw1 = (weekday = 1);
dw2 = (weekday = 2);
dw3 = (weekday = 3);
dw4 = (weekday = 4);
dw5 = (weekday = 5);
dw6 = (weekday = 6);
*Weathersit 4 = 0;
dwe1 = (weathersit = 1);
dwe2 = (weathersit = 2);
dwe3 = (weathersit = 3);

RUN;

proc reg data = day;
*model -- adjrsq;
model cnt = ds1 ds2 ds3 yr dw1 dw2 dw6 workingday dwe2 dwe3 temp hum windspeed/vif stb influence r;
run; 

*remove outliers and influential points;
data Day_2;
set day;
if _n_ in (442, 554, 668, 669, 692, 694) then delete;
run;

proc reg data = Day_2;
model cnt = ds1 ds2 ds3 yr dw1 dw2 dw6 workingday dwe2 dwe3 temp hum windspeed/vif stb influence r;
run; 

data Day_2;
set Day_2;
if _n_ in (204, 499, 553, 689) then delete;
run;

proc reg data = Day_2;
model cnt = ds1 ds2 ds3 yr dw1 dw2 dw6 workingday dwe2 dwe3 temp hum windspeed/stb influence r;
run; 

data Day_2;
set Day_2;
if _n_ in (238) then delete;
run;

proc reg data = Day_2;
model cnt = ds1 ds2 ds3 yr dw1 dw2 dw6 workingday dwe2 dwe3 temp hum windspeed/stb influence r;
run; 

title "Final model";
proc reg data = Day_2;
model cnt = ds1 ds2 ds3 yr dw1 dw6 workingday dwe2 dwe3 temp hum windspeed/vif stb influence r;
plot student.*(ds1 ds2 ds3 yr dw1 dw6 workingday dwe2 dwe3 temp hum windspeed predicted.);
plot npp.*student.;
run; 

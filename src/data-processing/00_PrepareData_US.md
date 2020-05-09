Prepare Data of US MTurk Sample
================

- DESCRIPTION: Clean and construct measures according to PAP (pp. 3--8)
- CREATES: [Cleaned Data for US Sample](../../data/intermediate/cleaned_us.csv)
-  DEPENDS: [Raw Data from MTurk](../../data/raw-private-encrypted/US_MTURK.csv)

Content
======

-   [Setup](#setup)
-   [Data](#data)
    -   [Scalability](#Scalability)
    -   [Tidy Data](#Tidy-Data)
    -   [Dependent Variable](#Dependent-Variable)
    -   [Independent Variable](#Independent-Variable)
    -   [Moderator](#Moderator)
    -   [Control Variables](#Control-Variables)
- [Missing Data](#Missing-Data)

Setup
=====

Load the required packages and source the auxiliary functions from `src/lib/functions.R`:

``` r
source("src/lib/functions.R")
```
Data
====
-  Requires access to the csv file prepared by MTurk. 
-  [Cleaned data](../../data/intermediate/cleaned_us.csv) and [codings](src/data-processing) are saved to public folders. 


``` r
df <- read_sav("../../data/raw-private-encrypted/MTURK_US.csv")
```

Scalability
------------
* The gratifications of the news is constructed as an additive scale using a Principal Components Factor Analysis using varimax rotation, similar to Diddi and LaRose (2006), PAP p.9.

```r
# Check Scalability of Uses and Gratifications of the News
hs <- df[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5")]
cols <- c(1:5)
hs[,cols] = apply(hs[,cols], 2, function(x) as.numeric(as.character(x)));
hs <- hs[complete.cases(hs), ]
fac_hs <- psych::fa(hs, rotate="varimax", fm="pa", scores="Bartlett")

surv <- df[c("Q1_5", "Q1_6", "Q1_7", "Q1_8", "Q1_9", "Q1_10", "Q1_11")]
cols <- c(1:7)
surv[,cols] = apply(surv[,cols], 2, function(x) as.numeric(as.character(x)));
surv <- surv[complete.cases(surv), ]
fac_surv <- psych::fa(surv, rotate="varimax", fm="pa", scores="Bartlett")

esc <- df[c("Q1_12", "Q1_13", "Q1_14", "Q1_15", "Q1_16")]
cols <- c(1:5)
esc[,cols] = apply(esc[,cols], 2, function(x) as.numeric(as.character(x)));
esc <- esc[complete.cases(esc), ]
fac_esc <- psych::fa(esc, rotate="varimax", fm="pa", scores="Bartlett")

pt <- df[c("Q1_17", "Q1_18", "Q1_19", "Q1_20", "Q1_21")]
cols <- c(1:5)
pt[,cols] = apply(pt[,cols], 2, function(x) as.numeric(as.character(x)));
pt <- pt[complete.cases(pt), ]
fac_pt <- psych::fa(pt, rotate="varimax", fm="pa", scores="Bartlett")

ent <- df[c("Q1_22", "Q1_23")]
cols <- c(1:2)
ent[,cols] = apply(ent[,cols], 2, function(x) as.numeric(as.character(x)));
ent <- ent[complete.cases(ent), ]
fac_ent <- psych::fa(ent, rotate="varimax", fm="pa", scores="Bartlett")

tibble(Scale = c("Habit Strengt", "Surveillance", "Escapism","Passing Time", "Entertainment"),
       `Chi Square` = c(fac_hs[3]$chi, fac_surv[3]$chi, fac_esc[3]$chi, fac_pt[3]$chi, fac_ent[3]$chi),
       Fit = c(fac_hs[10]$fit, fac_surv[10]$fit, fac_esc[10]$fit, fac_pt[10]$fit, fac_ent[10]$fit),
       PA = c(fac_hs[29]$R2, fac_surv[29]$R2, fac_esc[29]$R2, fac_pt[29]$R2, fac_ent[27]$R2))
```

|  Scale		 | Chi Square |	Fit 		  | PA		|
|-------------------- | --------------- | ---------------- | ----------------- |
| Habit Strengt 	| 6.04	     | 0.811 		  | 0.825		|
| Surveillance	| 24.5	     | 0.789 	  | 0.851 		|
| Escapism	| 9.39	     | 0.742 	  | 0.826		|
| Passing Time	| 22.8 	     | 0.807 	  | 0.874		|
| Entertainment	| 0.0000857   | 0.785 	  | 0.652		|

- Variables created with an additive scale:
	- News Consumption
	- Trust in News
	- Political Efficacy
```r
#Scalability of Controls
news <- df %>%
  select(Q20_1:Q20_5) %>%
  mutate(Q20_1 = rename1(df$Q20_1),
         Q20_2 = rename1(df$Q20_2),
         Q20_3 = rename1(df$Q20_3),
         Q20_4 = rename1(df$Q20_4),
         Q20_5 = rename1(df$Q20_5))
news <- news[complete.cases(news), ]
news <- psy::cronbach(news)

trust <- df %>%
  select(Q23_1:Q23_9) %>%
  mutate(Q23_1 = rename2(df$Q23_1),
         Q23_2 = rename2(df$Q23_2),
         Q23_3 = rename2(df$Q23_3),
         Q23_4 = rename2(df$Q23_4),
         Q23_5 = rename2(df$Q23_5),
         Q23_6 = rename2(df$Q23_6),
         Q23_7 = rename2(df$Q23_7),
         Q23_8 = rename2(df$Q23_8),
         Q23_9 = rename2(df$Q23_9))
trust <- trust[complete.cases(trust), ]
trust <- psy::cronbach(trust)

polef <- df %>%
  select(Q23_1.1:Q23_3.1) %>%
  mutate(Q23_1.1 = rename2(df$Q23_1.1),
         Q23_2.1 = rename2(df$Q23_2.1),
         Q23_3.1 = rename2(df$Q23_3.1))
polef <- polef[complete.cases(polef), ]
polef <- psy::cronbach(polef)

tibble(Scale = c("News Usage", "Trust in Media", "Political Efficacy"),
       `Cronbach's Alpha` = c(news[3]$alpha, trust[3]$alpha,polef[3]$alpha))
```

| Scale			| Cronbach's Alpha	|
| -------------------------- | ------------------------- |
| News Usage		| 0.707			|

Tidy Data
-------
``` r

```

Dependent Variable
 -------
* Algorithmic Appreciation

 ``` r
# Dependent Variable

```
![Figure](../../report/figures/Distributions_DV_experiment2.png)

Independent Variable
-------
- Gratifications of the News (**H1**) 

 ``` r
# Independent Variable
```

Moderator
-------
- Epistemic Overconfidence (**H2**) 

 ``` r
# Moderator 
```




Control Variables
-------
- Trust in Media
- News Usage
- Political Efficacy
- Party ID
- Gender
- Age

 ``` r
 # Controls
 
```
![Figure](../../report/figures/Distributions_Demographics_experiment2.png)

Missing Data
====
We employ the following criteria: 

- If 10\% or less of the values on the dimension are missing, then we re-code the missing values to the overall mean. 
- If 11\% or more of the values on the dimension are missing, then we re-code the missing values to a constant (for instance 0) and include a dummy variable indicating whether the response on the covariate was missing or not.

```r

```

| Covariate 					| Percentage	|
|------------------------------------------------|---------------------|
| Likeability CDA                 		| 0.06 		|
| Likeability VVD                 		| 0.05 		|
| Likeability FvD                  		| 0.1 			|
| Perceived Extremity CDA 		| 0.06		|
| Perceived Extremity VVD	 	| 0.06		|
| Perceived Extremity FvD  		| 0.1 			|
| Popularity of FvD in Eyes of Others | 0.1			|
| Treatment                               		| 0   			|
| Left-Right Self Placement            	| 0.02 		|
| Left-Right Placement CDA      		| 0.17 		|
| Left-Right Placement FvD       		| 0.19 		|
| Left-Right Placement VVD     		| 0.16 		|
| Turnout                      			| 0   			|
| Vote Choice               			| 0    			|
| Gender                    	 		| 0   			|
| Age                          			| 0   			|
| Education               				| 0 			|
  
 We recode the missing values of the variables XXX to the mean value of the respective variables.

 ```r
# Change missing values in variables where missings are <10% to mean

``` 
 
 We recode the missing values of the variables XXX to XXX and  include the variables XXX to the data, indicating whether the response on the covariate was missing (value of `1`) or not (value of `0`).
 
 ```r
# Recode missing values to 5 and add dummies

```  
 
 Save data to `data/intermediate` and make public.
 
 ```r
 #save data
 
 write_csv(df, "../../data/intermediate/cleaned_US.csv")         

 ```
 
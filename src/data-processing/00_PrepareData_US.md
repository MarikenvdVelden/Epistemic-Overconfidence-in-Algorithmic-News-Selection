Prepare Data of US MTurk Sample
================

- DESCRIPTION: Clean and construct measures according to PAP (pp. 3--8)
- CREATES: [Cleaned Data for US Sample](data/intermediate/cleaned_us.csv)
-  DEPENDS: [Raw Data from MTurk](data/raw-private-encrypted/US_MTURK.csv)

Content
======

-   [Setup](#setup)
-   [Data](#data)
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
-  [Cleaned data](data/intermediate/cleaned_us.csv) and [codings](src/data-processing) are saved to public folders. 


``` r
df <- read_sav("data/raw-private-encrypted/MTURK_US.csv")
```

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
 
 write_csv(df, "data/intermediate/cleaned_US.csv")         

 ```
 

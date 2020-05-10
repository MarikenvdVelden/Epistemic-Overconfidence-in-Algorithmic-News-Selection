Prepare Data of Dutch PollFish Sample
================

- DESCRIPTION: Clean and construct measures according to PAP (pp. 3--8)
- CREATES: [Cleaned Data for Dutch Sample](../../data/intermediate/cleaned_NL.csv)
-  DEPENDS: [Raw Data from PollFish](../../data/raw-private-encrypted/POLLFISH.csv)

Content
======

-   [Setup](#setup)
-   [Data](#data)
    -   [Scalability](#Scalability)
    -   [Tidy Data](#Tidy-Data)
    -   [Dependent Variable](#Dependent-Variable)
    -   [Independent Variables](#Independent-Variables)
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
-  Requires access to the csv file prepared by PollFish. 
-  [Cleaned data](../../data/intermediate/cleaned_NL.csv) and [codings](src/data-processing) are saved to public folders. 


``` r
df <- read_sav("../../data/raw-private-encrypted/POLLFISH.csv")
```

Scalability
------------
* The gratifications of the news is constructed as an additive scale using a Principal Components Factor Analysis using varimax rotation, similar to Diddi and LaRose (2006), PAP p.9.

```r
# Check Scalability of DV
hs <- df %>%
  select(ugt_1:ugt_4,)
cols <- c(1:4)
hs[,cols] = apply(hs[,cols], 2, function(x) as.numeric(as.character(x)));
hs <- hs[complete.cases(hs), ]
fac_hs <- psych::fa(hs, rotate="varimax", fm="pa", scores="Bartlett")

surv <- df %>%
  select(ugt_5:ugt_11)
cols <- c(1:7)
surv[,cols] = apply(surv[,cols], 2, function(x) as.numeric(as.character(x)));
surv <- surv[complete.cases(surv), ]
fac_surv <- psych::fa(surv, rotate="varimax", fm="pa", scores="Bartlett")

esc <- df %>%
  select(ugt_12:ugt_16)
cols <- c(1:5)
esc[,cols] = apply(esc[,cols], 2, function(x) as.numeric(as.character(x)));
esc <- esc[complete.cases(esc), ]
fac_esc <- psych::fa(esc, rotate="varimax", fm="pa", scores="Bartlett")

pt <- df %>%
  select(ugt_17:ugt_21)
cols <- c(1:5)
pt[,cols] = apply(pt[,cols], 2, function(x) as.numeric(as.character(x)));
pt <- pt[complete.cases(pt), ]
fac_pt <- psych::fa(pt, rotate="varimax", fm="pa", scores="Bartlett")

ent <- df %>%
  select(ugt_22:ugt_23)
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
| Habit Strengt 	| 24.2	     | 0.911 		  | 0.876		|
| Surveillance	| 37.7	     | 0.951 	  | 0.920 		|
| Escapism	| 12.0	     | 0.941 	  | 0.915		|
| Passing Time	| 13.6 	     | 0.938 	  | 0.908		|
| Entertainment	| 0.000107     | 0.961 	  | 0.859		|

- Variables created with an additive scale:
	- News Consumption
	- Trust in News
	- Political Efficacy
```r
#Scalability of Controls
news <- df %>%
  select(mediagebruik_1:mediagebruik_9) 
news <- news[complete.cases(news), ]
news <- psy::cronbach(news)

trust <- df %>%
  select(trust_media_1:trust_media_9)
trust <- trust[complete.cases(trust), ]
trust <- psy::cronbach(trust)

polef <- df %>%
  select(pol_efficacy_1:pol_efficacy_5)
polef <- polef[complete.cases(polef), ]
polef <- psy::cronbach(polef)

tibble(Scale = c("News Usage", "Trust in Media", "Political Efficacy"),
       `Cronbach's Alpha` = c(news[3]$alpha, trust[3]$alpha,polef[3]$alpha))
```

| Scale			| Cronbach's Alpha	|
| -------------------------- | ------------------------- |
| News Usage		| 0.808			|
| Trust in Media		| 0.804			|
| Political Efficacy	| 0.584			|

Tidy Data
-------
``` r
# Mutate data
df <-  df %>%
  mutate(news = round((rename1(df$Q20_1) + rename1(df$Q20_2) + rename1(df$Q20_3) +
                         rename1(df$Q20_4) + rename1(df$Q20_5))/5, digits = 0),
         hs = round(rename2(df$Q1_1) + rename2(df$Q1_2) + rename2(df$Q1_3) +
                      rename2(df$Q1_4)/4, digits = 0),
         surv = round((rename2(df$Q1_5) + rename2(df$Q1_6) + rename2(df$Q1_7) + 
                         rename2(df$Q1_8) + rename2(df$Q1_9) + rename2(df$Q1_10) +
                         rename2(df$Q1_11))/7, digits = 0),
         esc = round((rename2(df$Q1_12) + rename2(df$Q1_13) + rename2(df$Q1_14) + 
                        rename2(df$Q1_15) + rename2(df$Q1_16))/5, digits = 0),
         pt = round((rename2(df$Q1_17) + rename2(df$Q1_18) + rename2(df$Q1_19) + 
                       rename2(df$Q1_20) + rename2(df$Q1_21))/5, digits = 0),
         ent = round((rename2(df$Q1_22) + rename2(df$Q1_23))/2, digits = 0),
         algo_app = (rename3(df$Q3_1) + rename3(df$Q4_1) + rename3(df$Q6_1) +
                       rename3(df$Q8_1)),
         trust = round((rename2(df$Q23_1) + rename2(df$Q23_2) + rename2(df$Q23_3) +
                        rename2(df$Q23_4) + rename2(df$Q23_5) + rename2(df$Q23_6) +
                        rename2(df$Q23_7) + rename2(df$Q23_8) + rename2(df$Q23_9))/9, 
                       digits = 0),
         polef = round((rename2(df$Q23_1.1) + rename2(df$Q23_2.1) + rename2(df$Q23_3.1))/3, 
                       digits = 0),
         Q11 = str_to_lower(Q11, locale = "en"),
         Q11 = ifelse(Q11 == "second", 1,
               ifelse(Q11 == "second place", 1,
               ifelse(Q11 == "2", 1,
               ifelse(Q11 == "2nd", 1,
               ifelse(Q11 == "second place. if you guessed first place, the person in first place is still there.",
                       1, 0))))),
         Q12 = ifelse(Q12 == "8", 1, 0),
         Q14 = str_to_lower(Q14),
         Q14 = ifelse(Q14 == "emily", 1,
               ifelse(Q14 == "emily april may", 1, 0)),
         Q15 = str_to_lower(Q15),
         Q15 = ifelse(Q15 == "0", 1,
               ifelse(Q15 == "0 cubic feet", 1,
               ifelse(Q15 == "none", 1,
               ifelse(Q15 == "zero", 1,
               ifelse(Q15 == "there is no dirt in a hole", 1,
               ifelse(Q15 == "there's no dirt in a hole", 1, 0)))))),     
         Q16 = ifelse(Q16 == ".05", 1,
               ifelse(Q16 == "$0.05", 1,
               ifelse(Q16 == "0.05", 1,
               ifelse(Q16 == "05", 1,
               ifelse(Q16 == "5", 1,
               ifelse(Q16 == "5 cents", 1, 0)))))),
         Q17 = str_to_lower(Q17),
         Q17 = str_replace(df$Q17, "5", "1"),
         Q17 = ifelse(Q17 == "1", 1, 0),
         Q18 = str_to_lower(Q18),
         Q18 = str_replace(df$Q18, "47", "1"),
         Q18 = ifelse(Q18 == "1", 1, 0),
         correct = (Q11 + Q12 + Q14 + Q15 + Q16 + Q17 + Q18),
         eo = Q19 - correct,
         pid = Q31,
         pid = recode(Q31, "Independent"= "Other", "Something Else" = "Other"),
         pid =factor(pid, levels = c("Other", "Democrat", "Republican")),
         gender = Q37,
         gender = recode(Q37, "Transgender Female" = "Female"),
         gender = na_if(gender, "Prefer not to answer"),
         gender = factor(gender, levels = c("Female", "Male")),
         age = (2019 - Q39)
         ) %>%
  select(ResponseId, news, hs, surv, esc, pt, ent, 
         algo_app, trust, polef, eo, pid, gender, age) 
         
#Check Correlations between Main Variables
df%>%
  select(algo_app, hs, surv, esc, ent, pt, eo) %>%
  ggstatsplot::ggcorrmat(
    type = "robust", # correlation method
    sig.level = 0.05, # threshold of significance
    p.adjust.method = "holm", # p-value adjustment method for multiple comparisons
    cor.vars = c(algo_app:eo), # a range of variables can be selected
    cor.vars.names = c(
      "Algorithmic Appreciation", # variable names
      "Habit Strength",
      "Surveillance",
      "Escapism",
      "Entertainment",
      "Passing Time",
      "Epistemic Overconfidence"
    ),
    matrix.type = "upper", # type of visualization matrix
    colors = c("#B2182B", "white", "darkgreen"),
    title = "Correlalogram for Variables under Study",
  )         
         
```
![Figure](../../report/figures/Corrplot_US.png)



Dependent Variable
 -------
* Algorithmic Appreciation

 ``` r
##  Dependent Variable
tibble(values = round(table(df$algo_app)/dim(df)[1],2),
       algo_app = 0:4) %>%
  ggplot(aes(x = algo_app, y = values)) +
  geom_col(fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y="", title = "Dependent Variable: Algorithmic Appreciation",
       subtitle = "Mean: 2.15, Standard Deviation: 1.38")
ggsave("../../report/figures/Distributions_DV_US.png", width=8, height=4, dpi=900)
```

![Figure](../../report/figures/Distributions_DV_US.png)

Independent Variables
-------
- Gratifications of the News (**H1**) 
	- Entertainment
	- Escapisme
	- Habit Strength
	- Passing Time
	- Surveillance

 ``` r
##  Independent Variables
rbind(tibble(freq = round(table(df$hs)/dim(df)[1],2),
             values = 1:7,
             id = "Habit Strength \n Mean: 4.78, Standard Deviation: 1.94"), 
      tibble(freq = round(table(df$surv)/dim(df)[1],2),
             values = 1:7,
             id = "Surveillance \n Mean: 5, Standard Deviation: 1.24"),
      tibble(freq = round(table(df$esc)/dim(df)[1],2),
             values = 1:7,
             id = "Escapism \n Mean: 4.08, Standard Deviation: 1.57"),
      tibble(freq = round(table(df$pt)/dim(df)[1],2),
             values = 1:7,
             id = "Passing Time \n Mean: 4.30, Standard Deviation: 1.59"),
      tibble(freq = round(table(df$ent)/dim(df)[1],2),
             values = 1:7,
             id = "Entertainment \n Mean: 4.37, Standard Deviation: 1.64")) %>%
  ggplot(aes(x = values, y = freq)) +
  geom_col(fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ id, ncol = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "", y="", title = "Independent Variable: Gratifications of the News") 
ggsave("../../report/figures/Distributions_IV_US.png", width=8, height=6, dpi=900)
```

![Figure](../../report/figures/Distributions_IV_US.png)


Moderator
-------
- Epistemic Overconfidence (**H2**) 

 ``` r
#Moderator
tibble(values = round(table(df$eo)/dim(df)[1],2),
       eo = c(-6,-3:7)) %>%
  ggplot(aes(x = eo, y = values)) +
  geom_bar(stat = "identity", fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = -6:7) +
  labs(x = "", y="", title = "Moderator: Epistemic Overconfidence",
       subtitle = "Mean: 2.46, Standard Deviation: 2.28") 
ggsave("../../report/figures/Distributions_Moderator_US.png", width=8, height=4, dpi=900)
```

![Figure](../../report/figures/Distributions_Moderator_US.png)


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
df <- df%>%
  mutate(age_group = ifelse(age > 21 & age < 30, "20's",
                     ifelse(age > 29 & age < 40, "30's",
                     ifelse(age > 39 & age < 50, "40's",
                     ifelse(age > 49 & age < 60, "50's",
                     ifelse(age > 59 & age < 70, "60's",
                     ifelse(age > 69 & age < 80, "70's", NA)))))))
         
rbind(tibble(freq = round(table(df$trust)/dim(df)[1],2),
             values = 1:7,
             id = "Trust in Media \n Mean: 4.91, Standard Deviation: 1.10"), 
      tibble(freq = round(table(df$news)/dim(df)[1],2),
             values = 0:7,
             id = "News Usage \n Mean: 4.42, Standard Deviation: 1.56"),
      tibble(freq = round(table(df$polef)/dim(df)[1],2),
             values = 1:7,
             id = "Political Efficacy \n Mean: 5.02, Standard Deviation: 1.11"),
      tibble(freq = round(table(df$pid)/dim(df)[1],2),
             values = levels(df$pid),
             id = "Party ID \n Median: Democrat"),
      tibble(freq = round(table(df$gender)/dim(df)[1],2),
             values = levels(df$gender),
             id = "Gender \n Median: Male"),
      tibble(freq = round(table(df$age_group)/dim(df)[1],2),
             values = c("20's", "30's", "40's", "50's", "60's", "70's"),
             id = "Age \n Mean: 38.74, Standard Deviation: 12.33")) %>%
  ggplot(aes(x = values, y = freq)) +
  facet_wrap(~ id, ncol = 3, scales = "free") +
  geom_col(fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  #scale_x_continuous(breaks = 1:7) +
  labs(x = "", y="", title = "Independent Variable: Gratifications of the News") 
ggsave("../../report/figures/Distributions_Controls_US.png", width=10, height=6, dpi=900) 
```
![Figure](../../report/figures/Distributions_Controls_US.png)

Missing Data
====
We employ the following criteria: 

- If 10\% or less of the values on the dimension are missing, then we re-code the missing values to the overall mean. 
- If 11\% or more of the values on the dimension are missing, then we re-code the missing values to a constant (for instance 0) and include a dummy variable indicating whether the response on the covariate was missing or not.

```r
#Check Missing Values
tibble(Covariate = c("Algorithmic Appreciation", "UGT: Entertainment",
                     "UGT: Escapism", "UGT: Habit Strength", "UGT: Passing Time", 
                     "UGT: Surveillance","Epistemic Overconfidence",
                     "Age","Gender","News Usage",
                     "Party ID", "Political Efficacy",
                     "Trust in Media"),
       Percentage =c(round(sum(is.na(df$algo_app))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$ent))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$esc))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$hs))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$pt))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$surv))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$eo))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$age))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$gender))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$news))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$pid))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$polef))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$trust))/prod(dim(df)[1]),2)))
```

| Covariate 					| Percentage	|
|------------------------------------------------|---------------------|
| Algorithmic Appreciation			| 0			|   
| UGT: Entertainment            		| 0			|
| UGT: Escapism                  		| 0			|   
| UGT: Habit Strength            		| 0			|   
| UGT: Passing Time              		| 0   			|
| UGT: Surveillance              		| 0   			|
| Epistemic Overconfidence       	| 0.04		|
| Age                            			| 0.07		|
| Gender                         			| 0.07		|
| News Usage                     		| 0   			|
| Party ID                       			| 0.07		|
| Political Efficacy             			| 0   			|
| Trust in Media                 			| 0			|
  
 We recode the missing values of the variables `Epistemic Overconfidence`, `Age`, `Gender`, and `Party ID` to the mean value of the respective variables.

 ```r
# Change missing values in variables where missings are <10% to mean
df <- df %>%
  select(-age_group) %>%
  mutate(eo = tidyr::replace_na(eo, round(mean(df$eo, na.rm=T),0)),
         age = tidyr::replace_na(age, mean(df$age, na.rm=T)),
         gender = tidyr::replace_na(gender, "Male"),
         pid = tidyr::replace_na(pid, "Democrat"))
``` 
  
 Save data to `data/intermediate` and make public.
 
 ```r
 #save data
 write_csv(df, "../../data/intermediate/cleaned_US.csv")         

 ```
 

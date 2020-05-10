Testing Hypothesis 1 and 2 using US MTurk Sample
================

-   [Setup](#setup)
-   [Data](#data)
-   [Results](#results)
    -   [Hypothesis 1](#hypothesis-1)
    -   [Hypothesis 2](#hypothesis-2)
    	- [Coefficient Plots](#coefficient-plots)
	- [Predicted Effects](#predicted-effects)

Setup
=====

Load the required packages and source the auxiliary functions from `lib/functions.R`:

``` r
source("../lib/functions.R")
```

Data
====

Load the cleaned data set for the US Mturk Sample

``` r
df <- read_csv("../../data/intermediate/cleaned_US.csv")
```

Results
====

Hypothesis 1
-------------------

This hypothesis test the main effect of gratifications of the news on algorithmic appreciation.
Using an OLS model, we will examine the effect of `algorithmic appreciation` on `gratifications of the news`, `overconfidence`, `trust in media`, `political efficacy`, `media usage`, `party id`, `age`, and `gender`. 
We expect more algorithmic appreciations for the gratifications `escapism`, `entertainment`, and `passing time` - signaled by a positive and statistically significant coefficient for these variables.
We expect less algorithmic appreciations for the gratification `surveillance` - signaled by a negative and statistically significant coefficient for the surveillance measure.

```r
# Test H1 - Main Effect of UGT on Algorithmic Appreciation
H1 <- lm(algo_app ~ esc + ent + hs + pt + surv +
           eo + news + factor(pid) + polef + trust + age +
            factor(gender),  data = df)
H1  %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable =  c("UGT: Escapism","UGT: Entertainment", "UGT: Habit Strenght",
                       "UGT: Passing Time", "UGT: Surveillance", "Epistemic Overconfidence",
                       "News Usage", "PID: Other", "PID: Republicans",
                       "Political Efficacy", "Trust in Media",
                       "Age","Gender: Male")) %>%
  select(-term) %>%
  ggplot(aes(y = variable)) +
  stat_dist_halfeyeh(aes(dist = "student_t", arg1 = df.residual(H1), 
                         arg2 = estimate, arg3 = std.error))  +
  labs(x = "", y = "") +
  geom_vline(xintercept = 0, size = .5) +
  theme_bw() +
  ggtitle("DV: Algorithmic Appreciation") +
  theme(plot.title = element_text(hjust = 0.5))
```
![Figure](../../report/figures/H1_US.png)


Hypothesis 2
-------------------------
This hypothesis test the interaction effect of epistemic overconfidence and gratifications of the news on algorithmic appreciation.
Using an OLS regression, we add an interaction effect between  `epistemic overconfidence` and the `gratifications on the news` to the above described OLS model.
We expect more algorithmic appreciations for the gratification `surveillance` for those with high values on the epistemic overconfidence measure - signaled by a positive and statistically significant coefficient for the interaction term.

### Coefficient Plots
```r
#H2 Interaction Effect Epistemic Overconfidence
H2_1 <-  lm(algo_app ~ esc *eo + ent + hs + pt + surv +
              news + factor(pid) + polef + trust + age +
              factor(gender),  data = df)
f2_1 <- H2_1  %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable =  c("UGT: Escapism","Epistemic Overconfidence", "UGT: Entertainment", 
                       "UGT: Habit Strenght", "UGT: Passing Time", "UGT: Surveillance", 
                       "News Usage", "PID: Other", "PID: Republicans",
                       "Political Efficacy", "Trust in Media",
                       "Age","Gender: Male", " Interaction"),
         id = rep("Escapism", 14)) %>%
  select(-term)

H2_2 <-  lm(algo_app ~ esc + ent*eo + hs + pt + surv +
              news + factor(pid) + polef + trust + age +
              factor(gender),  data = df)
f2_2 <- H2_2  %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable =  c("UGT: Escapism", "UGT: Entertainment", "Epistemic Overconfidence",
                       "UGT: Habit Strenght", "UGT: Passing Time", "UGT: Surveillance", 
                       "News Usage", "PID: Other", "PID: Republicans",
                       "Political Efficacy", "Trust in Media",
                       "Age","Gender: Male", " Interaction"),
         id = rep("Entertainment", 14)) %>%
  select(-term)

H2_3 <-  lm(algo_app ~ esc + ent + hs*eo + pt + surv +
              news + factor(pid) + polef + trust + age +
              factor(gender),  data = df)
f2_3 <- H2_3  %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable =  c("UGT: Escapism", "UGT: Entertainment", "UGT: Habit Strenght", 
                       "Epistemic Overconfidence","UGT: Passing Time", "UGT: Surveillance", 
                       "News Usage", "PID: Other", "PID: Republicans",
                       "Political Efficacy", "Trust in Media",
                       "Age","Gender: Male", " Interaction"),
         id = rep("Habit Strength", 14)) %>%
  select(-term)

H2_4 <-  lm(algo_app ~ esc + ent + hs + pt*eo + surv +
              news + factor(pid) + polef + trust + age +
              factor(gender),  data = df)
f2_4 <- H2_4  %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable =  c("UGT: Escapism", "UGT: Entertainment","UGT: Habit Strenght", 
                       "UGT: Passing Time", "Epistemic Overconfidence", "UGT: Surveillance", 
                       "News Usage", "PID: Other", "PID: Republicans",
                       "Political Efficacy", "Trust in Media",
                       "Age","Gender: Male", " Interaction"),
         id = rep("Passing Time", 14)) %>%
  select(-term)

H2_5 <-  lm(algo_app ~ esc + ent + hs + pt + surv*eo +
              news + factor(pid) + polef + trust + age +
              factor(gender),  data = df)
f2_5 <- H2_5  %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable =  c("UGT: Escapism", "UGT: Entertainment", "UGT: Habit Strenght", 
                       "UGT: Passing Time", "UGT: Surveillance", "Epistemic Overconfidence",
                       "News Usage", "PID: Other", "PID: Republicans",
                       "Political Efficacy", "Trust in Media",
                       "Age","Gender: Male", " Interaction"),
         id = rep("Surveillance", 14)) %>%
  select(-term)
  
 rbind(f2_1, f2_2, f2_3, f2_4, f2_5) %>%
  ggplot(aes(y = variable)) +
  facet_wrap(~ id, ncol = 2, scales = "free") +
  stat_dist_halfeyeh(aes(dist = "student_t", arg1 = df.residual(H1), 
                         arg2 = estimate, arg3 = std.error))  +
  labs(x = "", y = "") +
  geom_vline(xintercept = 0, size = .5) +
  theme_classic() +
  ggtitle("DV: Algorithmic Appreciation") +
  theme(plot.title = element_text(hjust = 0.5)) 
 ```
![Figure](../../report/figures/H2_coef_US.png)

### Predicted Effects

```r
#Placeholder
```

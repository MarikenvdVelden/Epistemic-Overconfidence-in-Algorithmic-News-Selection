Testing Hypothesis 1 and 2 using US MTurk Sample
================

-   [Setup](#setup)
-   [Data](#data)
-   [Results](#results)
    -   [Hypothesis 1](#hypothesis-1)
    -   [Hypothesis 2](#hypothesis-2)

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

This hypothesis test the main effect of algorithmic appreciation on the gratifications of the news.
Using an OLS model, we will examine the effect of algorithmic appreciation on gratifications of the news, overconfidence, trust in media, political knowledge, and media usage. 
We expect more algorithmic appreciations for the gratifications `escapism`, `entertainment`, and `passing time` - signaled by a positive and statistically significant coefficient for these variables.
We expect less algorithmic appreciations for the gratification `surveillance` - signaled by a negative and statistically significant coefficient for the surveillance measure.

```r
# Test H1 - Main Effect of UGT on Algorithmic Appreciation
covs <- c("UGT: Escapism","UGT: Entertainment", "UGT: Habit Strenght",
          "UGT: Passing Time", "UGT: Surveillance", "Epistemic Overconfidence",
          "News Usage", "PID: Other", "PID: Republicans",
          "Political Efficacy", "Trust in Media",
          "Age","Gender: Male")

H1 <- lm(algo_app ~ esc + ent + hs + pt + surv +
           eo + news + factor(pid) + polef + trust + age +
            factor(gender),  data = df)
H1  %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable =  covs) %>%
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


### Hypothesis 5
This hypothesis test the main effect of information on a coalition formation between moderate and extreme parties on the perceived extremity of parties.
Using an OLS regression, we examine the effect of being in the treatment conditions on the four different items about cooperation with ideologically extreme parties with controls: demographics (age, gender and education), and ideological self-placement.

```r
# Test H5 - Extremity (with controls)
H5_cdaW <- lm(Extreme_CDA ~ factor(Treatment) + LeRi_Selfplacement + 
                Age + factor(Sex) + Education, data = df)
f5b_1 <- H5_cdaW %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable = covs) %>%
  ggplot(aes(y = variable)) +
  stat_dist_halfeyeh(aes(dist = "student_t", arg1 = df.residual(H5_cdaW), 
                         arg2 = estimate, arg3 = std.error))  +
  labs(x = "", y = "") +
  geom_vline(xintercept = 0, size = .5) +
  theme_bw() +
  ggtitle("DV: CDA's Perceived Extremity Score") +
  theme(plot.title = element_text(hjust = 0.7))

H5_vvdW <- lm(Extreme_VVD ~ factor(Treatment) + LeRi_Selfplacement + 
                Age + factor(Sex) +  Education, data = df)
f5b_2 <-H5_vvdW %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable = covs) %>%
  ggplot(aes(y = variable)) +
  stat_dist_halfeyeh(aes(dist = "student_t", arg1 = df.residual(H5_vvdW), 
                         arg2 = estimate, arg3 = std.error))  +
  labs(x = "", y = "") +
  geom_vline(xintercept = 0, size = .5) +
  theme_bw() +
  ggtitle("DV: VVD's Perceived Extremity Score") +
  theme(plot.title = element_text(hjust = 0.7))

H5_fvdW <- lm(Extreme_FvD ~ factor(Treatment) + LeRi_Selfplacement 
              + Age + factor(Sex) + Education, data = df)
f5b_3 <- H5_fvdW %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable = covs) %>%
  ggplot(aes(y = variable)) +
  stat_dist_halfeyeh(aes(dist = "student_t", arg1 = df.residual(H5_fvdW), 
                         arg2 = estimate, arg3 = std.error))  +
  labs(x = "", y = "") +
  geom_vline(xintercept = 0, size = .5) +
  theme_bw() +
  ggtitle("DV: FvD's Perceived Extremity Score") +
  theme(plot.title = element_text(hjust = 0.7))
ggplot2.multiplot(f5b_1,f5b_2, f5b_3, cols=1)
```

![Figure](../../report/figures/H5_withcontrols.png)


### Hypothesis 6
```r
#placeholder
```


### Hypothesis 7
This hypothesis test the main effect of information on a coalition formation between moderate and extreme parties on the perceived popularity of extreme parties (Forum for Democracy) in the eyes of others.
Using an OLS regression, we examine the effect of being in the treatment conditions on the four different items about cooperation with ideologically extreme parties with controls: demographics (age, gender and education), and ideological self-placement.

```r
H7W<- lm(Pop_FvD_Other ~ factor(Treatment) + LeRi_Selfplacement 
              + Age + factor(Sex) + Education, data = df)
H7W %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable = covs) %>%
  ggplot(aes(y = variable)) +
  stat_dist_halfeyeh(aes(dist = "student_t", arg1 = df.residual(H7W), 
                         arg2 = estimate, arg3 = std.error))  +
  labs(x = "", y = "") +
  geom_vline(xintercept = 0, size = .5) +
  theme_bw() +
  ggtitle("DV: FvD's Perceived Popularity in the Eyes of Others") +
  theme(plot.title = element_text(hjust = 0.7))
ggsave("report/figures/H7_with controls.png", width=7, height=3, dpi=900)
```

![Figure](../../report/figures/H7_withcontrols.png)

### Hypothesis 8
```r
#placeholder
```

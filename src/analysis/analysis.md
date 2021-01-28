Analysis for Epistemic Overconfidence in Algorithmic News Selection
Paper
================

# Scripts

  - [Required Packages &
    Reproducibility](#required-packages-&-reproducibility)
  - [Load Data](#load-data)
  - [Analysis](#analysis)
      - [H1: Direct Effect of UGT on Algorithmic
        Appreciation](#h1:-direct-effect-of-ugt-on-algorithmic-appreciation)
      - [H2: Interaction UGT \* Epistemic
        Overconfidence](#h2:-interaction-ugt-*-epistemic-overconfidence)
      - [Predicted Effects](#predicted-effects)
  - [Exploratory Analyses](#exploratory-analyses)

## Required Packages & Reproducibility

``` r
rm(list=ls())

renv::snapshot()
```

    ## * The lockfile is already up to date.

``` r
source("../lib/functions.R")
```

## Load Data

``` r
load("../../data/intermediate/cleaned_data.RData") 
df <- df %>%
  mutate(esc = round(esc, 0),
         ent = round(ent, 0),
         hs = round(hs, 0),
         pt = round(pt, 0),
         surv = round(surv, 0))
#  pid = factor(pid, levels = c("None", "Other", "Democrat",
#                                      "Republican")))
```

## Analysis

### H1: Direct Effect of UGT on Algorithmic Appreciation

![](analysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#df <- within(df, pid <- relevel(pid, ref = "Other"))
H1 <- lm(algo_app ~ esc + ent + hs + pt + surv +
           eo + factor(missing_eo) + news + polef + trust +
           age + factor(gender) + factor(country),  data = df)
```

![](analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### H2: Interaction UGT \* Epistemic Overconfidence

``` r
H2_1 <-  lm(algo_app ~ esc *eo + ent + hs + pt + surv +
            missing_eo + news + polef + trust + age +
            factor(gender) + factor(country),  data = df)

H2_2 <-  lm(algo_app ~ esc + ent*eo + hs + pt + surv +
              factor(missing_eo) + news + polef + trust + age +
              factor(gender) + factor(country),  data = df)

H2_3 <-  lm(algo_app ~ esc + ent + hs*eo + pt + surv +
              factor(missing_eo) + news + polef + trust +
              age + factor(gender) + factor(country),  data = df)

H2_4 <-  lm(algo_app ~ esc + ent + hs + pt*eo + surv +
              factor(missing_eo) + news + polef + trust + 
              age + factor(gender) + factor(country),  data = df)

H2_5 <-  lm(algo_app ~ esc + ent + hs + pt + surv*eo +
              factor(missing_eo) + news + polef + trust + age +
              factor(gender) + factor(country),  data = df)
```

![](analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Predicted Effects

![](analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Exploratory Analyses

### Is Relationship between UGT and Epistemic Overconfidence Conditional upon Gender

![](analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Relationship between Appreciation for News Selector and Trust in Media

![](analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
Ht1 <- lm(own_select ~ esc + ent + hs + pt + surv +
           eo + factor(missing_eo) + news + polef + trust +
           age + factor(gender) + factor(country),  data = df)

Ht2 <- lm(jou_select ~ esc + ent + hs + pt + surv +
           eo + factor(missing_eo) + news + polef + trust +
           age + factor(gender) + factor(country),  data = df)

Ht3 <- lm(trust ~ own_select + jou_select + eo +
          factor(missing_eo) + news + polef + age + 
          factor(gender) + factor(country),  data = df)
```

![](analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->![](analysis_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

### Relationship between Appreciation for Journalists as News Selectors and Overconfidence

``` r
Hj1 <-  lm(jou_select ~ esc *eo + ent + hs + pt + surv +
            missing_eo + news + polef + trust + age +
            factor(gender) + factor(country),  data = df)

Hj2 <-  lm(jou_select ~ esc + ent*eo + hs + pt + surv +
              factor(missing_eo) + news + polef + trust + age +
              factor(gender) + factor(country),  data = df)

Hj3 <-  lm(jou_select ~ esc + ent + hs*eo + pt + surv +
              factor(missing_eo) + news + polef + trust +
              age + factor(gender) + factor(country),  data = df)

Hj4 <-  lm(jou_select ~ esc + ent + hs + pt*eo + surv +
              factor(missing_eo) + news + polef + trust + 
              age + factor(gender) + factor(country),  data = df)

Hj5 <-  lm(jou_select ~ esc + ent + hs + pt + surv*eo +
              factor(missing_eo) + news + polef + trust + age +
              factor(gender) + factor(country),  data = df)
```

![](analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Predicted Effects

![](analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### Relationship between Appreciation for Self Selecting the News and Overconfidence

``` r
Hs1 <-  lm(own_select ~ esc *eo + ent + hs + pt + surv +
            missing_eo + news + polef + trust + age +
            factor(gender) + factor(country),  data = df)

Hs2 <-  lm(own_select ~ esc + ent*eo + hs + pt + surv +
              factor(missing_eo) + news + polef + trust + age +
              factor(gender) + factor(country),  data = df)

Hs3 <-  lm(own_select ~ esc + ent + hs*eo + pt + surv +
              factor(missing_eo) + news + polef + trust +
              age + factor(gender) + factor(country),  data = df)

Hs4 <-  lm(own_select ~ esc + ent + hs + pt*eo + surv +
              factor(missing_eo) + news + polef + trust + 
              age + factor(gender) + factor(country),  data = df)

Hs5 <-  lm(own_select ~ esc + ent + hs + pt + surv*eo +
              factor(missing_eo) + news + polef + trust + age +
              factor(gender) + factor(country),  data = df)
```

![](analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Predicted Effects

![](analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

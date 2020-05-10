## Epistemic Overconfidence

source("../lib/functions.R")

df <- qualtrics("../../data/raw-private-encrypted/US_MTURK.csv")

# Check Scalability of DV
hs <- df %>%
  select(Q1_1:Q1_4,)
cols <- c(1:4)
hs[,cols] = apply(hs[,cols], 2, function(x) as.numeric(as.character(x)));
hs <- hs[complete.cases(hs), ]
fac_hs <- psych::fa(hs, rotate="varimax", fm="pa", scores="Bartlett")

surv <- df %>%
  select(Q1_5:Q1_11)
cols <- c(1:7)
surv[,cols] = apply(surv[,cols], 2, function(x) as.numeric(as.character(x)));
surv <- surv[complete.cases(surv), ]
fac_surv <- psych::fa(surv, rotate="varimax", fm="pa", scores="Bartlett")

esc <- df %>%
  select(Q1_12:Q1_16)
cols <- c(1:5)
esc[,cols] = apply(esc[,cols], 2, function(x) as.numeric(as.character(x)));
esc <- esc[complete.cases(esc), ]
fac_esc <- psych::fa(esc, rotate="varimax", fm="pa", scores="Bartlett")

pt <- df %>%
  select(Q1_17:Q1_21)
cols <- c(1:5)
pt[,cols] = apply(pt[,cols], 2, function(x) as.numeric(as.character(x)));
pt <- pt[complete.cases(pt), ]
fac_pt <- psych::fa(pt, rotate="varimax", fm="pa", scores="Bartlett")

ent <- df %>%
  select(Q1_22:Q1_23)
cols <- c(1:2)
ent[,cols] = apply(ent[,cols], 2, function(x) as.numeric(as.character(x)));
ent <- ent[complete.cases(ent), ]
fac_ent <- psych::fa(ent, rotate="varimax", fm="pa", scores="Bartlett")

tibble(Scale = c("Habit Strengt", "Surveillance", "Escapism","Passing Time", "Entertainment"),
       `Chi Square` = c(fac_hs[3]$chi, fac_surv[3]$chi, fac_esc[3]$chi, fac_pt[3]$chi, fac_ent[3]$chi),
       Fit = c(fac_hs[10]$fit, fac_surv[10]$fit, fac_esc[10]$fit, fac_pt[10]$fit, fac_ent[10]$fit),
       PA = c(fac_hs[29]$R2, fac_surv[29]$R2, fac_esc[29]$R2, fac_pt[29]$R2, fac_ent[27]$R2))

#Scalability of Controls
news <- df %>%
  select(Q20_1:Q20_5) %>%
  mutate(Q20_1 = rename1(df$Q20_1),
         Q20_2 = rename1(df$Q20_2),
         Q20_3 = rename1(df$Q20_3),
         Q20_4 = rename1(df$Q20_4),
         Q20_5 = rename1(df$Q20_5))
news <- news[complete.cases(news), ]
news <- psy::cronbach(news)#alpha of 0.71 

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
trust <- psy::cronbach(trust)#alpha of 0.85 

polef <- df %>%
  select(Q23_1.1:Q23_3.1) %>%
  mutate(Q23_1.1 = rename2(df$Q23_1.1),
         Q23_2.1 = rename2(df$Q23_2.1),
         Q23_3.1 = rename2(df$Q23_3.1))
polef <- polef[complete.cases(polef), ]
polef <- psy::cronbach(polef)#alpha of 0.85 

tibble(Scale = c("News Usage", "Trust in Media", "Political Efficacy"),
       `Cronbach's Alpha` = c(news[3]$alpha, trust[3]$alpha,polef[3]$alpha))

# Mutate data
df <-  df %>%
  mutate(news = round((rename1(df$Q20_1) + rename1(df$Q20_2) + rename1(df$Q20_3) +
                         rename1(df$Q20_4) + rename1(df$Q20_5))/5, digits = 0),
         hs = round((rename2(df$Q1_1) + rename2(df$Q1_2) + rename2(df$Q1_3) +
                      rename2(df$Q1_4))/4, digits = 0),
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
         pid = factor(pid, levels = c("Other", "Democrat", "Republican")),
         gender = Q37,
         gender = recode(Q37, "Transgender Female" = "Female"),
         gender = na_if(gender, "Prefer not to answer"),
         gender = factor(gender, levels = c("Female", "Male")),
         age = (2019 - Q39)
         ) %>%
  select(ResponseId, news, hs, surv, esc, pt, ent, 
         algo_app, trust, polef, eo, pid, gender, age)

#Check Correlations
# as a default this function outputs a correlation matrix plot
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



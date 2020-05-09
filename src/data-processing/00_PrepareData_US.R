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

##  Dependent Variable
tibble(values = round(table(df$algo_app)/dim(df)[1],2),
       algo_app = 0:4) %>%
  ggplot(aes(x = algo_app, y = values)) +
  geom_col(fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y="", title = "Dependent Variable: Algorithmic Appreciation") 
ggsave("../../report/figures/Distributions_DV_US.png", width=8, height=4, dpi=900)

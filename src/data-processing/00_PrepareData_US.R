rm(list=ls())

source("src/lib/Functions.R")

d <- qualtrics("Dataset/Algorithm_appreciation -English_October 17, 2019_09.58.csv")

#Tidy data
#1. News consumption (Q20_1:Q20_5)
d <-  d %>%
  mutate(news1 = rename1(d$Q20_1),
         news2 = rename1(d$Q20_2),
         news3 = rename1(d$Q20_3),
         news4 = rename1(d$Q20_4),
         news5 = rename1(d$Q20_5),
         news = round((news1 + news2 + news3 +
                         news4 + news5)/5, digits = 0))
#check reliability of scale
v1 <- select(d, news1:news5)
psy::cronbach(v1)#alpha of 0.71 

#2. News Gratifications (Q1_1:Q1_23)
#2a. Habit strenght (4 items)
d <- d %>%
  mutate(hs1 = rename2(d$Q1_1),
         hs2 = rename2(d$Q1_2),
         hs3 = rename2(d$Q1_3),
         hs4 = rename2(d$Q1_4),
         hs = round((hs1 + hs2 + hs3 +
                       hs4)/4, digits = 0))
#check reliability of scale
v1 <- select(d, hs1:hs4)
psy::cronbach(v1)#alpha of 0.84 

#2b. Surveillance (7 items)
d <- d %>%
  mutate(surv1 = rename2(d$Q1_5),
         surv2 = rename2(d$Q1_6),
         surv3 = rename2(d$Q1_7),
         surv4 = rename2(d$Q1_8),
         surv5 = rename2(d$Q1_9),
         surv6 = rename2(d$Q1_10),
         surv7 = rename2(d$Q1_11),
         surv = round((surv1 + surv2 + surv3 + surv4 +
                         surv5 + surv6 + surv7)/7, digits = 0))
#check reliability of scale
v1 <- select(d, surv1:surv7)
psy::cronbach(v1)#alpha of 0.85 

#2c. Escapism (5 items)
d <- d %>%
  mutate(esc1 = rename2(d$Q1_12),
         esc2 = rename2(d$Q1_13),
         esc3 = rename2(d$Q1_14),
         esc4 = rename2(d$Q1_15),
         esc5 = rename2(d$Q1_16),
         esc = round((esc1 + esc2 + esc3 + 
                        esc4 + esc5)/5, digits = 0))
#check reliability of scale
v1 <- select(d, esc1:esc5)
psy::cronbach(v1)#alpha of 0.85 

#2d. Pass Time (5 items)
d <- d %>%
  mutate(pt1 = rename2(d$Q1_17),
         pt2 = rename2(d$Q1_18),
         pt3 = rename2(d$Q1_19),
         pt4 = rename2(d$Q1_20),
         pt5 = rename2(d$Q1_21),
         pt = round((pt1 + pt2 + pt3 + pt4 +
                       pt5)/5, digits = 0))
#check reliability of scale
v1 <- select(d, pt1:pt5)
psy::cronbach(v1)#alpha of 0.87 

#2e. Entertainment (2 items)
d <- d %>%
  mutate(ent1 = rename2(d$Q1_22),
         ent2 = rename2(d$Q1_23),
         ent = round((ent1 + ent2)/2, digits = 0))
#check reliability of scale
v1 <- select(d, ent1:ent2)
psy::cronbach(v1)#alpha of 0.77 

##DV: Algorithmic Appreciation (Q3_1:Q8_4)
d <- d %>%
  mutate(Q3_1 = rename3(d$Q3_1),
         Q4_1 = rename3(d$Q4_1),
         Q6_1 = rename3(d$Q6_1),
         Q8_1 = rename3(d$Q8_1),
         algo_app = (Q3_1 + Q4_1 + Q6_1 + Q8_1))

## Trust in Media (Q23_1:Q23_9)
d <- d %>%
  mutate(trust1 = rename2(d$Q23_1),
         trust2 = rename2(d$Q23_2),
         trust3 = rename2(d$Q23_3),
         trust4 = rename2(d$Q23_4),
         trust5 = rename2(d$Q23_5),
         trust6 = rename2(d$Q23_6),
         trust7 = rename2(d$Q23_7),
         trust8 = rename2(d$Q23_8),
         trust9 = rename2(d$Q23_9),
         trust = round((trust1 + trust2 + trust3 +
                          trust4 + trust5 + trust6 +
                          trust7 + trust8 + trust9)/9, digits = 0))

#check reliability of scale
v1 <- select(d, trust1:trust9)
psy::cronbach(v1)#alpha of 0.85 

## Political Efficacy (Q23_1.1:Q23_3.1)
d <- d %>%
  mutate(polef1 = rename2(d$Q23_1),
         polef2 = rename2(d$Q23_2),
         polef3 = rename2(d$Q23_3),
         polef = round((polef1 + polef2 + polef3)/3, digits = 0))

#check reliability of scale
v1 <- select(d, polef1:polef3)
psy::cronbach(v1)#alpha of 0.57 

## Epistemic Overconfidence (Q11:Q19)
d$Q11 <- tolower(d$Q11)
d$Q11 <- ifelse(d$Q11=="second", 1,
                ifelse(d$Q11=="second place", 1,
                       ifelse(d$Q11=="2", 1,
                              ifelse(d$Q11=="2nd", 1,
                                     ifelse(d$Q11=="second place. if you guessed first place, the person in first place is still there.",1,
                                            0)))))
d$Q12 <- ifelse(d$Q12=="8", 1, 0)
d$Q12 <- as.numeric(d$Q12)
d$Q14 <- tolower(d$Q14)
d$Q14 <- ifelse(d$Q14=="emily", 1,
                ifelse(d$Q14=="emily april may", 1, 0))
d$Q15 <- tolower(d$Q15)
d$Q15 <- ifelse(d$Q15=="0", 1,
                ifelse(d$Q15=="0 cubic feet", 1,
                       ifelse(d$Q15=="none", 1,
                              ifelse(d$Q15=="zero", 1,
                                     ifelse(d$Q15=="there is no dirt in a hole", 1,
                                            ifelse(d$Q15=="there's no dirt in a hole.", 1, 0))))))
d$Q16 <- ifelse(d$Q16==".05", 1,
                ifelse(d$Q16=="$0.05", 1,
                       ifelse(d$Q16=="0.05", 1,
                              ifelse(d$Q16=="05", 1,
                                     ifelse(d$Q16=="5", 1,
                                            ifelse(d$Q16=="5 cents", 1, 0))))))
d$Q17 <- tolower(d$Q17)
d$Q17 <- ifelse(d$Q17=="5", 1,
                ifelse(d$Q17=="5 min", 1,
                       ifelse(d$Q17=="5 mins", 1,
                              ifelse(d$Q17=="5 minutes", 1,
                                     ifelse(d$Q17=="5min", 1,
                                            ifelse(d$Q17=="5minutes", 1, 0))))))
d$Q18 <- tolower(d$Q18)
d$Q18 <- ifelse(d$Q18=="47", 1,
                ifelse(d$Q18=="47 days", 1,
                       ifelse(d$Q18=="47days", 1, 0)))

d <- d %>%
  mutate(correct = (Q11 + Q12 + Q14 + Q15 + Q16 + Q17 + Q18),
         eo = Q19 - correct)

## Controls: PID, Gender, Age 
d <- d %>%
  mutate(pid = Q31,
         gender = Q37,
         age = 2019 - Q39)

d$pid <- ifelse(d$pid=="Independent", "Other",
                ifelse(d$pid=="Something Else", "Other", d$pid))
d$pid <- factor(d$pid, 
                levels = c("Other", "Democrat", "Republican"),
                labels = c("Other", "Democrat", "Republican"))

d$gender <- ifelse(d$gender=="Prefer not to answer", NA,
                   ifelse(d$gender=="Transgender Female", "Female", d$gender))
d$gender <- factor(d$gender, 
                   levels = c("Female", "Male"),
                   labels = c("Female", "Male"))

d <- select(d, ResponseId, news, hs, surv, esc, pt, ent, 
            algo_app, trust, polef, eo, pid, gender, age)

rm(v1)

#EDA
## Sample info
tmp <- tibble(values = round(table(d$pid)/dim(d)[1],2),
              pid = c("Other", "Democrat", "Republican"))
tmp$pid <- factor(tmp$pid, 
                  levels = c("Other", "Democrat", "Republican"),
                  labels = c("Other", "Democrat", "Republican"))

p1 <- ggplot(tmp, aes(x = pid, y = values)) +
  geom_bar(stat = "identity", fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y="", title = "Party Identity") +
  scale_y_continuous(labels = scales::percent)

tmp <- tibble(values = round(table(d$gender)/dim(d)[1],2),
              gender = c("Female", "Male"))
p2 <- ggplot(tmp, aes(x = gender, y = values)) +
  geom_bar(stat = "identity", fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y="", title = "Gender") +
  scale_y_continuous(labels = scales::percent)

tmp <- d$age[-which(is.na(d$age))]
tmp <- tibble(values = round(table(d$age)/dim(d)[1],2),
              age = unique(tmp))
p3 <- ggplot(tmp, aes(x = age, y = values)) +
  geom_bar(stat = "identity", fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y="", title = "Age") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(20,25,30,35,40,45,50,55,60,
                                65,70,75,80))

multiplot(p2,p3,p1, cols = 3)

## Visualization DV
tmp <- tibble(values = round(table(d$algo_app)/dim(d)[1],2),
              algo_app = 0:4)
dv <- ggplot(tmp, aes(x = algo_app, y = values)) +
  geom_bar(stat = "identity", fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y="", title = "Dependent Variable: Algorithmic Appreciation") 

tmp <- rbind(tibble(freq = round(table(d$hs)/dim(d)[1],2),
                    values = 1:7,
                    id = "Habit Strength"), 
             tibble(freq = round(table(d$surv)/dim(d)[1],2),
                    values = 1:7,
                    id = "Surveillance"),
             tibble(freq = round(table(d$esc)/dim(d)[1],2),
                    values = 1:7,
                    id = "Escapism"),
             tibble(freq = round(table(d$pt)/dim(d)[1],2),
                    values = 1:7,
                    id = "Passing Time"),
             tibble(freq = round(table(d$ent)/dim(d)[1],2),
                    values = 1:7,
                    id = "Entertainment"))

iv <- ggplot(tmp, aes(x = values, y = freq)) +
  geom_bar(stat = "identity", fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(.~id) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "", y="", title = "Independent Variable: Gratifications of the News") 

tmp <- tibble(values = round(table(d$eo)/dim(d)[1],2),
              eo = c(-6,-3:7))
mod <- ggplot(tmp, aes(x = eo, y = values)) +
  geom_bar(stat = "identity", fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = -6:7) +
  labs(x = "", y="", title = "Moderator: Epistemic Overconfidence") 

multiplot(dv,iv,mod, cols = 1)

# Covariates
tmp <- tibble(values = round(table(d$news)/dim(d)[1],2),
              news = 0:7)
p1 <- ggplot(tmp, aes(x = news, y = values)) +
  geom_bar(stat = "identity", fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 0:7) +
  labs(x = "", y="", title = "Frequency of News Usage") 

tmp <- tibble(values = round(table(d$trust)/dim(d)[1],2),
              trust = 1:7)
p2 <- ggplot(tmp, aes(x = trust, y = values)) +
  geom_bar(stat = "identity", fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 0:7) +
  labs(x = "", y="", title = "Trust in Media") 

tmp <- tibble(values = round(table(d$polef)/dim(d)[1],2),
              polef = 1:7)
p3 <- ggplot(tmp, aes(x = polef, y = values)) +
  geom_bar(stat = "identity", fill = "gray85", colour = "black") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "", y="", title = "Political Efficacy") 

multiplot(p1, p3, p2, cols = 3)

## Patterns
hs <- ggplot(d, aes(y = algo_app, x = hs)) +
  geom_smooth(colour = "black") +
  theme_classic() +
  labs(x = "Habith Strength", y = "Algorithmic Appreciation") +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 0:4)
surv <- ggplot(d, aes(y = algo_app, x = surv)) +
  geom_smooth(colour = "black") +
  theme_classic() +
  labs(x = "Surveilance", y = "Algorithmic Appreciation") +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 0:4)
esc <- ggplot(d, aes(y = algo_app, x = esc)) +
  geom_smooth(colour = "black") +
  theme_classic() +
  labs(x = "Escapism", y = "Algorithmic Appreciation") +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 0:4)
pt <- ggplot(d, aes(y = algo_app, x = pt)) +
  geom_smooth(colour = "black") +
  theme_classic() +
  labs(x = "Passing Time", y = "Algorithmic Appreciation") +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 0:4)
ent <- ggplot(d, aes(y = algo_app, x = ent)) +
  geom_smooth(colour = "black") +
  theme_classic() +
  labs(x = "Entertainment", y = "Algorithmic Appreciation") +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 0:4)
eo <- ggplot(d, aes(y = algo_app, x = eo)) +
  geom_smooth(colour = "black") +
  theme_classic() +
  labs(x = "Epistemic Overconfidence", y = "Algorithmic Appreciation") +
  scale_x_continuous(breaks = -6:7) +
  scale_y_continuous(breaks = 0:4)
multiplot(ent, pt, esc, surv, hs, cols = 3)

# Analysis
#Dichotomize EO for interactions
d$eo_factor <- ifelse(d$eo<3, "No Epistimic Overconfidence",
               ifelse(d$eo>2, "High Levels of Epistemic Overconfidence", NA))
d$eo_factor <- factor(d$eo_factor,
                      levels = c("No Epistimic Overconfidence", 
                                 "High Levels of Epistemic Overconfidence"),
                      labels = c("No Epistimic Overconfidence", 
                                 "High Levels of Epistemic Overconfidence"))

source("Analysis/analysis.R")

## TO DO: check effects for all rankings seperately.

#save(d, file = "replication_data.RData")


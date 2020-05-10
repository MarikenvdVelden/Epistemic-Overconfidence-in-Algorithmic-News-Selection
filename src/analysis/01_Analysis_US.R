# Analysis
source("../lib/functions.R")

df <- read_csv("../../data/intermediate/cleaned_US.csv")


#H1
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

#H2
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

d <- ggeffects::ggpredict(H2_1, terms = c("esc","eo"))
tmp = tibble(freq = table(df$hs),
             values = 1:7)
eo.labs <- c("Epistemic Overconfidence \n 1st Quantile", 
                    "Epistemic Overconfidence \n 2nd Quantile", 
                    "Epistemic Overconfidence \n 3rd Quantile")
names(eo.labs) <- c("0.2", "2.4", "4.7")
ggplot(d, aes(x, predicted)) + 
  facet_grid(cols = vars(group),
             labeller = labeller(group = eo.labs)) +
  geom_line(aes(linetype=group)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), alpha=0.15) +
  scale_fill_manual(values = c("gray85","gray85","gray85")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_x_continuous(breaks = 1:7) +
  theme_classic() +
  labs(x = "Escapism", y = "Predicted Values of Algorithmic Apprecation") +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") +
  geom_bar(data = tmp, aes(x=values, y=freq/400), stat = "identity",
           fill="white", colour = "gray80") 

df <- ggeffects::ggpredict(m3, terms = c("surv","eo_factor"))
tmp = data.frame(table(d$surv))
tmp$Var1 = as.character(tmp$Var1)
tmp$Var1 = as.integer(tmp$Var1)
surv_eo <- ggplot(df, aes(x, predicted)) + 
  geom_line(aes(linetype=group)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), alpha=0.15) +
  scale_fill_manual(values = c("gray85","gray85","gray85")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_x_continuous(breaks = 1:7) +
  theme_classic() +
  labs(x = "Surveilance", y = "Predicted Values of Algorithmic Apprecation") +
  theme(legend.title=element_blank()) +
  theme(legend.position="none")  +
  geom_bar(data = tmp, aes(x=Var1, y=Freq/400), stat = "identity",
           fill="white", colour = "gray80") 

df <- ggeffects::ggpredict(m4, terms = c("esc","eo_factor"))
tmp = data.frame(table(d$esc))
tmp$Var1 = as.character(tmp$Var1)
tmp$Var1 = as.integer(tmp$Var1)
esc_eo <-ggplot(df, aes(x, predicted)) + 
  geom_line(aes(linetype=group)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), alpha=0.15) +
  scale_fill_manual(values = c("gray85","gray85","gray85")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_x_continuous(breaks = 1:7) +
  theme_bw() +
  labs(x = "Escapism", y = "Predicted Values of Algorithmic Apprecation") +
  theme(legend.title=element_blank()) +
  theme(legend.position="none")  +
  geom_bar(data = tmp, aes(x=Var1, y=Freq/400), stat = "identity",
           fill="white", colour = "gray80") 

df <- ggeffects::ggpredict(m5, terms = c("pt","eo_factor"))
tmp = data.frame(table(d$pt))
tmp$Var1 = as.character(tmp$Var1)
tmp$Var1 = as.integer(tmp$Var1)
pt_eo <- ggplot(df, aes(x, predicted)) + 
  geom_line(aes(linetype=group)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), alpha=0.15) +
  scale_fill_manual(values = c("gray85","gray85","gray85")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_x_continuous(breaks = 1:7) +
  theme_classic() +
  labs(x = "Passing Time", y = "Predicted Values of Algorithmic Apprecation") +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") +
  geom_bar(data = tmp, aes(x=Var1, y=Freq/400), stat = "identity",
           fill="white", colour = "gray80") 

df <- ggeffects::ggpredict(m6, terms = c("ent","eo_factor"))
tmp = data.frame(table(d$ent))
tmp$Var1 = as.character(tmp$Var1)
tmp$Var1 = as.integer(tmp$Var1)
ent_eo <- ggplot(df, aes(x, predicted)) + 
  geom_line(aes(linetype=group)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), alpha=0.15) +
  scale_fill_manual(values = c("gray85","gray85","gray85")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_x_continuous(breaks = 1:7) +
  theme_classic() +
  labs(x = "Entertainment", y = "Predicted Values of Algorithmic Apprecation") +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") +
  geom_bar(data = tmp, aes(x=Var1, y=Freq/400), stat = "identity",
           fill="white", colour = "gray80") 

multiplot(ent_eo,pt_eo, esc_eo, surv_eo, hs_eo, cols = 3)


texreg::texreg(list(m1, m2, m3, m4, m5, m6), float.pos="h",
               custom.model.names = c("Model 1","Model2", "Model 3", 
                                      "Model 4", "Model 5", "Model 6"),
               #   custom.coef.names=c("Intercept","Habit Strenght", "Surveillance",
               #                       "Escapism","Passing Time", "Entertainment",
               #                       "Epistemic Overconfidence", "Trust in Media",
               #                       "News Usage","Political Efficacy",
               #                       "PID: Democrat (Ref. Other)",
               #                       "PID: Republican (Ref. Other)",
               #                       "Male (ref. Female)",
               #                       "Age", "Habit Strength * Epistemic Overconfidence",
               #                       "Surveillance * Epistemic Overconfidence",
               #                       "Escapism * Epistemic Overconfidence",
               #                       "Passing Time * Epistemic Overconfidence",
               #                       "Entertainment * Epistemic Overconfidence"),
               stars = c(0.05,0.1),
               digits=2,
               caption=c("Overview Models"),
               label=c("ols"),
               dcolumn=TRUE)
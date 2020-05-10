m1 <- lm(algo_app ~ hs + surv + esc + pt + ent + 
           eo_factor + trust + news + polef + as.factor(pid) +
            as.factor(gender) + age, data = d)
m2 <- lm(algo_app ~ hs*eo_factor + surv + esc + pt + ent + 
             trust + news + polef + as.factor(pid) +
            as.factor(gender) + age, data = d)
m3 <- lm(algo_app ~ hs + surv*eo_factor + esc + pt + ent + 
           trust + news + polef + as.factor(pid) +
           as.factor(gender) + age, data = d)
m4 <- lm(algo_app ~ hs + surv + esc*eo_factor + pt + ent + 
           trust + news + polef + as.factor(pid) +
           as.factor(gender) + age, data = d)
m5 <- lm(algo_app ~ hs + surv + esc + pt*eo_factor + ent + 
           trust + news + polef + as.factor(pid) +
           as.factor(gender) + age, data = d)
m6 <- lm(algo_app ~ hs + surv + esc + pt + ent*as.factor(eo_factor) + 
           trust + news + polef + as.factor(pid) +
           as.factor(gender) + age, data = d)

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

out <- summary(m1)
df <- data.frame(coef = out$coefficients[,1],
                 lower = (out$coefficients[,1] - (1.645 *out$coefficients[,2])),
                 higher = (out$coefficients[,1] + (	1.645 *out$coefficients[,2])),
                 variable = c("(Intercept)","Habit Strength", "Surveillance",
                              "Escapism","Pass Time", "Entertainment",
                              "Epistemic Overconfidence",
                              "Trust in Media", "News Usage",
                              "Political Efficacy", "PID: Democrat",
                              "PID: Republican", "Male", "Age"))
df <- df[-1,]
df <- df[13:1,]
df <- df[c(1:4,7,5:6,8,10,12,13,11,9),]

df$variable <- factor(df$variable,
                      levels = c("Age", "Male", "PID: Republican",
                                 "PID: Democrat", "Trust in Media",
                                 "Political Efficacy",
                                 "News Usage", "Epistemic Overconfidence",
                                 "Pass Time", "Surveillance",
                                 "Habit Strength", "Escapism",
                                 "Entertainment"),
                      labels = c("Age", "Male", "PID: Republican",
                                 "PID: Democrat", "Trust in Media",
                                 "Political Efficacy",
                                 "News Usage", "Epistemic Overconfidence",
                                 "Pass Time", "Surveillance",
                                 "Habit Strength", "Escapism",
                                 "Entertainment"))

ggplot(df, aes(x = variable, y = coef)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=0) +
  theme_classic() +
  labs(x ="", y = "Regression Model Predicting Algorithmic Appreciation") +
  coord_flip() +
  geom_hline(yintercept = 0, size = .5, linetype = "dashed")

df <- ggeffects::ggpredict(m2, terms = c("hs","eo_factor"))
tmp = data.frame(table(d$hs))
tmp$Var1 = as.character(tmp$Var1)
tmp$Var1 = as.integer(tmp$Var1)
hs_eo <- ggplot(df, aes(x, predicted)) + 
  geom_line(aes(linetype=group)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), alpha=0.15) +
  scale_fill_manual(values = c("gray85","gray85","gray85")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_x_continuous(breaks = 1:7) +
  theme_classic() +
  labs(x = "Habit Strength", y = "Predicted Values of Algorithmic Apprecation") +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") +
  geom_bar(data = tmp, aes(x=Var1, y=Freq/400), stat = "identity",
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
  theme_classic() +
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

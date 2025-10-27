library(datadictionary)
library(gtsummary)
library(flextable)
library(parallelly)
library(future)
library(jtools)
library(car)
library(forcats)
library(interactions)
library(emmeans)
library(ResourceSelection)
library(survival)
library(ggfortify)
library(survminer)
library(multcomp)
#rm(list = ls())


####################### REPORT ########################

Table1 <- tbl_summary(data = first6,
                      include = c(qoi, t2qoi, above200, AGE, GENDER, RACE),
                      by = twoclass,
                      type = all_dichotomous() ~ "categorical", # display all categorical levels
                      missing = "no",
                      digits = all_continuous() ~ 1,
                      label = list(qoi ~ "AIDS event (0 = No, 1 = Yes)",
                                   t2qoi ~ "Time to AIDS event",
                                   above200 ~ "CD4+ >= 200 at baseline (0 = No, 1 = Yes)",
                                   AGE ~ "Age at enrollment",
                                   GENDER ~ "Gender(1 = male, 2 = female)",
                                   RACE ~ "Race(1=latino 2=afr.am. 3=white 4=other)")
)%>% 
  add_n() %>% # add column with total number of non-missing observations
  add_overall() %>% # add column with overall summary statistics add_stat_label() %>% # add statistics labels to each row, rather than footnote
  #modify_header(label = "") %>%
  modify_header(label ~ "",
                stat_1 ~ "**Two-class strategy**,N = {n}",
                stat_2 ~ "**Three-class strategy**, N = {n}")%>%
  bold_labels()
print(Table1)


as_flex_table(Table1) %>%
  save_as_docx(path = "Homeworkreport8_Table1.docx")



#Creating survival for each time
kmplot_AIDS <- survfit(Surv(t2qoi, qoi) ~ twoclass,
                           data = first6)
#Survival plot
autoplot(kmplot_AIDS, conf.int = FALSE, censor.shape = "X") +
  xlab("Time (months)") +
  ylab("AIDS event") +
  labs(title = "Survival time for AIDS", subtitle = "By Strategy",
       caption = "Event = AIDS (1), 0 = censored",
       color = "Treatment strategy") +
  scale_color_discrete(labels = c("Two-Class", "Three-class")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

#log rank test (rho = 0)
survdiff(Surv(t2qoi, qoi) ~ twoclass,
         data = first6,
         rho = 0)
#Wilcoxon test (rho = 1)
survdiff(Surv(t2qoi, qoi) ~ twoclass, data = first6,
         rho = 1)


AIDSclass <- coxph(Surv(t2qoi, qoi) ~ factor(twoclass) + above200 + factor(UNIT),
            data = first6)
summary(AIDSclass)

AIDSclass_resid <- cox.zph(AIDSclass)
ggcoxzph(AIDSclass_resid)


################################ Problem set ############################

#creating Cox model 
m1 <- coxph(Surv(t2primary, primary) ~ factor(twoclass) + above200 + factor(UNIT),
            data = first6)
summary(m1)

#Schoenfeld test
m1_resid <- cox.zph(m1)
ggcoxzph(m1_resid)

#Creating new age variable: 1 = 40 or older, 0 = below 40
first6_twoclass$agege40 <- ifelse(first6_twoclass$AGE >= 40, 1, 0)

#plot to verify agege40 coded correctly
plot(first6_twoclass$AGE, first6_twoclass$agege40,
     xlab = "AGE",
     ylab = "agege40 (1 = ≥40, 0 = <40)",
     main = "Scatter plot of age variables",
     col = "purple")
abline(v = 40, col = "red")


#model to check if NNRTI vs PI differs by age group
m2 <- coxph(Surv(t2primary, primary)~ RANDGRP*agege40 + above200 + factor(UNIT),
            data = first6_twoclass)
summary(m2)

#hazard ratio estimate
m2.slopes<-emmeans(m2,pairwise~RANDGRP|agege40, type = "response")





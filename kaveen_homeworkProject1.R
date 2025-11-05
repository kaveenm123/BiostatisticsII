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



Table1 <- tbl_summary(data = Blood.Storage,
                      include = c(MedianRBCAge, Recurrence, TimeToRecurrence, AA),
                      by = RBCAgeGroup,
                      type = all_dichotomous() ~ "categorical", # display all categorical levels
                      missing = "no",
                      digits = all_continuous() ~ 1,
                      label = list(MedianRBCAge ~ "Median RBC",
                                   Recurrence ~ "Biochemical Recurrence of Prostate Cancer (0 = No, 1 = Yes)",
                                   TimeToRecurrence ~ "Time to Biochemical Recurrence of Prostate Cancer (months)"))%>% 
  add_n() %>% # add column with total number of non-missing observations
  add_overall() %>% # add column with overall summary statistics add_stat_label() %>% # add statistics labels to each row, rather than footnote
  #modify_header(label = "") %>%
  modify_header(label ~ "",
                stat_1 ~ "**Younger (Less than 13 days)**,N = {n}",
                stat_2 ~ "**Middle (13 to 18 days)**, N = {n}",
                stat_3 ~ "**Older (More than 18 days)**, N = {n}")%>%
  bold_labels()
print(Table1)

as_flex_table(Table1) %>%
  save_as_docx(path = "Project1_Table1.docx")

#Creating survival for each time
kmplot_blood <- survfit(Surv(TimeToRecurrence, Recurrence) ~ RBCAgeGroup,
                       data = Blood.Storage)
#Survival plot
autoplot(kmplot_blood, conf.int = FALSE, censor.shape = "X") +
  xlab("Time (months)") +
  ylab("Survival time before recurrence") +
  labs(title = "Survival Time to Recurrence", subtitle = "By RBC Storage Duration",
       caption = "Event = Prostate cancer recurrence (1), 0 = censored",
       color = "Storage duration") +
  scale_color_discrete(labels = c("Younger", "Middle", "Older")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))


#log rank test (rho = 0)
survdiff(Surv(TimeToRecurrence, Recurrence) ~ RBCAgeGroup,
         data = Blood.Storage,
         rho = 0)
#Wilcoxon test (rho = 1)
survdiff(Surv(TimeToRecurrence, Recurrence) ~ RBCAgeGroup,
         data = Blood.Storage,
         rho = 1)

#Unadjusted model
BloodCox <- coxph(Surv(TimeToRecurrence, Recurrence) ~ factor(RBCAgeGroup),
                   data = Blood.Storage)
summary(BloodCox)

#Schoenfield test
BloodCox_resid <- cox.zph(BloodCox)
ggcoxzph(BloodCox_resid)


#Adjusted model
BloodCox_adj <- coxph(Surv(TimeToRecurrence, Recurrence) ~ factor(RBCAgeGroup) + factor(TStage) + factor(sGS) + PVol + Units,
                  data = Blood.Storage)
summary(BloodCox_adj)

#Schoenfield test for adjusted
BloodCoxadj_resid <- cox.zph(BloodCox_adj)
ggcoxzph(BloodCoxadj_resid)



#reduced model
BloodCox_reduced <- coxph(Surv(TimeToRecurrence, Recurrence) ~ factor(TStage) + factor(sGS) + PVol + Units,
                      data = Blood.Storage)
summary(BloodCox_reduced)
#LRT test
anova(BloodCox_reduced, BloodCox_adj, test = "LRT")

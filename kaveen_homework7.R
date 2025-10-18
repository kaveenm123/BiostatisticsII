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
#rm(list = ls())

table1 <- tbl_summary(data = first6,
                      include = c(AGE, CD4BL, GENDER, RACE, RNABL, above200, death,
                                  podbl),
                      by = twoclass,
                      type = list(all_dichotomous() ~ "categorical"),
                      missing = "no",
                      digits = all_continuous() ~ 1
                      )%>%
  add_n() %>% # add column with total number of non-missing observations
  add_overall() %>% # add column with overall summary statistics add_stat_label()
  modify_header(label ~ "",
                stat_1 ~ "**Two Class Strategy**,N = {n}",
                stat_2 ~ "**Three Class Strategy**, N = {n}")%>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment strategy**") %>%
  bold_labels()

print(table1)
as_flex_table(table1) %>%
  save_as_docx(path = "Homeworkreport7_Table1.docx")

#Creating survival for each time
kmplot_strategy <- survfit(Surv(t2primary, primary) ~ twoclass,
                           data = first6)
#Survival plot
autoplot(kmplot_strategy, conf.int = FALSE, censor.shape = "X") +
  xlab("Time (months)") +
  ylab("Survival Probability") +
  labs(title = "Time to event", subtitle = "By Strategy",
       caption = "Event = Death or Progression of Disease (1); 0 = Censored",
       color = "Treatment strategy") +
  scale_color_discrete(labels = c("Two-Class", "Three-class")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))


kmplot_strategy

#log rank test (rho = 0)
survdiff(Surv(t2primary, primary) ~ twoclass, data = first6,
         rho = 0)
#Wilcoxon test (rho = 1)
survdiff(Surv(t2primary, primary) ~ twoclass, data = first6,
         rho = 1)

#estimate the 25th percentile event-free survival time 
#(with 95% confidence interval) in each treatment strategy group
quantile(kmplot_strategy)

#survival rate at 4 years (48 months)
summary(kmplot_strategy, times=c(48))

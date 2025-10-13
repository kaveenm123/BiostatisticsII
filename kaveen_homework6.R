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
#rm(list = ls())


#########report

Table1 <- tbl_summary(data = dekaf_dgf,
                      include = c(dcd_yn,agedonor16),
                      by = dgf,
                      type = all_dichotomous() ~ "categorical", # display all categorical levels
                      missing = "no",
                      digits = all_continuous() ~ 1,
                      label = list(dcd_yn ~ "Donation after Cardiac Death (1=yes, 0=No)",
                                   agedonor16 ~ "Donor age")
)%>% 
  add_n() %>% # add column with total number of non-missing observations
  add_overall() %>% # add column with overall summary statistics add_stat_label() %>% # add statistics labels to each row, rather than footnote
  #modify_header(label = "") %>%
  modify_header(label ~ "",
                stat_1 ~ "**No DGF**,N = {n}",
                stat_2 ~ "**DGF**, N = {n}")%>%
  bold_labels()

print(Table1)
as_flex_table(Table1) %>%
  save_as_docx(path = "Homeworkreport6_Table1.docx")


boxplot(agedonor16 ~ dgf,
        data = dekaf_dgf,
        main = "Donor age vs DGF",
        xlab = "DGF (0 = No DGF, 1 = DGF)",
        ylab = "Donor age",
        col = c("purple", "green"))

barplot(dekaf_dgf$dcd_yn, dekaf_dgf$dgf,
        main = "Donor cardiac death vs. DGF status",
        xlab = "Donor cardiac death(1 = yes, 0 = no)",
        ylab = "DGF status",
        col = c("purple","green"))

tab <- table(dekaf_dgf$dcd_yn, dekaf_dgf$dgf)
barplot(
  prop.table(tab, 1)[,2],
  names.arg = c("No cardiac death", "Cardiac death"),
  ylim = c(0,1),
  main = "Proportion of DGF vs. DCD status",
  xlab = "Donation after cardiac death",
  ylab = "Proportion with DGF",
  col = c("red", "blue")
)

adjage <- glm(dgf~dcd_yn + agedonor16,
              data = dekaf_dgf,
              family = binomial(link="logit"))
summ(adjage,confint = TRUE, ci.width = 0.95, digits = 4, exp = FALSE)
summ(adjage,confint = TRUE, ci.width = 0.95, digits = 4, exp = TRUE)
exp(cbind(OR = coef(adjage), confint(adjage)))

adjdcd <- glm(dgf~agedonor16 + dcd_yn,
              data = dekaf_dgf,
              family = binomial(link="logit"))
summ(adjdcd,confint = TRUE, ci.width = 0.95, digits = 4, exp = FALSE)
exp(cbind(OR = coef(adjdcd), confint(adjdcd)))

vif(adjage)
hoslem.test(dekaf_dgf$dgf, fitted(adjage))


influenceIndexPlot(adjage, vars = "Cook")
abline(h=1, lty = 2)



##########Problem Set
Table1 <- tbl_summary(data = dekaf_dgf,
                      include = c(pump, raceblack, gender, priorktx, pcause_diab, daysondial, wtkg_donor),
                      by = dgf,
                      type = all_dichotomous() ~ "categorical", # display all categorical levels
                      missing = "no",
                      digits = all_continuous() ~ 1
                      )%>% 
  add_n() %>% # add column with total number of non-missing observations
  add_overall() %>% # add column with overall summary statistics add_stat_label() %>% # add statistics labels to each row, rather than footnote
  modify_header(label ~ "",
                stat_1 ~ "**No DGF**,N = {n}",
                stat_2 ~ "**DGF**, N = {n}")%>%
  bold_labels()

print(Table1)

as_flex_table(Table1) %>%
  save_as_docx(path = "Homework6_Table1.docx")


# Box plot of dgf and dialysis
boxplot(daysondial ~ dgf,
        data = dekaf_dgf,
        main = "Days on Dialysis vs DGF",
        xlab = "DGF (0 = No DGF, 1 = DGF)",
        ylab = "Days on Dialysis",
        col = c("purple", "green"))

#Creating 2x2 of the study participants by gender (rows) and DGF (columns)
dekaf_dgf$gender1 <- factor(dekaf_dgf$gender,
                            levels = c(0, 1),
                            labels = c("Female", "Male"))

dekaf_dgf$dgf1 <- factor(dekaf_dgf$dgf,
                         levels = c(0, 1),
                         labels = c("No", "Yes"))
table(dekaf_dgf$gender1, dekaf_dgf$dgf1)



# Create a table to test if there is a significant relationship between gender and DGF
tab <- table(dekaf_dgf$gender, dekaf_dgf$dgf)
mychi.test <- chisq.test(tab, correct = FALSE)
mychi.test


#logistic regression model predicting log odds of DGF by gender
m1 <- glm(dgf~gender, data = dekaf_dgf,
          family = binomial(link="logit"))
summ(m1,confint = TRUE, ci.width = 0.95, digits = 4, exp = TRUE) #exponentiated
summ(m1,confint = TRUE, ci.width = 0.95, digits = 4, exp = FALSE)


#logistic regression model predicting log odds of DGF by days on dialysis prior to transplant
m2 <- glm(dgf~daysondial, data = dekaf_dgf,
          family = binomial(link="logit"))
summ(m2,confint = TRUE, ci.width = 0.95, digits = 4, exp = TRUE)


exp(cbind(OR = coef(m2), confint(m2)))

# logistic regression model predicting log odds of DGF by 
# days on dialysis prior to transplant adjusted for
# whether the participant received transfusions prior to transplant

m3 <- glm(dgf~daysondial+transfusions_yn,data = dekaf_dgf,
          family = binomial(link = "logit"))
summ(m3, confint = TRUE, ci.width = 0.95, digits = 4, exp = TRUE)
exp(cbind(OR = coef(m3), confint(m3)))

vif(m3)
PRESS <- function(model) {
  i <- residuals(model)/(1 - lm.influence(model)$hat)
  sum(i^2)
}

PRESS(m3)

influence.measures(m3)
cooks.distance(m3)

influenceIndexPlot(m3, vars = "Cook")
abline(h=1, lty = 2)

hoslem.test(dekaf_dgf$dgf, fitted(m3))

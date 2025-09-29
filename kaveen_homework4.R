library(datadictionary)
library(gtsummary)
library(flextable)
library(parallelly)
library(future)
library(jtools)
library(cars)
#rm(list = ls())


#Report graphs

tomhs$wtchg<-tomhs$wt12 - tomhs$wtbl   
tomhs$cholchg<-tomhs$chol12 - tomhs$cholbl

#Table 1 
Table1 <- tbl_summary(data = tomhs,
                      include = c(age, wtbl, wt12, wtchg, cholbl, chol12, cholchg),
                      by = sex,
                      type = all_dichotomous() ~ "categorical", # display all categorical levels
                      missing = "no",
                      digits = all_continuous() ~ 1,
                      label = list(wtbl ~ "Baseline Weight",
                                   cholbl ~ "Baseline Total Cholesterol",
                                   wt12 ~ "Follow-up Weight measurement (12 months)",
                                   chol12 ~ "Follow-up Cholesterol measurement (12 months)",
                                   wtchg ~ "Weight change",
                                   cholchg ~ "Total cholesterol change")) %>% # add statistics labels to each row, rather than footnote
  add_n() %>% # add column with total number of non-missing observations
  add_overall() %>% # add column with overall summary statistics
  modify_header(label = "", stat_1 = "**Men**, N = {n}", stat_2 = "**Women**, N = {n}") %>% # update the column header
  add_stat_label() %>% # add statistics labels to each row, rather than footnote
  bold_labels()

print(Table1)

as_flex_table(Table1) %>%
  save_as_docx(path = "Homework4_Table1.docx")

m3<- lm(cholchg ~ wtchg + sex, data = tomhs)
summary(m3)
confint(m3)

plot(m3, which = 2, col = "blue")

influenceIndexPlot(m3, vars = "Cook", col = "brown")
abline(h=1, lty = 2)

vif(m3)


# Week 4 problem set

tomhs$wtchg <- tomhs$wt12 - tomhs$wtbl
tomhs$cholchg<-tomhs$chol12 - tomhs$cholbl

m1 <- glm(cholchg ~ wtchg, data = tomhs) # Simple linear regeression
summary(m1)
summ(m1, confint = TRUE, ci.width = 0.95, digits = 4)

m2 <- glm(cholchg ~ wtchg + nowsmk, data = tomhs) #multi linear regression adjusting for smoking
summary(m2)
summ(m2, confint = TRUE, ci.width = 0.95, digits = 4)

#CREATING PRESS
PRESS <- function(model) {
  i <- residuals(model)/(1 - lm.influence(model)$hat)
  sum(i^2)
}

PRESS(m1)
PRESS(m2)

plot(wtchg ~ cholchg,
     data = tomhs,
     main = "Cholesterol change vs weight change",
     xlab = "Cholesterol change",
     ylab = "Weight change")

#plot of adjusted model, residuals vs fitted values
plot(fitted(m2),residuals(m2),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted values")

#Histogram of the adjusted model's residual
hist(residuals(m2),
     #xlim = c(-100,100),
     #ylim = c(0,30),
     xlab = "Residuals",
     ylab = "Frequency",
     main = "Histogram of residuals",
     col = "purple")

#QQ plot for adjusted model
plot(m2, which = 2)


# plot for the standardized residuals
plot(m2, which = 3, id.n = 10)
abline(h = sqrt(3), lty = 2) # adding threshold line

#Studentized residual plot
library(car)
influenceIndexPlot(m2, vars = "Studentized")
abline(h = -2, lty = 2)
abline(h = 2, lty = 2)

#Cook's D plot against observation numbers
influenceIndexPlot(m2, vars = "Cook")
abline(h=1, lty = 2)

#Plot of DFFIT's values against observation numbers
plot(dffits(m2),
     ylab = "DFFIT's",
     xlab = "Observation Number")
thresh.dff <- 2*sqrt((length(m2$coefficients)-1) / length(m2$fitted.values))
abline(h = thresh.dff, lty = 2)
abline(h = -thresh.dff, lty = 2)


#Plotting DFBETAs

# Computing the DFBETAs
DFBETAS <- dfbetas(m2)
## Calculating threshold for DFBETAs
thresh.dfb <- 2 / sqrt(length(m2$fitted.values))
colnames(DFBETAS)
par(mfrow=c(2, 2))


plot(DFBETAS[, "(Intercept)"],
     xlab = "Observation Number",
     ylab = "Intercept")
abline(h = thresh.dfb, lty = 2)
abline(h = -thresh.dfb, lty = 2)

plot(DFBETAS[, "wtchg"],
     xlab = "Observation Number",
     ylab="Weight change")
abline(h = thresh.dfb, lty = 2)
abline(h = -thresh.dfb, lty = 2)

plot(DFBETAS[, "nowsmk"],
     xlab = "Observation Number",
     ylab = "Smoking status")
abline(h = thresh.dfb, lty = 2)
abline(h = -thresh.dfb, lty = 2)


# checking for multicollinearity 
vif(m2)


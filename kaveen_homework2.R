library(datadictionary)
library(gtsummary)
library(flextable)


############################################
#REPORT 
############################################

#Creating continuous variables in the dataset
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
    save_as_docx(path = "Homework2_Table1.docx")

#Scatter plot showing male and female weight change
plot(tomhs$wtchg, tomhs$sex,
     main = "weight change vs. sex",
     xlab = "weight change",
     ylab = "sex  (1 = male, 2 = female)",
     #col = c("purple", "red")
     col = ifelse(tomhs$sex==1, "purple","red"))

#Linear regression model - Investigating the effect of weight change on total cholesterol change without adjusting for anything
m1 <- lm(cholchg ~ wtchg, data = tomhs) # Simple linear regeression
summary(m1)
confint(m1)

#Multi-linear regression model 
m2<- lm(cholchg ~ wtchg + sex, data = tomhs)
summary(m2)
confint(m2)


############################################
#PROBLEM SET
############################################

# Simulate ages and compute means for men vs women
ages <- seq(0, 80)
sbp_women <- 100 + 0.3*ages      # Increases by 3mmHg per 10 years of age
sbp_men <- sbp_women + 2         # men higher than women by 2 mmHg at all ages

plot(ages, sbp_women, type="l", lwd=2,
     xlab="Age, years",
     ylab="Mean Systolic Blood Pressure, mmHg",
     xlim = c(20,80),
     col = "red")
lines(ages, sbp_men,
      lwd=2,
      lty=2,
      col = "purple")
legend("topleft", c("Women","Men"), lwd=2, lty=c(1,2), col = c("red", "purple"), bty="n")

######################################################################################
######### What is the effect of weight change on total cholesterol change? ###########
######################################################################################

#Creating continuous variables in the dataset
tomhs$wtchg<-tomhs$wt12 - tomhs$wtbl   
tomhs$cholchg<-tomhs$chol12 - tomhs$cholbl

View(tomhs)

print(tomhs$wtchg)

head(tomhs[, c("wt12","wtbl", "wtchg","chol12", "cholbl", "cholchg")]) # verifying wtchg and cholchg variables


# Summary statistics for cholchg
summary(tomhs$cholchg)
range(tomhs$cholchg, na.rm = TRUE)
sd(tomhs$cholchg, na.rm = TRUE)
IQR(tomhs$cholchg, na.rm = TRUE)
sum(is.na(tomhs$cholchg))

#Histogram of cholchg 
hist(tomhs$cholchg,
     main = "Total cholesterol change (cholchg)",
     xlab = "Total cholesterol change",
     ylab = "Frequency",
     col = "purple")

# Summary statistics for wtchg
summary(tomhs$wtchg)
range(tomhs$wtchg, na.rm = TRUE)
sd(tomhs$wtchg, na.rm = TRUE)
IQR(tomhs$wtchg, na.rm = TRUE)
sum(is.na(tomhs$wtchg))

#Histogram of wtchg
hist(tomhs$wtchg,
     main = "Total weight change (wtchg)",
     xlab = "Total weight change",
     ylab = "Frequency",
     col = "green")

#summary statistics for nowsmk
summary(tomhs$nowsmk)
range(tomhs$nowsmk, na.rm = TRUE)
sd(tomhs$nowsmk, na.rm = TRUE)
IQR(tomhs$nowsmk, na.rm = TRUE)
table(tomhs$nowsmk,useNA='always')

colnames(tomhs)

Table1 <- tbl_summary(data = tomhs,
                      include = c(age, sex, race, income, educ, marital,
                                  eversmk, nowsmk, sbpbl, dbpbl, wtbl, cholbl),
                      type = all_dichotomous() ~ "categorical", # display all categorical levels
                      missing = "no",
                      digits = all_continuous() ~ 1)%>%
  add_n() %>% # add column with total number of non-missing observations
  add_overall() %>% # add column with overall summary statistics
  modify_header(label = "") %>% # update the column header
  add_stat_label() %>% # add statistics labels to each row, rather than footnote
  bold_labels()
print(Table1)

as_flex_table(Table1) %>%
  save_as_docx(path = "HW2_Table1.docx")

#Scatter plot of cholchg vs wtchg
plot(tomhs$cholchg, tomhs$wtchg,
     main = "Total cholesterol change vs. weight change",
     xlab = "Total cholesterol change",
     ylab = "Weight change")

# Simple linear regression model between total cholesterol change vs. total weight change
m1 <- lm(cholchg ~ wtchg, data = tomhs) # Simple linear regeression
summary(m1)
confint(m1)

#multiple linear regression for cholchg vs wtchg, adjusting for nowsmk (current smokers)
m2<- lm(cholchg ~ wtchg + nowsmk, data = tomhs)
summary(m2)
confint(m2)

#Testing swapping variables in the model above
m3<- lm(cholchg ~ nowsmk + wtchg, data = tomhs)
summary(m3)
confint(m3)
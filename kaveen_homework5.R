library(datadictionary)
library(gtsummary)
library(flextable)
library(parallelly)
library(future)
library(jtools)
library(cars)
library(forcats)
library(interactions)
library(emmeans)

#rm(list = ls())


tomhs$wtchg<-tomhs$wt12 - tomhs$wtbl   
tomhs$cholchg<-tomhs$chol12 - tomhs$cholbl

# model to investigate  weightchange - smoking interaction's effect on cholesterol
tomhs$nowsmk1<- factor(tomhs$nowsmk,levels=c(1, 2),labels=c("Yes","No"))
m1 <- glm(cholchg ~ wtchg*nowsmk1,
          data = tomhs, family = "gaussian")

#Creating interaction plot
interact_plot(model = m1,
              pred = wtchg,
              modx = nowsmk1,
              plot.points = TRUE,
              x.label = "Change in Weight from Baseline",
              y.label = "Change in Total Cholesterol from Baseline",
              main = "Interaction plot",
              legend.main = "Current Smoking Status",
              colors = c("purple","green"))

#model with nowsmk reference level is No (2)

tomhs$nowsmk1 <- relevel(tomhs$nowsmk1, ref = "No")


m2<-glm(cholchg ~ wtchg*nowsmk1,
        data = tomhs, family = "gaussian")

summ(m2,confint = TRUE, ci.width = 0.95, digits = 4)

#Getting CI
m2.slopes<-emtrends(m2,pairwise~nowsmk1|wtchg, var = "wtchg")
summary(m2.slopes, infer = TRUE)


# model without interaction term
m3 <- glm(cholchg ~wtchg+nowsmk1,
          data = tomhs,
          family = "gaussian")

summ(m3,confint = TRUE, ci.width = 0.95, digits = 4)


#Quiz

tomhs$sex01 <- ifelse(tomhs$sex == 1, 1, 0)
alcohol_sex_model <- glm(chol12~alcdrk12*sex01, data = tomhs, family = "gaussian")
summ(alcohol_sex_model, confint = TRUE, ci.width = 0.95, digits = 4)
interact_plot(model = alcohol_sex_model,
              pred = alcdrk12,
              modx = sex01,
              plot.points = TRUE,
              x.label = "Alcohol drinks at 12 months",
              y.label = "Cholesterol at 12 months",
              main = "Interaction plot",
              legend.main = "Current Smoking Status",
              colors = c("purple","green"))

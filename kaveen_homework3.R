library(datadictionary)
library(gtsummary)
library(flextable)
library(parallelly)
library(future)
library(jtools)
#install.packages("jtools")
#install.packages("parallelly", type = "binary")
#install.packages("future", type = "binary")




#install.packages("lmtest")
#rm(list = ls())


avgsbp<- rep(120, length(tomhs$age))
oversbp<- avgsbp + 2
obesesbp<- avgsbp + 6


#Scatterplot of Average SBP vs age
plot(tomhs$age, avgsbp,
     type = "l",
     ylim = c(115, 135),
     main = "SBP vs. age",
     xlab = "Age, years",
     ylab = "Mean Systolic Blood Pressure, mmHg",
     col = "purple")
lines(tomhs$age, oversbp, col = "green", lwd = 2)
lines(tomhs$age, obesesbp, col = "red", lwd = 2)
legend("topright", legend = c("Normal weight", "Overweight","Obese"),
       col = c("purple", "green", "red"),lwd=2)



#box plot for SBP vs weight
boxplot(avgsbp ~tomhs$wtchgcat,
        ylab = "Mean Systolic Blood Pressure, mmHg",
        xlab = "Weight Group",
        main = "SBP by Weight Group",
        col = c("blue", "green", "red"))


#Scatterplot of Average SBP vs age (where SBP increases by 10mmHg every 10 years)


normsbp <- 110 + 0.3 * tomhs$age
oversbp <- normsbp + 2
obesesbp <- normsbp + 6


plot(tomhs$age,normsbp ,
     type = "l",
     ylim = c(120, 140),
     main = "SBP vs. age",
     xlab = "Age, years",
     ylab = "Mean Systolic Blood Pressure, mmHg",
     col = "purple")
lines(tomhs$age, oversbp, col = "green", lwd = 2)
lines(tomhs$age, obesesbp, col = "red", lwd = 2)
legend("topleft", bty = "n", legend = c("Normal weight", "Overweight","Obese"),
       col = c("purple", "green", "red"),lwd=2)



tomhs$wtchg <- tomhs$wt12 - tomhs$wtbl
tomhs$cholchg<-tomhs$chol12 - tomhs$cholbl
nrow(tomhs)
colSums(is.na(tomhs["wtchg"]))
summary(tomhs["wtchg"])

tomhs$wtchgcat <- cut(tomhs$wtchg,
                      breaks = c(-Inf, -10, 0, Inf),
                      labels = c("lost10plus", "lost0to10", "gained"),
                      right = TRUE)
head(tomhs[, c("wtchgcat")], n = 12)
table(tomhs$wtchgcat)


boxplot(wtchg ~ wtchgcat,
        data = tomhs,
        main = "Box plot of weight change against category",
        ylab = "Weight change",
        xlab = "Categories of weight change",
        col = c("purple", "blue", "green"))


# Show min and max for each category using tapply
tapply(tomhs$wtchg, tomhs$wtchgcat,
       function(x) c(Min = min(x, na.rm = TRUE),
                     Max = max(x, na.rm = TRUE)))
# getting count of each category
summary(tomhs$wtchgcat)

#Boxplot of cholchg and wtchgcat
boxplot(cholchg ~ wtchgcat,
        data = tomhs,
        main = "Cholesterol change against weight change categories",
        ylab = "Cholesterol change, mmHg",
        xlab = "Weight categories",
        col = c("purple", "brown", "gray"))

#linear regression of cholchg and wtchgcat, lost10plus as reference

tomhs$wtchgcat <- relevel(factor(tomhs$wtchgcat), ref = "lost10plus")
m1 <- glm(cholchg ~ wtchgcat, data = tomhs)
summary(m1)

m2 <- glm(cholchg ~ relevel(factor(wtchgcat), ref = "gained"),
              data = tomhs,
              family = "gaussian")

summary(m2)


#Likelihood ratio test
library(lmtest)

summ(m1, confint = TRUE, ci.width = 0.95, digits = 4)
reducedm1<-glm(cholchg ~ wtchgcat,
               data = tomhs,
               family = "gaussian")

lrtest(reducedm1, m1)

# anova f-test for model 1

library(car)
Anova(m1, type ='III')


# Scatterplot of cholchg and height
plot(tomhs$height, tomhs$cholchg,
     col = c("purple", "blue", "red")[tomhs$wtchgcat],
     main = "Cholesterol change against height",
     xlab = "Height",
     ylab = "Cholesterol change, mmHg")

#mlr
tomhs$wtchgcat <- relevel(factor(tomhs$wtchgcat), ref = "lost10plus")
m3 <- glm(cholchg ~ wtchgcat + height,
          data = tomhs,
          family = "gaussian")

summ(m3, conf.int = TRUE, ci.width = 0.95, digits = 4)

library(car)
Anova(m3, type = 'III')



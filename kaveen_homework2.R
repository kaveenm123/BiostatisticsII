library(datadictionary)
library(gtsummary)
library(flextable)


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



library(datadictionary)
library(gtsummary)
library(flextable)

ncol(RheumArth) # Show the number of columns for the dataframe
nrow(RheumArth) # Show the number of rows (Observations) for the dataframe


Rdict<-create_dictionary(RheumArth,id_var = NULL,file = NULL,var_labels = NULL) # Create data dictionary for dataframe
print(Rdict)

str(RheumArth) # Getting the variable types

colSums(is.na(RheumArth)) #Getting the number of missing values for the variables

dxgt20<-ifelse(RheumArth$Yrs_From_Dx > 20, 1, 0) #Dividing participants based on length of time, 1 if diagnosed more than 20, 0 if not
print(dxgt20)

barplot(table(dxgt20),
        main = "Distribution of Binary Variable",
        xlab = "Category",
        ylab = "Count")

plot(RheumArth$Yrs_From_Dx,RheumArth$dxgt20,
     xlab = "Yrs_From_Dx",
     ylab = "dxgt20",
     main = "Scatterplot of dxgt20 vs. Yrs_from_Dx") # Scatter plot to verify dxgt20

Table1 <- tbl_summary(data = RheumArth,
                      include = c(Sex, Age, Yrs_From_Dx, CDAI, CDAI_YN, DAS_28, DAS28_YN,
                                  Steroids_GT_5, DMARDs, Biologics, sDMARDS, OsteopScreen),
                      by = AgeGp, # split table by group
                      type = all_dichotomous() ~ "categorical", # display all categorical levels
                      statistic = list(
                        Yrs_From_Dx ~ "{mean} ({sd})"),
                      missing = "no",
                      digits = all_continuous() ~ 1)%>%
  add_n() %>% # add column with total number of non-missing observations
  add_overall() %>% # add column with overall summary statistics
  modify_header(label = "") %>% # update the column header
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Age Groups**") %>%
  add_stat_label() %>% # add statistics labels to each row, rather than footnote
  bold_labels()
print(Table1)

as_flex_table(Table1) %>%
  save_as_docx(path = "myTable1.docx")


print(summary(RheumArth$Yrs_From_Dx)) #Summary statistics for Yrs_From_Dx (Min, Q1, Q2, mean, Q3, max, NAs)
print(sd(RheumArth$Yrs_From_Dx,na.rm = TRUE)) # SD of Yrs_From_Dx
IQR(RheumArth$Yrs_From_Dx, na.rm = TRUE) # IQR of Yrs_From_Dx


#Creating a histogram for variable Yrs_from_Dx
hist(RheumArth$Yrs_From_Dx,
     main = "Frequency of Rheumatoid Arthritis vs. Years from diagnosis",
     xlab = "Years From Diagnosis",
     ylab = "Frequency of Rheumatoid Arthritis",
     col = "purple")

#Summary stats for OsteopScreen variable

prop.table(table(RheumArth$OsteopScreen)) * 100
colSums(is.na(RheumArth["OsteopScreen"]))

#Creating table to investigate how Osteoporosis is related to Age Group
OstTable <- tbl_summary(data = RheumArth,
                      include = OsteopScreen,
                      by = AgeGp, # split table by group
                      type = all_dichotomous() ~ "categorical", # display all categorical levels
                      #statistic = list(
                      #  Yrs_From_Dx ~ "{mean} ({sd})"),
                      missing = "ifany",
                      digits = all_continuous() ~ 1)%>%
  add_n() %>% # add column with total number of non-missing observations
  add_overall() %>% # add column with overall summary statistics
  modify_header(label = "") %>% # update the column header
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Age Groups**") %>%
  add_stat_label() %>% # add statistics labels to each row, rather than footnote
  bold_labels()
print(OstTable)

#Scatterplot of DAS_28 against age group
plot(RheumArth$Age, RheumArth$DAS_28,
     main = "Disease activity score for 28 joints vs. age",
     xlab = "Age Group",
     ylab = "DAS_28")

#Box plot of DAS_28 against age group
boxplot(RheumArth$DAS_28 ~ RheumArth$AgeGp,
        data = RheumArth,
        names = c("40 to 70", "75+"),
        xlab = "Age Group",
        ylab = "DAS_28",
        main = "DAS_28 vs. Age group")

# Subset dataset DAS_28 > 4
View(subset(RheumArth, DAS_28 > 4))

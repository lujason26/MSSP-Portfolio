# install.packages("aif360")
# library(aif360)
# install_aif360()
# load_aif360_lib()
# 
# formatted_dataset <- aif360::aif_dataset(data_path = cancer,
#                                          favor_label = 0,
#                                          unfavor_label = 1,
#                                          unprivileged_protected_attribute = 0,
#                                          privileged_protected_attribute = 1,
#                                          target_column = "label",
#                                          protected_attribute = "feature1")


setwd("C:/Users/lujas/Desktop/MA 679")
library(readxl)

cancer <- read_excel("Health Literacy Transformed Only.xlsx")
# Mets	               chr -> factor
# Cause of Death	     chr -> factor
# Surgery Performed?   chr -> factor
# Surgery Decision     chr -> factor
# Radiation	           chr -> factor
# Chemotherapy         chr -> factor

table(cancer$Mets)
table(cancer$`Cause of Death`)
table(cancer$`Surgery Performed?`)
table(cancer$`Surgery Decision`)     # hard
table(cancer$Radiation)              # hard
table(cancer$Chemotherapy)

cancer$Sex <- as.factor(cancer$Sex)
cancer$Race <- as.factor(cancer$Race)
cancer$`SEER Registry` <- as.factor(cancer$`SEER Registry`)
cancer$Site <- as.factor(cancer$Site)
cancer$Subsite <- as.factor(cancer$Subsite)
cancer$`AJCC 7 Stage` <- as.factor(cancer$`AJCC 7 Stage`)
cancer$`Lymph Nodes` <- as.factor(cancer$`Lymph Nodes`)
cancer$Radiation <- as.factor(cancer$Radiation)
cancer$Insurance <- as.factor(cancer$Insurance)
cancer$Mets <- as.factor(cancer$Mets)
cancer$`Cause of Death` <- as.factor(cancer$`Cause of Death`)
cancer$`Surgery Performed?` <- as.factor(cancer$`Surgery Performed?`)
cancer$`Surgery Decision` <- as.factor(cancer$`Surgery Decision`)
cancer$Chemotherapy <- as.factor(cancer$Chemotherapy)

colnames(cancer)[4] <- "age_at_diagnosis"
colnames(cancer)[11] <- "Below_Poverty_Perc"
colnames(cancer)[13] <- "Median_Income"
colnames(cancer)[14] <- "Language_Isolation_Perc"
colnames(cancer)[21] <- "Cause_of_Death"
colnames(cancer)[22] <- "Surgery_Performed"
colnames(cancer)[23] <- "Surgery_Decision"

# categorizing age (under 30, 31-44, 45-60, 61-75, 76-90, 90+) ####
for (i in 1:length(cancer$age_at_diagnosis)) {
  if (cancer$age_at_diagnosis[i] >= 0 & cancer$age_at_diagnosis[i] < 30) {
    cancer$age_at_diagnosis_category[i] <- 'Under 30'
  } else if(cancer$age_at_diagnosis[i] > 29 & cancer$age_at_diagnosis[i] < 45) {
    cancer$age_at_diagnosis_category[i] <- '30-44'
  } else if(cancer$age_at_diagnosis[i] > 44 & cancer$age_at_diagnosis[i] < 61) {
    cancer$age_at_diagnosis_category[i] <- '45-60'
  } else if(cancer$age_at_diagnosis[i] > 60 & cancer$age_at_diagnosis[i] < 76) {
    cancer$age_at_diagnosis_category[i] <- '60-75'
  } else if(cancer$age_at_diagnosis[i] > 75 & cancer$age_at_diagnosis[i] < 91) {
    cancer$age_at_diagnosis_category[i] <- '76-90'
  } else if(cancer$age_at_diagnosis[i] > 90) {
    cancer$age_at_diagnosis_category[i] <- '90+'
  } else {
    cancer$age_at_diagnosis_category[i] <- NA
  }
}
cancer$age_at_diagnosis_category <- factor(cancer$age_at_diagnosis_category, ordered = TRUE, levels=c("Under 30","30-44","45-60","60-75","76-90","90+"))

# categorizing poverty (5-10, 10-15, 15-20, 20-25, 25-30, 30+) ####
for (i in 1:length(cancer$Below_Poverty_Perc)) {
  if (cancer$Below_Poverty_Perc[i] >= 5 & cancer$Below_Poverty_Perc[i] < 10.01) {
    cancer$Below_Poverty_Perc_category[i] <- '5-10'
  } else if(cancer$Below_Poverty_Perc[i] > 10 & cancer$Below_Poverty_Perc[i] < 15.01) {
    cancer$Below_Poverty_Perc_category[i] <- '10-15'
  } else if(cancer$Below_Poverty_Perc[i] > 15 & cancer$Below_Poverty_Perc[i] < 20.01) {
    cancer$Below_Poverty_Perc_category[i] <- '15-20'
  } else if(cancer$Below_Poverty_Perc[i] > 20 & cancer$Below_Poverty_Perc[i] < 25.01) {
    cancer$Below_Poverty_Perc_category[i] <- '20-25'
  } else if(cancer$Below_Poverty_Perc[i] > 25 & cancer$Below_Poverty_Perc[i] < 30.01) {
    cancer$Below_Poverty_Perc_category[i] <- '25-30'
  } else if(cancer$Below_Poverty_Perc[i] > 30) {
    cancer$Below_Poverty_Perc_category[i] <- '30+'
  } else {
    cancer$Below_Poverty_Perc_category[i] <- NA
  }
}
cancer$Below_Poverty_Perc_category <- factor(cancer$Below_Poverty_Perc_category, ordered = TRUE, levels=c("5-10","10-15","15-20","20-25","25-30","30+"))

# categorizing median income (Under 30, 30-40, 40-50, 50-60, 60-70, 70+) ####
for (i in 1:length(cancer$Median_Income)) {
  if (cancer$Median_Income[i] >= 0 & cancer$Median_Income[i] < 30001) {
    cancer$Median_Income_category[i] <- 'Under 30'
  } else if(cancer$Median_Income[i] > 30000 & cancer$Median_Income[i] < 40001) {
    cancer$Median_Income_category[i] <- '30-40'
  } else if(cancer$Median_Income[i] > 40000 & cancer$Median_Income[i] < 50001) {
    cancer$Median_Income_category[i] <- '40-50'
  } else if(cancer$Median_Income[i] > 50000 & cancer$Median_Income[i] < 60001) {
    cancer$Median_Income_category[i] <- '50-60'
  } else if(cancer$Median_Income[i] > 60000 & cancer$Median_Income[i] < 70001) {
    cancer$Median_Income_category[i] <- '60-70'
  } else if(cancer$Median_Income[i] > 70000) {
    cancer$Median_Income_category[i] <- '70+'
  } else {
    cancer$Median_Income_category[i] <- NA
  }
}
cancer$Median_Income_category <- factor(cancer$Median_Income_category, ordered = TRUE, levels=c("Under 30","30-40","40-50","50-60","60-70","70+"))

# categorizing language (0-5, 5-10, 10-15, 15+) ####
# table(cancer$Language_Isolation_Perc)
for (i in 1:length(cancer$Language_Isolation_Perc)) {
  if (cancer$Language_Isolation_Perc[i] >= 0 & cancer$Language_Isolation_Perc[i] < 5.01) {
    cancer$Language_Isolation_Perc_category[i] <- '0-5'
  } else if(cancer$Language_Isolation_Perc[i] > 5 & cancer$Language_Isolation_Perc[i] < 10.01) {
    cancer$Language_Isolation_Perc_category[i] <- '5-10'
  } else if(cancer$Language_Isolation_Perc[i] > 10 & cancer$Language_Isolation_Perc[i] < 15.01) {
    cancer$Language_Isolation_Perc_category[i] <- '10-15'
  } else if(cancer$Language_Isolation_Perc[i] > 15) {
    cancer$Language_Isolation_Perc_category[i] <- '15+'
  } else {
    cancer$Language_Isolation_Perc_category[i] <- NA
  }
}
cancer$Language_Isolation_Perc_category <- factor(cancer$Language_Isolation_Perc_category, ordered = TRUE, levels=c("0-5","5-10","10-15","15+"))


## separating radiation into "No radiation and/or cancer-directed surgery, Radiation after surgery"
##     and the "rest"
cancer_large_radiation <- subset(cancer, Radiation=="No radiation and/or cancer-directed surgery" | Radiation=="Radiation after surgery")
cancer_large_radiation$Radiation <- droplevels(cancer_large_radiation$Radiation)
cancer_small_radiation <- cancer[cancer[24] != "No radiation and/or cancer-directed surgery",]
cancer_small_radiation <- cancer_small_radiation[cancer_small_radiation[24] != "Radiation after surgery",]
cancer_small_radiation$Radiation <- droplevels(cancer_small_radiation$Radiation)

## separating Surgery_Decision into "Not recommended, Surgery performed"
##     and the "rest"
cancer_large_surgery <- subset(cancer, Surgery_Decision=="Not recommended" | Surgery_Decision=="Surgery performed")
cancer_large_surgery$Surgery_Decision <- droplevels(cancer_large_surgery$Surgery_Decision)
cancer_small_surgery <- cancer[cancer[23] != "Not recommended",]
cancer_small_surgery <- cancer_small_surgery[cancer_small_surgery[23] != "Surgery performed",]
cancer_small_surgery$Surgery_Decision <- droplevels(cancer_small_surgery$Surgery_Decision)

# EDA ####
barplot(table(cancer$Mets))
barplot(table(cancer$Cause_of_Death))
barplot(table(cancer$Surgery_Performed))
table(cancer$Surgery_Decision) # Surgery Decision
table(cancer$Radiation) # Radiation
barplot(table(cancer$Chemotherapy))

library(ggplot2)
library(tidyr)

### stacked barplots of Surgery_Decision sorted by Insurance
ggplot(cancer_large_surgery, aes(fill=Surgery_Decision, y=1000, x=Insurance)) + 
  geom_col(position="fill")
ggplot(cancer_small_surgery, aes(fill=Surgery_Decision, y=1000, x=Insurance)) + 
  geom_col(position="fill")

### stacked barplots of Tumor Size sorted by Insurance
ggplot(cancer, aes(fill=Insurance, y=1000, x=Size_category)) + 
  geom_bar(position="stack", stat="identity")
ggplot(cancer, aes(fill=Insurance, y=1000, x=Size_category)) + 
  geom_col(position="fill")

### ugly plot of insurance against everything
plot(Insurance~., data=cancer)
# size tumor vs insured
plot(Insurance~Size, data=cancer)
# most of the categories have a trend (but prob just due to being categorized biasedly)
# maybe like BY something, ex. surgery performed by Insurance for those greater than 10 poverty vs less
ggplot(cancer, aes(fill=Insurance, y=Insurance, x=Size_category)) + 
  geom_col(position="fill") +
  facet_grid(. ~ Below_Poverty_Perc_category)


### only works for numeric variables ####
# # ++++++++++++++++++++++++++++
# # flattenCorrMatrix
# # ++++++++++++++++++++++++++++
# # cormat : matrix of the correlation coefficients
# # pmat : matrix of the correlation p-values
# flattenCorrMatrix <- function(cormat, pmat) {
#   ut <- upper.tri(cormat)
#   data.frame(
#     row = rownames(cormat)[row(cormat)[ut]],
#     column = rownames(cormat)[col(cormat)[ut]],
#     cor  =(cormat)[ut],
#     p = pmat[ut]
#   )
# }
# library(Hmisc)
# res2<-rcorr(as.matrix(cancer[,3:4]))
# View(flattenCorrMatrix(res2$r, res2$P))    # gives every variable's correlation against each other
# # Insignificant correlations are leaved blank
# library(corrplot)
# corrplot((res2$r),type="upper", order="hclust", 
#          p.mat = res2$P, sig.level = 0.01, insig = "blank")







## How do you plan on quantifying bias?
## As in, how do you tell whether the data we have is biased, numerically?
##    ex. (from article) sex bias: difference between fraction of female participants in study minus prevalence fraction of females for each disease category
# https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2737103

# VCD Package ####
install.packages("vcd")
library(vcd)

# Blue means more than predicted, red means less than predicted
#use gp=shading_Friendly for different shading
# Sex, Race, Insurance, Age, Poverty, Median, Language vs. Surgery Performed, Radiation, Chemotherapy
# https://towardsdatascience.com/mosaic-plot-and-chi-square-test-c41b1a527ce4
# https://www.statisticshowto.com/expected-frequency/

# Sex ####
sex.table1 <- with(cancer, table(Sex, Surgery_Performed))
mosaic(sex.table1, shade=TRUE, legend=TRUE, main="Sex vs. Surgery Performed")
sex.table2_large <- with(cancer_large_radiation, table(Sex, Radiation))
mosaic(sex.table2_large, shade=TRUE, legend=TRUE, main="Sex vs. Radiation")
sex.table2_small <- with(cancer_small_radiation, table(Sex, Radiation))
mosaic(sex.table2_small, shade=TRUE, legend=TRUE, main="Sex vs. Radiation") 
sex.table3 <- with(cancer, table(Sex, Chemotherapy))
mosaic(sex.table3, shade=TRUE, legend=TRUE, main="Sex vs. Chemotherapy")

# Race ####
# Race order is White, Hispanic, Black, Asian/PI, American Indian/Alaskan Native
race.table1 <- with(cancer, table(Race, Surgery_Performed))
mosaic(race.table1, shade=TRUE, legend=TRUE, main="Race vs. Surgery Performed")
race.table2_large <- with(cancer_large_radiation, table(Race, Radiation))
mosaic(race.table2_large, shade=TRUE, legend=TRUE, main="Race vs. Radiation")
race.table2_small <- with(cancer_small_radiation, table(Race, Radiation))
mosaic(race.table2_small, shade=TRUE, legend=TRUE, main="Race vs. Radiation")
race.table3 <- with(cancer, table(Race, Chemotherapy))
mosaic(race.table3, shade=TRUE, legend=TRUE, main="Race vs. Chemotherapy")

# Insurance ####
# Insurance order is Uninsured, Insured/No specifics, Insured, Any Medicaid
insurance.table1 <- with(cancer, table(Insurance, Surgery_Performed))
mosaic(insurance.table1, shade=TRUE, legend=TRUE, main="Insurance vs. Surgery Performed")
insurance.table2_large <- with(cancer_large_radiation, table(Insurance, Radiation))
mosaic(insurance.table2_large, shade=TRUE, legend=TRUE, main="Insurance vs. Radiation")      
insurance.table2_small <- with(cancer_small_radiation, table(Insurance, Radiation))
mosaic(insurance.table2_small, shade=TRUE, legend=TRUE, main="Insurance vs. Radiation")
insurance.table3 <- with(cancer, table(Insurance, Chemotherapy))
mosaic(insurance.table3, shade=TRUE, legend=TRUE, main="Insurance vs. Chemotherapy")

# categorizing tumor size (0-10, 10-20, 20-30, 30-40, 40-50, 50+) ####
for (i in 1:length(cancer$Size)) {
  if (cancer$Size[i] >= 0 & cancer$Size[i] < 10.01) {
    cancer$Size_category[i] <- '0-10'
  } else if(cancer$Size[i] > 10 & cancer$Size[i] < 20.01) {
    cancer$Size_category[i] <- '10-20'
  } else if(cancer$Size[i] > 20 & cancer$Size[i] < 30.01) {
    cancer$Size_category[i] <- '20-30'
  } else if(cancer$Size[i] > 30 & cancer$Size[i] < 40.01) {
    cancer$Size_category[i] <- '30-40'
  } else if(cancer$Size[i] > 40 & cancer$Size[i] < 50.01) {
    cancer$Size_category[i] <- '40-50'
  } else if(cancer$Size[i] > 50) {
    cancer$Size_category[i] <- '50+'
  } else {
    cancer$Size_category[i] <- NA
  }
}
cancer$Size_category <- factor(cancer$Size_category, ordered = TRUE, levels=c("0-10","10-20","20-30","30-40","40-50","50+"))

tumor.insurance.table <- with(cancer, table(Insurance, Size_category))
mosaic(tumor.insurance.table, shade=TRUE, legend=TRUE, main="Insurance vs. Tumor Size")

# Age ####
age.table1 <- with(cancer, table(age_at_diagnosis_category, Surgery_Performed))
#vnames <- list(set_varnames=c(s_Feel="Feeling", Year.Born.category="Age"))
#lnames <- list(Year.Born.category = c("65+","55-64","45-54","35-44","25-34","18-24","l","l"),
#               s_Feel = c("Negative","Normal","Positive"))
mosaic(age.table1, shade=TRUE, main="Age vs. Surgery Performed") # legend=TRUE, labeling_args=vnames, set_labels=lnames, 
age.table2_large <- with(cancer_large_radiation, table(age_at_diagnosis_category, Radiation))
mosaic(age.table2_large, shade=TRUE, main="Age vs. Radiation") # legend=TRUE, labeling_args=vnames, set_labels=lnames, 
age.table2_small <- with(cancer_small_radiation, table(age_at_diagnosis_category, Radiation))
mosaic(age.table2_small, shade=TRUE, main="Age vs. Radiation") # legend=TRUE, labeling_args=vnames, set_labels=lnames, 
age.table3 <- with(cancer, table(age_at_diagnosis_category, Chemotherapy))
mosaic(age.table3, shade=TRUE, main="Age vs. Chemotherapy") # legend=TRUE, labeling_args=vnames, set_labels=lnames, 

age.table4 <- with(cancer, table(age_at_diagnosis_category, Site))
mosaic(age.table4, shade=TRUE, main="Age vs. Site") 


# Poverty, 5-10, 10-15, 15-20, 20-25, 25-30, 30+####
poverty.table1 <- with(cancer, table(Below_Poverty_Perc_category, Surgery_Performed))
mosaic(poverty.table1, shade=TRUE, main="Poverty vs. Surgery Performed") 
poverty.table2_large <- with(cancer_large_radiation, table(Below_Poverty_Perc_category, Radiation))
mosaic(poverty.table2_large, shade=TRUE, main="Poverty vs. Radiation")  
poverty.table2_small <- with(cancer_small_radiation, table(Below_Poverty_Perc_category, Radiation))
mosaic(poverty.table2_small, shade=TRUE, main="Poverty vs. Radiation") 
poverty.table3 <- with(cancer, table(Below_Poverty_Perc_category, Chemotherapy))
mosaic(poverty.table3, shade=TRUE, main="Poverty vs. Chemotherapy") 

# poverty.table2 <- with(cancer, table(Below_Poverty_Perc_category, Radiation)) # full radiation list for comparison
# mosaic(poverty.table2, shade=TRUE, main="Poverty vs. Radiation") 

# Median Income, Under 30, 30-40, 40-50, 50-60, 60-70, 70+ ####
Median.table1 <- with(cancer, table(Median_Income_category, Surgery_Performed))
mosaic(Median.table1, shade=TRUE, main="Median Income vs. Surgery Performed") 
Median.table2_large <- with(cancer_large_radiation, table(Median_Income_category, Radiation))
mosaic(Median.table2_large, shade=TRUE, main="Median Income vs. Radiation")  
Median.table2_small <- with(cancer_small_radiation, table(Median_Income_category, Radiation))
mosaic(Median.table2_small, shade=TRUE, main="Median Income vs. Radiation") 
Median.table3 <- with(cancer, table(Median_Income_category, Chemotherapy))
mosaic(Median.table3, shade=TRUE, main="Median Income vs. Chemotherapy") 

# Language, 0-5, 5-10, 10-15, 15+ ####
Language.table1 <- with(cancer, table(Language_Isolation_Perc_category, Surgery_Performed))
mosaic(Language.table1, shade=TRUE, main="Language vs. Surgery Performed") 
Language.table2_large <- with(cancer_large_radiation, table(Language_Isolation_Perc_category, Radiation))
mosaic(Language.table2_large, shade=TRUE, main="Language vs. Radiation")  
Language.table2_small <- with(cancer_small_radiation, table(Language_Isolation_Perc_category, Radiation))
mosaic(Language.table2_small, shade=TRUE, main="Language vs. Radiation") 
Language.table3 <- with(cancer, table(Language_Isolation_Perc_category, Chemotherapy))
mosaic(Language.table3, shade=TRUE, main="Language vs. Chemotherapy") 

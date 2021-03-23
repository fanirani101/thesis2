# Install required packages

if (!require(Hmisc)) install.packages("Hmisc")
library(Hmisc)

if (!require(pastecs)) install.packages("pastecs")
library(pastecs)

if (!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)

if (!require(ppcor)) install.packages("ppcor")
library(ppcor)

# Initialise required functions and data

source("Code/Sort data.R")

# This script analyses the univariate statistics per cohort and plots it
describe <- Hmisc::describe
desc_master <- describe(master_data, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1, type=3,check=FALSE,fast=FALSE, digits=3)
desc_master_final <- describe(master_data_final, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1, type=3,check=FALSE,fast=FALSE, digits=3)
desc_master_max <- describe(master_data_max, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1, type=3,check=FALSE,fast=FALSE, digits=3)

LL <- list()
p <- 1
for (df in L_2){
  s <- names(df)
  df$Pass <- as.factor(df$Pass) 
  
  LL$df <- list()
  k<-1
  
  par(mfrow=c(dim(df)[2]-1, dim(df)[2]-2))
  for (i in seq(2,dim(df)[2]-1)){
    for (j in seq(i+1, dim(df)[2])){
      LL$df[[k]] <- ggscatter(df, x=s[i], y=s[j], add="reg.line", color=s[1], shape=s[1], palette = c(`1` = "black", `0` = "red"))
      k <- k+1
    }
  }
  names(LL)[length(names(LL))] <- names(L_2)[p]
  p <- p+1
}

# It is clear from the following that the data from cohorts 2016-17 and 2017-18 
## are most useful due to the number of complete cases. Moreover the below plots 
## indicate that the cohorts for 2015-16 and 2018-19 do not have enough complete 
## rows to involve all courses.

df <- L_2[[1]]
desc_df <- describe(df, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1, type=3,check=FALSE,fast=FALSE, digits=3)
plot(desc_df, which='continuous')
#dev.off()

df <- L_2[[2]]
desc_df <- describe(df, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1, type=3,check=FALSE,fast=FALSE, digits=3)
plot(desc_df, which='continuous')
#dev.off()

df <- L_2[[3]]
desc_df <- describe(df, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1, type=3,check=FALSE,fast=FALSE, digits=3)
plot(desc_df, which='continuous')
#dev.off()

df <- L_2[[4]]
desc_df <- describe(df, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1, type=3,check=FALSE,fast=FALSE, digits=3)
plot(desc_df, which='continuous')
#dev.off()

# Therefore we save these two data frames for further use.

cohort_1617 <- L_2[[2]]
cohort_1718 <- L_2[[3]]

cohort_1617_pass <- subset(cohort_1617, cohort_1617$Pass == 1)[, -1]
cohort_1617_fail <- subset(cohort_1617, cohort_1617$Pass == 0)[, -1]

cohort_1718_pass <- subset(cohort_1718, cohort_1718$Pass == 1)[, -1]
cohort_1718_fail <- subset(cohort_1718, cohort_1718$Pass == 0)[, -1]

# In order to correctly impute the missing data, we must analyse the conditional
## dependencies between courses and the univariate statistics of each course.

## It is clear from the below that the distribution of grades is generally not 
### normal. 

round(stat.desc(cohort_1617, norm=TRUE, p=0.95)[-c(2,6:7,13:18),], digits=3)
round(stat.desc(cohort_1718, norm=TRUE, p=0.95)[-c(2,6:7,13:18),], digits=3)

round(stat.desc(cohort_1617_pass, norm=TRUE, p=0.95)[-c(2,6:7,13:18),], digits=3)
round(stat.desc(cohort_1617_fail, norm=TRUE, p=0.95)[-c(2,6:7,13:18),], digits=3)

round(stat.desc(cohort_1718_pass, norm=TRUE, p=0.95)[-c(2,6:7,13:18),], digits=3)
round(stat.desc(cohort_1718_fail, norm=TRUE, p=0.95)[-c(2,6:7,13:18),], digits=3)

gghistogram(cohort_1617[, -1], x=names(cohort_1617[, -1]), add = "mean", combine=TRUE, bins=19, na.rm=TRUE)
gghistogram(cohort_1718[, -1], x=names(cohort_1718[, -1]), add = "mean", combine=TRUE, bins=19, na.rm=TRUE)

gghistogram(cohort_1617_pass, x=names(cohort_1617_pass), add = "mean", combine=TRUE, bins=19, na.rm=TRUE)
gghistogram(cohort_1617_fail, x=names(cohort_1617_fail), add = "mean", combine=TRUE, bins=19, na.rm=TRUE)

gghistogram(cohort_1718_pass, x=names(cohort_1718_pass), add = "mean", combine=TRUE, bins=19, na.rm=TRUE)
gghistogram(cohort_1718_fail, x=names(cohort_1718_fail), add = "mean", combine=TRUE, bins=19, na.rm=TRUE)

## As the data is semi-continuous (rounded to nearest 0.5), Spearman's Rho is 
### used to measure the pairwise-correlation

round(cor(cohort_1617[, -1], method="spearman", use="pairwise.complete.obs"), 3)
round(cor(cohort_1718[, -1], method="spearman", use="pairwise.complete.obs"), 3)

round(cor(cohort_1617_pass, method="spearman", use="pairwise.complete.obs"), 3)
round(cor(cohort_1617_fail, method="spearman", use="pairwise.complete.obs"), 3)

round(cor(cohort_1718_pass, method="spearman", use="pairwise.complete.obs"), 3)
round(cor(cohort_1718_fail, method="spearman", use="pairwise.complete.obs"), 3)

## The semi-partial correlations are also computed. Note that there are only 7 
### complete row entries for the last data set, which indicates that we must 
### remove some columns in order to maximise the accuracy of the imputations and 
### therefore the final analysis.

cor_mat_fun(spcor(cc(cohort_1617[, -1]), method="kendall"))$p.value
cor_mat_fun(spcor(cc(cohort_1718[, -1]), method="kendall"))$p.value

cor_mat_fun(spcor(cc(cohort_1617_pass), method="kendall"))$p.value
cor_mat_fun(spcor(cc(cohort_1617_fail), method="kendall"))$p.value

cor_mat_fun(spcor(cc(cohort_1718_pass), method="kendall"))$p.value
cor_mat_fun(spcor(cc(cohort_1718_fail), method="kendall"))$p.value



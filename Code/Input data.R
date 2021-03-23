# Install required packages 

if (!require(readxl)) install.packages("readxl")
library(readxl)

if (!require(stringr)) install.packages("stringr")
library(stringr)

if (!require(DescTools)) install.packages("DescTools")
library(DescTools)

# Initialise required functions

source("Code/Functions.R")

# This import the excel from file and compiles a data frame of grades
## Analysis 
### 2016 - 2017

#### First attempt
analysis_1617_first <- input_manage("Data/GRADES ALL/ANALYSIS/Analysis_1617_first.xlsx")
#### Second attempt
analysis_1617_resit <- input_manage("Data/GRADES ALL/ANALYSIS/Analysis_1617_resit.xlsx")
#### Final grade
analysis_1617_final <- update_set(analysis_1617_first, analysis_1617_resit, "Analysis 1617", "Username")

### 2017 - 2018

#### First attempt
analysis_1718_first <- input_manage("Data/GRADES ALL/ANALYSIS/Analysis_1718_first.xlsx")
#### Second attempt
analysis_1718_resit <- input_manage("Data/GRADES ALL/ANALYSIS/Analysis_1718_resit.xlsx")
#### Final grade
analysis_1718_final <- update_set(analysis_1718_first, analysis_1718_resit, "Analysis 1718", "Username")

### 2018 - 2019

#### First attempt
analysis_1819_first <- input_manage("Data/GRADES ALL/ANALYSIS/Analysis_1819_first.xlsx")
#### Second attempt
analysis_1819_resit <- input_manage("Data/GRADES ALL/ANALYSIS/Analysis_1819_resit.xlsx")
#### Final grade
analysis_1819_final <- update_set(analysis_1819_first, analysis_1819_resit, "Analysis 1819", "Username")

### Combine
L <- list(analysis_1617_first, analysis_1617_resit, analysis_1718_first, analysis_1718_resit, analysis_1819_first, analysis_1819_resit)
analysis_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

## Calculus 1 
### 2016 - 2017

#### First attempt
calculus1_1617_first <- input_manage("Data/GRADES ALL/CALCULUS 1/calculus1_1617_first.xlsx")
#### Second attempt
calculus1_1617_resit <- input_manage("Data/GRADES ALL/CALCULUS 1/calculus1_1617_resit.xlsx")
#### Final grade
calculus1_1617_final <- update_set(calculus1_1617_first, calculus1_1617_resit, "Calculus 1 1617", "Username")

### 2017 - 2018

#### First attempt
calculus1_1718_first <- input_manage("Data/GRADES ALL/CALCULUS 1/calculus1_1718_first.xlsx")
#### Second attempt
calculus1_1718_resit <- input_manage("Data/GRADES ALL/CALCULUS 1/calculus1_1718_resit.xlsx")
#### Final grade
calculus1_1718_final <- update_set(calculus1_1718_first, calculus1_1718_resit, "Calculus 1 1718", "Username")

### 2018 - 2019

#### First attempt
calculus1_1819_first <- input_manage("Data/GRADES ALL/CALCULUS 1/calculus1_1819_first.xlsx")
#### Second attempt
calculus1_1819_resit <- input_manage("Data/GRADES ALL/CALCULUS 1/calculus1_1819_resit.xlsx")
#### Final grade
calculus1_1819_final <- update_set(calculus1_1819_first, calculus1_1819_resit, "Calculus 1 1819", "Username")

### Combine
L <- list(calculus1_1617_first, calculus1_1617_resit, calculus1_1718_first, calculus1_1718_resit, calculus1_1819_first, calculus1_1819_resit)
calculus1_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

## Calculus 2 
### 2015 - 2016

#### First attempt
calculus2_1516_first <- input_manage("Data/GRADES ALL/CALCULUS 2/calculus2_1516_first.xlsx")
#### Second attempt
calculus2_1516_resit <- input_manage("Data/GRADES ALL/CALCULUS 2/calculus2_1516_resit.xlsx")
#### Final grade
calculus2_1516_final <- update_set(calculus2_1516_first, calculus2_1516_resit, "Calculus 2 1516", "Username")

### 2016 - 2017

#### First attempt
calculus2_1617_first <- input_manage("Data/GRADES ALL/CALCULUS 2/calculus2_1617_first.xlsx")
#### Second attempt
calculus2_1617_resit <- input_manage("Data/GRADES ALL/CALCULUS 2/calculus2_1617_resit.xlsx")
#### Final grade
calculus2_1617_final <- update_set(calculus2_1617_first, calculus2_1617_resit, "Calculus 2 1617", "Username")

### 2017 - 2018

#### First attempt
calculus2_1718_first <- input_manage("Data/GRADES ALL/CALCULUS 2/calculus2_1718_first.xlsx")
#### Second attempt
calculus2_1718_resit <- input_manage("Data/GRADES ALL/CALCULUS 2/calculus2_1718_resit.xlsx")
#### Final grade
calculus2_1718_final <- update_set(calculus2_1718_first, calculus2_1718_resit, "Calculus 2 1718", "Username")

### 2018 - 2019

#### First attempt
calculus2_1819_first <- input_manage("Data/GRADES ALL/CALCULUS 2/calculus2_1819_first.xlsx")
#### Second attempt
calculus2_1819_resit <- input_manage("Data/GRADES ALL/CALCULUS 2/calculus2_1819_resit.xlsx")
#### Final grade
calculus2_1819_final <- update_set(calculus2_1819_first, calculus2_1819_resit, "Calculus 2 1819", "Username")

### Combine
L <- list(calculus2_1617_first, calculus2_1617_resit, calculus2_1718_first, calculus2_1718_resit, calculus2_1819_first, calculus2_1819_resit)
calculus2_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

## Computer-Aided Problem Solving
### 2016 - 2017

#### First attempt
caps_1617_first <- input_manage("Data/GRADES ALL/CAPS/caps_1617_first.xlsx")
#### Second attempt
caps_1617_resit <- input_manage("Data/GRADES ALL/CAPS/caps_1617_resit.xlsx")
#### Final grade
caps_1617_final <- update_set(caps_1617_first, caps_1617_resit, "CAPS 1617", "Username")

### 2017 - 2018

#### First attempt
caps_1718_first <- input_manage("Data/GRADES ALL/CAPS/caps_1718_first.xlsx")
#### Second attempt
caps_1718_resit <- input_manage("Data/GRADES ALL/CAPS/caps_1718_resit.xlsx")
#### Final grade
caps_1718_final <- update_set(caps_1718_first, caps_1718_resit, "CAPS 1718", "Username")

### 2018 - 2019

#### First attempt
caps_1819_first <- input_manage("Data/GRADES ALL/CAPS/caps_1819_first.xlsx")
#### Second attempt
caps_1819_resit <- input_manage("Data/GRADES ALL/CAPS/caps_1819_resit.xlsx")
#### Final grade
caps_1819_final <- update_set(caps_1819_first, caps_1819_resit, "CAPS 1819", "Username")

### Combine
L <- list(caps_1617_first, caps_1617_resit, caps_1718_first, caps_1718_resit, caps_1819_first, caps_1819_resit)
caps_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

## Complex Analysis
### 2017 - 2018

#### First attempt
complex_1718_first <- input_manage("Data/GRADES ALL/COMPLEX ANALYSIS/complex_1718_first.xlsx")
#### Second attempt
complex_1718_resit <- input_manage("Data/GRADES ALL/COMPLEX ANALYSIS/complex_1718_resit.xlsx")
#### Final grade
complex_1718_final <- update_set(complex_1718_first, complex_1718_resit, "Complex 1718", "Username")

### 2018 - 2019

#### First attempt
complex_1819_first <- input_manage("Data/GRADES ALL/COMPLEX ANALYSIS/complex_1819_first.xlsx")
#### Second attempt
complex_1819_resit <- input_manage("Data/GRADES ALL/COMPLEX ANALYSIS/complex_1819_resit.xlsx")
#### Final grade
complex_1819_final <- update_set(complex_1819_first, complex_1819_resit, "Complex 1819", "Username")

### Combine
L <- list(complex_1718_first, complex_1718_resit, complex_1819_first, complex_1819_resit)
complex_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

## Linear Algebra 1
### 2016 - 2017

#### Final grade
LinAlg1_1617_final <- input_manage("Data/GRADES ALL/LINEAR ALGEBRA 1/LinAlg1_1617.xlsx")

### 2017 - 2018

#### Final grade
LinAlg1_1718_final <- input_manage("Data/GRADES ALL/LINEAR ALGEBRA 1/LinAlg1_1718.xlsx")

### 2018 - 2019

#### Final grade
LinAlg1_1819_final <- input_manage("Data/GRADES ALL/LINEAR ALGEBRA 1/LinAlg1_1819.xlsx")

### Combine
L <- list(LinAlg1_1617_final, LinAlg1_1718_final, LinAlg1_1819_final)
LinAlg1_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

## Linear Algebra 2
### 2016 - 2017

#### First attempt
LinAlg2_1617_first <- input_manage("Data/GRADES ALL/LINEAR ALGEBRA 2/LinAlg2_1617_first.xlsx")
#### Second attempt
LinAlg2_1617_resit <- input_manage("Data/GRADES ALL/LINEAR ALGEBRA 2/LinAlg2_1617_resit.xlsx")
#### Final grade
LinAlg2_1617_final <- update_set(LinAlg2_1617_first, LinAlg2_1617_resit, "LinAlg 2 1617", "Username")

### 2017 - 2018

#### First attempt
LinAlg2_1718_first <- input_manage("Data/GRADES ALL/LINEAR ALGEBRA 2/LinAlg2_1718_first.xlsx")
#### Second attempt
LinAlg2_1718_resit <- input_manage("Data/GRADES ALL/LINEAR ALGEBRA 2/LinAlg2_1718_resit.xlsx")
#### Final grade
LinAlg2_1718_final <- update_set(LinAlg2_1718_first, LinAlg2_1718_resit, "LinAlg 2 1718", "Username")

### 2018 - 2019

#### First attempt
LinAlg2_1819_first <- input_manage("Data/GRADES ALL/LINEAR ALGEBRA 2/LinAlg2_1819_first.xlsx")
#### Second attempt
LinAlg2_1819_resit <- input_manage("Data/GRADES ALL/LINEAR ALGEBRA 2/LinAlg2_1819_resit.xlsx")
#### Final grade
LinAlg2_1819_final <- update_set(LinAlg2_1819_first, LinAlg2_1819_resit, "LinAlg 2 1819", "Username")

### Combine
L <- list(LinAlg2_1617_first, LinAlg2_1617_resit, LinAlg2_1718_first, LinAlg2_1718_resit, LinAlg2_1819_first, LinAlg2_1819_resit)
LinAlg2_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

## Numerical Mathematics 1
### 2017 - 2018

#### First attempt
NumMat1_1718_first <- input_manage("Data/GRADES ALL/NUMERICAL MATHEMATICS 1/NumMat1_1718_first.xlsx")
#### Second attempt
NumMat1_1718_resit <- input_manage("Data/GRADES ALL/NUMERICAL MATHEMATICS 1/NumMat1_1718_resit.xlsx")
#### Final grade
NumMat1_1718_final <- update_set(NumMat1_1718_first, NumMat1_1718_resit, "NumMat 1 1718", "Username")

### 2018 - 2019

#### First attempt
NumMat1_1819_first <- input_manage("Data/GRADES ALL/NUMERICAL MATHEMATICS 1/NumMat1_1819_first.xlsx")
#### Second attempt
NumMat1_1819_resit <- input_manage("Data/GRADES ALL/NUMERICAL MATHEMATICS 1/NumMat1_1819_resit.xlsx")
#### Final grade
NumMat1_1819_final <- update_set(NumMat1_1819_first, NumMat1_1819_resit, "NumMat 1 1819", "Username")

### Combine
L <- list(NumMat1_1718_first, NumMat1_1718_resit, NumMat1_1819_first, NumMat1_1819_resit)
NumMat1_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

## Ordinary Differential Equations
### 2016 - 2017

#### First attempt
ode_1617_first <- input_manage("Data/GRADES ALL/ODE/ode_1617_first.xlsx")
#### Second attempt
ode_1617_resit <- input_manage("Data/GRADES ALL/ODE/ode_1617_resit.xlsx")
#### Final grade
ode_1617_final <- update_set(ode_1617_first, ode_1617_resit, "ODE 1617", "Username")

### 2017 - 2018

#### First attempt
ode_1718_first <- input_manage("Data/GRADES ALL/ODE/ode_1718_first.xlsx")
#### Second attempt
ode_1718_resit <- input_manage("Data/GRADES ALL/ODE/ode_1718_resit.xlsx")
#### Final grade
ode_1718_final <- update_set(ode_1718_first, ode_1718_resit, "ODE 1718", "Username")

### 2018 - 2019

#### First attempt
ode_1819_first <- input_manage("Data/GRADES ALL/ODE/ode_1819_first.xlsx")
#### Second attempt
ode_1819_resit <- input_manage("Data/GRADES ALL/ODE/ode_1819_resit.xlsx")
#### Final grade
ode_1819_final <- update_set(ode_1819_first, ode_1819_resit, "ODE 1819", "Username")

### Combine
L <- list(ode_1617_first, ode_1617_resit, ode_1718_first, ode_1718_resit, ode_1819_first, ode_1819_resit)
ode_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

## Probability Theory
### 2016 - 2017

#### First attempt
prob_1617_first <- input_manage("Data/GRADES ALL/PROB THEORY/prob_1617_first.xlsx")
#### Second attempt
prob_1617_resit <- input_manage("Data/GRADES ALL/PROB THEORY/prob_1617_resit.xlsx")
#### Final grade
prob_1617_final <- update_set(prob_1617_first, prob_1617_resit, "Prob Theory 1617", "Username")

### 2017 - 2018

#### First attempt
prob_1718_first <- input_manage("Data/GRADES ALL/PROB THEORY/prob_1718_first.xlsx")
#### Second attempt
prob_1718_resit <- input_manage("Data/GRADES ALL/PROB THEORY/prob_1718_resit.xlsx")
#### Final grade
prob_1718_final <- update_set(prob_1718_first, prob_1718_resit, "Prob Theory 1718", "Username")

### 2018 - 2019

#### First attempt
prob_1819_first <- input_manage("Data/GRADES ALL/PROB THEORY/prob_1819_first.xlsx")
#### Second attempt
prob_1819_resit <- input_manage("Data/GRADES ALL/PROB THEORY/prob_1819_resit.xlsx")
#### Final grade
prob_1819_final <- update_set(prob_1819_first, prob_1819_resit, "Prob Theory 1819", "Username")

### Combine
L <- list(prob_1617_first, prob_1617_resit, prob_1718_first, prob_1718_resit, prob_1819_first, prob_1819_resit)
prob_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

## Statistical Reasoning
### 2016 - 2017

#### First attempt
StatR_1617_final <- input_manage("Data/GRADES ALL/REASONING/StatR_1617_first.xlsx")
names(StatR_1617_final)[2] <- "StatR 1617"

### 2017 - 2018

#### First attempt
StatR_1718_first <- input_manage("Data/GRADES ALL/REASONING/StatR_1718_first.xlsx")
#### Second attempt
StatR_1718_resit <- input_manage("Data/GRADES ALL/REASONING/StatR_1718_resit.xlsx")
#### Final grade
StatR_1718_final <- update_set(StatR_1718_first, StatR_1718_resit, "StatR 1718", "Username")

### 2018 - 2019

#### First attempt
StatR_1819_first <- input_manage("Data/GRADES ALL/REASONING/StatR_1819_first.xlsx")
#### Second attempt
StatR_1819_resit <- input_manage("Data/GRADES ALL/REASONING/StatR_1819_resit.xlsx")
#### Final grade
StatR_1819_final <- update_set(StatR_1819_first, StatR_1819_resit, "StatR 1819", "Username")

### Combine
L <- list(StatR_1617_final, StatR_1718_first, StatR_1718_resit, StatR_1819_first, StatR_1819_resit)
StatR_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

## Statistics
### 2016 - 2017

#### First attempt
stat_1617_first <- input_manage("Data/GRADES ALL/STATISTICS/stat_1617_first.xlsx")
#### Second attempt
stat_1617_resit <- input_manage("Data/GRADES ALL/STATISTICS/stat_1617_resit.xlsx")
#### Final grade
stat_1617_final <- update_set(stat_1617_first, stat_1617_resit, "Prob Theory 1617", "Username")

### 2017 - 2018

#### First attempt
stat_1718_first <- input_manage("Data/GRADES ALL/STATISTICS/stat_1718_first.xlsx")
#### Second attempt
stat_1718_resit <- input_manage("Data/GRADES ALL/STATISTICS/stat_1718_resit.xlsx")
#### Final grade
stat_1718_final <- update_set(stat_1718_first, stat_1718_resit, "Stats 1718", "Username")

### 2018 - 2019

#### First attempt
stat_1819_first <- input_manage("Data/GRADES ALL/STATISTICS/stat_1819_first.xlsx")
#### Second attempt
stat_1819_resit <- input_manage("Data/GRADES ALL/STATISTICS/stat_1819_resit.xlsx")
#### Final grade
stat_1819_final <- update_set(stat_1819_first, stat_1819_resit, "Stats 1819", "Username")

### Combine
L <- list(stat_1617_first, stat_1617_resit, stat_1718_first, stat_1718_resit, stat_1819_first, stat_1819_resit)
stat_all <- Reduce(function(x, y) merge(x, y, all=TRUE, by="Username"), L)

# Combine all first and resit exams, and then combine all final grades

L <- list(analysis_all, calculus1_all, calculus2_all, caps_all, complex_all, LinAlg1_all, LinAlg2_all, NumMat1_all, ode_all, prob_all, stat_all, StatR_all)
master_data <- Reduce(function(x,y) merge(x,y, all=TRUE, by="Username"), L)

L1 <- list("1516", "1617", "1718", "1819")
L2 <- list("analysis", "calculus1", "calculus2", "caps", "complex", "LinAlg1", "LinAlg2", "NumMat1", "ode", "prob", "stat", "StatR")
L <- grade_getr(L1, L2, "final")

master_data_final <- Reduce(function(x, y) merge(x, y, all=TRUE), L)

# It is useful to extract the data frame containing the final grade across all attempts

L <- list("analysis", "calculus 1", "calculus 2", "caps", "complex", "LinAlg1", "LinAlg 2", "NumMat 1", "ode", "prob", "stat", "StatR")  
master_data_max <- max_grade_getr(L, master_data_final)

# Now that we have three master data frames we can remove the student number identifies in the Username column

master_data <- master_data[,-1]
master_data_final <- master_data_final[,-1]
master_data_max <- master_data_max[,-1]

# To ensure that all column names are uniformly written

i = 1
for (y in names(master_data_final)){
  names(master_data_final)[i] <- gsub("_", " ", y)
  i = i+1
}

write.csv(master_data, file = "Data/master_data.csv")
write.csv(master_data_final, file = "Data/master_data_final.csv")
write.csv(master_data_max, file = "Data/master_data_max.csv")

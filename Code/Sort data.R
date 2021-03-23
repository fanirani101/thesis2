# Install required packages

if (!require(mice)) install.packages("mice")
library(mice)

if (!require(plotly)) install.packages("plotly")
library(plotly)

if (!require(numbers)) install.packages("numbers")
library(numbers)

if (!require(sjPlot)) install.packages("sjPlot")
library(sjPlot)

# Initialise required functions and data

source("Code/Functions.R")

master_data <- read.csv("Data/master_data.csv", row.names = 1)
master_data_final <- read.csv("Data/master_data_final.csv", row.names = 1)
master_data_max <- read.csv("Data/master_data_max.csv", row.names = 1)

# The next step is to find meaningful interpretations of the data, 
## and therefore we move to split the students into their respective cohorts

L <- cohort_split(master_data_final, c("Calculus", "LinAlg"))

# Now that we have sorted the students into their respective cohorts, we need to
## be critical of what data is valid or may be omitted based on the missing data
## patterns.
# 
# First we need to get the students final grades using max_grade_getr2

L1 <- list("analysis", "calculus 1", "calculus 2", "caps", "complex", "LinAlg1", "LinAlg 2", "NumMat 1", "ode", "prob", "stats", "StatR")  
L_2 <- list()

x <- length(L)

for (i in seq(1,x)){
  dff <- L[[i]]
  L_2[[i]] <- max_grade_getr2(L1, dff)
  names(L_2)[i] <- names(L)[i]
}

# Now we must look at the missing data patterns to determine which cohort data 
## frame is most useful, i.e. has missing data patterns which indicate that 
## imputation can be performed to complete the data 

#md.pattern(L_2[[1]], rotate.names=TRUE)
#dev.off()
#md.pattern(L_2[[2]], rotate.names=TRUE)
#dev.off()
#md.pattern(L_2[[3]], rotate.names=TRUE)
#dev.off()
#md.pattern(L_2[[4]], rotate.names=TRUE)
#dev.off()

# It is useful to deduce which students have passed and which have failed; the 
## cut-off grade is 5.5/10, and if one of the grades is less than 5.5 then the 
## student has failed, otherwise they pass. NA grades will be treated as 
## "passes" for the meantime.

L_2 <- pass_fail_split(L_2)


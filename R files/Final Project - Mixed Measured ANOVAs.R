# Final Project -> Repeated Measured ANOVAs
## Repeated ANOVAs -> different years 2010, 2012, 2014, 2016
### ANOVAs bc of homogeneous frequency 
### Measure changes over time -> measuring same thing over and over again

# Install Packages
install.packages("rcompanion") 
install.packages("car")
install.packages("reshape2")
install.packages("reshape")
install.packages("dplyr")
library("rcompanion") #check for the assumption of normality
library("car") #if you need to run an ANOVA that will correct for a violation of homogeneity of variance
library("reshape2")
library("reshape")
library("data.table")
library("dplyr")
library("fastR2") #which is used for some data wrangling to get your data in the right shape for repeated measures ANOVAs

# Load Data -> England Total
EnglandG <- read.csv("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Datasets/Data Wrangled Datasets/Eng_Reshaped_Location.csv")

# Question Setup 
# Changes over time for England waste gen from years 2010, 2012, 2014, 2016 by various categories.
## such as household, commercial, Industrial, etc

#Data Wrangle -> melt() -> reshape data 
  #data format is required for repeated measures ANOVAs in R -> Long 

# Testing Assumptions
## Normality
plotNormalHistogram(EnglandG$Weight)
  #positively skewed 
EnglandG$WeightSQRT <- sqrt(EnglandG$Weight)
plotNormalHistogram(EnglandG$WeightSQRT)
EnglandG$WeightLOG <- log(EnglandG$Weight)
plotNormalHistogram(EnglandG$WeightLOG)
EnglandG2 <- NaRV.omit(EnglandG)
plotNormalHistogram(EnglandG2$WeightLOG)
  #Looking normally distributed 

##Homogeneity of Variance ->  changed over time based -> car lib
#leveneTest(DV~IV*contrasts, data=df)   ; Location_Type = time variable 
#Weight (total waste generated) by waste type over time
leveneTest(Weight ~ EWC.STAT.description*, data=EnglandG)
  #p<0.05

#Sample Size -> 33 which is more than at least 20 per IVs
#Sphericity -> only way to test for sphericity in R is to take a multivariate approach and make it work for an ANOVA, which is a bit complex



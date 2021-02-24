#Final Project -> MANOVA
# - DV: total waste generation & total sent treatment / IV: waste type

#NOTES: 
### STEPS TO MANOVA
# define: multivariate analysis of variance, is like an ANOVA, but on steroids!
# (The assumptions for MANOVA are relatively similar to those for ANOVAs, 
# but they are ramped up a notch to handle the addition of multiple DV and the increased power.)

### Sample Size
# There must be more cases than DV in every cell. In addition, there must be at 
# least 20 cases per IV, as per ANOVAs.

### Multivariate Normality
# What? You know all about normality, but what is this suspicious multivariate normality?! 
# All it means is that your dependent variables need to be normally distributed when they are lumped all together in one uber-variable that you'll use for your MANOVA.

### Homogeneity of Variance
# Like ANOVAs, you need to make sure that the variables you are using have 
# relatively equal variance. 

### Absence of Multicollinearity
# Multicollinearity is when there is a significant relationship between the DV in your model.
# Testing for multicollinearity just requires a correlation matrix, 
# although there are specific statistics designed to test for it as well.

### Independence
# assumption of independence is the same for ANOVAs as it is for MANOVAs. 
# In a nutshell, the different levels of your independent variable should NOT be related to each other!
# violated the assumption of independence and should not choose to run a MANOVA!


### DATA WRANGLE:
# Ensure Variables are Numeric: format your dependent variables are in.
### str(kickstarter$pledged)
### str(kickstarter$backers)

# Best convert them using the as.numeric() function.
### kickstarter$pledged <- as.numeric(kickstarter$pledged)
### kickstarter$backers <- as.numeric(kickstarter$backers)

# Subsetting
### keeps <- c("pledged", "backers")
### kickstarter1 <- kickstarter[keeps]
# Then limit the number of rows:
### kickstarter2 <- kickstarter1[1:5000,]

# Format as a Matrix
# Lastly, format the data as a matrix:
### kickstarter3 <- as.matrix(kickstarter2)

# TEST ASSUMPTIONS:
# With the data wrangling out of the way, 
# it is now time to test assumptions!

# Sample Size: (least 20 cases per independent variable, and that there must be 
# more cases then dependent variables in every cell.)

# Multivariate Normality
# To test for multivariate normality, you will use the dataset you wrangled, 
# kickstarter3, in the Wilks-Shapiro test. You can do that with the function 
# mshapiro.test() pulled from the mvnormtest library:

### mshapiro.test(t(kickstarter3))
# p value is significant at p < .05

### Homogeneity of Variance
# levenes test
### leveneTest(pledged ~ country, data=kickstarter)
# results for pleged
# Don't forget to test for backers as well!
### leveneTest(backers ~ country, data=kickstarter)
# variable met the assumption of homogeneity of variance, since they were 
# both significant at p < .05. You have violated the assumption of 
# homogeneity of variance, but you will proceed for now for learning purposes.

# Absence of Multicollinearity
# Typically, multicollinearity can be assessed simply by running correlations 
# of your dependent variables with each other. A general rule of thumb is that 
# anything above approximately .7 for correlation (i.e. a strong correlation) 
# indicates the presence of multicollinearity. Check out the correlation between 
# pledged and backers with a simple cor.test() function:
### cor.test(kickstarter$pledged, kickstarter$backers, method="pearson", use="complete.obs")
# And voila! Finally an assumption you have met! With a correlation of r = .32, 
# you have an absence of multicollinearity.

# The Analysis
### MANOVA <- manova(cbind(pledged, backers) ~ country, data = kickstarter)
### summary(MANOVA)

# Post Hocs
### 

# ANOVAs as Post Hocs
### summary.aov(MANOVA, test = "wilks") 

### Assumptions of MANOVA:
# MANOVA can be used in certain conditions:
# The dependent variables should be normally distribute within groups. 
# The R function mshapiro.test( )[in the mvnormtest package] can be used to perform 
# the Shapiro-Wilk test for multivariate normality. 
# This is useful in the case of MANOVA, which assumes multivariate normality.
# Homogeneity of variances across the range of predictors.
# Linearity between all pairs of dependent variables, all pairs of covariates, and all dependent variable-covariate pairs in each cell

#___________________Eng and UK Total Waste and Treatment________________________

library("rcompanion")
library("car")
library("IDPmisc")
library("dplyr")
library("ggplot2")
library("mvnormtest") #test for multivariate normality mshapiro

#Load Data
EngTWaT <- read.csv("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Datasets/Data Wrangled Datasets/Eng_Total_updated.csv")

#Data Wrangling 
##Ensure Variables are Numeric 
str(EngTWaT$Total_waste_gen) 
str(EngTWaT$Total_treatment)  
  #are numeric 
    #df$cn <- as.numeric(df$cn) function if they are not set up as numbers 

## Drop missing -> data as mshaprio will give you an error
      
##Subsetting
### keep only your two dependent variables & then limit the number of rows df2 <- df1[1:5000,]
keeps <- c("Total_waste_gen", "Total_treatment")
EngTWaT2 <- EngTWaT[keeps]

##Format as a Matrix
EngTWaT3 <- as.matrix(EngTWaT2)
### You are now ready to perform the assumptions test for multivariate normality

#Test Assumptions 
## Sample size -> at least 20 cases per IV
  #Meets sample size -> 2 IVs = 40 cases, we have 132 entries 

## Multivariate Normality
mshapiro.test(t(EngTWaT3))
  #p<0.05
  #Violated the assumption of multivariate normality as the p value is significant at p < .05
  #so unfortunately, these data do not meet the assumption for MANOVAs

# Act with caution-> have violated the assuption******* 

## Homogeneity of Variance -> on both of your DVs:
leveneTest(Total_waste_gen ~ Waste_types, data=EngTWaT)
leveneTest(Total_treatment ~ Waste_types, data=EngTWaT) 
  #p<0.05
  #Unfortunately, neither variable met the assumption of homogeneity of variance, since they were both significant at p < .05. 
  #You have violated the assumption of homogeneity of variance

## Absence of Multicollinearity
  #Notes: can be assessed simply by running correlations of your dependent variables with each other
  #General rule of thumb is that anything above approximately .7 for correlation (i.e. a strong correlation) indicates the presence of multicollinearity
cor.test(EngTWaT$Total_waste_gen, EngTWaT$Total_treatment, method="pearson", use="complete.obs")
  #Results: Highly correlated 0.97, meaning presence of Multicolliearity 

# The Analysis
  #Notes: You are binding your 2 DVs together with the function cbind(), so they can be examined as one. 
    #Then you are able to specify your IV, like any other ANOVA. 
    # Results when you call summary()
MANOVA <- manova(cbind(Total_waste_gen, Total_treatment) ~ Waste_types, data = EngTWaT)
summary(MANOVA)
  #Looks like it was significant, too - there is a significant difference in total waste treated and the total waste generated by waste types.

# Post Hocs
  #But which dependent variable you ask? Well, that's where the post-hocs come in.
## ANOVAs as Post Hocs
  #The initial post-hoc for a MANOVA is, in fact, an ANOVA. Use summary.aov() function
  #Simply feed in the name of your MANOVA model. This function would work without the additional argument of test=, 
    #but like post-hocs for ANOVAs, it is nice to use a correction for Type I error since you are doing so many multiple comparisons. 
    #In this case, you can use the "wilks correction, specified above.
summary.aov(MANOVA, test = "wilks") 
  #there is a significant difference in both the amount of waste generated and treated by waste types.

#______UK_____
UK_TWaT <- read.csv("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Datasets/Data Wrangled Datasets/UK_Total_updated.csv")

#Data Wrangling 
## Verify var is numeric 
str(UK_TWaT$Total_waste_gen) 
str(UK_TWaT$Total_treatment) 

##Subsetting 
keeps <- c("Total_waste_gen", "Total_treatment")
UK_TWaT2 <- UK_TWaT[keeps]

##Format as Matrix 
UK_TWaT3 <- as.matrix(EngTWaT2)

#Test Assumptions 
## Sample size -> at least 20 cases per IV
#Meets sample size -> 2 IVs = 40 cases, we have 132 entries 

## Multivariate Normality
mshapiro.test(t(EngTWaT3))
#p<0.05, Violated the assumption of multivariate normality 

## Homogeneity of Variance -> on both of your DVs:
leveneTest(Total_waste_gen ~ Waste_types, data=UK_TWaT)
leveneTest(Total_treatment ~ Waste_types, data=UK_TWaT) 
#p<0.05, Violated the assumption of multivariate normality 

## Absence of Multicollinearity
cor.test(UK_TWaT$Total_waste_gen, UK_TWaT$Total_treatment, method="pearson", use="complete.obs")
  #Results: Highly correlated 0.97, meaning presence of Multicolliearity 

# The Analysis
MANOVA1 <- manova(cbind(Total_waste_gen, Total_treatment) ~ Waste_types, data = UK_TWaT)
summary(MANOVA1)
  #Looks like it was significant, too - there is a significant difference in total waste treated and the total waste generated by waste types.

# Post Hocs
summary.aov(MANOVA1, test = "wilks") 
  #there is a significant difference in both the amount of waste generated and treated by waste types.

# Final Project -> Basic One-Way ANOVAs & Two-Way ANOVAs
### Basic ANOVA for waste type and total waste
### Basic ANOVA for waste type and total treatment
### 2 way ANOVAs for Year and waste type, and total waste 
### 2 way ANOVAS for Year and waste type, and total treatment 
### 1 way ANOVA for countries(IVs +2 levels(UK&Eng)), and total waste gen and treatment (2 DVs)

#NOTES: 
# Steps of ANOVA:
## Normality 
## Homogeneity of Variance- equal amount of change among the groups
## Sample size- 20 rows per IV
## Independence- groups are unrelated

# Barlett's: 
##  barlett.test(DV ~ IV, data=dataframe)

# FLinger test:
##  flinger.test(DV ~ IV, dataframe)

## pvalue > .05 to pass the assumption 
## if dont pass, use Welch's one-way test

#ANOVA: 

# w/ Homogeneity of variance
## modelName <- aov(DV ~ IV)

# w/o Homogeneity of variance
## modelName <- lm(DV ~ IV, data=dataFrame)
## Anova(modelName, Type="II", white.adjust=TRUE)

#Post Hoc ('after this' in latin)
## pairwise.t.test(DV,IV,p.adjust="bonferroni")
## (then use dplyr  aggregation to get the means for each category)
## (explore, what groups from each other.) 
#_______________________________________________________________________________

#________Question: Waste Type and Total Waste___________________________________ 
#Import Packages 
install.packages("rcompanion")
install.packages("car")
install.packages("IDPmisc")
install.packages("dplyr")
install.packages("ggplot2")
library("rcompanion") #histograms and test normality OR 
library("ggplot2")  #histograms and test normality 
library("car")  #to do actual ANOVA
library("IDPmisc")  #to remove missing value 
library("dplyr")  #to look at the mean towards to end-> aggregate functions 

#__________________ England -> Total Generated Waste 
# Load Data -> England
EnglandG <- read.csv("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Taking Care of Rubbish- Chitra&Julie_FinalProject/Datasets/3.0) 3rd wrangled /3.3) Eng_Reshaped_Location.csv")

# Data Wrangle -> Python file -> melt() function to reshape data all classification freq in 1 column 
# In R: england = melt(England, id = c("X", "Year", 'Waste_types', 'Haz_NonHaz'), measured = c('A', 'B', 'C10_C12', 'C13_C15', 'C16', 'C17_C18', 'C19', 'C20_C22', 'C23', 'C24_C25', 'C26_C30', 'C31_C33', 'D', 'E36_E37_E39', 'F', 'G_U_X_G4677', 'EP_HH'), variable.name="Location", value.name="Freq")

# Test for Assumptions
## 1. Normality (rcompanion function) -> for DV which is total waste 
plotNormalHistogram(EnglandG$Weight)
  # Positively skewed, hence need to adjust
### Square root for positive skew
EnglandG$WeightSQRT <- sqrt(EnglandG$Weight)
plotNormalHistogram(EnglandG$WeightSQRT)
  #Looks little normal, but let try log
### Log 
EnglandG$WeightLOG <- log(EnglandG$Weight)
plotNormalHistogram(EnglandG$WeightLOG)
  #error b/c we gone so high with log that someone of them are registered as infinite value. The way to that issue is to remove infinity. Form of missing data with IDPmisc package
EnglandG2 <- NaRV.omit(EnglandG)  #check with histogram 
plotNormalHistogram(EnglandG2$WeightLOG)
  # yay, it looks like a normal distribution, hence we will be using log for rest of our testing analysis 

## 2. Homogeneity of Variance -> equal amount of change among the groups -> either Barlett's/ FLinger test. DV ~ (telda means by) IV
bartlett.test(Weight ~ EWC.STAT.description, data=EnglandG)
fligner.test(Weight ~ EWC.STAT.description, data=EnglandG)
  #results: p-value is < 0.05 meaning we have violated the assumption of homogeneity of variance-> we have heterogeneity or unequal variance

# ANOVAs without homogeneity of variance 
##notes:name <- lm(DV~IV, data=df)
ANOVA <- lm(WeightLOG ~ EWC.STAT.description, data = EnglandG2)
##notes:function(name, Type="", white.adjust=TRUE) white.adjust=TRUE -> makes this ANOVA appropriate when you have unequal variance
Anova(ANOVA, Type="II", white.adjust=TRUE) #running anova for unequal var 
  # results: p-value < 0.05 is significant overall, that means there is an influence on the waste type  and how much waste is being generated 
  # we know that there is a difference, the way we do that is with post hocs, post hocs at it after the analysis

# Post Hocs 
##notes:pairwise is type of Post hocs, DV and IV, p.adjust="bonferroni" helps us control for the number of ttest we are going to be doing, pool.sd=FALSE use this option as we have not met the assumption of Homogeneity another way to control for that
##notes:not want to look at normality changed dataframe as it wont make a whole lot of sense; so original data
pairwise.t.test(EnglandG$Weight, EnglandG$EWC.STAT.description, p.adjust="bonferroni", pool.sd=FALSE)
  #results: we do not have difference b/w acid waste and animal waste as p-value is 1.0 > 0.05 expect few.
  #results:Following has difference between both -> p < 0.05 
                  #Used oils and animal faeces, urine and manure 1.9e-0.5
                  #Mineral waste from waste treatment & stabilised waste, & Industrial effluent sludges 0.003
                  #Sludges & liquid wastes from waste treatment  & Industrial effluent sludges  - 0.0019
                  #Waste containing PCB & Industrial effluent sludges Metallic wastes, ferrous - 0.0025 
                  #Used oils & Mineral waste from waste treatment & stabilised waste 4.3e-05
                  #Used oils & Sludges & liquid wastes from waste treatment 1.9e-05
                  #Used oils & Sorting residues 1.9e-0.5
                  #Waste containing PCB & Used oils 2.9e-0.5

  #still isn't helpful, as we do not know whats higher and lower so look at the mean 
 
# you only need to do the mean after post hocs, if anything shows of significant value.
# the mean tells us which one is the highest mean
##notes: newname <- df and then grouping by (IV) then summarize function and find the means(DV), then arrange in biggest to smallest
EnglandG_Means <- EnglandG %>% filter(EWC.STAT.description %in% c('Used oils', 'Animal faeces, urine & manure', "Mineral waste from waste treatment & stabilised waste", "Industrial effluent sludges", "Waste containing PCB", "Sludges & liquid wastes from waste treatment", "Sorting residues"))
EnglandG_Means2 <- EnglandG_Means %>% group_by(EWC.STAT.description) %>% summarise(Mean= mean(Weight)) %>% arrange(desc(Mean))
  # results: Highest to lowest means 
            #Industrial effluent sludges -> 13247.5
            #Used oils -> 11814.5870
            #Mineral waste from waste treatment & stabilised waste ->	307.6284
            #Waste containing PCB -> 195.1069
            #Animal faeces, urine & manure	-> 0.00
            #Sludges & liquid wastes from waste treatment -> 0.00
            #Sorting residues	-> 0.00

#notes: some data needs more wrangling some IVs can go under 1 as might be a same thing, we may not care about some cat as it might not be revevant, you can really play with the cat and get different results more meaningful results as well, and that is important to do as well. 

#_____________________ UK Total Generated Waste
# Load Data -> UK
UK_G <- read.csv("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Taking Care of Rubbish- Chitra&Julie_FinalProject/Datasets/3.0) 3rd wrangled /3.0) UK_Reshaped_Location.csv")

#Test for Assumptions
## 1. Normality 
plotNormalHistogram(UK_G$Weight)
#positively skewed 
### Square root
UK_G$WeightSQRT <- sqrt(UK_G$Weight)
plotNormalHistogram(UK_G$WeightSQRT)
### Log 
UK_G$WeightLOG <- log(UK_G$Weight)
plotNormalHistogram(UK_G$WeightLOG) # drop infinite numbers 
# this code had a error code...Error in seq.default(min(x), max(x), length = length) : 
# 'from' must be a finite number
UK_G2 <- NaRV.omit(UK_G)  
plotNormalHistogram(UK_G2$WeightLOG)

## 2. Homogeneity of Variance
bartlett.test(Weight ~ Waste_types, data=UK_G)
#we have violate the assumptions of homogeneity of variance 

#ANOVAs without Homogeneity of Variance 
ANOVA1 <- lm(WeightLOG ~ Waste_types, data = UK_G2)
Anova(ANOVA1, Type="II", white.adjust=TRUE)
#pvalue < 0.05 is significant overall, that means there is an influence on the waste type and how much waste is being generated 

# Post Hocs
pairwise.t.test(UK_G$Weight, UK_G$Waste_types, p.adjust="bonferroni", pool.sd=FALSE)
#results: there is no pvalue < 0.05, there is no difference between (both variables)
          #Sludges & liquid wastes from waste treatment & Acid, alkaline or saline wastes 0.025
          #Waste containing PCB & Acid, alkaline or saline wastes 0.027
          #Mineral waste from waste treatment & stabilised waste & Industrial effluent sludges 0.00394
          #Sludges & liquid wastes from waste treatment & Industrial effluent sludges 0.00015
          #Waste containing PCB & Industrial effluent sludge 0.00016
          #Used oils & Mineral waste from waste treatment & stabilised waste 0.00569
          #Used oils & Sludges & liquid wastes from waste treatment Soils 0.00012
          #Waste containing PCB & Used oils 0.00012

# you only need to do the mean after post hocs, if anything shows of significant value.
# the mean tells us which one is the highest mean
UK_G_Means <- UK_G %>% filter(Waste_types %in% c("Sludges & liquid wastes from waste treatment", 'Acid, alkaline or saline wastes', 'Waste containing PCB', 'Mineral waste from waste treatment & stabilised waste', 'Industrial effluent sludges', 'Used oils'))
UK_G_Mean2 <- UK_G_Means %>% group_by(Waste_types) %>% summarise(Mean= mean(Weight)) %>% arrange(desc(Mean))
    # results: Highest to lowest means 
              #Industrial effluent sludges ->	21752.317
              #Used oils ->	18873.562
              #Acid, alkaline or saline wastes ->	11221.075
              #Mineral waste from waste treatment & stabilised waste ->	2875.130
              #Waste containing PCB ->	213.039
              #Sludges & liquid wastes from waste treatment ->	169.528

#______________________________Total___Treatment________________________________________________
# Load Data -> England Treatment
EnglandT <- read.csv("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Taking Care of Rubbish- Chitra&Julie_FinalProject/Datasets/3.0) 3rd wrangled /3.4) Eng_Reshaped_Treatment.csv")

#Test for Assumptions
## 1. Normality 
plotNormalHistogram(EnglandT$Weight)
  #positively skewed 
### Square root
EnglandT$WeightSQRT <- sqrt(EnglandT$Weight)
plotNormalHistogram(EnglandT$WeightSQRT)
### Log 
EnglandT$WeightLOG <- log(EnglandT$Weight)
plotNormalHistogram(EnglandT$WeightLOG) # drop infinite numbers 
EnglandT2 <- NaRV.omit(EnglandT)  
plotNormalHistogram(EnglandT2$WeightLOG)

## 2. Homogeneity of Variance
bartlett.test(Weight ~ EWC.STAT.description, data=EnglandT)
fligner.test(Weight ~ EWC.STAT.description, data=EnglandT)
  #we have violate the assumptions of homogeneity of variance 

#ANOVAs without Homogeneity of Variance 
ANOVA1 <- lm(WeightLOG ~ EWC.STAT.description, data = EnglandT2)
Anova(ANOVA1, Type="II", white.adjust=TRUE)
  #pvalue < 0.05 is significant overall, that means there is an influence on the waste type and how much waste is being treated 

# Post Hocs
pairwise.t.test(EnglandT$Weight, EnglandT$EWC.STAT.description, p.adjust="bonferroni", pool.sd=FALSE)
  #results: there is some pvalue < 0.05, meaning there is difference between both variables
    #There was not any values under 0.05, they were all above p-value > 0.05. 

#_____________ UK Treatment
UK_T <- read.csv("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Taking Care of Rubbish- Chitra&Julie_FinalProject/Datasets/3.0) 3rd wrangled /3.1) UK_Reshaped_Treatment.csv")

#Test for Assumptions
## 1. Normality 
plotNormalHistogram(UK_T$Weight)
#positively skewed 
### Square root
UK_T$WeightSQRT <- sqrt(UK_T$Weight)
plotNormalHistogram(UK_T$WeightSQRT)
### Log 
UK_T$WeightLOG <- log(UK_T$Weight)
plotNormalHistogram(UK_T$WeightLOG) # drop infinite numbers 
UK_T2 <- NaRV.omit(UK_T)  
plotNormalHistogram(UK_T2$WeightLOG)

## 2. Homogeneity of Variance
bartlett.test(Weight ~ Waste_types, data=UK_T)
#we have violate the assumptions of homogeneity of variance 

#ANOVAs without Homogeneity of Variance 
ANOVA1 <- lm(WeightLOG ~ Waste_types, data = UK_T2)
Anova(ANOVA1, Type="II", white.adjust=TRUE)
#pvalue < 0.05 is significant overall, that means there is an influence on the waste type and how much waste is being treated 

# Post Hocs
pairwise.t.test(UK_T$Weight, UK_T$Waste_types, p.adjust="bonferroni", pool.sd=FALSE)
  #results: there is no pvalue < 0.05, there is no difference between (both variables)
    #There was not any values under 0.05, they were all above p-value > 0.05.


#_________________ 2 ways ANOVA with Waste types & Year (IVs)_____________________________
eng_Two_way_ANOVAsG <- lm(WeightLOG ~ EWC.STAT.description*Year, data = EnglandG2)
eng_Two_way_ANOVAsG2 <- aov(WeightLOG ~ EWC.STAT.description + Year + EWC.STAT.description:Year, data=EnglandG2)
summary(eng_Two_way_ANOVAsG2)
  #Sig: waste types is significant as p<0.05. 
  #Not sig: Year p=0.40, Waste types and Year p=1.0

eng_Two_way_ANOVAsT <- lm(WeightLOG ~ EWC.STAT.description*Year, data = EnglandT2)
eng_Two_way_ANOVAsT2 <- aov(WeightLOG ~ EWC.STAT.description + Year + EWC.STAT.description:Year, data=EnglandT2)
summary(eng_Two_way_ANOVAsT2)
  #Sig: waste types is significant as p<0.05. 
  #Not sig: Year p=0.477, Waste types and Year p=1.00

uk_Two_way_ANOVAsG <- lm(WeightLOG ~ Waste_types*Year, data = UK_G2)
uk_Two_way_ANOVAsG1 <- aov(WeightLOG ~ Waste_types + Year + Waste_types:Year, data=UK_G2)
summary(uk_Two_way_ANOVAsG1)
  #Sig: waste types is significant as p<0.05. 
  #Not sig: Year p=0.728, Waste types and Year p=0.995

uk_Two_way_ANOVAsT <- lm(WeightLOG ~ Waste_types*Year, data = UK_T2)
uk_Two_way_ANOVAsT1 <- aov(WeightLOG ~ Waste_types + Year + Waste_types:Year, data=UK_T2)
summary(uk_Two_way_ANOVAsT1)
  #Sig: waste types is significant as p<0.05. 
  #Not sig: Year p=0.71, Waste types and Year p=0.99

# Same Post Hocs and Means as one way ANOVAs for waste types (b/c sig p<0.05) only. 

#______________One Way ANOVAs of Countries (Eng/UK) as one_________________________
#country IV; DV info on cat and class
#Load Dataset
countries <- read.csv("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Taking Care of Rubbish- Chitra&Julie_FinalProject/Datasets/3.0) 3rd wrangled /3.6) Reshaped_Countries.csv")
#Test for Assumptions
## 1. Normality 
plotNormalHistogram(countries$Weight)
#positively skewed 
### Square root
countries$WeightSQRT <- sqrt(countries$Weight)
plotNormalHistogram(countries$WeightSQRT)
### Log 
countries$WeightLOG <- log(countries$Weight)
plotNormalHistogram(countries$WeightLOG) # drop infinite numbers 
countries2 <- NaRV.omit(countries)  
plotNormalHistogram(countries2$WeightLOG)

## 2. Homogeneity of Variance
bartlett.test(Weight ~ Waste_types, data=countries)
# p<0.05, we have violate the assumptions of homogeneity of variance 

#ANOVAs without Homogeneity of Variance 
ANOVA1 <- lm(WeightLOG ~ Waste_types, data = countries2)
Anova(ANOVA1, Type="II", white.adjust=TRUE)
#pvalue < 0.05 is significant overall, that means there is an influence on the waste type and how much waste is being treated 

# Post Hocs
pairwise.t.test(countries$Weight, countries$Waste_types, p.adjust="bonferroni", pool.sd=FALSE)
#results: there is alot of p-value < 0.05, there is difference between (both variables)
  #Animal & mixed food waste & Acid, alkaline or saline wastes = 1.6e-13
  #Chemical wastes & Acid, alkaline or saline wastes = 1.3e-07                        
  #Combustion wastes & Acid, alkaline or saline wastes = 1.4e-05                        
  #Common sludges & Acid, alkaline or saline wastes = 3.0e-07                        
  #Discarded equipment & Acid, alkaline or saline wastes = 0.00104 
  #etc (a lot more)...

countries_Means <- countries %>% group_by(Waste_types) %>% summarise(Mean= mean(Weight)) %>% arrange(desc(Mean))
# Highest Mean: Mineral waste from construction & demolition ->	4793918.5496
# Lowest Mean: Waste containing PCB	-> 283.3142




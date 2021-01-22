# Final Project 

# Install Packages 
library(readxl)     #download csv
library(dplyr)      
library(data.table)   #setnames 

# England Waste Gen Worksheet
## DW-> Change columns (column name) and rows
england.industry.df <- read_excel("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Datasets/Original Datasets/UK_Statistics_on_Waste_dataset_Mar_2020_accessible_FINAL_v2.xlsx", sheet = "Table 5.1 Waste Gen Eng 2010-16")

## Getting rid of unwanted top 6 rows 
england.industry.df.1 <- england.industry.df[7:415,] 

# setnames <- function from data.table
#setnames(dataframe, old<- keyword argument colnames()<-vector of the column names that they are now, new= df<- isolating the row)
setnames(england.industry.df.1, old = colnames(england.industry.df.1), new = england.industry.df.1[1,] %>% slice(1) %>% as.character())

## Getting of the 1st row bc repeated column names as a row
england.industry.df.2 <- england.industry.df.1[2:409, ]

## Exporting cvs file 
write.csv(england.industry.df.2, "Updated_England_Industry.1.csv")

#____________
# England_treatment Worksheet

england.management.df <- read_excel("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Datasets/Original Datasets/UK_Statistics_on_Waste_dataset_Mar_2020_accessible_FINAL_v2.xlsx", sheet = "Table 5.3 Waste Tre Eng 2010-16")

england.management.df.1 <- england.management.df[7:415,]

setnames(england.management.df.1, old = colnames(england.management.df.1), new = england.management.df.1[1,] %>% slice(1) %>% as.character())

england.management.df.2 <- england.management.df.1[2:409, ]

write.csv(england.management.df.2, "Updated_England_Management.1.csv")

#___________
# UK Waste Gen Worksheet
uk.industry.df <- read_excel("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Datasets/Original Datasets/UK_Statistics_on_Waste_dataset_Mar_2020_accessible_FINAL_v2.xlsx", sheet = "Table 5.1 Waste Gen Eng 2010-16")

uk.industry.df.1 <- uk.industry.df[7:415,] 

setnames(uk.industry.df.1, old = colnames(uk.industry.df.1), new = uk.industry.df.1[1,] %>% slice(1) %>% as.character())

uk.industry.df.2 <- uk.industry.df.1[2:409, ]

write.csv(uk.industry.df.2, "Updated_UK_Industry.1.csv")

#___________
# UK_treatment Worksheet
uk.management.df <- read_excel("/Users/patelca/Desktop/WozU Data Science Program /WozU Final Project/Datasets/Original Datasets/UK_Statistics_on_Waste_dataset_Mar_2020_accessible_FINAL_v2.xlsx", sheet = "Table 5.3 Waste Tre Eng 2010-16")

uk.management.df.1 <- uk.management.df[7:415,]

setnames(uk.management.df.1, old = colnames(uk.management.df.1), new = uk.management.df.1[1,] %>% slice(1) %>% as.character())

uk.management.df.2 <- uk.management.df.1[2:409, ]

write.csv(uk.management.df.2, "Updated_UK_Management.1.csv")

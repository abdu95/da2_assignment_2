#######################
##  DA2 and Coding   ##
##    Assignment     ##
## Analysis of house ##
##     price         ##
##                   ##
## NO. 1: Get  data  ##
##                   ##
#######################


# Clear memory and call packages
rm(list=ls())
library(tidyverse)

# get Housing cross-sectional data
houses_raw <- read.csv('data/raw/Housing_raw.csv')

houses_raw$SalePrice[1]
str(houses_raw)

is.na(houses_raw$SalePrice)
sum(is.na(houses_raw$SalePrice))
# no missing values in SalesPrice

which(is.na(houses_raw$MS.SubClass))
str(houses_raw$MS.SubClass)
unique(houses_raw[c("MS.SubClass")])
unique(houses_raw$MS.SubClass)
houses_raw$MS.SubClass.fact <- as.factor(houses_raw$MS.SubClass)
str(houses_raw$MS.SubClass.fact)

which(is.na(houses_raw$MS.Zoning))
str(houses_raw$MS.Zoning)
unique(houses_raw[c("MS.Zoning.fact")])
unique(houses_raw$MS.Zoning)
houses_raw$MS.Zoning.fact <- as.factor(houses_raw$MS.Zoning)
str(houses_raw$MS.Zoning.fact)

# 490 missing values. do I need this variable? if yes, how to handle them?
sum(is.na(houses_raw$Lot.Frontage))


# MS SubClass 
# Bldg Type
# House Style
# 
# Almost all variables are categorical (nominal/ordinal)

# Total Bsmt SF (Continuous)

# Ordinal variables are example for 1-5 star hotels

# 1st Flr SF (Continuous): First Floor square feet

# 2nd Flr SF (Continuous)	: Second floor square feet
# 
# Low Qual Fin SF (Continuous): Low quality finished square feet (all floors)
# 
# Gr Liv Area (Continuous): Above grade (ground) living area square feet


# How many Discrete & Continuous variables I have

# NA means	No Garage



# houses_raw$Yr.Sold --> 2006 - 2010

ggplot(houses_raw, aes(Gr.Liv.Area, SalePrice)) +
  geom_point()

ggplot(houses_raw, aes(Gr.Liv.Area, log(SalePrice))) +
  geom_point()

nrow(houses_raw)

outliers <- houses_raw[houses_raw$Gr.Liv.Area > 4000,]
houses_raw = houses_raw[houses_raw$Gr.Liv.Area < 4000,]
nrow(houses_raw)


drops <- c("MS.SubClass","MS.Zoning", "Street", "Alley", "Lot.Shape")
houses_raw <- houses_raw[ , !(names(houses_raw) %in% drops)]


houses_raw$Street.fact <- as.factor(houses_raw$Street)
houses_raw$Alley.fact <- as.factor(houses_raw$Alley)


# houses_raw %>% replace_na(houses_raw$Alley = "No alley access")
houses_raw$Alley <- houses_raw$Alley %>% replace_na("No alley access")
sum(is.na(houses_raw$Alley))

houses_raw$Lot.Shape.fact <- factor(houses_raw$Lot.Shape, order = TRUE, 
          levels = c("Reg", "IR1", "IR2", "IR3"))


str(houses_raw$Lot.Shape.fact)
unique(houses_raw$Lot.Shape)




write_csv(df, 'data/clean/Housing_clean.csv')

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

houses_raw <- read.csv('data/raw/Housing.csv')
houses_raw$SalePrice[1]
str(houses_raw)

is.na(houses_raw$SalePrice)
which(is.na(houses_raw$SalePrice))
sum(is.na(houses_raw$SalePrice))

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

which(is.na(houses_raw$Lot.Frontage))
sum(is.na(houses_raw$Lot.Frontage))

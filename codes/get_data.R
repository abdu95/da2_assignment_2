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
df <- read.csv('data/raw/Housing_raw.csv')

df$SalePrice[1]
str(df)

is.na(df$SalePrice)
sum(is.na(df$SalePrice))
# no missing values in SalesPrice

which(is.na(df$MS.SubClass))
str(df$MS.SubClass)
unique(df[c("MS.SubClass")])
unique(df$MS.SubClass)
df$MS.SubClass.fact <- as.factor(df$MS.SubClass)
str(df$MS.SubClass.fact)

which(is.na(df$MS.Zoning))
str(df$MS.Zoning)
unique(df[c("MS.Zoning.fact")])
unique(df$MS.Zoning)
df$MS.Zoning.fact <- as.factor(df$MS.Zoning)
str(df$MS.Zoning.fact)

# 490 missing values. do I need this variable? if yes, how to handle them?
sum(is.na(df$Lot.Frontage))

 
# 23 nominal, 23 ordinal variables 
# 20 continuous variables, 14 discrete variables

# Total Bsmt SF (Continuous)

# Ordinal variables are example for 1-5 star hotels

# 1st Flr SF (Continuous): First Floor square feet

# 2nd Flr SF (Continuous)	: Second floor square feet
# 
# Low Qual Fin SF (Continuous): Low quality finished square feet (all floors)
# 
# Gr Liv Area (Continuous): Above grade (ground) living area square feet


# df$Yr.Sold --> 2006 - 2010

ggplot(df, aes(Gr.Liv.Area, SalePrice)) +
  geom_point()

ggplot(df, aes(Gr.Liv.Area, log(SalePrice))) +
  geom_point()

nrow(df)

outliers <- df[df$Gr.Liv.Area > 4000,]
df = df[df$Gr.Liv.Area < 4000,]
nrow(df)


drops <- c("MS.SubClass","MS.Zoning", "Street", "Alley", "Lot.Shape",
           "Land.Contour", "Utilities", "Lot.Config", "Land.Slope", 
           "Neighborhood", "Condition.1", "Condition.2", "Bldg.Type",
           "House.Style", "Overall.Qual", "Overall.Cond", "Roof.Style", 
           "Roof.Matl", "Exterior.1st", "Exterior.2nd", "Mas.Vnr.Type", 
           "Exter.Qual", "Exter.Cond", "Foundation", "Bsmt.Qual", 
           "Bsmt.Cond", "Bsmt.Exposure")
df <- df[ , !(names(df) %in% drops)]

df$MS.SubClass.fact <- as.factor(df$MS.SubClass)
df$MS.Zoning.fact <- as.factor(df$MS.Zoning)
df$Street.fact <- as.factor(df$Street)
df$Alley <- df$Alley %>% replace_na("No alley access")
df$Alley.fact <- as.factor(df$Alley)

df$Lot.Shape.fact <- factor(df$Lot.Shape, order = TRUE, 
          levels = c("IR3", "IR2", "IR1", "Reg"))
df$Land.Contour.fact <- as.factor(df$Land.Contour)
# there is no  ELO -	Electricity only
df$Utilities.fact <-  factor(df$Utilities, order = TRUE,
                                     levels = c("NoSeWa", "NoSewr", "AllPub"))
df$Lot.Config.fact <- as.factor(df$Lot.Config)
df$Land.Slope.fact <- factor(df$Land.Slope, order = TRUE, 
                                    levels = c("Sev", "Mod", "Gtl"))
df$Neighborhood.fact <- as.factor(df$Neighborhood)

df$Condition.1.fact <- as.factor(df$Condition.1)
# 8 levels, instead of 9
df$Condition.2.fact <- as.factor(df$Condition.2)
# the difference between # MS SubClass # Bldg Type # House Style
df$Bldg.Type.fact <- as.factor(df$Bldg.Type)

df$House.Style.fact <- as.factor(df$House.Style)
df$Overall.Qual.fact <- factor(df$Overall.Qual, order = TRUE, 
                                     levels = 1:10)
df$Overall.Cond.fact <- factor(df$Overall.Cond, order = TRUE,
                                       levels = 1:10)
df$Roof.Style.fact <- as.factor(df$Roof.Style)
df$Roof.Matl.fact <- as.factor(df$Roof.Matl)
df$Exterior.1st.fact <- as.factor(df$Exterior.1st)
# Other is present only here (not in 1st)
df$Exterior.2nd.fact <- as.factor(df$Exterior.2nd)

# 23 empty string
sum(df$Mas.Vnr.Type == "")
# change empty string to NA
df$Mas.Vnr.Type[df$Mas.Vnr.Type==''] <- NA
df$Mas.Vnr.Type.fact <- as.factor(df$Mas.Vnr.Type)
df$Exter.Qual.fact <- factor(df$Exter.Qual, order = TRUE,
                                     levels = c("Fa", "TA", "Gd", "Ex"))
# Poor is present only here (not in Qual
df$Exter.Cond.fact <- factor(df$Exter.Qual, order = TRUE,
                                     levels = c("Po", "Fa", "TA", "Gd", "Ex"))
df$Foundation.fact <- as.factor(df$Foundation)

df$Bsmt.Qual <- df$Bsmt.Qual %>% replace_na("No Basement")
# empty string: 1. # 903230120 has no Qual and Cond
sum(df$Bsmt.Qual == "")
df$Bsmt.Qual[df$Bsmt.Qual ==''] <- NA
# bsmt.qual.empty <- df[is.na(df$Bsmt.Qual),]
df$Bsmt.Qual.fact <- factor(df$Bsmt.Qual, order = TRUE,
                             levels = c("No Basement", "Po", "Fa", "TA", "Gd","Ex"))

df$Bsmt.Cond <- df$Bsmt.Cond %>% replace_na("No Basement")
df$Bsmt.Cond[df$Bsmt.Cond ==''] <- NA
df$Bsmt.Cond.fact <- factor(df$Bsmt.Cond, order = TRUE,
                            levels = c("No Basement", "Po", "Fa", "TA", "Gd","Ex"))

df$Bsmt.Exposure <- df$Bsmt.Exposure %>% replace_na("No Basement")
sum(df$Bsmt.Exposure == "")
# bsmt.exposure.empty <- df[df$Bsmt.Exposure == '',]
df$Bsmt.Exposure[df$Bsmt.Exposure ==''] <- NA
df$Bsmt.Exposure.fact <- factor(df$Bsmt.Exposure, order = TRUE,
                            levels = c("No Basement", "No", "Mn", "Av", "Gd" ))



table(is.na(df$Lot.Frontage))
# NA means	No Garage
sum(is.na(df$Garage.Type))
df$Garage.Type <- df$Garage.Type %>% replace_na("No Garage")
sum(is.na(df$Garage.Type))


write_csv(df, 'data/clean/Housing_clean.csv')

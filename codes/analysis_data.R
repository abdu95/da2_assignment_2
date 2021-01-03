#######################
##  DA2 and Coding   ##
##    Assignment     ##
## Analysis of house ##
##     price         ##
##                   ##
## NO. 2: Analysis   ##
##                   ##
#######################


rm(list = ls())

install.packages("AER")
library(AER)
library(tidyverse)
require(scales)
library(lspline)
library(estimatr)
library(texreg)
library(corrplot)
install.packages("varhandle")
library(varhandle)


# read clean data
df <- read.csv('data/clean/Housing_clean.csv')


# Re-iterated research question:
#   Does a house with higher above ground living area have higher price from 2006 - 2010 in Ames (Iowa) ?
#
#####
# Model setup
# Outcome variable:      SalePrice: Sale price $$
# Parameter of interest: Gr Liv Area: Above ground living area square feet


df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

summary( df )

# Check the main parameter of interests and potential confounders:

# SalePrice
ggplot(df, aes(x = SalePrice)) +
  geom_histogram(fill='navyblue') +
  labs(x = "Sale price of houses")

# Above ground living area 
ggplot(df, aes(x = Gr.Liv.Area)) +
  geom_histogram(fill='navyblue') +
  labs(x = "Total Area of a house")


# Fireplaces
ggplot(df, aes(x = Fireplaces)) +
  geom_histogram(fill='navyblue') +
  labs(x = "Fireplaces")

#Fireplaces (Discrete). Number of fireplaces transform to dummy:  0/1
df$HasFireplace<-ifelse(df$Fireplaces>0,1,0)
#remodeled to 0/1
df$Remodeled<-ifelse(df$Year.Remod.Add>df$Year.Built,1,0)

# df$Street.fact <-ifelse(df$Street.fact == "Paved",1,0)

ggplot(df, aes(x = HasFireplace)) +
  geom_histogram(fill='navyblue') +
  labs(x = "Fireplaces")

# Checking some scatter-plots:
# Create a general function to check the pattern
chck_sp <- function(x_var, x_lab){
  ggplot( df , aes(x = x_var, y = SalePrice)) +
    geom_point() +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(y = "Sale price of houses", x = x_lab) 
}

# Our main interest: total area: # TotalArea = Total Bsmt SF + Gr Liv Area:
summary(df$BsmtFin.Type.1.fact)



# Total Bsmt SF - Total square feet of basement area. basement - below the ground floor
# Gr Liv Area: Above ground living area square feet
ggplot(df, aes(Total.Bsmt.SF + Gr.Liv.Area, SalePrice)) +
  geom_point()

chck_sp(df$TotalArea, "Total Area of a house")
# The total square footage model indicates some possible curvature (convex) 
# which could be better interpreted with quadratic variables


chck_sp(df$Total.Bsmt.SF + df$X1st.Flr.SF + df$X2nd.Flr.SF, 
        "Total basement area + 1st floor area + 2nd floor area")
# also convex

chck_sp(df$X1st.Flr.SF, "First Floor square feet")
chck_sp(df$X2nd.Flr.SF, "Second Floor square feet")

ggplot(df, aes(Total.Bsmt.SF + X1st.Flr.SF + X2nd.Flr.SF, SalePrice)) +
  geom_point(aes(color = Sale.Condition.fact, shape = Sale.Condition.fact)) + 
  labs(x = "Total basement area + 1st floor area + 2nd floor area")


chck_sp(df$Lot.Area, "Lot Area")
# bunch of dots near 0
# probably not correlated

chck_sp(df$Lot.Shape.fact, "General shape of property")
# The more regular shape house has, the higher the price 
# but some Irregular houses has same price as Regular houses


chck_sp(df$HasFireplace, "Has Fireplace")
# houses with 1 or more fireplaces have higher price
chck_sp(df$Fireplaces, "Has Fireplace")
# more fireplace --> sale price increases

chck_sp(df$Garage.Area, "Garage Area")
# almost positive linear, but some outliers at the end causing the line to curve
# seems important

chck_sp(df$Total.Bsmt.SF, "Total square feet of basement area")
# linear relationship

chck_sp(df$Gr.Liv.Area, " Above ground living area square feet")  
# strong (straight) linear relationship

chck_sp(df$Garage.Cars, "Size of garage in car capacity")
# houses with more garage cars have higher price

chck_sp(df$Land.Contour.fact, "Land Contour")
# Near Flat/Level	and Hillside houses have higher prices

# "Utilities" - 2922 houses has AllPub

chck_sp(df$Lot.Config.fact, " Lot configuration")
# Cul-de-sac and Inside lot has higher prices

chck_sp(df$Land.Slope.fact, "Slope")
# Houses with Gentle slope and Moderate Slope have higher prices

ggplot( df , aes(x = Neighborhood.fact, y = SalePrice)) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs(y = "Sale price of houses", x = "Neighborhood") + 
  theme(axis.text.x = element_text(angle = 90))  
# Houses from Northridge, Northridge Heights, and Stone Brook neighborhoods have relatively high prices

chck_sp(df$Condition.1.fact, "Condition 1")
# Houese with Normal and "Adjacent to postive off-site feature" conditions have relatively high prices

chck_sp(df$Condition.2.fact, "Condition 2")

chck_sp(df$Bldg.Type.fact, "Type of building")
# Most of the houses are Single-family Detached
# Single-family Detached and Townhouse End Unit houses have relatively high prices

chck_sp(df$House.Style.fact, "Style of house")
# most of the houses are One story and Two story
# One story and Two story houses have relatively high prices

chck_sp(df$Overall.Qual.fact, "Rating of overall material and finish of the house")
# higher rating - higher sale price
# but some houses has quality 6 but has same price as quality 2

chck_sp(df$Overall.Cond.fact, "Rating of the overall condition of the house")
# most of the houses have Average (5) condition.
# Average conditioned houses have relatively high prices

chck_sp(df$Year.Built, "Year Built")
# the newer the house (the later is the year built) -  the higher the price
# but some houses that are built before 1900 were priced realtively high
# upward trend

chck_sp(df$Year.Remod.Add, "Remodel date")

chck_sp(df$Roof.Style.fact, "Type of roof")
# most of the houses have Gable (2320) and Hip (547) roof style

chck_sp(df$TotRms.AbvGrd, "Rooms")
chck_sp(df$Bsmt.Qual.fact, "The height of the basement")
chck_sp(df$BsmtFin.Type.1.fact, "Rating of basement finished area")
chck_sp(df$MS.Zoning.fact, "General zoning classification of the sale.")
chck_sp(df$Kitchen.AbvGr, "Kitchens above grade")
chck_sp(df$Garage.Yr.Blt, "Year garage was built")

str(df$Bsmt.Qual.fact)
df$Bsmt.Qual.unfact <- unfactor(df$Bsmt.Qual.fact)
class(df$Bsmt.Qual.unfact)
unique(df$Bsmt.Qual.fact)

df$Bsmt.Qual.num <- as.numeric(factor(df$Bsmt.Qual.fact,
                  levels = c("No Garage","Po", "Fa", "TA", "Gd", "Ex")))

df$MS.Zoning.num <- as.numeric(factor(df$MS.Zoning.fact))
# RM 7 RL 6 RH 5 I 4  FV 3 C 2 A 1

df$Overall.Qual.num <- unfactor(as.factor(df$Overall.Qual.fact))

# dfs <- data.frame(df$Overall.Qual.fact,df$Overall.Qual.num)


df$BsmtFin.Type.1.num <- 
  as.numeric(factor(df$BsmtFin.Type.1.fact,
                    levels = c("No Basement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")))
# GLQ 7 ALQ 6 BLQ 5 Rec 4 LwQ 3 Unf 2 No Basement 1

# dfs <- data.frame(df$BsmtFin.Type.1.fact, df$BsmtFin.Type.1.num)


# TotalArea
# Lot.Area
# HasFireplace
# Garage.Area
# Total.Bsmt.SF
# Gr.Liv.Area
# Bsmt.Qual.fact (Bsmt.Qual.num)
# MS.Zoning.fact (MS.Zoning.num)
# Overall.Qual.fact (Overall.Qual.num)
# BsmtFin.Type.1.fact (BsmtFin.Type.1.num )
# ~~ Neighborhood.fact
# Year.Built - convex
# Year.Remod.Add
# Garage.Yr.Blt
# Garage.Cars
# TotRms.AbvGrd
# df$X1st.Flr.SF + df$X2nd.Flr.SF

df$TwoFloorArea <- df$X1st.Flr.SF + df$X2nd.Flr.SF
chck_sp(df$TwoFloorArea, "Area of two floors")

selected_var_df <- data.frame("SalePrice" = df$SalePrice, "TotalArea" = df$TotalArea, 
                              "Lot.Area" = df$Lot.Area, "HasFireplace" = df$HasFireplace, 
                              "Garage.Area" = df$Garage.Area, "Total.Bsmt.SF" = df$Total.Bsmt.SF, 
                              "Liv.Area" = df$Gr.Liv.Area, "Bsmt.Qual" = df$Bsmt.Qual.num, 
                              "MS.Zoning" = df$MS.Zoning.num, "Overall.Qual" = df$Overall.Qual.num, 
                              "BsmtFin.Type.1" = df$BsmtFin.Type.1.num, "Year.Built" = df$Year.Built, 
                              "Year.Remod.Add" = df$Year.Remod.Add, "Garage.Yr.Blt" = df$Garage.Yr.Blt, 
                              "Garage.Cars" = df$Garage.Cars, "TotRms.AbvGrd" = df$TotRms.AbvGrd, "TwoFloorArea" = df$TwoFloorArea) 

# Now we have an idea how to include these variables into our regression

corTable <- cor(selected_var_df, use = "complete.obs")

# see correlation 
corrplot(cor(selected_var_df, use = "complete.obs"),method='e')

# Check for highly correlated values:
sum( abs(corTable) >= 0.8 & corTable != 1 ) / 2

# Find the correlations which are higher than 0.8
id_cr <- which( abs(corTable) >= 0.8 & corTable != 1 )
pair_names <- expand.grid( variable.names(selected_var_df) , variable.names(selected_var_df) )

# Get the pairs:
high_corr <- pair_names[ id_cr , ]
high_corr <- mutate( high_corr , corr_val = corTable[ id_cr ] )

# highly correlated - confounder (leads to multicollinearity)
high_corr


SalePrice: Lot.Area, HasFireplace, Garage.Area, Total.Bsmt.SF, 
Liv.Area, Bsmt.Qual, MS.Zoning, BsmtFin.Type.1, Year.Built, 
Year.Remod.Add, Garage.Yr.Blt, Garage.Cars,  TotRms.AbvGrd, TwoFloorArea,

SalePrice = 7851 + 1.72 Lot Area + 41.2 Total Bsmt SF + 40.8 Gr Liv Area
+ 20952 Garage Cars + 8379 FireYN

reg1 <- lm_robust(SalePrice ~ TotalArea, data = df)
summary(reg1)


# The most obvious simple regression
model is to predict sales price based on above ground living space (GR LIVE AREA) or total
square footage (TOTAL BSMT SF + GR LIV AREA).



ggplot(df, aes(Gr.Liv.Area, log(SalePrice))) +
  geom_point()

ggplot(df, aes(Total.Bsmt.SF, SalePrice)) +
  geom_point()
# weird dots at 0




#  Removed 2779 rows containing missing values (geom_point). 

ggplot(data = df, aes(x = MS.Zoning.fact)) + 
  geom_bar(stat="count")
# most of the houses (more than 2000) are RL - Residential Low Density

sum(is.na(df$Sale.Type.fact))

sum(is.na(df$Total.Bsmt.SF))

missing <- df[is.na(df$Total.Bsmt.SF),]






# 490 missing values. do I need this variable? if yes, how to handle them?
sum(is.na(df$Lot.Frontage))
table(is.na(df$Lot.Frontage))

num_data <- select_if(df, is.numeric)


# Total Bsmt SF (Continuous)

# Ordinal variables are example for 1-5 star hotels
# for exammple Garage Car? No. We have only 1 house with 5 garage cars. 
# we need more observation for all levels (1-5)

# 1st Flr SF (Continuous): First Floor square feet
# 2nd Flr SF (Continuous)	: Second floor square feet

# Low Qual Fin SF (Continuous): Low quality finished square feet (all floors)
# Gr Liv Area (Continuous): Above grade (ground) living area square feet

# df$Yr.Sold --> 2006 - 2010


qplot(log(df$SalePrice), geom= "histogram") 

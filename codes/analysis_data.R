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

# read clean data
df <- read.csv('data/clean/Housing_clean.csv')

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

summary( df )

# SalePrice
ggplot(df, aes(x = SalePrice)) +
  geom_histogram(fill='navyblue') +
  labs(x = "Sale price of houses")

# TotalArea
ggplot(df, aes(x = TotalArea)) +
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

# Our main interest: total area:
chck_sp(df$TotalArea, "Total Area of a house")



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

# Total Bsmt SF - Total square feet of basement area. basement - below the ground floor
# Gr Liv Area: Above ground living area square feet
ggplot(df, aes(Total.Bsmt.SF + Gr.Liv.Area, SalePrice)) +
  geom_point()
# curvature,  convex?
# The total square footage model indicates some possible curvature which could lead into discussions of quadratic variables.

ggplot(df, aes(Total.Bsmt.SF + X1st.Flr.SF + X2nd.Flr.SF, SalePrice)) +
  geom_point(aes(color = Sale.Condition.fact, shape = Sale.Condition.fact)) + 
  labs(x = "Total basement area + 1st floor area + 2nd floor area")


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

# 1st Flr SF (Continuous): First Floor square feet
# 2nd Flr SF (Continuous)	: Second floor square feet

# Low Qual Fin SF (Continuous): Low quality finished square feet (all floors)
# Gr Liv Area (Continuous): Above grade (ground) living area square feet

# df$Yr.Sold --> 2006 - 2010


qplot(log(df$SalePrice), geom="histogram") 

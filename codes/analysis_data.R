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
library(kableExtra)


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

# Our main interest - total area: # TotalArea = Total Bsmt SF + Gr Liv Area:

# Total Bsmt SF - Total square feet of basement area. basement - below the ground floor
# Gr Liv Area: Above ground living area square feet
ggplot(df, aes(Total.Bsmt.SF + Gr.Liv.Area, SalePrice)) +
  geom_point()

chck_sp(df$TotalArea, "Total Area of a house")
# The total square footage model indicates some possible curvature (convex) 
# which could be better interpreted with quadratic variables

chck_sp(df$Total.Bsmt.SF + df$Gr.Liv.Area, 
        "Total basement area + Above ground area")
# also convex

ggplot(df, aes(Total.Bsmt.SF + Gr.Liv.Area, SalePrice)) +
  geom_point(aes(color = Sale.Condition.fact, shape = Sale.Condition.fact)) + 
  labs(x = "Total basement area + Above ground area")


chck_sp(df$X1st.Flr.SF, "First Floor square feet")
chck_sp(df$X2nd.Flr.SF, "Second Floor square feet")

chck_sp(df$Lot.Area, "Lot Area")
# bunch of dots near 0
# probably not correlated

ggplot( df , aes(x = log(Lot.Area), y = log(SalePrice))) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs(y = "Sale price of houses", x = "Lot Area") 
# probably linear spline, with knots at 8 and 10

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
                  levels = c("No Basement","Po", "Fa", "TA", "Gd", "Ex")))

df$MS.Zoning.num <- as.numeric(factor(df$MS.Zoning.fact))
# RM 7 RL 6 RH 5 I 4  FV 3 C 2 A 1

df$Overall.Qual.num <- unfactor(as.factor(df$Overall.Qual.fact))

# dfs <- data.frame(df$Overall.Qual.fact,df$Overall.Qual.num)


df$BsmtFin.Type.1.num <- 
  as.numeric(factor(df$BsmtFin.Type.1.fact,
                    levels = c("No Basement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")))
# GLQ 7 ALQ 6 BLQ 5 Rec 4 LwQ 3 Unf 2 No Basement 1

# dfs <- data.frame(df$BsmtFin.Type.1.fact, df$BsmtFin.Type.1.num)

# We have seen a pattern of association between y and x variables.
# Now we have an idea what variables to include into our regression

selected_var_df <- data.frame("SalePrice" = df$SalePrice, "TotalArea" = df$TotalArea, 
                              "Lot.Area" = df$Lot.Area, "HasFireplace" = df$HasFireplace, 
                              "Garage.Area" = df$Garage.Area, "Total.Bsmt.SF" = df$Total.Bsmt.SF, 
                              "Gr.Liv.Area" = df$Gr.Liv.Area, "Bsmt.Qual" = df$Bsmt.Qual.num, 
                              "MS.Zoning" = df$MS.Zoning.num, "Overall.Qual" = df$Overall.Qual.num, 
                              "BsmtFin.Type.1" = df$BsmtFin.Type.1.num, "Year.Built" = df$Year.Built, 
                              "Year.Remod.Add" = df$Year.Remod.Add, "Garage.Yr.Blt" = df$Garage.Yr.Blt, 
                              "Garage.Cars" = df$Garage.Cars, "TotRms.AbvGrd" = df$TotRms.AbvGrd) 


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

# highly correlated - confounder (leads to multicollinearity )
high_corr

# These variables are highly correlated with each other, we don't include them in our model
# because we may face multicollinearity  issue

# 1      TotalArea     SalePrice 0.8306139
# 2   Overall.Qual     SalePrice 0.8013891
# 3      SalePrice     TotalArea 0.8306139
# 4  Total.Bsmt.SF     TotalArea 0.8064269
# 5    Gr.Liv.Area     TotalArea 0.8586815
# 6    Garage.Cars   Garage.Area 0.8483277
# 7      TotalArea Total.Bsmt.SF 0.8064269
# 8      TotalArea   Gr.Liv.Area 0.8586815
# 9  TotRms.AbvGrd   Gr.Liv.Area 0.8074845
# 10     SalePrice  Overall.Qual 0.8013891
# 11 Garage.Yr.Blt    Year.Built 0.8344967
# 12    Year.Built Garage.Yr.Blt 0.8344967
# 13   Garage.Area   Garage.Cars 0.8483277
# 14   Gr.Liv.Area TotRms.AbvGrd 0.8074845


# Variables with low correlation: 
#' SalePrice: Lot.Area, HasFireplace, Garage.Area, Total.Bsmt.SF, 
#' Gr.Liv.Area, Bsmt.Qual, MS.Zoning, BsmtFin.Type.1, Year.Built, 
#' Year.Remod.Add, Garage.Yr.Blt, Garage.Cars,  TotRms.AbvGrd, TwoFloorArea,

# Think about interactions:
# How to do interaction of Gr.Liv.Area and HasFireplace
#     1) The parameter of interest does not include interaction.
#     2) strong reason for controlling for such interaction: 
#       fireplace building requires additional expenditure that leads to higher house price
#

#####
# 6) Modelling
#
# Start from simple to complicated
# Remember: few hundreds obs, 5-10 variable could work
#
# Main regression: saleprice = b0 + b1*Gr.Liv.Area
#   reg1: NO controls, simple linear
#   reg11: SalePrice ~ Lot.Area + HasFireplace
#   reg2: Weighted linear regression, using rooms as weights.
#   reg23: SalePrice ~ Total.Bsmt.SF + Gr.Liv.Area

# reg3:  SalePrice ~ Gr.Liv.Area + HasFireplace
# reg31: SalePrice ~ Gr.Liv.Area + HasFireplace + Gr.Liv.Area * HasFireplace,
# reg32: SalePrice ~ Total.Bsmt.SF + Gr.Liv.Area + HasFireplace + Gr.Liv.Area * HasFireplace
# reg4: SalePrice ~ Lot.Area + Total.Bsmt.SF + Gr.Liv.Area + Garage.Cars + HasFireplace




reg1 <- lm_robust(SalePrice ~ Gr.Liv.Area, data = df)
summary(reg1)
# R2 = 51


reg11 <- lm_robust(SalePrice ~ Lot.Area + HasFireplace, data = df)
summary( reg11 )
# R2 = 0.27

reg12 <- lm_robust(SalePrice ~ TotalArea, data = df)
summary(reg12)
# R2 = 68
# We dont take this model as TotalArea is higly correlated to SalePrice -->
# multicollinearity issue

reg13 <- lm_robust(SalePrice ~ Garage.Area, data = df)
summary(reg13)
# R2 = 42

reg14 <- lm_robust(SalePrice ~ Total.Bsmt.SF, data = df)
summary(reg14)
# R2 = 43

reg15 <- lm_robust(SalePrice ~ Bsmt.Qual.num, data = df)
summary(reg15)
# R2 = 37

reg16 <- lm_robust(SalePrice ~ MS.Zoning.num, data = df)
summary(reg16)
# R2 = 0.01

reg17 <- lm_robust(SalePrice ~ BsmtFin.Type.1.num, data = df)
summary(reg17)
# R2 = 0.11

reg18 <- lm_robust(SalePrice ~ Year.Built, data = df)
summary(reg18)
# R2 = 0.31

reg19 <- lm_robust(SalePrice ~ Year.Remod.Add, data = df)
summary(reg19)
# R2 = 0.29

reg20 <- lm_robust(SalePrice ~ Garage.Yr.Blt, data = df)
summary(reg20)
# R2 = 0.28

reg21 <- lm_robust(SalePrice ~ Garage.Cars, data = df)
summary(reg21)
# R2 = 0.42

reg22 <- lm_robust(SalePrice ~ TotRms.AbvGrd, data = df)
summary(reg22)
# R2 = 0.24

reg23 <- lm_robust(SalePrice ~ Total.Bsmt.SF + Gr.Liv.Area, data = df)
summary(reg23)
# R2 = 0.68

# Intercept (Beta0): -36647. Does not have meaningful interpretation
# Total.Bsmt.SF (Beta1): 82.30. On average, SalePrice is 82.30 dollars higher in the data for houses with one square feet larger Total Basement Area but with the same Above Ground Living Area.
# Gr.Liv.Area (Beta2): 87.63. On average, SalePrice is 87.63 dollars higher in the data for houses with one square feet larger Above Ground Living Area but with the same Total Basement Area.

# since SalePrice - TotalArea plot had a curvature, 
# I tried to make quadratic model as well
reg24 <- lm_robust(SalePrice ~ TotalArea + TotalArea^2 , data = df )
summary( reg24 )
# R2 = 0.68, same as previous

# log - log Price - Lot Area. linear spline
reg25 <- lm_robust(log(SalePrice) ~ lspline(log(Lot.Area),c(8,10)), data = df)
summary(reg25)
# R2 = 0.14

#' Finished simple regression (without control) with all selected variables.
#' time to do regression with controls


# model: Weighted linear regression, using rooms as weights.
reg2 <- lm_robust(SalePrice ~ Gr.Liv.Area, data = df , weights = TotRms.AbvGrd)
summary( reg2)
# R2 = 0.49


reg3 <- lm_robust( SalePrice ~ Gr.Liv.Area + HasFireplace, data = df )
summary( reg3 )
# R2 = 55

# interaction of HasFireplace dummy variable
reg31 <- lm_robust(SalePrice ~ Gr.Liv.Area + HasFireplace + 
                     Gr.Liv.Area * HasFireplace, data = df)
summary( reg31 )
# R2 = 57


reg32 <- lm_robust(SalePrice ~ Total.Bsmt.SF + Gr.Liv.Area + HasFireplace + 
                     Gr.Liv.Area * HasFireplace, data = df)
summary(reg32)
# R2 = 0.71

reg4 <- lm_robust(SalePrice ~ Lot.Area + Total.Bsmt.SF + 
                    Gr.Liv.Area + Garage.Cars + HasFireplace, data = df)
summary(reg4)
# R2 = 74

##
# Summarize our findings:
data_out <- "out/"

# reg1 0.51 reg13 0.42 reg23 0.68 reg31 0.57  reg32 0.71
reg4$fitted.values

htmlreg( list(reg1 , reg13 , reg23 , reg31, reg32),
         type = 'html',
         custom.model.names = c("(1) SalePrice - Above ground area","(2) SalePrice - Garage Area",
                                "(3) SalePrice - Basement and Above ground area", "(4) SalePrice - Above ground are interacted with HasFireplace", 
                                "(5) SalePrice - Basement and Above ground are interacted with HasFireplace "),
         caption = "Modelling Sale Price of houses",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)


# calculate some additional important fit measures:
#
# 1) y_hat-y plot - use reg23 - not handling missing values properly...
df <- mutate( df , y_hat = reg23$fitted.values )

# Predict is more general and can handle missing values...
df <- mutate( df , y_hat = predict( reg23 , df ) )

#y_hat-y plot
ggplot( data = df ) +
  geom_point (aes( x = y_hat , y = SalePrice ) ,  color="red")+
  geom_line( aes( x = SalePrice , y = SalePrice ) , color = "navyblue" , size = 1.5 )+
  labs( x = "Predicted sale prices", y = "Actual sale prices")


# Does fireplace interaction increase the prediction?
reg23_lm <- lm(SalePrice ~ Total.Bsmt.SF + Gr.Liv.Area, 
               data = subset(df,complete.cases(df)))
summary(reg23_lm)

reg31_lm <- lm(SalePrice ~ Gr.Liv.Area + HasFireplace + 
                 Gr.Liv.Area * HasFireplace, data = subset(df,complete.cases(df)))
summary( reg31_lm )
summary(reg31)

BIC(reg23_lm, reg31_lm)
AIC(reg23_lm, reg31_lm)

# a lower BIC means that a model is considered to be more likely to be the true model



# a varibale is significant
# # p value of variable is significant (p < 0.05), 
# Our first model include all variables, in which we check the p value of all variables and eliminate one with p value larger pre-set significant level(5%).


# Residuals 

# y_hat predicted y values from the model

# Calculate the errors of the model
df$reg23_res <- df$SalePrice - df$y_hat

# Find houses with largest negative errors
df %>% top_n( -3 , reg23_res ) %>% 
  select(PID, SalePrice, y_hat, reg23_res) %>% kable(caption = "List of Houses with largest negative errors, Top 3")

```

```{r, echo=F, message=FALSE, warning=FALSE}
# Find houses with largest positive errors
df %>% top_n( 3 , reg4_res ) %>% 
  select( Country, life_exp,exp_pred, reg4_res) %>%  kable(caption = "List of Coutnries with largest positive errors, Top 3")



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

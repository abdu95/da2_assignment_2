#######################
##  DA2 and Coding   ##
##    Assignment     ##
## Analysis of house ##
##     price         ##
##                   ##
## NO. 1: Clean the  ##
##     data          ##
##                   ##
#######################

install.packages("ggplot2")
library(ggplot2)
library(tidyverse)


# get Housing cross-sectional data
df <- read.csv('data/raw/Housing_raw.csv')

str(df)


sum(is.na(df$SalePrice))
# no missing values in SalesPrice

sum(is.na(df$MS.SubClass))
# no missing values in MS.SubClass

which(is.na(df$MS.Zoning))
# no missing values in MS.Zoning

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

df$BsmtFin.Type.1 <- df$BsmtFin.Type.1 %>% replace_na("No Basement")
df$BsmtFin.Type.1[df$BsmtFin.Type.1 ==''] <- NA
df$BsmtFin.Type.1.fact <- factor(df$BsmtFin.Type.1, order = TRUE,
                                 levels = c("No Basement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))

df$BsmtFin.Type.2 <- df$BsmtFin.Type.2 %>% replace_na("No Basement")
df$BsmtFin.Type.2[df$BsmtFin.Type.2 ==''] <- NA
df$BsmtFin.Type.2.fact <- factor(df$BsmtFin.Type.2, order = TRUE,
                                 levels = c("No Basement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
df$Heating.fact <- as.factor(df$Heating)
df$Heating.QC.fact <- factor(df$Heating.QC, order = TRUE,
                             levels = c("Po", "Fa", "TA", "Gd", "Ex"))
df$Central.Air.fact <- as.factor(df$Central.Air)
df$Electrical[df$Electrical ==''] <- NA
df$Electrical.fact <- factor(df$Electrical, order = TRUE,
                             levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"))
df$Kitchen.Qual.fact <- factor(df$Kitchen.Qual, order = TRUE,
                               levels = c("Po", "Fa", "TA", "Gd", "Ex"))
df$Functional.fact <- factor(df$Functional, order = TRUE,
                             levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"))

# 1422 houses with 0 Fireplaces
# bsmt.exposure.empty.2 <- df[is.na(df$Fireplace.Qu),]
df$Fireplace.Qu <- df$Fireplace.Qu %>% replace_na("No Fireplace")
df$Fireplace.Qu.fact <- factor(df$Fireplace.Qu, order = TRUE,
                               levels = c("No Fireplace","Po", "Fa", "TA", "Gd", "Ex"))

# 157 houses has no garage
sum(is.na(df$Garage.Type))
df$Garage.Type <- df$Garage.Type %>% replace_na("No Garage")
df$Garage.Type.fact <- as.factor(df$Garage.Type)

# 2 houses has garage, but garage finish empty
# garage.finish.empty <- df[df$Garage.Finish == "",]
df$Garage.Finish <- df$Garage.Finish %>% replace_na("No Garage")
df$Garage.Finish[df$Garage.Finish == ""] <- NA
df$Garage.Finish.fact <- factor(df$Garage.Finish, order = TRUE,
                                levels = c("Not given", "Unf", "RFn", "Fin"))

df$Garage.Qual <- df$Garage.Qual %>% replace_na("No Garage")
# 2 empty, 1 has garage
# garage.qual.empty <- df[df$Garage.Qual == "",]
df$Garage.Qual[df$Garage.Qual == ""] <- NA
df$Garage.Qual.fact <- factor(df$Garage.Qual, order = TRUE,
                              levels = c("No Garage","Po", "Fa", "TA", "Gd", "Ex"))

df$Garage.Cond <- df$Garage.Cond %>% replace_na("No Garage")
df$Garage.Cond[df$Garage.Cond == ""] <- NA
df$Garage.Cond.fact <- factor(df$Garage.Cond, order = TRUE,
                              levels = c("No Garage","Po", "Fa", "TA", "Gd", "Ex"))


df$Paved.Drive.fact <- factor(df$Paved.Drive, order = TRUE,
                              levels = c("N", "P", "Y"))
df$Pool.QC <- df$Pool.QC %>% replace_na("No Pool")
# 2917 houses has no pool. only 13 has
sum(df$Pool.QC == "No Pool")
df$Pool.QC.fact <- factor(df$Pool.QC, order = TRUE,
                          levels = c("No Pool", "Fa", "TA", "Gd", "Ex"))

df$Fence <- df$Fence %>% replace_na("No Fence")
df$Fence.fact = factor(df$Fence, order = TRUE,
                       levels = c("No Fence", "MnWw", "GdWo", "MnPrv", "GdPrv"))

df$Misc.Feature <- df$Misc.Feature %>% replace_na("None")
df$Misc.Feature.fact <- as.factor(df$Misc.Feature)

df$Sale.Type.fact <- as.factor(df$Sale.Type)
df$Sale.Condition.fact <- as.factor(df$Sale.Type)

drops <- c("MS.SubClass","MS.Zoning", "Street", "Alley", "Lot.Shape",
           "Land.Contour", "Utilities", "Lot.Config", "Land.Slope", 
           "Neighborhood", "Condition.1", "Condition.2", "Bldg.Type",
           "House.Style", "Overall.Qual", "Overall.Cond", "Roof.Style", 
           "Roof.Matl", "Exterior.1st", "Exterior.2nd", "Mas.Vnr.Type", 
           "Exter.Qual", "Exter.Cond", "Foundation", "Bsmt.Qual", 
           "Bsmt.Cond", "Bsmt.Exposure", "BsmtFin.Type.1", "BsmtFin.Type.2",
           "Heating", "Heating.QC", "Central.Air", "Electrical",
           "Kitchen.Qual", "Functional", "Fireplace.Qu", "Garage.Type",
           "Garage.Finish", "Garage.Qual", "Garage.Cond", "Paved.Drive",
           "Pool.QC", "Fence", "Misc.Feature", "Sale.Type", "Sale.Condition") 

df <- df[ , !(names(df) %in% drops)]



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





table(is.na(df$Lot.Frontage))
# NA means	No Garage
sum(is.na(df$Garage.Type))
df$Garage.Type <- df$Garage.Type %>% replace_na("No Garage")
sum(is.na(df$Garage.Type))


write_csv(df, 'data/clean/Housing_clean.csv')





qplot(log(houses_raw$SalePrice), geom="histogram") 


what <- colSums(sapply(houses_raw, is.na))
what
# 26 missing values

num_data <- select_if(df, is.numeric)

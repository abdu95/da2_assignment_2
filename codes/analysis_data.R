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

# read clean data
df <- read.csv('data/clean/Housing_clean.csv')

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

summary( df )


ggplot(df, aes(x = SalePrice)) +
  geom_histogram(fill='navyblue') +
  labs(x = "Sale price of houses")



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

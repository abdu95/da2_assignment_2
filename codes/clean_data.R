#######################
##  DA2 and Coding   ##
##    Assignment     ##
## Analysis of house ##
##     price         ##
##                   ##
## NO. 2: Clean the  ##
##     data          ##
##                   ##
#######################

install.packages("ggplot2")
library(ggplot2)


qplot(log(houses_raw$SalePrice), geom="histogram") 





what <- colSums(sapply(houses_raw, is.na))
what
# 26 missing values

num_data <- select_if(houses_raw, is.numeric)

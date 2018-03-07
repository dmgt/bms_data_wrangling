library(tidyverse)

#Temporarily using manual columnnames to demonstrate, since orignial column names include encoding that gave error
# error was not not solved by locale = locale(encoding = "UTF-8")
vav <- read_csv("sample_data/VAV 2-01.csv", na = "nan", skip = 1,  col_names = c("Time", "DamperPos", "SpaceTemp"))
    
head(vav)
summary(vav)

vav_subset <- vav[1:10,]

transform_diagonal_data(vav_subset)

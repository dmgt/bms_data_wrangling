library(tidyverse)

#Temporarily using manual columnnames to demonstrate, since orignial column names include encoding that gave error
# error was not not solved by locale = locale(encoding = "UTF-8")
raw_vav <- read_csv("sample_data/VAV 2-01.csv", na = "nan", skip = 1,  col_names = c("Time", "DamperPos", "SpaceTemp")) 
    
head(raw_vav)
summary(raw_vav)

raw_vav_subset <- raw_vav[1:10,]

#Note that this outputs a tibble with the Time column as type chronological
vav <- transform_diagonal_data(raw_vav_subset)
vav

setwd('S:/Trainning/Data')
df <- read.csv('hw1_data.csv')
mean(df[df$Ozone>31 &  df$Temp>90,]$Solar.R, na.rm = TRUE)
mean(df[df$Month==6,]$Temp, na.rm = TRUE)
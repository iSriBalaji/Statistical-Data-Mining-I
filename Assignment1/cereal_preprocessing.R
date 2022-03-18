################
## Sri Balaji Muruganandam
## 50414549
## SDM Assignment 1
## Cereal Data
###############

#install.packages("skimr")
#skimr provides in-depth statistics about the data
library(skimr)

rm(list = ls())
setwd("G:\\SDM_Sem01\\Assignment1")

c_data <- read.delim("cereal.csv",sep = ",")

#Dimension of the data
dim(c_data)

#Viewing the sample data
head(c_data, 10)
tail(c_data, 5)

#Summary of the dataset
summary(c_data)

#ZValues and Counts of Categorical predictors
table(c_data$mfr)
table(c_data$type)

#Checking for missing data
sum(is.na(c_data))   #There is no missing value in the dataset

#Skimming to get in-depth statistics about the data
skim(c_data)

#Grouping data by manufacturer and skimming the data
grp_data <- c_data %>% dplyr::group_by(mfr)
skim(grp_data)

#Plotting cereal data
x11()
plot(c_data)

x11()
plot(c_data$calories,c_data$protein,col="#4B3869")

x11()
col <- c_data$potass
a = hist(col,breaks = 15, col="#4B3869", border = "#6D8299")
x_axis <- seq(min(a),max(a), length = 40)
y_axis <- dnorm(x_axis,mean = mean(col), sd = sd(col))
y_axis <- y_axis*diff(h$mids[1:2])*length(col)
lines(x_axis, y_axis, col = "red", lwd = 2)

den <- density(col)
plot(den)

x11()
hist(c_data$calories,breaks = 10, col="#4B3869", border = "#6D8299")

den <- density(c_data$calories)
plot(den)

x11()
hist(c_data$protein,breaks = 6, col="#4B3869", border = "#6D8299")



x11()
hist(c_data$fat,breaks = 5, col="#4B3869", border = "#6D8299")

x11()
hist(c_data$sodium,breaks = 15, col="#4B3869", border = "#6D8299")

x11()
hist(c_data$fiber,breaks = 14, col="#4B3869", border = "#6D8299")

x11()
hist(c_data$carbo,breaks = 12, col="#4B3869", border = "#6D8299")

x11()
hist(c_data$sugars,breaks = 12, col="#4B3869", border = "#6D8299")

x11()
hist(c_data$vitamins,breaks = 10, col="#4B3869", border = "#6D8299")

x11()
hist(c_data$shelf,breaks = 7, col="#4B3869", border = "#6D8299")

x11()
hist(c_data$weight,breaks = 7, col="#4B3869", border = "#6D8299")

x11()
hist(c_data$cups,breaks = 7, col="#4B3869", border = "#6D8299")

x11()
hist(c_data$rating,breaks = 10, col="#4B3869", border = "#6D8299")



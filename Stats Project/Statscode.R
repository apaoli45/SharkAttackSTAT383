directory<-getwd()
setwd(directory)


# ANOVA Testing written by Aidan Paoli
# install packages ggpubr
# Read in csv data for Anova
my_data<-read.csv("SharkattackmonthdataforANOVA.csv")

# Do the ANOVA calculation
levels(my_data$Month)
my_data$Month<-ordered(my_data$Month,
                       levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
Anova_sharkattack<-aov(Count~Month,data=my_data)

# Export Results in Summary and Show in dividual P-values
summary(Anova_sharkattack)
TukeyHSD(Anova_sharkattack)


# Creates Plots for ANOVA Results
library("ggpubr")
ggboxplot(my_data,x="Month",y="Count",
          color="Month",
          Order=c("1","2","3","4","5","6","7","8","9","10","11","12"),
          ylabel="Count of Shark Attacks",xlab="Month")
ggline(my_data,x="Month",y="Count",
       add=c("mean_se","jitter"),
       order=c("1","2","3","4","5","6","7","8","9","10","11","12"),
       ylabel="Count of Shark Attacks",xlab="Month")




# Chi Square Test written by Christopher Treutlein
Time <- read.csv("Timecsv - Sheet1.csv") 
Time <- na.omit(Time)
obs <- Time$Count
expected_pmf <- as.numeric(Time$expected_pmf)
chi_squared_test <- chisq.test(x = obs, p = expected_pmf)
print(chi_squared_test)

# Reject Null Hypothesis
# At least one count was significantly different that others




# Linear Regression for Year written by Charles Nash
# Lets read the data
library(readxl)
df <- read_excel("DataforYearLinearRegression.xlsx")
fit_lm <- lm( Frequency ~ Year1, data = df)
fit2_lm <- lm(Frequency ~ Year2, data = df)
print(summary(fit_lm))
print(summary(fit2_lm))
# Plotting the data points
plot(df$Year1, df$Frequency, main = "Number of Attacks per Year (1900-2019)",
     xlab = "Year", ylab = "Number of Attacks",
     pch = 19)

# Adding the regression line to the above plot
abline(fit_lm, col = "blue")
abline(fit2_lm, col = "red")
legend(1900, 140, legend=c("Linear estimate of data from 1900-2019", "Linear estimate of data from 1990-2019"),
       col=c("blue", "red"), lty=1:1, cex=1)





# Linear Regression for number of attacks vs age
# Author: Paul Mashimo

# tidyverse, library, and data.table libraries required to run this code

# if not installed, run this:

#install.packages('tidyverse','readxl','data.table')

library(readxl)
library(data.table)
library(dplyr)
df <- read_excel("OriginalSharkAttackData.xlsx")

# use only entries that have an exact age
df1 <- df[!is.na(as.numeric(as.character(df$Age))),] # remove non-numeric data
df1$Age <- as.numeric(as.character(df1$Age))   # convert to numeric data

freqTab = table(df1$Age)    # unfiltered values for age

freqTab <- t(freqTab)
freqTab <- as.data.frame(freqTab)    # transpose the data
freqTab <- subset(freqTab, TRUE, select=c(Var2,Freq))# remove A column

# turn variables to integer
freqTab$Var2 <- as.integer(freqTab$Var2)
freqTab$Freq <- as.integer(freqTab$Freq)

# Separating the data between above and below 18 years of age


under18 <- subset(freqTab, Var2 < 18)
over18  <- subset(freqTab, Var2 >= 18)


# Plotting the data points
plot(freqTab$Var2, freqTab$Freq, main = "# of attacks by age",
     xlab = "Age", ylab = "Frequency",
     pch = 19)

y1 <- over18$Freq
x1 <- over18$Var2

fit_lm <- lm(y1 ~ x1, data = over18)
print(summary(fit_lm))

# Adding the regression line to the above plot
abline(fit_lm, col = "blue")

y2 <- under18$Freq
x2 <- under18$Var2

fit_lm <- lm(y2 ~ x2, data = under18)
print(summary(fit_lm))

# Adding the regression line to the above plot
abline(fit_lm, col = "red")


################################################################################
#
# Looking at Outlier Detection
# 
#
################################################################################

# load in the PregnancyDuration
PregnancyDuration <- read.csv("PregnancyDuration.csv")
summary(PregnancyDuration)

# perform all the calculations
PregnancyDuration.IQR <- IQR(PregnancyDuration$GestationDays)
LowerInnerFence <- 260 - 1.5*PregnancyDuration.IQR
UpperInnerFence <- 272 + 1.5*PregnancyDuration.IQR

# determine the points and their indices that violate the fences
PregnancyDuration$GestationDays[which(PregnancyDuration$GestationDays > UpperInnerFence)]

# show the visualization using boxplot(), where range=3 will draw the Tukey fences inside 3 x IQR
boxplot(PregnancyDuration$GestationDays, range=3)

# alternatively, extract the data in the console using $stats and $out
boxplot(PregnancyDuration$GestationDays, range=3)$stats
boxplot(PregnancyDuration$GestationDays, range=3)$out

# load in the call center data
CallCenter <- read.csv("CallCenter.csv")
summary(CallCenter)

# need to scale and center the data. To do this, use the scale() function
CallCenter.scale <- scale(CallCenter[2:11])
summary(CallCenter.scale)

# load the DMwR package
library(DMwR)

# To calculate LOF, use the lofactor() function by passing it the dataset and a k value, e.g. 5
# Data with the highest factors (LOFs usually hover around 1)
CallCenter.lof <- lofactor(CallCenter.scale, 5)
CallCenter[which(CallCenter.lof > 1.5), ]

################################################################################
#
# Forecasting of Sword Demands
# Generate the exponential smoothing forecast 
#
################################################################################

# load in the sword demand data
sword <- read.csv("SwordDemand.csv")

# tell R that this is a time series data using ts()
# passing a frequency value and a starting point Jan 2010
# this will then format it nicely into a time table by month
sword.ts <- ts(sword, frequency=12, start=c(2010, 1))

# plot the demand
plot(sword.ts)

# load the forecast package
library(forecast)

# use the forecast() to perform the forecast and save into a new variable
# this will give you a forecast with prediction intervals built-in
sword.forecast <- forecast(sword.ts)

# check the actual forecasting technique used by printing the method value
sword.forecast$method

# plot it 
plot(sword.forecast)


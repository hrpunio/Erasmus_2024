##
## Estimate linear trend for any variable
## imported from the WorldBank
##
library("tidyverse")
library('WDI')       ## easy import from WBank
## We need forecast to easily compute linear trend
library("forecast")  ##

## example indicator: GDP constant prices
wdi.indicators <- c(
  'NY.GDP.MKTP.KD'   #= GDP (constant 2015 US$)
)

year.start <- 1990

## get the data
d0 <- WDI(wdi.indicators, country = "POL", start=year.start)

## transform the data 
## time-serives are in reverase chronological order at WorldBank
d1 <- d0 %>% arrange(NY.GDP.MKTP.KD) %>%
  ## remove missing values
  na.omit()

## Convert data on GDP to time-series!!!
## frequency = 1 denotes annual data
## start = 1990 denotes number of the first year
gdp <- ts(d1$NY.GDP.MKTP.KD, start=1990)

## summary statistics
summary(gdp)
## simple plot
plot(gdp)

m0 <- tslm(gdp ~ trend )
summary(m0)

## Forecast for the next year (h=1)
## 
f0 <- forecast(m0, h=1, level=95)
f0
plot(f0)

## try logarithms
## log(gdp) ~ trend
m1 <- tslm(log(gdp) ~ trend )
summary(m1)

# Forecast and plot
f1 <- forecast(m1, h=1, level=95)
f1

plot(f1)

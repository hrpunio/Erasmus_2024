## Environmental Kuznets Curve EKC
##
library("tidyverse")
library("ggplot2")
library("ggfortify")
library("forecast")
library("knitr")
library('qpcR')
library("gvlma")
library('mctest')
library("lmtest")
library("car") # D-W test
library("WDI")

## The Problem
## Put some economics theory here
## Its is assumed that relation between CO2 emission and GDP is 
## of `inverted U` type
## E = a + b GDP + c GDP^2
## Note: if c=0 linear
##       if c<0 inverted U


## Rehersal, plot E = a + b GDP + c GDP^2 relation in R
##


MyFunction <- function(x){y = -3 + 1 * x + -0.2 * x**2 }
x <- seq(from = -6, to = 12, length.out = 101)
y <- MyFunction(x)
xy <- data_frame(x, y)

ggplot(xy, aes(x=x, y=y)) + geom_point()

## or even
n <- rnorm(101, mean = 0, sd = 1.1)
y <- y + n
xy <- data_frame(x, y)
ggplot(xy, aes(x=x, y=y)) + geom_point()


##########################################################################
## The data
##########################################################################
### Read all the data
## vector of indicators
wdi.indicators <- c(
'NY.GDP.MKTP.KD',    #= GDP constant US$ 2015
'SP.POP.TOTL',       #= Population total
'EN.ATM.CO2E.PC',    #= CO2 emission in tons per/capita
"EG.ELC.RNWX.ZS",    #=  Electricity from renovable sources % of total
'NY.GDP.PCAP.CD',    #= GDP per capita (current US$) 
"NY.GDP.PCAP.PP.CD", #= GDP per capita, PPP (current international $) 
'EG.ELC.COAL.ZS',    #= Electricity produced from coal % of total
'AG.PRD.LVSK.XD',    #= Livstock production index (2014--2016=100)
"AG.CON.FERT.ZS",    #= Fertilizer consumption (kg/hectare of arable land)
"EG.USE.PCAP.KG.OE"  #= Energy use (kg of oil equivalent per capita)
)
year.start <- 1990

f0 <- WDI(wdi.indicators, country = "all", start=year.start)

wb.groups <- read.csv("wb_groups.csv", sep=';')
## Countries/groups
countries <- wb.groups %>%
  group_by(CountryCode) %>%
  summarise(code=first(CountryCode)) %>% pull (code)


## Transform to _wider_ format
## change colum names to something less complicated
## Crossectional data: year == 2015
## Remove small countries: year >= 1990
wbl.0 <- f0 %>% filter (iso3c %in% countries ) %>%
  dplyr::select (
          countryname=country, 
          code=iso3c, 
          year, 
          co2=EN.ATM.CO2E.PC, 
          gdp_per_cap='NY.GDP.PCAP.PP.CD', 
          e4coal='EG.ELC.COAL.ZS',
          ren_energy = 'EG.ELC.RNWX.ZS',
          fcons='AG.CON.FERT.ZS', 
          euse='EG.USE.PCAP.KG.OE', 
          livestock ='AG.PRD.LVSK.XD',
          gdpt=NY.GDP.MKTP.KD,  
          pop=SP.POP.TOTL) %>%
  #dplyr::filter (year == 2015) %>%
  dplyr::filter(pop > 1000000) %>% 
  mutate(log_gdp_per_cap = log(gdp_per_cap),
                   log_co2 = log(co2),
                   gdp_per_cap_squared = gdp_per_cap^2,
                   log_gdp_per_cap_squared = log(gdp_per_cap)^2)

## check sample size:
wbl <- wbl.0 %>% dplyr::filter (year == 2015) 
  
nrow(wbl)


## Scatter-plot matrix
## scatter-plot matrix for co2, gdp, fcons, euse
wbl %>% dplyr::select(co2, gdp_per_cap, fcons, euse, e4coal) %>% pairs()


##Correlation matrix for
## NOTE: na.omit function omits rows with NA values
wb.corr <- wbl %>% dplyr::select (co2, gdp_per_cap, fcons, euse, e4coal) %>% 
  na.omit() %>% 
  cor()

wb.corr
## #######################################################################

## Model 0
##
## $CO2 = a + b GDP$
##
## lin-lin model

lm0 <- lm(co2 ~ gdp_per_cap, data=wbl)
summary(lm0)

## Model 1
##
## $\ln CO2 = a + b \ln GDP$
## 
## log-log model


lm1 <- lm(log_co2 ~ log_gdp_per_cap, data=wbl)
summary(lm1)

## Model 2
##
##$\ln CO2 = a + b \ln GDP + c \ln^2 GDP $
##
##the environmental Kuznets curve with logs
##as the theory of the Kuznets curves suggests, the sign 
##of the coefficient before log_gdp_per_cap is positive and log_gdp_per_cap_squared is negative, 
##resulting in the U-shape
##The model is great in terms of R2adj and p-values of the coefficients  


lm2 <- lm(log_co2 ~ log_gdp_per_cap + log_gdp_per_cap_squared, data=wbl)
summary(lm2)

### chart residuals vs fitted
plot(lm2, which = 1) 


### Check for heteroscedasticity
###
### Breusch-Pagan Test For Homoscedasticity
###
#high p = no heteroscedasticity
bptest(lm2)


### Check for autocorrelation of error term
## Check autocorrelation of error term
## Durbin Watson Test for Autocorrelation
## high p = errors not autocorrelated
durbinWatsonTest(lm2)

### Check for normality of residuals

## visually looking at qq-plot
plot(lm2, which = 2) 
qqPlot(lm2)
shapiro.test(lm2$residuals)


## Check multicollinearity
## for models with 

## omcdiag(lm2)

vif(lm2)

## lm2 is highly collinear but it is OK

## Perhaps some other models ...
##
## Model 3
##
##$\ CO2 = a + b GDP + c GDP^2 $
##
##the classical environmental Kuznets curve 
##the sings of the coefficients are concurring with the theory, but the values are very hard to work with - the previous model is clearly better (both by R2adj and p-values of the coefficients)

lm3 <- lm(co2 ~ gdp_per_cap + gdp_per_cap_squared, data=wbl)
summary(lm3)

## Model 4
##
##after Cederborg, J., & Snöbohm, S. (2016). Is there a relationship between economic growth and carbon dioxide emissions?.
##
lm4 <- lm(co2 ~ gdp_per_cap + gdp_per_cap_squared + ren_energy + e4coal + livestock , data=wbl)
summary(lm4)
##

## Model 5
##
## Another, less theory-driven approach - let the step function find the best model using AIC
##
lm_no_log_full <- lm(co2 ~ gdp_per_cap + gdp_per_cap_squared + ren_energy + e4coal + livestock + fcons + euse + log(gdpt) + log(pop), data=wbl)
lm5 <- stats::step(lm_no_log_full, direction = "backward")
summary(lm5)

## Model 6
##
##The same as the previous model, but with logs

lm_log_full <- lm(log_co2 ~ log_gdp_per_cap + log_gdp_per_cap_squared + ren_energy + e4coal + livestock, data=wbl)
lm6 <- stats::step(lm_log_full, direction = "backward")
summary(lm6)


## 
## Select between nested models with anova
## (significance of difference of variance explainded)
##
## The main problems with using anova function are the following:
##
##1) different dependant variables 
##
##2) because some columns contain NA's, the models are trained 
##on different number of rows  => cannot be compared directly


anova(lm0, lm3, test="F")

anova(lm1, lm2, test="F")

##Concluding remarks: 
##However, we can use another approach - rmse for the models with the same dependant variables: 


qpcR::RMSE(lm1)
qpcR::RMSE(lm2)
qpcR::RMSE(lm6)

qpcR::RMSE(lm0)
qpcR::RMSE(lm3)
qpcR::RMSE(lm4)
qpcR::RMSE(lm5)


##Judging by the combination of the following metrics: 
##1) R2adjusted
#2) statistical significance of the coefficients
##3) RMSE
##
##we can conclude that the model lm6: 
##log_co2 ~ log_gdp_per_cap + log_gdp_per_cap_squared + ren_energy + e4coal
##is the best model for explaining co2 emissions
##
## Repeat for other sample


wb.highmid <- read.csv('https://raw.githubusercontent.com/hrpunio/Erasmus_2021EE/main/wb_groups.csv', sep = ';',  header=T, na.string="NA") %>%
  ## about 30 countries:
  ##filter ( GroupName == 'Low income')
  ##filter ( GroupName == 'High income')
  dplyr::filter ( GroupName == 'High income' | GroupName == 'Middle income')

nrow(wb.highmid)

wbl_2 <- wb %>% dplyr::filter (code %in% wb.highmid$code ) %>%
  pivot_wider( names_from = indicatorcode, values_from = value) %>%
  dplyr::select (countryname, code, year, 
          co2=EN.ATM.CO2E.PC, 
          gdp_per_cap=NY.GDP.PCAP.PP.CD, 
          e4coal=EG.ELC.COAL.ZS,
          ren_energy = EG.ELC.RNWX.ZS,
          fcons=AG.CON.FERT.ZS, 
          euse=EG.USE.PCAP.KG.OE, 
          livestock = AG.PRD.LVSK.XD,
          gdpt=NY.GDP.MKTP.KD,  
          pop=SP.POP.TOTL) %>%
  mutate(log_gdp_per_cap = log(gdp_per_cap),
                   log_co2 = log(co2),
                   log_pop = log(pop),
                   log_gdp = log(gdpt),
                   gdp_per_cap_squared = gdp_per_cap^2,
                   log_gdp_per_cap_squared = log(gdp_per_cap)^2) %>%
  dplyr::filter (year == 2015)

nrow(wbl)

lm_1_new  <-  lm(log_co2 ~ log_gdp_per_cap + log_gdp_per_cap_squared, data=wbl_2 ) 
lm_1_new %>% summary()

lm_2_new  <-  lm(log_co2 ~ log_gdp_per_cap + log_gdp_per_cap_squared + 
    ren_energy + e4coal, data=wbl_2 ) 
lm_2_new %>% summary()

qpcR::RMSE(lm_1_new)
qpcR::RMSE(lm_2_new)

##Judging by the combination of the following metrics: 
##
##1) R2adjusted
##
##2) statistical significance of the coefficients
##
##3) RMSE
##
##we can conclude that the model lm_2_new: 
##
##log_co2 ~ log_gdp_per_cap + log_gdp_per_cap_squared + ren_energy + e4coal
##is the best model for explaining co2 emissions in the High income and Middle income countries

#################################################################################################
####  Panel data
#################################################################################################
wbl <- wbl.0 

library("plm")


wbl.p <- pdata.frame(wbl, index=c('code', 'year'))

m.fixed <- plm(log_co2 ~ log_gdp_per_cap + log_gdp_per_cap_squared, data=wbl.p, model="within" )  #fixed model
summary(m.fixed)

##

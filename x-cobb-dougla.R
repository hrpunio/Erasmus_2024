###
### Cobb-Douglas on World Bank data
###
### output = a · L^b  · C^c       (^ means power)
### or
### log(output) = log(a) + b·log(L) + c·log (C)
### where L = labour input
###       C = capital input
###

## You should probably install all the above librarier first
## use:
## install.packages(c("tidyverse", 'ggplot2', 'WDI', 'knitr', 'lmtest', 'car') )
##
library("tidyverse") ## data manipulation 
library("ggplot2")   ## better graphics
library('WDI')       ## easy import from WBank
library('knitr')     ## decent tables
library("lmtest")    ##
library("car")       ## D-W test

## vector of indicators to download
## Variables at WorldBank database has unique identifier
## for example NE.GDI.FTOT.ZS is gross fixed capital formation
wdi.indicators <- c(
  'NE.GDI.FTOT.KD',  #= Gross fixed capital formation Constant 2015 USD
  'SL.TLF.TOTL.IN',  #= Labor force, total
  'NY.GDP.MKTP.KD'   #= GDP (constant 2015 US$)
)

## Declare first year 
year.start <- 1990

f0 <- WDI(wdi.indicators, country = "all", start=year.start)

## Modify data set
f1 <- f0 %>%
## select country, iso3c, year, NE.GDI.FTOT.KD, SL.TLF.TOTL.IN, NY.GDP.MKTP.KD
## columns; rename column NE.GDI.FTOT.KD to C, column SL.TLF.TOTL.IN to L
## and column NY.GDP.MKTP.KD to  gdp
  select (
      country,
      iso3c,
      year,
      C = NE.GDI.FTOT.KD,
      L = SL.TLF.TOTL.IN,
      gdp = NY.GDP.MKTP.KD)

## Estimate Cobbb-Douglas for Poland
f1.pl <- f1 %>% 
  ## Filter data for Poland
  filter ( iso3c == 'POL') %>%
  ## Compute (natural) logarithms:
  mutate (lC = log(C),
          lL = log(L),
          lgdp = log(gdp))
  
## Estimate regression equation:
##     ln(GDP) = a + b ln(C) + c ln(L)
## results are stored in m0 variable
m0 <- lm (lgdp ~ lC + lL, data=f1.pl)
#        ##########################
#          equation spec,  data-set

## Print results
summary(m0)

## Plot some charts (optional)
p1 <- ggplot(f1.pl, aes(x=lC, y=lgdp)) +
  geom_point()
p1

p2 <- ggplot(f1.pl, aes(x=lL, y=lgdp)) +
  geom_point()
p2

## OR estimate Cobb-Douglas for cross-sectional dataset
## let say we want to estimate C-D for OECD countries

## Declare vector for all OECD ISO3 identifiers:
oecd <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST", "FIN", "FRA",
          "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX",
          "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP",
          "SWE", "CHE", "TUR", "GBR", "USA")
## or UE-27 if you like:
ue <- c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN',
        'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 'LTU', 'LUX',
        'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 'SVN', 'ESP', 'SWE' )
## or same other group defined in a similar way

## Estimate C-D for OECD for 2022
## Filter data for OECD (using oece vector declared above)
f2 <- f1 %>% filter (iso3c %in% oecd ) %>%
  ## filter for 2022
  filter (year == 2022) %>%
  ## compute logarithms
  mutate (
          lC = log(C),
          lL = log(L),
          lgdp = log(gdp))

p1 <- ggplot(f2, aes(x=lC, y=lgdp)) +
    geom_point()
p1
  
## Scatter plot matrix
f2 %>% dplyr::select(lgdp, lC, lL) %>% pairs()
  
  
## Estimate regression equation:
##     ln(GDP) = a + b ln(C) + c ln(L)
## results are stored in m1 variable
m1 <- lm (lgdp ~ lC + lL, data=f2)
  

summary(m1)


# Breusch-Pagan Test For Homoscedasticity
# high p = no heteroscedasticity
bptest(m1)

## Check autocorrelation of error term
## Durbin Watson Test for Autocorrelation
## high p = errors not autocorrelated
durbinWatsonTest(m1)

## Check for normality of residuals
## 
shapiro.test(m1$residuals)

## Check multicollinearity
vif(m1)


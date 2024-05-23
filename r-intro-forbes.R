## R
## Minimalistic introduction

## NOTE: Everything after # is ignored by R interpreter

## Forbes billionaires
## https://www.forbes.com/billionaires/

## wektors napisów (character vector)

## More vectors (think columns in spreadsheets)
## character vector:
milioner <- c('Jeff Bezos', 'Bill Gates', 'Bernard Arnault',
              'Warren Buffett', 
              'Larry Ellison', "Amancio Ortega", 
              "Mark Zuckerberg", "Jim Walton", "Alice Walton", "Rob Walton")

## numeric vector:
## majątek (wealth)
majatek <- c(113, 98,76, 67.5,59,55.1,54.7,54.6,54.4, 54.1)

## another numeric vector
## wiek (age)
wiek <- c(56, 64, 71, 89, 75, 84, 35, 71, 70, 75 )

## character vector (fake) dates
## urodzony (born)
urodzony <- c( 
   '1964-01-01', '1956-01-01', '1949-01-01', '1931-01-01', '1945-01-01',
   '1936-01-01', '1985-01-01', '1949-01-01', '1950-01-01', '1945-01-01' )

## country ISO codes 
kraj <- c( 'US', 'US', 'FR', 'US', 'US', 'ES', 'US', 'US', 'US', 'US' );

## F&I = finance and investment
## branch/sector
branza <- c("Technology", "Technology", "Retail", "F&I", "Technology", "Retail",
   "Technology", "Retail", "Retail", "Retail")

## Dataframe = fundamental R data structure (think spreadsheet)
## dataframe = list of named vectors
forbes <- data.frame(milioner, majatek, urodzony, wiek, kraj, branza)

## str(ucture) = inspect structure of any R object (incl dataframe)
str(forbes)

## print frame (not particularily useful)
forbes

## Print column (more useful)
## df$column = access column from df
## df$colum

## another column:
forbes$majatek

##
## access columns using numbers
## columns are numbered from 1
forbes[, 3]


## Print first 13 rows
## useful functions
head(forbes, n=13)

## Print some rows from the bottom of dataframe
tail(forbes)

## Number of rows in a dataframe
nrow(forbes)


##
## Basic (descriptive) statistics (at least)

## mean of a column majatek (wealth) from dataframe forbes
mean(forbes$majatek)

## some summary statistics
summary(forbes$wiek)
## standard deviation
sd(forbes$wiek)
## minimum value
min(forbes$wiek)

## Simple plot
## plot with default chart (R decides based on the data to plot)
## Guess the plot type :-)
plot(forbes$wiek)

plot(forbes$wiek, forbes$majatek)

## histogram
hist(forbes$wiek)

##
## In practice data are not manually typed as above, but loaded from files or URLs
## As a example file FB2020.csv contains data on Forbs billionaires

## Import data into frame (function csv.read):

forbes <- read.csv("FB2020.csv", dec=".", sep = ';',  header=T, na.string="NA");

## dec = determines decimal 
## sep = cell separation character (';')
## header = if there is a header with variable names (T) or not (F)
## na.string = determines how missing values are encodes (here as 'NA')

## examine structure
str(forbes)

## indexing
w  <- forbes[,3]
p <- forbes[1,]

p
w <- forbes$worth

billionares <- forbes[,"name"]
## or: billionares <- forbes$name

## Basic statistics again

summary(forbes$worth)
forbes.summary <- summary(forbes$worth)
str(forbes.summary)
forbes.summary[1]

## Extract atribute 'Median'
forbes.summary["Median"]
forbes.median <- forbes.summary["Median"]
forbes.median


forbes.mean  <- mean(forbes$age, na.rm = T)

## Printing results
print (forbes.mean)

## Formatted print
sprintf ("%.2f", forbes.mean)

## or (cat = concatenate)
cat ("Median:", forbes.median)

summary(forbes)

## The distibution is extremly skew
forbes.table <- table(forbes$worth)

length(forbes.table)

forbes.table

cut(forbes$worth, breaks=seq(0,120, by=10))

qq <- table(cut(forbes$worth, breaks=seq(0,120, by=10)))

hist(qq)

## Core R can be extended by attaching libraries. Some
## libraries are very useful. Installing a library is very easy:
## install.packages("library") 

## Filtering/selecting/modyfing dataframes with dplyr/tidyverse

## Filtering rows
library("dplyr")
## install.packages("dplyr") 
## installation is automatic (upon confirmation) in RStudio

## filter all billionaires who are non US:
nonus.forbes <- filter(forbes, country != "US")
nonus.forbes

## Modification oprations can be connected to one sequence with %>% operator
## Example: filter some rows %>% select some columns:
nonus.forbes.worth <- filter(forbes, country != "US") %>% 
  select(worth)

## Compute total wealth:
sum(nonus.forbes.worth)

## Print all countries  without repetitions
select(forbes, country) %>% unique

## How many countries:
select(forbes, country) %>% unique %>% nrow

## alternative syntax:
forbes %>% select(country) %>% unique %>% nrow

## Grouping is useful for summarization
## Example: compute total wealth and number of billionaires by country 
by.country <- forbes %>% 
    group_by(country) %>% ## group by country
    summarise(t = sum(worth), n=n()) ## summarise (in groups)

## Print results
by.country

## #####################################################################
## Graphics
## default chart for list of numbers
## ####################################################################

plot (forbes$worth)
## boxplot
boxplot(forbes$worth)
## color= breaks
hist(forbes$worth)

## 
boxplot(worth ~ branch, data=forbes)
select(forbes, branch) %>% unique %>% nrow

## mutate = create new variables
forbes.x <- mutate(forbes, 
  branch = case_when(branch == "Technology" ~ "IT", 
                     branch == "Fashion & Retail" ~ "FR",
                     TRUE ~ "Other"))
forbes.x
boxplot(worth ~ branch, data=forbes.x)

## basic plot
## for two lists (XY-plot)
plot(forbes.x$age, forbes.x$worth)

## Much better quality graphics with ggplot2
##
library ("ggplot2")

#  quick-plot (default charts are generated based on data types)
qplot(data=forbes.x, age, worth, color=branch)
qplot(data=forbes.x, age, worth, facets = . ~ branch)



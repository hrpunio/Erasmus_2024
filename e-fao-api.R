library(readr)
library('tidyverse')
library('WDI')

## Read data
## From file or URL
## read.csv()
##
## If data is compressed there is a problem

url <- 'https://bulks-faostat.fao.org/production/FoodBalanceSheets_E_All_Data.zip'

temp <- tempfile()
download.file(url, temp)


#import data1.csv located within my_data.zip
b0 <- read_csv(unzip(temp, "FoodBalanceSheets_E_All_Data_NOFLAG.csv")) %>%
  filter (Item == 'Meat') %>%
  filter (Element == 'Food supply quantity (kg/capita/yr)') %>%
  pivot_longer(cols = starts_with('Y'), names_to = 'year') %>%
  mutate (year = substr(year, 2,5)) %>%
  select (code='Area Code (M49)', year, value, Area) %>%
  mutate (year = as.numeric(year)) %>%
  mutate (code = substr(code, 2,4)) 

## Problem is M49 codes 
## https://wits.worldbank.org/countryprofile/metadata/en/country/all
unsd <- read.csv(file='m49toIso.csv', sep=';', colClasses=c( M49 ="character"), header=T) %>%
  select (CountryName, CodeM49=M49, ISO3, UE)

## World Bank 
wb_gni <- 'NY.GDP.PCAP.KD'   #= GDP constant US$ 2015 per capita
f0 <- WDI(wb_gni, country = "all", start=2010)

b1 <- left_join(b0, unsd, by=c('code'='CodeM49')) %>%
  left_join(f0, by=c('ISO3'='iso3c', 'year'='year')) 

### Meat consumption
### kg/year per capita

fao.aggregates <- c("001", "002", "014", "017", "015", "018", "011", "019",
                    "021", "013", "029", "005", "142", "143", "030", "034", "035", "145", "150",
                    "151", "154", "039", "155", "009", "053", "054", "097", "199", "432", "722",
                    "901", "902", "159")

b2 <- b1 %>% filter (! code %in% fao.aggregates)

### summary statistics
### summarise
b2 <- b2 %>% filter(year==2021) 

b2 %>% summary()

## box plot

p1 <- ggplot(b2, aes(x='', y=value)) + 
  geom_boxplot() + 
  ylab("#") + 
  xlab('')
p1


p2 <- ggplot(b2, aes(x=NY.GDP.PCAP.KD, y=value)) + 
  geom_point() + 
  ylab("#") + 
  xlab('')
p2
##
## by groups
##
b2 <- b1 %>% filter (year %in% c(2010, 2015, 2020))

b2.s <- b2 %>% group_by(year) %>% summarise(
  m = mean (value, na.rm = T),
  me = median(value, na.rm=T),
  sd = sd(value, na.rm = T)
)
b2.s



p2 <- ggplot(b2, aes(x=NY.GDP.PCAP.KD, y=value)) + 
  geom_point() + 
  ylab("#") + 
  facet_wrap(~ year) +
  xlab('')
p2

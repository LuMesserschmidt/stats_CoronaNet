#------------------------------------------------
# Subsetting, transforming (and plotting) data
# Author: Cindy Cheng & Luca Messerschmidt


# -------------------------------------------------
# load packages 
# -------------------------------------------------
rm(list=ls())

library(readr)
library(tidyverse)

# -------------------------------------------------
# load data
# -------------------------------------------------
coronaNet <- read_csv("data/coronanet_release.csv")
wb <- read_csv("data/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_988619.csv", skip = 4)


# -------------------------------------------------
# # explore data
# -------------------------------------------------
# lets take a look at the first 6 rows
head(coronaNet)

# lets take a look at what classes the different data are stored in and the values they can take on
str(coronaNet)

# lets take a look at the distribution of the countries that have initiated a policy
table(coronaNet$country)%>% sort(decreasing = TRUE) 

# lets look at the distribution of different policies
table(coronaNet$type) %>% sort()

# Class question: how could you look at the distribution of different sub-types: type_sub_cat ? 

names(coronaNet)

# -------------------------------------------------
# clean data
# -------------------------------------------------
coronaNet  = coronaNet %>% 
  mutate(type = ifelse(type == 'Quarantine/Lockdown', 'Lockdown', type))

table(coronaNet$type)
names(wb)

wb = wb %>% gather("year", "gdpPPP", -`Country Name`, -`Country Code`, -`Indicator Name`, -`Indicator Code`, -X65)

?gather

wb = wb %>% select( -`Indicator Name`, -`Indicator Code`, -X65)
wb$year = as.numeric(wb$year)
wb$lgdpPPP= log(wb$gdpPPP)


# -------------------------------------------------
# # subset data
# -------------------------------------------------

businessRestrict = coronaNet %>% filter(type == "Restriction of Non-Essential Businesses")

# what are the dimensions of this new subsetted data?
dim(businessRestrict)
# which countries have instituted restrictions of non-essential businesses?

businessRestrict$country %>% unique()
unique(businessRestrict$country)


# what are the different sub types for the broader non-essentital businesses policy type?
unique(businessRestrict$type_sub_cat)

# -------------------------------------------------
# # Merge data
# -------------------------------------------------
names(wb)[1] = 'country'

names(businessRestrict)

data = merge(businessRestrict, wb %>% filter(year == 2018), by= 'country', all.x = TRUE)


# how can we find out what gdp of the Germany is in this 'data'?
data %>% filter(country == 'Germany') %>% select(gdpPPP)


# how can we find out what the gdp of the United States is in this data?
data %>% filter(country == 'United States of America') %>% select(gdpPPP)


# how can we investigate which countries match across different datasets?
intersect(coronaNet$country, wb$country)


# and the countries that don't match?
cbind(setdiff(coronaNet$country, wb$country), 
      setdiff( wb$country, coronaNet$country)) %>% head()

names(wb)[2] = 'ccode'
names(businessRestrict)[31] = 'ccode'


data = merge(businessRestrict, wb %>% filter(year == 2018), by= 'ccode', all.x = TRUE)

# how can we find out what the gdp of the United States is in this data?
data %>% filter(ccode == 'USA') %>% select(gdpPPP)


# lets see if there's a relationship between how rich a country as measured by gdpPPP is and how many business restrictions it imposes

dataAgg = data %>% 
  group_by(ccode) %>% 
  summarise(numBusinessRestrictions = n(),
            gdpPPP = mean(gdpPPP))

head(dataAgg)

dataAgg %>% filter(ccode == 'DEU') %>% select(gdpPPP)


plot(dataAgg$gdpPPP, dataAgg$numBusinessRestrictions)
cor(dataAgg$gdpPPP, dataAgg$numBusinessRestrictions, use = 'complete.obs')

# Class question: how about the relationship between log gdpPPP and how many business restrictions it imposes?

plot(log(dataAgg$gdpPPP), dataAgg$numBusinessRestrictions)
cor(log(dataAgg$gdpPPP), dataAgg$numBusinessRestrictions, use = 'complete.obs')








#------------------------------------------------
# Difference in Difference & Regression Discontinuity Designs
# Author: Luca Messerschmidt
# Contributions: Princeton.edu: http://www.princeton.edu/~otorres/DID101R.pdf


# -------------------------------------------------
# load packages 
# -------------------------------------------------
rm(list=ls())

library(readr)
library(tidyverse)

# -------------------------------------------------
# load data
# -------------------------------------------------
coronaNet <- read_csv("data/coronanet_release_allvars.csv")
wb <- read_csv("data/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_988619.csv", skip = 4)

# -------------------------------------------------
# # subset and  merge data
# -------------------------------------------------
businessRestrict = coronaNet %>% filter(type %in% c("Restriction and Regulation of Businesses" ,"Restriction of Non-Essential Businesses"))

wb = wb %>% gather("year", "gdpPPP", -`Country Name`, -`Country Code`, -`Indicator Name`, -`Indicator Code`, -X65)

wb = wb %>% select( -`Indicator Name`, -`Indicator Code`, -X65)
wb$year = as.numeric(wb$year)
wb$lgdpPPP= log(wb$gdpPPP)


data = merge(businessRestrict, wb %>% filter(year == 2018), by.x = 'ISO_A3', by.y = "Country Code", all.x = TRUE)

dataAgg = data %>% 
  group_by(country) %>% 
  summarise(numBusinessRestrictions = n(),
            gdpPPP = mean(gdpPPP))

businessRestrictAgg = businessRestrict %>% 
  group_by(date_announced, country) %>% 
  summarise(numBusinessRestrictions = n())

dframe = expand.grid(unique(coronaNet$date_announced), unique(coronaNet$country))
names(dframe) = c('date_announced', 'country')

businessRestrictFull = merge(dframe, businessRestrictAgg, by = c('date_announced', 'country'), all.x = TRUE)
businessRestrictFull$numBusinessRestrictions = ifelse(is.na(businessRestrictFull$numBusinessRestrictions), 0, businessRestrictFull$numBusinessRestrictions)

RestrictAgg = coronaNet %>% 
  group_by(date_announced, country) %>% 
  summarise(numpolicies = n())%>%
  filter(!is.na(date_announced))

businessRestrictFull = merge(businessRestrictFull, RestrictAgg, by = c('date_announced', 'country'), all.x = TRUE)
businessRestrictFull$numpolicies = ifelse(is.na(businessRestrictFull$numpolicies), 0, businessRestrictFull$numpolicies)

businessRestrictFull$ratio<-businessRestrictFull$numBusinessRestrictions/businessRestrictFull$numpolicies

# -------------------------------------------------
# CoronaNet Example
# -------------------------------------------------
#We want to know if countries that experienced Corona early on have a higher number of business restrictions


# Create a dummy variable to indicate the time when the treatment started. Lets assume the Corona crisis started April 2020. In this case, days before April 2020 will have a value of 0 and April+ a 1. 

businessRestrictFull$time = ifelse(businessRestrictFull$date_announced >= "2020-04-01", 1, 0)



# Create a dummy variable to identify the group exposed to the treatment. In this example lets assumed that countries Italy, Iran and China exprienced hard COVID-Waves (=1). Other countries did not (=0). 

businessRestrictFull$treated = ifelse(businessRestrictFull$country == "Italy" | businessRestrictFull$country == "China" | businessRestrictFull$country == "Iran", 1, 0)

# Create an interaction between time and treated. We will call this interaction ‘did’.

businessRestrictFull$did = businessRestrictFull$time * businessRestrictFull$treated

# Estimating the DID estimator

didreg = lm(numBusinessRestrictions ~ treated + time + did, data = businessRestrictFull)

summary(didreg)

coefplot(didreg)

# The coefficient for ‘did’ is the differences-in-differences estimator. The effect is not significant with the treatment having a positive effect on business restrictions


#The problem is that we can not clearly identify countries that received the treatment (early cases) and does that did not as countries policies. 


# With the Regression Discontinuity Design, we loose the Control/Treatment group character and focus on the time difference.
# In our preliminary example, we want to check the influence of cases on the number of policies (be careful: This is only an example to illustrate the stats and does not make a lot of sense)

names(coronaNet)

download.file("https://covid.ourworldindata.org/data/ecdc/total_cases.csv", "./data/cases.csv")
cases <- read_csv("data/cases.csv")

cases<-gather(cases,"country","cases",3:212,-date)
cases$date_announced<- cases$date
cases$ration <- cases$cases/cases$World

cases<-cases[,-1]

test<- left_join(businessRestrictFull,cases, by=c("country","date_announced"))

didreg = lm(numpolicies ~ ration + time + I(ratio * time), data = test)

summary(didreg)
library(arm)

coefplot(didreg)



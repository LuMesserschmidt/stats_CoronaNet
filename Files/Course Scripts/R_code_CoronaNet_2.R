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
# exploring and manipulating the data
# -------------------------------------------------
# https://r4ds.had.co.nz/transform.html
#https://garthtarr.github.io/meatR/dplyr.html

# the dplyr counterpart to the str() function is the 'glimpse' function
glimpse(coronaNet)

# dplyr counterpart to the 'which' function is the 'filter' function
filter(coronaNet, type == "Closure and Regulation of Schools")

# Class question: what if you want to find all observations that fits the condition above but only if the initiating country (country) is Germany?

# Class question: what if you want to find all observations that fits the condition above AND the type of policy (type) is 'External Border Restrictions'?


# Class question: what if you want to find all observations that fits the condition above that are not NA for the end of policy field (date_end)



# the dplyr counterpart to the '[ ]' command to subset by rows is the slice function
slice(coronaNet, 1:10)


# the dplyr counterpart to the 'sort' function is the arrange function

arrange(coronaNet, date_start)

arrange(coronaNet, desc(date_start))


# Class question: How could you use arrange() to sort all missing values to the date_end variable? (Hint: use is.na()).



# the dplyr counterpart to the $ operator or the [,variableName] operator is 'select'

select(coronaNet, entry_type)

select(coronaNet, entry_type, country)

select(coronaNet, entry_type:country)

select(coronaNet, -c(entry_type:country))

select(coronaNet, contains('type'))



# Class question: what are some ways you can select record_id, policy_id, target_direction, ISO_A3, and ISO_A2?


# the dplyr counterpart to making a new variable is the 'mutate' function

mutate(coronaNet, newVar1 = 1, 
       newVar2 = ifelse(entry_type == 'new_entry', 1, 0)) %>%
  select(newVar1, newVar2)

# if you only want to keep the new variables, use transmute()
transmute(coronaNet, newVar1 = 1, 
          newVar2 = ifelse(entry_type == 'new_entry', 1, 0))  


# -------------------------------------------------
# summarizing data
# -------------------------------------------------

coronaNet %>% group_by(country) %>%
  summarise(count = n(),
            numberOfTypesOfPolicies = n_distinct(type),
            mostActive = median(date_start))


coronaNet %>% group_by(country) %>%
  mutate(correct_type = ifelse(correct_type == 'original', 0, 1)) %>% 
  summarise(count = n(),
            numberOfTypesOfPolicies = n_distinct(type),
            mostActive = median(date_start),
            percCorrected = mean(correct_type))


coronaNet %>% group_by(country, date_start) %>%
  summarise(count = n(),
            numberOfTypesOfPolicies = n_distinct(type))



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


plot(dataAgg$gdpPPP, dataAgg$numBusinessRestrictions)
cor(dataAgg$gdpPPP, dataAgg$numBusinessRestrictions, use = 'complete.obs')


# Frequency Table
library(kableExtra)
coronaNet %>% 
  filter(init_country_level == "National", #Only choose the national level
         #entry_type=="New Entry", #Remove # When you only want to have new entries (no updates or corrections)
         country=="Germany") %>% # Only Germany
  group_by(type) %>% 
  summarize(`Total Number of Policies`=n(),
            `Number of Countries`=length(unique(country)),
            `Number of Targeted Countries`=length(unique(target_country)),
            `% With Mandatory Enforcement`=round(mean(grepl(x=compliance,pattern="Mandatory")*100,na.rm=T),0)) %>% 
  dplyr::select(Type="type",everything()) %>% 
  filter(!is.na(Type)) %>% 
  arrange(desc(`Total Number of Policies`))  %>% 
  ungroup() %>%
  knitr::kable("html",booktabs=T,
               caption="Policies by Germany on National level") %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position")) %>% 
  kableExtra::column_spec(1,width="4cm") %>% 
  kableExtra::column_spec(2:5,width="2.5cm")

coronaNet %>% 
  filter(init_country_level == "Yes, it is at the province/state level",
         #entry_type=="new_entry",
         country=="Germany") %>%
  group_by(type) %>% 
  summarize(`Total Number of Policies`=n(),
            `Number of Countries`=length(unique(country)),
            `Number of Targeted Countries`=length(unique(target_country)),
            `% With Mandatory Enforcement`=round(mean(grepl(x=compliance,pattern="Mandatory")*100,na.rm=T),0)) %>% 
  dplyr::select(Type="type",everything()) %>% 
  filter(!is.na(Type)) %>% 
  arrange(desc(`Total Number of Policies`))  %>% 
  ungroup() %>%
  knitr::kable("html",booktabs=T,
               caption="Policies by Germany on state level (Bundesländer)") %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position")) %>% 
  kableExtra::column_spec(1,width="4cm") %>% 
  kableExtra::column_spec(2:5,width="2.5cm")

# Density Plot
library(lubridate)

d<- coronaNet%>% 
  filter(!is.na(type)) %>% 
  group_by(type,date_announced) %>% 
  summarize(Policies=length(unique(record_id))) %>% 
  arrange(type,date_announced) %>% 
  mutate(Policies=cumsum(Policies)) %>% 
  ungroup %>% 
  mutate( type=recode(type,
                      
                      `External Border Restrictions`="External\nBorder\nRestrictions",
                      `Restriction of Non-Essential Businesses`="Restriction of\nNon-Essential\nBusinesses",
                      `Restrictions of Mass Gatherings`="Restrictions of\nMass Gatherings",
                      `Restriction of Non-Essential Government Services`="Restriction of\nNon-Essential\nGovernment Services",
                      `Internal Border Restrictions`="Internal\nBorder Restrictions",
                      `External Border Restrictions`="External\nBorder Restrictions",
                      `Public Awareness Campaigns`="Public\nAwareness Campaigns",
                      `New Task Force or Bureau`="New Task Force")) %>% 
  ggplot(aes(y=Policies,x=date_announced)) +
  geom_area() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  xlab("") +
  facet_wrap(~type)

d_all<-d + scale_x_date(date_labels = "%d/%m")
d_all

ggsave(filename="data/viz/density_sub.jpeg",
       plot=d_all,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)


# --------------------------------------
# T-tests
# --------------------------------------
# https://www.dummies.com/education/science/biology/how-to-use-student-t-tests-to-compare-averages/

# Doing a t-test in R
# https://uc-r.github.io/t_test

# Doing a t-test by hand
# http://onlinestatbook.com/2/tests_of_means/difference_means.html
# https://stattrek.com/hypothesis-test/difference-in-means.aspx

# Interpreting p-values
# https://blog.minitab.com/blog/adventures-in-statistics-2/how-to-correctly-interpret-p-values


# class question: what is the average gdpPPP


# lets see if there is a statistical difference in the number of business restrictions for countries over the average GDP PPP level 
# compared to countries under the average GDP PPP level
t.test(dataAgg %>% filter(gdpPPP > mean(dataAgg$gdpPPP, na.rm = TRUE)) %>% select(numBusinessRestrictions),
       dataAgg %>% filter(gdpPPP <= mean(dataAgg$gdpPPP, na.rm = TRUE))%>% select(numBusinessRestrictions))


dataAgg %>% filter(gdpPPP > mean(dataAgg$gdpPPP, na.rm = TRUE)) %>% summarise(avgNumBusinessRestrictions = mean(numBusinessRestrictions, na.rm = TRUE))  
dataAgg %>% filter(gdpPPP <= mean(dataAgg$gdpPPP, na.rm = TRUE)) %>% summarise(avgNumBusinessRestrictions = mean(numBusinessRestrictions, na.rm = TRUE))  


#------------ Creating a ggplot ------------------------

businessRestrictAgg = businessRestrict %>% 
  group_by(date_announced, country) %>% 
  summarise(numBusinessRestrictions = n())

dframe = expand.grid(unique(coronaNet$date_announced), unique(coronaNet$country))
names(dframe) = c('date_announced', 'country')

businessRestrictFull = merge(dframe, businessRestrictAgg, by = c('date_announced', 'country'), all.x = TRUE)
businessRestrictFull$numBusinessRestrictions = ifelse(is.na(businessRestrictFull$numBusinessRestrictions), 0, businessRestrictFull$numBusinessRestrictions)



# lets plot business restrictions over time
ggplot(data = businessRestrictFull) +
  geom_point(aes(x = date_announced, y = numBusinessRestrictions ))


# hmmm, how can we distinguish by country?
ggplot(data =businessRestrictFull) +
  geom_point(aes(x = date_announced, y = numBusinessRestrictions, color = country)) +
  theme(legend.position =  "none")


ggplot(data = businessRestrictFull) +
  geom_point(aes(x = date_announced, y = numBusinessRestrictions, shape = country)) +
  theme(legend.position =  "none")


ggplot(data = businessRestrictFull) +
  geom_point(aes(x = date_announced, y = numBusinessRestrictions, size = country)) +
  theme(legend.position =  "none")


ggplot(data = businessRestrictFull) +
  geom_point(aes(x = date_announced, y = numBusinessRestrictions, alpha = country)) +
  theme(legend.position =  "none")


# what happens if we try to connect the dots?
ggplot(data = businessRestrictFull) +
  geom_point(aes(x = date_announced, y = numBusinessRestrictions, color = country)) +
  geom_line(aes(x = date_announced, y = numBusinessRestrictions, color = country)) +
  theme(legend.position =  "none")

ggplot(data = businessRestrictFull %>% filter(country %in% c('China', 'Germany'))) +
  geom_line(aes(x = date_announced, y = numBusinessRestrictions, color = country)) +
  theme(legend.position =  "none")


# ------ note you can also set the asethetic porperties of your geom manually. If you wanted to make all of the points red then...

ggplot(data = businessRestrictFull) +
  geom_point(aes(x = date_announced, y = numBusinessRestrictions, color = 'red'))+
  theme(legend.position= 'none')


ggplot(data = businessRestrictFull) +
  geom_point(aes(x = date_announced, y = numBusinessRestrictions, size = 15) )


# ------ Facets
# If you want to look at each country individually, you can split your plot into facets
ggplot(data = businessRestrictFull%>% filter(country %in% c('China', 'Germany'))) +
  geom_point(aes(x = date_announced, y = numBusinessRestrictions))+
  facet_wrap(~country, nrow = 2)

# ------ Geom_smooth
# Lets plot the smoothed out data

ggplot(data = businessRestrictFull%>% filter(country %in% c('China', 'Germany'))) +
  geom_smooth(aes(x = date_announced, y = numBusinessRestrictions))



# We can layer different geoms on top of each other!

ggplot(data = businessRestrictFull%>% filter(country %in% c('China', 'Germany'))) +
  geom_point(aes(x = date_announced, y = numBusinessRestrictions, color = country))+
  geom_smooth(aes(x = date_announced, y = numBusinessRestrictions, linetype = country))


# Note geom_smooth is a predicted line, to plot the line that goes through the actual points, use geom_line
ggplot(data = businessRestrictFull%>% filter(country %in% c('China', 'Germany'))) +
  geom_line(aes(x = date_announced, y = numBusinessRestrictions, linetype = country)) + 
  geom_point(aes(x = date_announced, y = numBusinessRestrictions,color = country)) +
  geom_smooth(aes(x = date_announced, y = numBusinessRestrictions, linetype = country)) 

# ------ Density Plots

# how about plotting density plot?

dataAgg$gdpDum = ifelse(dataAgg$gdpPPP > mean(dataAgg$gdpPPP, na.rm = TRUE), "Above Average GDP", "Below Average GDP")
dataAgg = dataAgg %>% filter(!is.na(gdpDum))

ggplot(data = dataAgg, aes(x = numBusinessRestrictions, fill = gdpDum))+
  geom_density(alpha = .5)


# ----------- In order to make plots understandable to others, it would be helpful to have useful labels on the plots
#http://r4ds.had.co.nz/graphics-for-communication.html

p <-ggplot(data = dataAgg, aes(x = numBusinessRestrictions, fill = gdpDum))+
  geom_density(alpha = .5)+
  labs(title = "Are countries more or less likely to put restriction on businesses of they have above average vs. below average GDP",
       y = 'Density',
       x = 'Number of Policies Which Have Put Restrictions on Businesses')+
  theme_minimal()+
  theme(legend.position = 'bottom', 
        legend.title = element_blank())



# saving your plots

ggsave('viz/restrictionGDPDensityPlot.pdf', plot = p)


# ---------------------------------

# OLS
#-----------------------------------
# Here are some resources for learning more about OLS:
# http://setosa.io/ev/ordinary-least-squares-regression/
# http://r-statistics.co/Linear-Regression.html
# https://www.datacamp.com/community/tutorials/linear-regression-R
# https://www.albert.io/blog/key-assumptions-of-ols-econometrics-review/


# Simple Bivariate Model
model1 = lm(numBusinessRestrictions ~ gdpPPP  , data = dataAgg) 
summary(model1)

model2 = lm(numBusinessRestrictions ~ gdpPPP +gdpDum , data = dataAgg) 
summary(model2)



# -------- Making pretty tables with Stargazer----------------

stargazer::stargazer(model1,model2, type = "text")




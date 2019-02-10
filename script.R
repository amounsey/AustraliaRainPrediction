# Import Data ####
weather <- readr::read_csv("data/weather.csv")
comment(weather)<-c('Date-The date of observation',
                    'Location-The common name of the location of the weather station',
                    'MinTemp-The minimum temperature in degrees celsius',
                    'MaxTemp-The maximum temperature in degrees celsius',
                    'Rainfall-The amount of rainfall recorded for the day in mm',
                    'Evaporation-The so-called Class A pan evaporation (mm) in the 24 hours to 9am',
                    'Sunshine-The number of hours of bright sunshine in the day',
                    'WindGustDir-The direction of the strongest wind gust in the 24 hours to midnight',
                    'WindGustSpeed-The speed (km/h) of the strongest wind gust in the 24 hours to midnight',
                    'WindDir9am-Direction of the wind at 9am',
                    'WindDir3pm-Direction of the wind at 3pm',
                    'WindSpeed9am-Wind speed (km/hr) averaged over 10 minutes prior to 9am',
                    'WindSpeed3pm-Wind speed (km/hr) averaged over 10 minutes prior to 3pm',
                    'Humidity9am-Humidity (percent) at 9am',
                    'Humidity3pm-Humidity (percent) at 3pm',
                    'Pressure9am-Atmospheric pressure (hpa) reduced to mean sea level at 9am',
                    'Pressure3pm-Atmospheric pressure (hpa) reduced to mean sea level at 3pm',
                    'Cloud9am-Fraction of sky obscured by cloud at 9am. This is measured in \"oktas\", which are a unit of eigths. It records how many eigths of the sky are obscured by cloud. A 0 measure indicates completely clear sky whilst an 8 indicates that it is completely overcast',
                    'Cloud3pm-Fraction of sky obscured by cloud (in \"oktas\": eighths) at 3pm. See Cload9am for a description of the values',
                    'Temp9am-Temperature (degrees C) at 9am',
                    'Temp3pm-Temperature (degrees C) at 3pm',
                    'RainToday-Boolean: 1 if precipitation (mm) in the 24 hours to 9am exceeds 1mm, otherwise 0',
                    'RISK_MM-The amount of rain. A kind of measure of the \"risk\"',
                    'RainTomorrow-The target variable. Did it rain tomorrow?')

save.image('rda/datasets.rda')

# Required Libraries & Dataset ####
library(tidyverse)
library(summarytools)
load('rda/datasets.rda')

# Required Formulae ####
labelFinder<-function(x,df=weather){
  comment(df)[grep(deparse(substitute(x)),comment(df))]
}

# Data Cleaning ####
weather$Date<-as.Date(weather$Date,'%d/%m/%Y') # fix date
weather$Location<-as.factor(weather$Location)
summary(weather)
# class(weather$Location)
labelFinder(Sun)
labelFinder(Evap)
# Sunshine & Evaporation should be numeric
#class(weather)
weather<-weather%>%mutate_at(vars(Evaporation:Sunshine),funs(as.numeric(.)))
knitr::kable(table(weather$Location, useNA = 'ifany'))
table(is.na(weather$Date))

summary(weather)
#  Lots of NAs all over the place
cor(weatherSelect$Sunshine,weatherSelect$Evaporation,use =  "pairwise.complete.obs") # >[1] 0.3738033 

weather_WindDir_issu<-weather%>%filter((is.na(WindDir9am)&WindSpeed9am==0)|(is.na(WindDir3pm)&WindSpeed3pm==0))

knitr::kable(freq(weather_WindDir_issu$Location))
# WindDir can be NA if windspeed is 0 - WindDir issue. 
# There are about 9247 such cases  in weather dataframe
# Fixing this WindDir issue

weather<-as_tibble(rownames_to_column(weather,'Index')) # to row numbers to use later

WindDir9amIssue_index<-weather%>%filter(is.na(WindDir9am)&WindSpeed9am==0)%>%.$Index
WindDir9amIssue_index<-as.numeric(WindDir9amIssue_index)

WindDir3pmIssue_index<-weather%>%filter(is.na(WindDir3pm)&WindSpeed3pm==0)%>%.$Index
WindDir3pmIssue_index<-as.numeric(WindDir3pmIssue_index)

weather[WindDir9amIssue_index,"WindDir9am"]<-'NoDir' #WindDir9am issue fixed 
weather[WindDir3pmIssue_index,"WindDir3pm"]<-'NoDir' #WindDir3pm issue fixed

weather<-weather%>%select(-Index,-RISK_MM) #droping Index and droping Risk_MM as Advised (see README)

# Droping all cases(rows) containing NA values

weather<-weather%>%drop_na()
as.character(unique(weather$Location)) #  there are now only 26 unique Locations as opposed to 49 before
weather$Location<-as.character(weather$Location) #  to remove orginal factor levels  
weather$Location<-as.factor(weather$Location) # to Generate new factor levels
levels(weather$Location) #  confirmation check
  
summary(weather)

save.image('rda/datasets.rda')

# Import and Set Up Data ####
library(readr)
library(tidyverse)

weather <- read_csv("data/weather.csv")
save.image('rda/datasets.rda')
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

weather$Date<-as.Date(weather$Date,'%d/%m/%Y') # fix date
weather$Location<-as.factor(weather$Location)
knitr::kable(table(weather$Location, useNA = 'ifany'))
table(is.na(weather$Date))
varName<-names(weather)[3:24]

percentNotMissing<-weather%>%mutate_at(vars(varName),funs(ifelse(is.na(.),0,1)))
percentNotMissing_sum<-percentNotMissing%>%group_by(Location)%>%summarise_at(vars(varName),funs(sum(.)*100/n()))
str(percentNotMissing)

selectLocation<-percentNotMissing_sum%>%filter_at(vars(varName),all_vars(.>90))


test<-percentNotMissing[,3:24]
xtabs(MinTemp=~.,data =percentNotMissing[,3:24]) 

all(tab=test)

summary(weather)
#  Lots of NAs all over the place

knitr::kable(table(weather[is.na(weather$RainToday),"Location"]))
xtabs()
row

load('rda/datasets.rda')
comment(weather)[18]
grep('Cloud3',comment(weather))
str(weather)
tail(weather$Date,15)
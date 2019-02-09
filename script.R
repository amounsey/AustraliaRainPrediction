# Import and Set Up Data ####
library(readr)
weather <- read_csv("data/weather.csv")

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

labelFinder<-function(x,df=weather){
  comment(df)[grep(deparse(substitute(x)),comment(df))]
}


library(tidyverse)
library(summarytools)

weather$Date<-as.Date(weather$Date,'%d/%m/%Y') # fix date
weather$Location<-as.factor(weather$Location)
summary(weather)
labelFinder(Sun)
labelFinder(Evap)
# Sunshine & Evaporation should be numeric
#class(weather)
weather<-weather%>%mutate_at(vars(Evaporation:Sunshine),funs(as.numeric(.)))
knitr::kable(table(weather$Location, useNA = 'ifany'))
table(is.na(weather$Date))
varName<-names(weather)[3:24]

percentNotMissing<-weather%>%mutate_at(vars(varName),funs(ifelse(is.na(.),0,1)))
percentNotMissing_sum<-percentNotMissing%>%group_by(Location)%>%summarise_at(vars(varName),funs(sum(.)*100/n()))
str(percentNotMissing)
#varName[-c(4,5)]
selectLocation_dataSummary<-percentNotMissing_sum%>%filter_at(vars(varName[-c(4,5)]),all_vars(.>75))
selectLocation<-selectLocation_dataSummary%>%.$Location
# as.character(selectLocation)
# levels(selectLocation)



load('rda/datasets.rda')
weatherSelect<-weather%>%filter(Location%in%as.character(selectLocation))
# comment(weatherSelect) # comment carries over
cor(weatherSelect$Sunshine,weatherSelect$Evaporation,use =  "pairwise.complete.obs") # >[1] 0.3738033 

weatherSelect1<-as.data.frame(weatherSelect)%>%rowwise()%>%do(val=c(.$MinTemp,.$MaxTemp,.$Rainfall,.$Evaporation,
                                                                    .$Sunshine,.$WindGustDir,.$WindGustSpeed,.$WindDir9am,
                                                                    .$WindDir3pm,.$WindSpeed9am,.$WindSpeed3pm,.$Humidity9am,
                                                                    .$Humidity3pm,.$Pressure9am,.$Pressure3pm,.$Cloud9am,
                                                                    .$Cloud3pm,.$Temp9am,.$Temp3pm,.$RainToday,.$RISK_MM,.$RainTomorrow))

weatherSelect2<-weatherSelect1%>%summarise(NoNA=ifelse(sum(!is.na(val))==22,1,0),Missing=sum(is.na(val)))

weatherSelect<-cbind(weatherSelect,weatherSelect2)

knitr::kable(table(weatherSelect[weatherSelect$Missing==1,"Location"]))
knitr::kable(rel.freq(weatherSelect[weatherSelect$Missing==1,"Location"]))
knitr::kable(freq(
  weatherSelect[weatherSelect$Missing==1&
                  (is.na(weatherSelect$WindDir9am)&weatherSelect$WindSpeed9am==0|is.na(weatherSelect$WindDir3pm)&weatherSelect$WindSpeed3pm==0),
                "Location"]))
weatherSelect_WindDir_issu<-weatherSelect[weatherSelect$Missing==1&
                (is.na(weatherSelect$WindDir9am)&weatherSelect$WindSpeed9am==0|is.na(weatherSelect$WindDir3pm)&weatherSelect$WindSpeed3pm==0),]

knitr::kable(freq(weatherSelect_WindDir_issu$Location))
# WindDir can be NA if windspeed is 0. 
# There are about 1370 cases where there is only 1 missing & WindDir is the only missing in weatherSelect

sum(weatherSelect2$NoNA)

#weatherSelect1a<-as.data.frame(weatherSelect)%>%rowwise()%>%do(val=c(paste0('.$',varName)))
print(noquote(paste0('.$',varName)))
parse(varName)
rowwise()


labelFinder(Location)

as.character(substitute(e))
as.character(enquote(e))[-1]

as.character(expression(test))
xtabs(MinTemp=~.,data =percentNotMissing[,3:24]) 

all(tab=test)

summary(weather)
#  Lots of NAs all over the place

knitr::kable(table(weather[is.na(weather$RainToday),"Location"]))
xtabs()
row


comment(weather)[18]
grep('Cloud3',comment(weather))
str(weather)
tail(weather$Date,15)

save.image('rda/datasets.rda')
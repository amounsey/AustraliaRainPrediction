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
library(caret)
library(ggalt)
library(ggrepel)
load('rda/datasets.rda')

# Required Functions ####
labelFinder<-function(x,df=weather){
  comment(df)[grep(deparse(substitute(x)),comment(df))]
} # Search Comment Associated with dataframe for specific variable name (or part of variable name) 

numx<-function(df,x){
  x1<-as.matrix(df[,x])
  is.numeric(x1)
} # Simply indicates if x is a numeric column in the df dataframe (needed for function below)
num_col<-function(df,vars){
  sapply(vars,numx,df=df)
} # Produces a logical vector indicating weather the variable names listed in vars are numeric, 
  # where df is the dataframe containinig these variables

# Data Cleaning ####
weather$Date<-as.Date(weather$Date,'%d/%m/%Y') # fix date
weather$Location<-as.factor(weather$Location)
summary(weather)
# class(weather$Location)
# labelFinder(Sun)
# labelFinder(Evap)
# Sunshine & Evaporation should be numeric
# class(weather)
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

weather<-weather%>%drop_na() # Droping all cases(rows) containing NA values
str(weather)

weather<-weather%>%mutate(RainTomorrow=as.factor(RainTomorrow),
                          RainToday=as.factor(RainToday),WindGustDir=as.factor(WindGustDir),
                          WindDir9am=as.factor(WindDir9am),WindDir3pm=as.factor(WindDir3pm)) # convert chr variables into factors

# set.seed(34)
# sample(weather$RainTomorrow,30,replace = F)

weather$RainTomorrow<-ifelse(weather$RainTomorrow=='No',0,1) #  code Label 0-No and 1-Yes

knitr::kable(freq(weather$RainTomorrow)) #  check how much imbalance in the values of the label
#knitr::kable(freq(weather$RainToday))
unique(weather$Location) #  there are now only 26 unique Locations as opposed to 49 before
levels(weather$Location) #  Highlights the same problem
weather$Location<-as.character(weather$Location) #  to remove orginal factor levels  
weather$Location<-as.factor(weather$Location) # to Generate new factor levels
levels(weather$Location) #  confirmation check
  
summary(weather)
knitr::kable(freq(weather$Location))

# Creating Training and Test Datasets ####
set.seed(286)
trainIndex<-createDataPartition(y=weather$RainTomorrow,p=0.8,list = F)
trainSet<-weather[trainIndex,] #  Training dataset created
testSet<-weather[-trainIndex,] #  Testing dataset created

unique(trainSet$Location) # Confirming that all levels of Location are captured in the trainSet

# Data Visualization  ####
trainSet_dv<-trainSet # Create a training dataset for data visualisation the trainSet will be scaled and centered later
trainSet_dv$Month<-months(trainSet_dv$Date, abbreviate = T) #  extract month to get season variation
str(trainSet_dv$Month)
month_order<-unique(trainSet_dv$Month)
trainSet_dv$Month<-factor(trainSet_dv$Month, levels=month_order)
# Examining rain by locations
knitr::kable(trainSet_dv%>%group_by(Location)%>%
               summarise(mean_rainfall=mean(Rainfall),oneRainDayinDays=1/mean(RainTomorrow))%>%
               select(Location,mean_rainfall,oneRainDayinDays))

trainSet_dv%>%group_by(Location)%>%
  summarise(Annual_rainfall=365*mean(Rainfall),Number_rain_days=365*mean(RainTomorrow))%>%
  select(Location,Annual_rainfall,Number_rain_days)%>%
  ggplot(aes(x=Number_rain_days,y=Annual_rainfall,label=Location))+geom_point()+geom_text_repel()

trainSet_dv%>%group_by(Location,Month)%>%
  summarise(Daily_rainfall=mean(Rainfall),Number_rain_days=30.5*mean(RainTomorrow))%>%
  select(Location,Month,Daily_rainfall,Number_rain_days)%>%
  ggplot(aes(x=Number_rain_days,y=Daily_rainfall,label=abbreviate(Location,2)))+geom_point()+geom_text_repel()+facet_wrap(~Month)

trainSet_dv%>%group_by(Location,Month)%>%
  summarise(Daily_rainfall=mean(Rainfall),Number_rain_days=30.5*mean(RainTomorrow))%>%
  select(Location,Month,Daily_rainfall,Number_rain_days)%>%
  ggplot(aes(x=Month,y=Number_rain_days,size=Daily_rainfall))+geom_point()+facet_wrap(~Location)+
  labs(y='Mean Number of Rain Days',size='Mean Daily\nRainfall in mm')+
  theme_bw()+theme(axis.text.x = element_text(angle=90))

# Scaling numeric features
varNames<-names(trainSet)
# varNames
numInd<-num_col(testSet,varNames[-23]) #  logical vector indicating position of numeric and non-numeric variables (label excluded)
numVars<-varNames[numInd] # extracting the names of numeric features 
# numVars
preProcVals<-preProcess(trainSet[,numVars],method = c('center','scale'))
trainSet[,numVars]=predict(preProcVals,trainSet[,numVars])
testSet[,numVars]=predict(preProcVals,testSet[,numVars])

class(trainSet$RainTomorrow)

save.image('rda/datasets.rda')


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
library(repr)
library(glmnet)
library(ROCR)
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
  summarise(Daily_rainfall=mean(Rainfall),Number_rain_days=30.5*mean(ifelse(RainToday=='No',0,1)))%>%
  select(Location,Month,Daily_rainfall,Number_rain_days)%>%
  ggplot(aes(x=Month,y=Number_rain_days,size=Daily_rainfall))+geom_point()+facet_wrap(~Location)+
  labs(y='Mean Number of Rain Days',size='Mean Daily\nRainfall in mm')+
  theme_bw()+theme(axis.text.x = element_text(angle=90),plot.margin = margin(2,2,2,2))

summary(trainSet_dv)

cor.test(ifelse(trainSet$RainToday=='No',0,1),trainSet$RainTomorrow)
cor.test(trainSet$Humidity3pm,trainSet$RainTomorrow)

trainSet_dv%>%ggplot(aes(ifelse(RainTomorrow==0,'No','Yes'),Humidity3pm))+geom_boxplot()+
  labs(x='RainTomorrow')+theme_bw()

trainSet_dv%>%ggplot(aes(ifelse(RainTomorrow==0,'No','Yes'),Cloud3pm))+geom_boxplot()+
  labs(x='RainTomorrow')+theme_bw()

trainSet_dv%>%ggplot(aes(ifelse(RainTomorrow==0,'No','Yes'),Pressure3pm))+geom_boxplot()+
  labs(x='RainTomorrow')+theme_bw()

# Not So Well Separated
trainSet_dv%>%ggplot(aes(ifelse(RainTomorrow==0,'No','Yes'),WindSpeed9am))+geom_boxplot()+
  labs(x='RainTomorrow')+theme_bw()

trainSet_dv%>%ggplot(aes(ifelse(RainTomorrow==0,'No','Yes'),Temp9am))+geom_boxplot()+
  labs(x='RainTomorrow')+theme_bw()

numVars

plot_box<-function(x){print(trainSet_dv%>%ggplot(aes(x=ifelse(RainTomorrow==0,'No','Yes')))+geom_boxplot(aes_string(y=x))+
    labs(x='RainTomorrow')+theme_bw())}
# plot_box('Sunshine')
sapply(numVars,plot_box)

ksWrapper<-function(x){
z1<-as.matrix(trainSet[trainSet$RainTomorrow==0,x]) # will need to change to 'No' later
z2<-as.matrix(trainSet[trainSet$RainTomorrow==1,x]) # # will need to change to 'Yes' later
D<-ks.test(z1,z2)[1]
p_val<-ks.test(z1,z2)[2]
return(c(x,D,p_val))
}
# Code Already in report- start
ksWrapper("MinTemp")
ksWrapper("MinTemp")$statistic
x<-matrix(unlist(sapply(numVars,ksWrapper),use.names = F),ncol=3,byrow=T)
x<-as.data.frame(x)[,-3] #exclude pvalue
names(x)<-c('NumericVariable','D')
x$D<-round(as.numeric(as.character(x$D)),digits = 2)
knitr::kable(x)
# Code Already in report- end
st_options('round.digits', 1)
knitr::kable(ctable(trainSet_dv$Location,trainSet_dv$WindDir9am),digits = 1)

location<-levels(trainSet_dv$Location)

# To see conditional probabilites P(No_RainTomorrow/Dir) and P(Yes_RainTomorrow/Dir) for each local
# substitute $WindDir3pm for $WindDir9am if you want to see these conditional probabilities with the 9am wind direction.
for(l in location){
  print(l)
  x<-suppressMessages(ctable(ifelse(trainSet_dv[trainSet_dv$Location==l,]$RainTomorrow==0,'No','Yes'),
                             trainSet_dv[trainSet_dv$Location==l,]$WindDir3pm, prop = 'c')[2]) # the Crosstab as proportions the 2nd table in the list 
  print(knitr::kable(x,digits = 2))
    cat("_________________________________________________\n")
} 

for(l in location){
  print(l)
  x<-suppressMessages(ctable(ifelse(trainSet_dv[trainSet_dv$Location==l,]$RainTomorrow==0,'No','Yes'),
                             trainSet_dv[trainSet_dv$Location==l,]$WindGustDir, prop = 'c')[2]) # the Crosstab as proportions the 2nd table in the list 
  print(knitr::kable(x,digits = 2))
  cat("_________________________________________________\n")
} 

x1<-ifelse(trainSet_dv[trainSet_dv$Location=="Cobar",]$RainTomorrow==0,'No','Yes')
x2<- trainSet_dv[trainSet_dv$Location=="Cobar",]$WindGustDir
x<-ctable(x1,x2, prop = 'c')[2] # the Crosstab as proportions the 2nd table in the list 
kableExtra::kable_styling(knitr::kable(x,digits = 2,booktabs = TRUE,
                                       caption = 'Conditional Probabilities No Rain Tomorrow vs. Rain Tomorrow Given WindGust Direction (Location = Cobar) ',
                                       row.names = NA,col.names = NA),latex_options = "hold_position", full_width = T)

rainDir9am<-list()
for(l in location){
  x<-suppressMessages(ctable(ifelse(trainSet_dv[trainSet_dv$Location==l,]$RainTomorrow==0,'No','Yes'),
                             trainSet_dv[trainSet_dv$Location==l,]$WindDir9am)[2])
  x<-as.data.frame(x)
  x<-as.data.frame(t(x))
  x$dir<-row.names(x)
  dir<-strsplit(x$dir,'[.]')
  dir<-as.data.frame(dir)
  x$dir<-t(dir)[,2]
  x<-x[row.names(x)!='proportions.Total',]
  x$rainDir<-ifelse(x$Yes>=x$Total,1,0) # indicate directions the conditional probability P(Yes_RainTomorrow/Dir)>P(Yes_RainTomorrow)
  rainDir9am[[l]]<-x[x$rainDir==1,"dir"] # returns these directions to character vector bearing the location name in the rainDir9am list
}

rainDir3pm<-list()
for(l in location){
  x<-suppressMessages(ctable(ifelse(trainSet_dv[trainSet_dv$Location==l,]$RainTomorrow==0,'No','Yes'),
                             trainSet_dv[trainSet_dv$Location==l,]$WindDir3pm)[2])
  x<-as.data.frame(x)
  x<-as.data.frame(t(x))
  x$dir<-row.names(x)
  dir<-strsplit(x$dir,'[.]')
  dir<-as.data.frame(dir)
  x$dir<-t(dir)[,2]
  x<-x[row.names(x)!='proportions.Total',]
  x$rainDir<-ifelse(x$Yes>=x$Total,1,0) # indicate directions the conditional probability P(Yes_RainTomorrow/Dir)>P(Yes_RainTomorrow)
  rainDir3pm[[l]]<-x[x$rainDir==1,"dir"] # returns these directions to character vector bearing the location name in the rainDir3pm list
}

rainDirGust<-list()
for(l in location){
  x<-suppressMessages(ctable(ifelse(trainSet_dv[trainSet_dv$Location==l,]$RainTomorrow==0,'No','Yes'),
                             trainSet_dv[trainSet_dv$Location==l,]$WindGustDir)[2])
  x<-as.data.frame(x)
  x<-as.data.frame(t(x))
  x$dir<-row.names(x)
  dir<-strsplit(x$dir,'[.]')
  dir<-as.data.frame(dir)
  x$dir<-t(dir)[,2]
  x<-x[row.names(x)!='proportions.Total',]
  x$rainDir<-ifelse(x$Yes>=x$Total,1,0) # indicate directions where the conditional probability P(Yes_RainTomorrow/Dir)>P(Yes_RainTomorrow)
  rainDirGust[[l]]<-x[x$rainDir==1,"dir"] # returns these directions to character vector bearing the location name in the rainDir3pm list
}

# Creating Dummy Variables indicating whether wind is blowing from a direction 
# where the conditional probability P(Yes_RainTomorrow/Dir)>P(Yes_RainTomorrow)
trainSet_dv$rainDir9am<-NA
trainSet_dv$rainDir3pm<-NA
trainSet_dv$rainDirGust<-NA
for (l in location) {
 trainSet_dv[trainSet_dv$Location==l,"rainDir9am"]<-ifelse(trainSet_dv[trainSet_dv$Location==l,]$WindDir9am%in%rainDir9am[[l]],1,0)
 trainSet_dv[trainSet_dv$Location==l,"rainDir3pm"]<-ifelse(trainSet_dv[trainSet_dv$Location==l,]$WindDir3pm%in%rainDir3pm[[l]],1,0)
 trainSet_dv[trainSet_dv$Location==l,"rainDirGust"]<-ifelse(trainSet_dv[trainSet_dv$Location==l,]$WindGustDir%in%rainDirGust[[l]],1,0)
}

knitr::kable(ctable(trainSet_dv$rainDirGust,trainSet_dv$RainTomorrow,omit.headings = F))
# library(gridExtra)
# rowx<-c('rainDir=0','rainDir=1')
# x1<-as.data.frame(ctable(trainSet_dv$rainDirGust,trainSet_dv$RainTomorrow,omit.headings = F)[2],
#                   row.names = c('rainDirGust=0','rainDirGust=1','Total'))
# names(x1)<-c('Rain\nTomorrow=0','Rain\nTomorrow=1','Total')
# x2<-as.data.frame(ctable(trainSet_dv$rainDir9am,trainSet_dv$RainTomorrow,omit.headings = F)[2],
#                   row.names = c('rainDir9am=0','rainDir9am=1','Total'))
# names(x2)<-c('Rain\nTomorrow=0','Rain\nTomorrow=1','Total')
# x3<-as.data.frame(ctable(trainSet_dv$rainDir3pm,trainSet_dv$RainTomorrow,omit.headings = F)[2],
#                   row.names = c('rainDir3pm=0','rainDir3pm=1','Total'))
# names(x3)<-c('Rain\nTomorrow=0','Rain\nTomorrow=1','Total')

print(knitr::kable(list(x1,x2,x3),digits = 2))

knitr::kable(grid.arrange(tableGrob(x1),tableGrob(x2),tableGrob(x3), nrow=1),digits = 2)
# trainSet_dv[trainSet_dv$Location=='AliceSprings',"rainDir9am"]<-ifelse(
#   trainSet_dv[trainSet_dv$Location=='AliceSprings',]$WindDir9am%in%rainDir9am[['AliceSprings']],1,0)
  

rainDir9am[['AliceSprings']]
summary(trainSet_dv)
head(trainSet_dv[(trainSet_dv$Location=='AliceSprings')&(trainSet_dv$WindDir9am%in%rainDir9am[['AliceSprings']]),c("Location","WindDir9am","rainDir9am")])
head(trainSet_dv[(trainSet_dv$Location=='AliceSprings')&(!trainSet_dv$WindDir9am%in%rainDir9am[['AliceSprings']]),c("Location","WindDir9am","rainDir9am")])

x1<-as.data.frame(x[1])
x2<-as.data.frame(t(x1))
x2$dir<-row.names(x2)
dir<-strsplit(x2$dir,'[.]')
dir<-as.data.frame(dir)
x2$dir<-t(dir)[,2]
x2<-x2[row.names(x2)!='proportions.Total',]
x2$rainDir<-ifelse(x2$Yes>=x2$Total,1,0)
rainDir<-x2[x2$rainDir==1,"dir"]


# x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
# split x on the letter e
# xtest<-strsplit(x, "e")

labelFinder(sunsh)

check<-trainSet_dv%>%group_by(Location,Month)%>%
  summarise(rainDays=30.5*mean(RainTomorrow),sd_rainDays=sd(RainTomorrow))%>%
  mutate(label=paste(Location,Month, sep = '_'))%>%ungroup()%>%
  select(label,rainDays,sd_rainDays)
check<-as.data.frame(check)
row.names(check)<-check$label
check$label<-NULL
check1<-check
check<-scale(check)

d<-dist(check)
hc1<-hclust(d)
plot(hc1)
rect.hclust(hc1,k=4)
library(cluster)
library(factoextra)
hc<-agnes(check)
hc$ac
pltree(hc, main='Dendrogram of Location_Month')
fviz_nbclust(check,FUN=hcut,method = 'wss')
fviz_nbclust(check,FUN=hcut,method = 'silhouette')
gap_stat<-clusGap(check,FUN=hcut,nstart = 25, K.max=10, B=50)
fviz_gap_stat(gap_stat)

clust<-cutree(as.hclust(hc),k=4)
check1$clust<-clust
clustmat<-trainSet_dv%>%group_by(Location,Month)%>%
  summarise(rainDays=30.5*mean(RainTomorrow),sd_rainDays=sd(RainTomorrow))%>%
  mutate(label=paste(Location,Month, sep = '_'))%>%ungroup()%>%
  select(Location,Month)%>%mutate(cluster=clust)

spread_var<-c('Location','Month')
clustmat%>%spread(key=Month,value=cluster)

fviz_cluster(list(data=check,cluster=clust), geom = 'point',
             xlab = 'Rain Days (Standardized)',ylab = 'Variability in Rain Days (Standardized)',
             main = NULL,ggtheme = theme_bw())

# useful lines of code adapted from <https://uc-r.github.io/hc_clustering> -Begin
# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(check, method = x)$ac
}

map_dbl(m, ac)
# useful lines of code from <https://uc-r.github.io/hc_clustering> -End


# %>%
#   summarise(Number_rain_days=which.max(Number_rain_days),Month=Month[which.max(Number_rain_days)])%>%
#   ggplot(aes(x=Location,y=Number_rain_days,fill=Month))+geom_col()+
#   theme_bw()+theme(axis.text.x = element_text(angle=90))

which.max(check$Number_rain_days)

# Machine Learning ####
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
cluster4joining<-clustmat%>%mutate(locMon=paste(Location,Month, sep = '_'))%>%select(locMon,cluster)

trainSet_dv<-trainSet_dv%>%mutate(locMon=paste(Location,Month, sep = '_'))
trainSet_dv<-trainSet_dv%>%left_join(cluster4joining)
trainSet_dv$cluster<-as.factor(trainSet_dv$cluster)
trainSet_dv$rainDir3pm<-as.factor(trainSet_dv$rainDir3pm)
trainSet_dv$rainDir9am<-as.factor(trainSet_dv$rainDir9am)
trainSet_dv$rainDirGust<-as.factor(trainSet_dv$rainDirGust)

names(trainSet_dv)
summary(trainSet_dv)
summary(testSet)
#  Logistic Model
set.seed(2179)
logistic_mod<-glm(RainTomorrow~cluster+RainToday+MaxTemp+Rainfall+Evaporation+Sunshine+WindGustSpeed+Humidity9am+Humidity3pm+
                    Pressure9am+Pressure3pm+Cloud9am+Cloud3pm+Temp3pm, family = binomial,data = trainSet_dv)

summary(logistic_mod)

# Model Selection and Nested CV

# bringing label back to a factor
# trainSet_dv$RainTomorrow<-ifelse(trainSet_dv$RainTomorrow==1,'Yes','No')
# trainSet_dv$RainTomorrow<-factor(trainSet_dv$RainTomorrow,levels = c('Yes','No'))
# testSet$RainTomorrow<-ifelse(testSet$RainTomorrow==1,'Yes','No')
# testSet$RainTomorrow<-factor(testSet$RainTomorrow,levels = c('Yes','No'))

# Model Selection and Nested CV (ROC) ####

# Inner Loop for ROC 
weights<-ifelse(trainSet$RainTomorrow=='Yes',0.78,0.22) # to balance by reversing sample proportion

fitControl<-trainControl(method = 'cv',
                         number = 10,
                         classProbs = T,
                         summaryFunction = twoClassSummary)
set.seed(2376)
cv_mod_roc<-train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+RainToday+Rainfall+
                    Evaporation+Sunshine+WindGustSpeed+Humidity9am+Humidity3pm+Pressure9am+Pressure3pm+
                    Cloud9am+Cloud3pm+Temp3pm,
                  data = trainSet_dv,
                  method ='glmnet',
                  weights = weights,
                  metric='ROC',
                  trControl=fitControl)

cv_mod_roc
var_imp<-varImp(cv_mod_roc)
print(var_imp)

set.seed(2376)
cv_mod_roc<-train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+RainToday+
                    Sunshine+WindGustSpeed+Humidity3pm+Pressure9am+Pressure3pm+
                    Cloud9am+Cloud3pm,
                  data = trainSet_dv,
                  method ='glmnet',
                  weights = weights,
                  metric='ROC',
                  trControl=fitControl)

cv_mod_roc
#  Outer Loop (ROC) 
## Set the hyperparameter grid to the optimal values from the inside loop
paramGrid <- expand.grid(alpha = c(cv_mod_roc$bestTune$alpha),
                         lambda = c(cv_mod_roc$bestTune$lambda))

fitControl1 = trainControl(method = 'cv',
                          number = 10,
                          returnResamp="all",
                          savePredictions = TRUE,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

set.seed(4789)
cv_mod_outer<-train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+RainToday+Rainfall+
                      Evaporation+Sunshine+WindGustSpeed+Humidity9am+Humidity3pm+Pressure9am+Pressure3pm+
                      Cloud9am+Cloud3pm+Temp3pm,
                  data = trainSet_dv,
                  method ='glmnet',
                  weights = weights,
                  tuneGrid=paramGrid,
                  metric='ROC',
                  trControl=fitControl1)

cvReduced_outer<-train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+RainToday+
          Sunshine+WindGustSpeed+Humidity3pm+Pressure9am+Pressure3pm+
          Cloud9am+Cloud3pm,
        data = trainSet_dv,
        method ='glmnet',
        weights = weights,
        tuneGrid=paramGrid,
        metric='ROC',
        trControl=fitControl1)

print_metrics(cvReduced_outer)

print_metrics = function(mod){
  means = c(apply(mod$resample[,1:3], 2, mean), alpha = mod$resample[1,4], 
            lambda = mod$resample[1,5], Resample = 'Mean')
  stds = c(apply(mod$resample[,1:3], 2, sd), alpha = mod$resample[1,4], 
           lambda = mod$resample[1,5], Resample = 'STD')
  out = rbind(mod$resample, means, stds)
  out[,1:3] = lapply(out[,1:3], function(x) round(as.numeric(x), 3))
  out
}
print_metrics(cv_mod_outer)


knitr::kable(print_metrics(cv_mod_outer))
cv_mod_roc$results[,1:3]

# Model Selection and Nested CV (Recall) ####
# Inner Loop for Recall 
weights<-ifelse(trainSet$RainTomorrow=='Yes',0.78,0.22) # to balance by reversing sample proportion
fitControl<-trainControl(method = 'cv',
                         number = 10,
                         classProbs = T,
                         summaryFunction = prSummary)

set.seed(2376)
cv_mod_recall<-train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+MaxTemp+RainToday+Rainfall+
                       Evaporation+Sunshine+WindGustSpeed+Humidity9am+Humidity3pm+Pressure9am+Pressure3pm+
                       Cloud9am+Cloud3pm+Temp3pm,
                  data = trainSet_dv,
                  method ='glmnet',
                  weights = weights,
                  metric='Recall',
                  trControl=fitControl)

cv_mod_recall

#  Outer Loop (Recall)
## Set the hyperparameter grid to the optimal values from the inside loop
paramGrid <- expand.grid(alpha = c(cv_mod_recall$bestTune$alpha),
                         lambda = c(cv_mod_recall$bestTune$lambda))

fitControl = trainControl(method = 'cv',
                          number = 10,
                          returnResamp="all",
                          savePredictions = TRUE,
                          classProbs = TRUE,
                          summaryFunction = prSummary)

set.seed(4789)
cv_mod_outer<-train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+MaxTemp+RainToday+Rainfall+
                      Evaporation+Sunshine+WindGustSpeed+Humidity9am+Humidity3pm+Pressure9am+Pressure3pm+
                      Cloud9am+Cloud3pm+Temp3pm,
                    data = trainSet_dv,
                    method ='glmnet',
                    weights = weights,
                    tuneGrid=paramGrid,
                    metric='Recall',
                    trControl=fitControl)

print_metrics = function(mod){
  means = c(apply(mod$resample[,1:3], 2, mean), alpha = mod$resample[1,4], 
            lambda = mod$resample[1,5], Resample = 'Mean')
  stds = c(apply(mod$resample[,1:3], 2, sd), alpha = mod$resample[1,4], 
           lambda = mod$resample[1,5], Resample = 'STD')
  out = rbind(mod$resample, means, stds)
  out[,1:3] = lapply(out[,1:3], function(x) round(as.numeric(x), 3))
  out
}
print_metrics(cv_mod_outer)



# AdaBoost (Adaptive Boosting) ####
library(gbm)
library(e1071)
library(MLmetrics)

fitControl <- trainControl(method = "cv",
                           number = 5,
                           sampling = 'up',
                           returnResamp="all",
                           savePredictions = TRUE,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

set.seed(1234)
bb_fit_inside_tw <- train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+RainToday+Rainfall+
                            Evaporation+Sunshine+WindGustSpeed+Humidity9am+Humidity3pm+Pressure9am+Pressure3pm+
                            Cloud9am+Cloud3pm+Temp3pm, 
                          data = trainSet_dv,  
                          method = "gbm", # Gradient boosted tree model
                          trControl = fitControl, 
                          verbose = FALSE,
                          metric="ROC")
print(bb_fit_inside_tw)

var_imp = varImp(bb_fit_inside_tw)
print(var_imp)
plot(var_imp)

# Reduced model based on variable importance cutoff =2

set.seed(1234)
bb_fit_inside_tw <- train(RainTomorrow~rainDir9am+rainDir3pm+rainDirGust+Rainfall+
                            Sunshine+WindGustSpeed+Humidity3pm+Pressure9am+Pressure3pm+
                            Cloud3pm+Temp3pm, 
                          data = trainSet_dv,  
                          method = "gbm", # Gradient boosted tree model
                          trControl = fitControl, 
                          verbose = FALSE,
                          metric="ROC")
print(bb_fit_inside_tw)

## Set the hyperparameter grid to the optimal values from the inside loop
paramGrid <- expand.grid(n.trees = c(150), interaction.depth = c(3), shrinkage = c(0.1), n.minobsinnode = c(10))

set.seed(5678)
bb_fit_outside_tw <- train(RainTomorrow~rainDir9am+rainDir3pm+rainDirGust+Rainfall+
                             Sunshine+WindGustSpeed+Humidity3pm+Pressure9am+Pressure3pm+
                             Cloud3pm+Temp3pm, 
                           data = trainSet_dv, 
                           method = "gbm", # Gradient boosted tree model
                           trControl = fitControl, 
                           tuneGrid = paramGrid, 
                           verbose = FALSE,
                           metric="ROC")

print_metrics = function(mod){
  means = c(apply(mod$resample[,1:3], 2, mean), n.trees = mod$resample[1,4], interaction.depth = mod$resample[1,5], 
            shrinkage = mod$resample[1,6], n.minobsinnode = mod$resample[1,7], Resample = 'Mean')
  stds = c(apply(mod$resample[,1:3], 2, sd), n.trees = mod$resample[1,4], interaction.depth = mod$resample[1,5], 
           shrinkage = mod$resample[1,6], n.minobsinnode = mod$resample[1,7], Resample = 'STD')
  out = rbind(mod$resample, means, stds)
  out[,1:3] = lapply(out[,1:3], function(x) round(as.numeric(x), 3))
  out
}
print_metrics(bb_fit_outside_tw)

# save and other wrap up ####

save.image('rda/datasets.rda')


#***************************************************************************************#
# The Final Version                                                                     #
#                                                                                       #
#***************************************************************************************#

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

# save.image('rda/datasets.rda')
# Converting from png to pdf - easier for knitr to handle
pdf('figs/aussiemap.pdf')
aussie_map<-readPNG('figs/rain1mman.png')
grid::grid.raster(aussie_map)
dev.off()
rm(aussie_map)

# Required Libraries & Dataset ####
library(tidyverse)
library(summarytools)
library(caret)
library(gbm)
library(repr)
library(glmnet)
library(ROCR)
library(ggalt)
library(ggrepel)
library(cluster)
library(factoextra)
library(gbm)
library(pROC)
#library(e1071)
#library(MLmetrics)
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
summary(weather)
# Sunshine & Evaporation should be numeric, this adjustment is done below
weather<-weather%>%mutate_at(vars(Evaporation:Sunshine),funs(as.numeric(.)))

factorvars<-c("Location","WindGustDir","WindDir9am","WindDir3pm","RainToday","RainTomorrow") #variables to become factors
weather<-weather%>%mutate_at(vars(factorvars),funs(as.factor(.)))

# The levels in RainTomorrow are 'No', 'Yes'.
# As Caret by default recognises the first level as the +ve case, we reorder the levels 
weather$RainTomorrow<-factor(weather$RainTomorrow,levels = c('Yes','No'))

summary(weather)
# There are Lots of NAs all over the place
# Quick examination of these cases reveal a tendency for some stations Locations to consistently report NAs for some variables,
# in such cases imputation does seem advisable
# However the variables WindDir9am/3pm can legitimately take on an NA value in the absence of wind- that is WindSpeed9am/3pm = 0 respectively 

dim(weather%>%filter((is.na(WindDir9am)&WindSpeed9am==0)|(is.na(WindDir3pm)&WindSpeed3pm==0)))[1] # number legitimate NAs

# There are about 9247 such legitimate WindDir NAs (the WindDir issue)in weather dataframe
# Fixing this WindDir issue
weather<-as_tibble(rownames_to_column(weather,'Index')) # to row numbers to use later

# Returning WindDir9am and WindDir3pm to character vectors temporarily
weather<-weather%>%mutate_at(vars(WindDir9am,WindDir3pm),funs(as.character(.)))

WindDir9amIssue_index<-weather%>%filter(is.na(WindDir9am)&WindSpeed9am==0)%>%.$Index
WindDir9amIssue_index<-as.numeric(WindDir9amIssue_index)

WindDir3pmIssue_index<-weather%>%filter(is.na(WindDir3pm)&WindSpeed3pm==0)%>%.$Index
WindDir3pmIssue_index<-as.numeric(WindDir3pmIssue_index)

weather[WindDir9amIssue_index,"WindDir9am"]<-'NoDir' #WindDir9am issue fixed 
weather[WindDir3pmIssue_index,"WindDir3pm"]<-'NoDir' #WindDir3pm issue fixed

weather<-weather%>%select(-Index,-RISK_MM) #droping Index and droping Risk_MM as Advised (see README)

# WindDir Issue fixed, dropping NA values now.
weather<-weather%>%drop_na() # Droping all cases(rows) containing NA values

# Reinstating WindDir9am and WindDir3pm as factors
weather<-weather%>%mutate_at(vars(WindDir9am,WindDir3pm),funs(as.factor(.)))
summary(weather)
levels(weather$WindDir3pm)

unique(weather$Location) #  there are now only 26 unique Locations as opposed to 49 before
levels(weather$Location) #  Highlights the same problem
weather$Location<-as.character(weather$Location) #  to remove orginal factor levels  
weather$Location<-as.factor(weather$Location) # to Generate new factor levels
levels(weather$Location) #  confirmation check

rmlist<-c('factorvars',"WindDir3pmIssue_index","WindDir9amIssue_index",'rmlist')
rm(list = rmlist) # getting rid of unnecessary objects

# Creating Training and Test Datasets ####
set.seed(286)
trainIndex<-createDataPartition(y=weather$RainTomorrow,p=0.8,list = F)
trainSet<-weather[trainIndex,] #  Training dataset created
testSet<-weather[-trainIndex,] #  Testing dataset created

unique(trainSet$Location) # Confirming that all levels of Location are captured in the trainSet

# Data Visualization  ####
freq(trainSet$RainTomorrow) #  check how much imbalance in the values of the label  
freq(trainSet$Location)

#Doing all charts and Tables before scaling and centering trainSet and testSet

trainSet$Month<-months(trainSet$Date, abbreviate = T) #  extract month to get season variation
str(trainSet$Month) # Month is chr vector
month_order<-unique(trainSet$Month) # this will be used to generate factor levels when month is converted to a fct (thankfully no further reordering is required)
trainSet$Month<-factor(trainSet$Month, levels=month_order) #Month is now a Factor
# Examining rain by locations

# knitr::kable(trainSet%>%group_by(Location)%>%
#                summarise(mean_rainfall=mean(Rainfall),oneRainDayinDays=1/mean(ifelse(RainTomorrow=='No',0,1)))%>%
#                select(Location,mean_rainfall,oneRainDayinDays))

trainSet%>%group_by(Location,Month)%>%
  summarise(Daily_rainfall=mean(Rainfall),Number_rain_days=30.5*mean(ifelse(RainToday=='No',0,1)))%>%
  select(Location,Month,Daily_rainfall,Number_rain_days)%>%
  ggplot(aes(x=Month,y=Number_rain_days,size=Daily_rainfall))+geom_point()+facet_wrap(~Location)+
  labs(y='Mean Number of Rain Days',size='Mean Daily\nRainfall in mm')+
  theme_bw()+theme(axis.text.x = element_text(angle=90),plot.margin = margin(2,2,2,2))

# above figure would not show well in paper, so two contrasting rainfall patterns will be show see fig_seasonal

trainSet%>%group_by(Location,Month)%>%
  summarise(Daily_rainfall=mean(Rainfall),Number_rain_days=30.5*mean(ifelse(RainToday=='No',0,1)))%>%
  select(Location,Month,Daily_rainfall,Number_rain_days)%>%filter(Location=='Darwin'|Location=='Portland')%>%
  ggplot(aes(x=Month,y=Number_rain_days,size=Daily_rainfall))+geom_point()+facet_wrap(~Location)+
  labs(y='Mean Number of Rain Days',size='Mean Daily Rainfall in mm')+
  theme_bw()+theme(axis.text.x = element_text(angle=90),legend.position = 'bottom')
ggsave('figs/seasonal.pdf')

save.image('rda/datasets_wip.rda')
summary(trainSet)
summary(testSet)

summarytools::ctable(trainSet$RainToday,trainSet$RainTomorrow)  # RainToday seems to have some explanatory power on RainTomorrow

# Generating Boxplots to show separatedness for all numeric variables
plot_box<-function(x){print(trainSet%>%ggplot(aes(x=RainTomorrow))+geom_boxplot(aes_string(y=x))+
    labs(x='RainTomorrow')+theme_bw())}
numVars<-names(trainSet)[num_col(trainSet,names(trainSet))] # Generate character vector names of numeric variable in trainSet
sapply(numVars,plot_box)

ksWrapper<-function(x){
z1<-as.matrix(trainSet[trainSet$RainTomorrow=='No',x]) 
z2<-as.matrix(trainSet[trainSet$RainTomorrow=='Yes',x]) 
D<-ks.test(z1,z2)[1]
p_val<-ks.test(z1,z2)[2]
return(c(x,D,p_val))
}
# Generate D statistic in ks.test to get relative measures of separatedness
ksWrapper("MinTemp")
ksWrapper("MinTemp")$statistic
ks<-matrix(unlist(sapply(numVars,ksWrapper),use.names = F),ncol=3,byrow=T)
ks<-as.data.frame(ks)[,-3] #exclude pvalue, all p-values indicated statistically significant differences. 
#This however is not important. What we need is D to establish cut-off values
names(ks)<-c('NumericVariable','D')
ks$D<-round(as.numeric(as.character(ks$D)),digits = 2)

# Generating charts of examples of well and not well separated features
g1<-trainSet%>%ggplot(aes(RainTomorrow,Humidity3pm))+geom_boxplot()+
  labs(x='RainTomorrow')+theme_bw()

g2<-trainSet%>%ggplot(aes(RainTomorrow,Cloud3pm))+geom_boxplot()+
  labs(x='RainTomorrow')+theme_bw()

pdf('figs/wellsep.pdf',width = 7,height = 6)
gridExtra::grid.arrange(g1,g2,nrow=1)
dev.off()
rm(list = c('g1','g2'))

g1<-trainSet%>%ggplot(aes(RainTomorrow,WindSpeed9am))+geom_boxplot()+
  labs(x='RainTomorrow')+theme_bw()

g2<-trainSet%>%ggplot(aes(RainTomorrow,Temp9am))+geom_boxplot()+
  labs(x='RainTomorrow')+theme_bw()

pdf('figs/notwellsep.pdf',width = 7, height = 6)
gridExtra::grid.arrange(g1,g2, nrow=1)
dev.off()
rm(list = c('g1','g2'))

# Normalizing trainSet and testSet ####
preProcVals<-preProcess(trainSet[,numVars],method = c('center','scale'))
trainSet[,numVars]=predict(preProcVals,trainSet[,numVars])
testSet[,numVars]=predict(preProcVals,testSet[,numVars])

# Feature Engineering ####
location<-levels(trainSet$Location)

# To see conditional probabilites P(No_RainTomorrow/Dir) and P(Yes_RainTomorrow/Dir) for each local

for(l in location){
  print(l)
  suppressMessages(print(ctable(trainSet[trainSet$Location==l,]$RainTomorrow,
                             trainSet[trainSet$Location==l,]$WindDir3pm, prop = 'c')[2],digits = 2)) # the Crosstab as proportions the 2nd table in the list 
  #print.summary.table(x,digits = 2)
    cat("_________________________________________________\n")
} 

for(l in location){
  print(l)
  suppressMessages(print(ctable(trainSet[trainSet$Location==l,]$RainTomorrow,
                                trainSet[trainSet$Location==l,]$WindDir9am, prop = 'c')[2],digits = 2)) # the Crosstab as proportions the 2nd table in the list 
  #print.summary.table(x,digits = 2)
  cat("_________________________________________________\n")
}

for(l in location){
  print(l)
  suppressMessages(print(ctable(trainSet[trainSet$Location==l,]$RainTomorrow,
                                trainSet[trainSet$Location==l,]$WindGustDir, prop = 'c')[2],digits = 2)) # the Crosstab as proportions the 2nd table in the list 
  print(knitr::kable(x))
  cat("_________________________________________________\n")
} 

# Creating vectors indicating whether wind is blowing from a direction 
# where the conditional probability P(Yes_RainTomorrow/Dir)>P(Yes_RainTomorrow)
rainDir9am<-list()
for(l in location){
  x<-suppressMessages(ctable(trainSet[trainSet$Location==l,]$RainTomorrow,
                             trainSet[trainSet$Location==l,]$WindDir9am)[2]) # proportion is displayed for row (default)
  # cell values now represents P(Dir/RainTom = Y) or P(Dir/RainTom = N), putting this way makes coding easier.
  x<-as.data.frame(x)
  x<-as.data.frame(t(x))
  x$dir<-row.names(x)
  dir<-strsplit(x$dir,'[.]')
  dir<-as.data.frame(dir)
  x$dir<-t(dir)[,2]
  x<-x[row.names(x)!='proportions.Total',] # drops the proportions.Totals row
  x$rainDir<-ifelse(x$Yes>=x$Total,1,0) # indicate directions where the conditional probability P(Yes_RainTomorrow/Dir)>P(Yes_RainTomorrow),
                                        # see 'explanation.pdf' for more
  rainDir9am[[l]]<-x[x$rainDir==1,"dir"] # returns these directions to character vector bearing the location name in the rainDir9am list
}

rainDir3pm<-list()
for(l in location){
  x<-suppressMessages(ctable(trainSet[trainSet$Location==l,]$RainTomorrow,
                             trainSet[trainSet$Location==l,]$WindDir3pm)[2])
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
  x<-suppressMessages(ctable(trainSet[trainSet$Location==l,]$RainTomorrow,
                             trainSet[trainSet$Location==l,]$WindGustDir)[2])
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
trainSet$rainDir9am<-NA
trainSet$rainDir3pm<-NA
trainSet$rainDirGust<-NA
for (l in location) {
 trainSet[trainSet$Location==l,"rainDir9am"]<-ifelse(trainSet[trainSet$Location==l,]$WindDir9am%in%rainDir9am[[l]],'Yes','No')
 trainSet[trainSet$Location==l,"rainDir3pm"]<-ifelse(trainSet[trainSet$Location==l,]$WindDir3pm%in%rainDir3pm[[l]],'Yes','No')
 trainSet[trainSet$Location==l,"rainDirGust"]<-ifelse(trainSet[trainSet$Location==l,]$WindGustDir%in%rainDirGust[[l]],'Yes','No')
}

summary(trainSet)
trainSet<-trainSet%>%mutate_at(vars(rainDir9am:rainDirGust),funs(as.factor(.))) # converting rainDir9am:rainDirGust to factor variables
summary(trainSet)

rm(x)
save.image('rda/datasets_wip.rda')
# Probability of RainTomorrow given rainDir
ctable(trainSet$rainDir9am,trainSet$RainTomorrow,omit.headings = F)
ctable(trainSet$rainDir3pm,trainSet$RainTomorrow,omit.headings = F)
ctable(trainSet$rainDirGust,trainSet$RainTomorrow,omit.headings = F)
# Location_month clusters
check<-trainSet%>%group_by(Location,Month)%>%
  summarise(rainDays=30.5*mean(ifelse(RainToday=='No',0,1)),sd_rainDays=sd(ifelse(RainToday=='No',0,1)))%>%
  mutate(label=paste(Location,Month, sep = '_'))%>%ungroup()%>%
  select(label,rainDays,sd_rainDays)
check<-as.data.frame(check)
row.names(check)<-check$label
check$label<-NULL
check1<-check
check<-scale(check)

hc<-agnes(check)
hc$ac # agglomerative Coefficient of close to 1 - strong clustering structure
# pltree(hc, main='Dendrogram of Location_Month')
fviz_nbclust(check,FUN=hcut,method = 'wss')
fviz_nbclust(check,FUN=hcut,method = 'silhouette')
# gap_stat<-clusGap(check,FUN=hcut,nstart = 25, K.max=10, B=500)
# fviz_gap_stat(gap_stat)

clust<-cutree(as.hclust(hc),k=6)
check1$clust<-clust
clustmat<-trainSet%>%group_by(Location,Month)%>%
  summarise(rainDays=30.5*mean(ifelse(RainToday=='No',0,1)),sd_rainDays=sd(ifelse(RainToday=='No',0,1)))%>%
  mutate(label=paste(Location,Month, sep = '_'))%>%ungroup()%>%
  select(Location,Month)%>%mutate(cluster=clust)

spread_var<-c('Location','Month')
clustmat%>%spread(key=Month,value=cluster)

fviz_cluster(list(data=check,cluster=clust), geom = 'point',
             xlab = 'Rain Days (Standardized)',ylab = 'Variability in Rain Days (Standardized)',
             main = NULL,ggtheme = theme_bw())


cluster4joining<-clustmat%>%mutate(locMon=paste(Location,Month, sep = '_'))%>%select(locMon,cluster) 

trainSet<-trainSet%>%mutate(locMon=paste(Location,Month, sep = '_')) # variable locMon created to facilitate joining
trainSet<-trainSet%>%left_join(cluster4joining) # joining by locMon
trainSet$cluster<-as.factor(trainSet$cluster) # making cluster a factor variable

summary(trainSet)

# Machine Learning - Logistic Model ####
freq(trainSet$RainTomorrow) # establishing the weights to be used
# Model Selection and Nested CV (ROC) 

# Inner Loop for ROC 
weights<-ifelse(trainSet$RainTomorrow=='Yes',0.78,0.22) # to balance by reversing sample proportion

fitControl_logis<-trainControl(method = 'cv',
                         number = 10,
                         classProbs = T,
                         summaryFunction = twoClassSummary)
set.seed(2376)
logis_cv<-train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+RainToday+Rainfall+
                    Evaporation+Sunshine+WindGustSpeed+Humidity9am+Humidity3pm+Pressure9am+Pressure3pm+
                    Cloud9am+Cloud3pm+Temp3pm,
                  data = trainSet,
                  method ='glmnet',
                  weights = weights,
                  metric='ROC',
                  trControl=fitControl_logis)

logis_cv

var_imp_logistics<-varImp(logis_cv)
bestTune_logis_full<-logis_cv$bestTune
row.names(bestTune_logis_full)<-'Optimal'
print(var_imp_logistics)
plot(var_imp_logistics)

# using a varImp cut-off of 3  Temp3pm, Rainfall, Humidity9am & Evaporation are drop
# The ROC of the reduced model is compared to ROC of the optimal model from logis_cv 
set.seed(2376)
logis_cv_reduced<-train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+RainToday+
                          Sunshine+WindGustSpeed+Humidity3pm+Pressure9am+Pressure3pm+
                          Cloud9am+Cloud3pm,
                  data = trainSet,
                  method ='glmnet',
                  weights = weights,
                  metric='ROC',
                  trControl=fitControl_logis)

logis_cv_reduced
# Optimal model from logis_cv has similar hyper-parameters and model performance as logis_cv_reduced
# The reduced model specification is therefore favoured.
rm(logis_cv)

#  Outer Loop (ROC) 
## Set the hyperparameter grid to the optimal values from the inside loop
paramGrid_logistic <- expand.grid(alpha = logis_cv_reduced$bestTune$alpha,
                         lambda = logis_cv_reduced$bestTune$lambda)

fitControl_logis1 = trainControl(method = 'cv',
                          number = 10,
                          returnResamp="all",
                          savePredictions = TRUE,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

set.seed(4789)

logis_cv_outer<-train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+RainToday+
                        Sunshine+WindGustSpeed+Humidity3pm+Pressure9am+Pressure3pm+
                        Cloud9am+Cloud3pm,
        data = trainSet,
        method ='glmnet',
        weights = weights,
        tuneGrid=paramGrid_logistic,
        metric='ROC',
        trControl=fitControl_logis1)

# Function to append and print Mean and STD summary statistcs to resample output 
# This is not needed (wait until you have examined adaboost model before deleting)_start(1)
print_metrics_logistic = function(mod){
  means = c(apply(mod$resample[,1:3], 2, mean), alpha = mod$resample[1,4], 
            lambda = mod$resample[1,5], Resample = 'Mean')
  stds = c(apply(mod$resample[,1:3], 2, sd), alpha = mod$resample[1,4], 
           lambda = mod$resample[1,5], Resample = 'STD')
  out = rbind(mod$resample, means, stds)
  out[,1:3] = lapply(out[,1:3], function(x) round(as.numeric(x), 3))
  out
}

print_metrics_logistic(logis_cv_outer)
#_end(1)

plot.roc(logis_cv_outer$pred$obs,logis_cv_outer$pred$Yes, print.thres=T) # Plot expected ROC with threshold and Spec, Sens 

#knitr::kable(print_metrics(logis_cv_outer))
logis_cv_outer$results
rm(logis_cv_reduced)

# Machine Learning - bdt (Boosted Decision Trees) ####

fitControl_boost <- trainControl(method = "cv",
                           number = 5,
                           sampling = 'up',
                           returnResamp="all",
                           savePredictions = TRUE,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

set.seed(1234)
boost_cv<- train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+RainToday+Rainfall+
                            Evaporation+Sunshine+WindGustSpeed+Humidity9am+Humidity3pm+Pressure9am+Pressure3pm+
                            Cloud9am+Cloud3pm+Temp3pm, 
                          data = trainSet,  
                          method = "gbm", # Stochastic Gradient boosted tree model
                          trControl = fitControl_boost, 
                          verbose = FALSE,
                          metric="ROC")
boost_cv

var_imp_boost = varImp(boost_cv)
bestTune_boost_full<-boost_cv$bestTune
print(var_imp_boost)
plot(var_imp_boost)

row.names(bestTune_boost_full)<-"Optimal"
# # Reduced model based on variable importance cutoff =3
# 
# set.seed(1234)
# bost_cv_reduced <- train(RainTomorrow~cluster+rainDir3pm+rainDirGust+Rainfall+
#                                     Sunshine+WindGustSpeed+Humidity3pm+Pressure3pm+
#                                     Cloud3pm, 
#                           data = trainSet,  
#                           method = "gbm", # Stochastic Gradient boosted tree model
#                           trControl = fitControl_boost, 
#                           verbose = FALSE,
#                           metric="ROC")
# 
# bost_cv_reduced # ROC (0.8914) very similar to ROC for the boost_cv(0.8936)  
# rm(boost_cv) # Choose redcued model over full model

#skip reduce step as boosting is 
## Set the hyperparameter grid to the optimal values from the inside loop
paramGrid_boost <- expand.grid(n.trees = c(150), interaction.depth = c(3), shrinkage = c(0.1), n.minobsinnode = c(10))

set.seed(5678)
boost_cv_outer <- train(RainTomorrow~cluster+rainDir9am+rainDir3pm+rainDirGust+RainToday+Rainfall+
                          Evaporation+Sunshine+WindGustSpeed+Humidity9am+Humidity3pm+Pressure9am+Pressure3pm+
                          Cloud9am+Cloud3pm+Temp3pm,  
                           data = trainSet, 
                           method = "gbm", # Gradient boosted tree model
                           trControl = fitControl_boost, 
                           tuneGrid = paramGrid_boost, 
                           verbose = FALSE,
                           metric="ROC")

print_metrics_boost = function(mod){
  means = c(apply(mod$resample[,1:3], 2, mean), n.trees = mod$resample[1,4], interaction.depth = mod$resample[1,5], 
            shrinkage = mod$resample[1,6], n.minobsinnode = mod$resample[1,7], Resample = 'Mean')
  stds = c(apply(mod$resample[,1:3], 2, sd), n.trees = mod$resample[1,4], interaction.depth = mod$resample[1,5], 
           shrinkage = mod$resample[1,6], n.minobsinnode = mod$resample[1,7], Resample = 'STD')
  out = rbind(mod$resample, means, stds)
  out[,1:3] = lapply(out[,1:3], function(x) round(as.numeric(x), 3))
  out
}
print_metrics_boost(boost_cv_outer)

boostExpected<-boost_cv_outer$results
row.names(boostExpected)<-NULL

pdf('figs/rocPlot.pdf',width = 7,height = 6)
plot.roc(boost_cv_outer$pred$obs,boost_cv_outer$pred$Yes,lty=3,print.thres=T, col='blue',print.thres.pch=8,
         print.thres.adj=0, print.thres.cex=0.7, print.thres.col='blue')
plot.roc(logis_cv_outer$pred$obs,logis_cv_outer$pred$Yes,lty=2, print.thres=T,add=T, col='red',print.thres.pch=1,
         print.thres.adj=1, print.thres.cex=0.7, print.thres.col='red')
dev.off()

# Preparing testSet for prediction ####
summary(testSet)

#testSet1<-testSet # duplicating testset in case something goes wrong

testSet$Month<-months(testSet$Date, abbreviate = T) #  extract month to get season variation
# str(testSet$Month) # Month is chr vector
# month_order<-unique(trainSet$Month) # this will be used to generate factor levels when month is converted to a fct (thankfully no further reordering is required)
testSet$Month<-factor(testSet$Month, levels=month_order) #Month is now a Factor

# Creating Dummy Variables indicating whether wind is blowing from a direction 
# where the conditional probability P(Yes_RainTomorrow/Dir)>P(Yes_RainTomorrow)
testSet$rainDir9am<-NA
testSet$rainDir3pm<-NA
testSet$rainDirGust<-NA
for (l in location) {
  testSet[testSet$Location==l,"rainDir9am"]<-ifelse(testSet[testSet$Location==l,]$WindDir9am%in%rainDir9am[[l]],'Yes','No')
  testSet[testSet$Location==l,"rainDir3pm"]<-ifelse(testSet[testSet$Location==l,]$WindDir3pm%in%rainDir3pm[[l]],'Yes','No')
  testSet[testSet$Location==l,"rainDirGust"]<-ifelse(testSet[testSet$Location==l,]$WindGustDir%in%rainDirGust[[l]],'Yes','No')
}

# for (l in location) {
#   trainSet[trainSet$Location==l,"rainDir9am"]<-ifelse(trainSet[trainSet$Location==l,]$WindDir9am%in%rainDir9am[[l]],'Yes','No')
#   trainSet[trainSet$Location==l,"rainDir3pm"]<-ifelse(trainSet[trainSet$Location==l,]$WindDir3pm%in%rainDir3pm[[l]],'Yes','No')
#   trainSet[trainSet$Location==l,"rainDirGust"]<-ifelse(trainSet[trainSet$Location==l,]$WindGustDir%in%rainDirGust[[l]],'Yes','No')
# }

# testSet$rainDir9am<-as.factor(testSet$rainDir9am)
# testSet$rainDir3pm<-as.factor(testSet$rainDir3pm)
# testSet$rainDirGust<-as.factor(testSet$rainDirGust)

testSet<-testSet%>%mutate_at(vars(rainDir9am:rainDirGust),funs(as.factor(.))) # converting rainDir9am:rainDirGust to factor variables


testSet<-testSet%>%mutate(locMon=paste(Location,Month, sep = '_')) # variable locMon created to facilitate joining
testSet<-testSet%>%left_join(cluster4joining) # joining by locMon
testSet$cluster<-as.factor(testSet$cluster) # making cluster a factor variable
summary(testSet)
# Predicting & Final Evaluation  ####
testSet$predictLogis<-predict(logis_cv_outer,newdata = testSet)
testSet$predictBoost<-predict(boost_cv_outer,newdata = testSet)
confusionMatLogis<-confusionMatrix(testSet$predictLogis,testSet$RainTomorrow) # confusion matrix for logistic prediction
confusionMatLogis

confusionMatBoost<-confusionMatrix(testSet$predictBoost,testSet$RainTomorrow) # confusion matrix for adaboost prediction
confusionMatBoost

# testSet$predictHybrid<-ifelse(testSet$predictLogis=='Yes'&testSet$predictAdaboost=='Yes','Yes','No')
# testSet$predictHybrid<-factor(testSet$predictHybrid,levels = c('Yes','No'))
# levels(testSet$predictHybrid)
# confusionMatHybrid<-confusionMatrix(testSet$predictHybrid,testSet$RainTomorrow) # confusion matrix for Hybrid prediction
# 
# confusionMatHybrid

save.image('rda/datasets.rda')
#*************************************************Mar 02***********************************************

# Predicting - Adaboost Model ####

testSet1$prediction<-predict(bb_fit_outside_tw,newdata = testSet1)

confussionMat<-caret::confusionMatrix(testSet1$prediction,testSet1$RainTomorrow)
MLmetrics::Recall(testSet1$RainTomorrow,testSet1$prediction,positive = "Yes")
MLmetrics::Sensitivity(testSet1$RainTomorrow,testSet1$prediction)
MLmetrics::Specificity(testSet1$RainTomorrow,testSet1$prediction)
MLmetrics::Precision(testSet1$RainTomorrow,testSet1$prediction)
summary(testSet1)
levels(testSet1$Location)
unique(testSet1$Location)
# save and other wrap up ####

save.image('rda/datasets.rda')


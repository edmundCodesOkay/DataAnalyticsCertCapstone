################# DATA IMPORT, CLEANING & STAGING ##################
getwd()
setwd("c:/users/eddie.weekse/desktop/ckme 136")

#install and load appropriate packages
install.packages("readr")
install.packages("rattle")
install.packages("car")
install.packages("corrplot")
install.packages("RANN")
install.packages("GGally")

library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)
library(lubridate)
library(rpart)
library(rattle)
library(car)
library(caret)
library(corrplot)
library(rpart.plot)
library(RANN)
library(forecast)
library(GGally)

#read data into data frames
train<- read_csv("train.csv")
stores <- read_csv("stores.csv")
features <- read_csv("features.csv")

#summary of each table
train %>% summary() #notice that some sales are negative
#aggregate from dep/store level to total store level (this will remove all negative sales)
train.agg <- train %>% group_by(Store, Date) %>% summarise(Weekly_Sales2 = sum(Weekly_Sales, na.rm = TRUE))

#convert some variables to factor
train.agg$Store <- factor(train.agg$Store)
stores$Store <- factor(stores$Store)
stores$Type <- factor(stores$Type)
features$Store <- factor(features$Store)

#join Train, Store and Features together 
stores %>% str()
stores %>% summary()

features %>% str()
features %>% summary()

features$MDtot <- rowSums(features[,5:9], na.rm = TRUE)
features %>% summary()

train.agg <- left_join(train.agg,stores,by=c("Store"))
train.agg <- left_join(train.agg, features, by = c("Store", "Date"))
train.agg %>% head()

train.agg <- train.agg[,-c(8,9,10,11,12)] #remove superfluous columns

train.agg %>% head()
train.agg %>% summary()
train.agg %>% str()

################# EXPLORATORY ANALYSIS #############################
#feature engineering
train.agg$month <- as.numeric(substr(train.agg$Date, 6, 7)) #adds month
train.agg$week <- as.numeric(week(train.agg$Date))
train.agg$year <- train.agg$Date %>% substr(1,4) %>% as.numeric()#adds year

#decompose Type and IsHoliday into numeric variables
train.agg$TypeA <- 0
train.agg$TypeB <- 0
train.agg$TypeC <- 0
train.agg$HolidayInt <- as.numeric(train.agg$IsHoliday)

train.agg[which(train.agg$Type == 'A'),]$TypeA <- 1
train.agg[which(train.agg$Type == 'B'),]$TypeB <- 1
train.agg[which(train.agg$Type == 'C'),]$TypeC <- 1

#exploratory analysis and visualization of the data
  #data covers 142 weeks
  train.agg %>% summarize(min_date = min(Date), max_date = max(Date), total_weeks = difftime(min_date,max_date, unit = "weeks")) 
  
  #distribution by size and type
  #A stores generally have the largest size, with C stores being the smallest
  stores %>% ggplot(aes(x=Size)) + geom_histogram(binwidth = 15000) + facet_grid(Type~.) + ggtitle("Store Size by Store Type")
  
  #A stores generally tend to have more sales, and C stores do the least
  #there is negligible trend, but there definitey is seasonality, peaking at Christmas and Thanksgiving
  train.agg %>% group_by(Type, Date) %>% summarize(sales = sum(Weekly_Sales2)) %>% ggplot(aes(x = Date, y = sales, col = Type)) + geom_line() + scale_y_log10() + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "4 weeks") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Sales Over Time")

#assess relationship between various variables and sales
  #CPI and sales: as CPI increases, sales tend to decrease
  train.agg %>% ggplot(aes(x = CPI, y = Weekly_Sales2, col= Type)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales2~CPI, data = train.agg))), colour = "blue") + ggtitle("CPI vs Sales")
  
  #unemployment and sales:  as unemployment increases, sales tend to decrease 
  train.agg %>% ggplot(aes(x = Unemployment, y = Weekly_Sales2, col = Type)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales2~Unemployment, data = train.agg))), colour = "blue") + ggtitle("Unemployment vs Sales")
  
  #temperature and sales: as temperature increasis, sales tend to decrease
  train.agg %>% ggplot(aes(x = Temperature, y = Weekly_Sales2, col = Type)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales2~Temperature, data = train.agg))), colour = "blue") + ggtitle("Temperature vs Sales")
  
  #fuel price and sales: negligible relationship
  train.agg %>% ggplot(aes(x = Fuel_Price, y = Weekly_Sales2, col = Type)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales2~Fuel_Price, data = train))), colour = "blue") + ggtitle("Fuel Price vs Sales")
  
  #markdown and sales: results show positive trend (as md incres, sales increase)
  train.agg %>% filter(MDtot > 0 ) %>% ggplot(aes(x = MDtot, y = Weekly_Sales2, col = Type)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales2~MDtot, data = filter(train.agg, MDtot > 0)))), colour = "blue") + ggtitle("Markdown vs Sales")

#check box plot of sales to assess outliers 
  #filter first for non holidays, as there should be no cap on sales during holidays  
  train.agg %>% filter(IsHoliday == 0) %>% ggplot(aes(x = Type, y = Weekly_Sales2, col = Type)) + geom_boxplot() + ggtitle("Sales Boxplot")

  #there's a significant aount of outliers for B stores specifically. leave them for now

#assess Markdown data
  #markdowns seem to follow a negative trend throughout the year
  train.agg %>% filter(Store == '4', year > 2011) %>% ggplot(aes(x = Date, y = MDtot)) + geom_line() + geom_line(aes(y = fitted(lm(MDtot ~ Date, data = filter(train.agg, Store == '4', year > 2011)))), colour = "blue") + ggtitle("Markdown Time Series")

  #because sales is fairly similar year over year, assume markdowns will be similar
  #impute markdown data by repeating it from 2012
  train.agg %>% filter(Store == '4') %>% ggplot(aes(x = Date, y = MDtot)) + geom_line()
  
  train.agg.2012 <- train.agg %>% filter(Store == '4', year == 2012)
  train.agg.2011 <- train.agg %>% filter(Store == '4', year == 2011, MDtot != 0)
  
  maxweek2012 <- week(max(train.agg.2012$Date))
  minweek2012 <- week(min(train.agg.2012$Date))
  maxweek2011 <- week(max(train.agg.2011$Date))
  minweek2011 <- week(min(train.agg.2011$Date))
  minweek2010 <- week(min(train.agg$Date))
  
  #fill 2011 Markdown data
  for (i in 1:45) {

  datum2011 <- which(train.agg$year == 2011 & train.agg$Store == as.character(i) & train.agg$week >= minweek2012 & train.agg$week <= maxweek2012)
  datum2012 <- which(train.agg$year == 2012 & train.agg$Store == as.character(i))
  train.agg[datum2011,]$MDtot <- train.agg[datum2012,]$MDtot
  }
  
  #fill 2010 Mardown data
  for (i in 1:45) {
  datum2010 <- which(train.agg$year == 2010 & train.agg$Store == as.character(i) & train.agg$week >= minweek2010)
  datum2011v2 <- which(train.agg$year == 2011 & train.agg$Store == as.character(i) & train.agg$week >= minweek2010-1)
  train.agg[datum2010,]$MDtot <- train.agg[datum2011v2,]$MDtot
  }
  
  #plot again to ensure data has been filled
  train.agg %>% filter(Store == '4') %>% ggplot(aes(x = Date, y = MDtot)) + geom_line() + ggtitle("BackCast Markdowns")
  
  #test relationship between sales and md again --> strong positive relationship
  train.agg %>% ggplot(aes(x = MDtot, y = Weekly_Sales2, col = Type)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales2~MDtot, data = train.agg))), colour = "blue")
  
  #boxplot of markdowns
  #there seems to be significant outliers, but sales also had outliers so leave these in data set
  train.agg %>% filter(IsHoliday == 0) %>% ggplot(aes(x = Type, y = MDtot, col = Type)) + geom_boxplot()
  
################# PRE - MODELING #########################
#subset data to train and test (forecast 2012 as test using 2010 and 2011, h = 41)
train.agg.test <- subset(train.agg, year == 2012)
train.agg.train <- subset(train.agg, year != 2012)

a <- week(min(train.agg.train$Date))
b <- week(min(train.agg.test$Date))

#create a list of time series for each store
train.agg.bystore <- c()
for (i in 1:45) {
train.agg.bystore[[i]] <- train.agg %>% filter(Store == as.character(i)) %>% as.data.frame() %>% select(Weekly_Sales2) %>% ts(start = c(2010, a), frequency = 52)
}

#create a list of time series for test period for each store
train.agg.test.bystore <- c()
for (i in 1:45) {
  train.agg.test.bystore[[i]] <- train.agg.test %>% filter(Store == as.character(i)) %>% as.data.frame() %>% select(Weekly_Sales2) %>% ts(start = c(2012,b), frequency = 52)
}

################# SEASONAL NAIVE MODEL #########################

#fit seaosnal naive

  #create models for each store
    sn_ts_model_list <- c()
    for (i in 1:45) {
      #convert to ts 
      sn_ts <- train.agg.train %>% filter(Store == as.character(i)) %>% as.data.frame() %>% select(Weekly_Sales2) %>% ts(start = c(2010,a), frequency = 52)
      #fit model
      sn_ts_model <- snaive(sn_ts, h = 41)
      sn_ts_model_list[[i]] <- sn_ts_model
    }

#assess seasonal naive

  #plot one of the models for a given store 4
    sn_ts_model_list[[4]] %>% autoplot()

  #plot actuals with forecast --> forecast underestimates actuals and does not account for shifting holidays
    autoplot(train.agg.bystore[[4]]) + autolayer(sn_ts_model_list[[4]], PI = FALSE, series = "S Naive", col = "red") + ylab("Sales $") + xlab("Time") + ggtitle("Seasonal Naive vs Actual")

  #check residuals --> residuals are uncorrelated, but do not have mean zero. the forecast is biased
    checkresiduals(sn_ts_model_list[[4]])
  
  #find MPE (mean percentage error) of model for each store
    model_measures <- cbind(rep(0,45),rep(0,45),rep(0,45), rep(0,45))
    colnames(model_measures) <- c("SeasonalNaive", "NerualNetwork", "LinearRegression", "LinearRegression2")
    
    for (i in 1:45) {
      model_measures[i,1] <- accuracy(sn_ts_model_list[[i]], train.agg.test.bystore[[i]])[2,4]
    }

################# NEURAL NETWORK MODEL ########################
  
  #fit neural network model
    
    #create models for each store
      nn_ts_model_list <- c()
      
      for (i in 1:45) {
        #convert to ts 
        nn_ts <- train.agg.train %>% filter(Store == as.character(i)) %>% as.data.frame() %>% select(Weekly_Sales2) %>% ts(start = c(2010,a), frequency = 52)
        #fit model and perform all forecasts
        nn_ts_model <- forecast(nnetar(nn_ts), h = 41)
        nn_ts_model_list[[i]] <- nn_ts_model
      }
  
  #assess neural network
    
    #plot one of the models for a given store 4
      nn_ts_model_list[[4]] %>% autoplot()
    
    #plot actuals with forecast --> general shape seems to eb captured. spike in spring was not captured
      autoplot(train.agg.bystore[[4]]) + autolayer(nn_ts_model_list[[4]], PI = FALSE, series = "Neural Net", col = "red") + ylab("Sales $") + xlab("Time") + ggtitle("Neural Network vs Actual")
    
    #check residuals --> residuals are uncorrelated, and mean seems to be zero.
      checkresiduals(nn_ts_model_list[[4]])
    
    #find MPE (mean percentage error) of model for each store
      for (i in 1:45) {
        model_measures[i,2] <- accuracy(nn_ts_model_list[[i]], train.agg.test.bystore[[i]])[2,4]
      }


    
################# LINEAR REGRESSION MODEL #####################
    
  #fit linear model
      #create models for each store
        lm_ts_model_list <- c()
      
      for (i in 1:45){
          #convert to ts 
          lm_ts <- train.agg.train %>% filter(Store == as.character(i)) %>% as.data.frame() %>% select(Weekly_Sales2) %>% ts(start = c(2010,a), frequency = 52)
          #fit model and perform all forecasts
          lm_ts_model <- forecast(tslm(lm_ts ~ trend + season), PI = TRUE, h = 41)
          lm_ts_model_list[[i]] <- lm_ts_model
      }
  
  #assess linear model
      
    #plot one of the models for a given store 4
      lm_ts_model_list[[4]] %>% autoplot()
      
    #plot actuals with forecast --> general shape seems to eb captured. spike in spring was not captured
      autoplot(train.agg.bystore[[4]]) + autolayer(lm_ts_model_list[[4]], PI = FALSE, series = "Lin Reg", col = "red") + ylab("Sales $") + xlab("Time") + ggtitle("Time Series Linear Regression vs Actual")
      
    #check residuals --> residuals are fairly uncorrelated, however not commpletely. mean seems to be zero.
      checkresiduals(lm_ts_model_list[[4]])
      
    #find MPE (mean percentage error) of model for each store
      for (i in 1:45) {
        model_measures[i,3] <- accuracy(lm_ts_model_list[[i]], train.agg.test.bystore[[i]])[2,4]
      }    
    
      
################# LINEAR REGRESSION V2 MODEL #################

  #fit linear model
      #create models for each store
        lm2_ts_model_list <- c()
      
      for (i in 1:45){
        #convert to ts
        lm2_ts <- train.agg.train %>% filter(Store == as.character(i)) %>% as.data.frame() %>% select(Weekly_Sales2, Temperature, Fuel_Price, CPI, Unemployment, MDtot, month, HolidayInt) %>% ts(start = c(2010,a), frequency = 52)
        lm2_ts_xreg <- train.agg.test %>% filter(Store == as.character(i)) %>% as.data.frame() %>% select(Temperature, Fuel_Price, CPI, Unemployment, MDtot, month, HolidayInt)
        #fit models and perform all forecasts
        lm2_ts_model <- forecast(tslm(Weekly_Sales2 ~ trend + season + Temperature + Fuel_Price + CPI + Unemployment + MDtot + month + HolidayInt, data = lm2_ts), h = 41, newdata = lm2_ts_xreg)
        lm2_ts_model_list[[i]] <- lm2_ts_model
        
      }
  
  #assess linear model
    
    #plot one of the models for a given store 4
      lm2_ts_model_list[[4]] %>% autoplot()
      
    #plot actuals with forecast
      autoplot(train.agg.bystore[[4]]) + autolayer(lm2_ts_model_list[[4]], PI = FALSE, series = "Lin Reg 2", col = "red")+ ylab("Sales $") + xlab("Time") + ggtitle("Ex-post Linear Regression vs Actual")
      
    #check residuals --> residuals are autocorrelated still. mean zero. normally distributed
      checkresiduals(lm2_ts_model_list[[4]])
    
    #find MPE (mean percentage error) of model for each store
      for (i in 1:45) {
        model_measures[i,4] <- accuracy(lm2_ts_model_list[[i]], train.agg.test.bystore[[i]])[2,4]
      }

################# MODEL ASSESSMENT #########################

  #plot forecasts of all models on one plot 
    autoplot(train.agg.bystore[[4]]) + autolayer(sn_ts_model_list[[4]], PI = FALSE, series = "S Naive", col = "red") + autolayer(nn_ts_model_list[[4]], PI = FALSE, series = "Neural Net", col = "green") + autolayer(lm_ts_model_list[[4]], PI = FALSE, series = "Lin Reg", col = "blue") + autolayer(lm2_ts_model_list[[4]], PI = FALSE, series = "Lin Reg 2", col = "purple")

  #plot box plot of mean (absolute) percentage error 
    #Nerual Network model has lowest median, linear regression has smallest IQR  and second lowest median
    #linear regresion 2 has highest median, and largest range
    model_measures <- abs(model_measures)  
    opar <- par()
    par(mar = c(5,6,4,2))
    boxplot(model_measures, main = "Boxplots of MAPE by Model", xlab = "MAPE(%)", col = 'orange', border = 'brown', 
            horizontal = TRUE, cex.axis = 0.6, las = 2, pch = 19, frame = F)
    par(opar)
  
  #calculate mean and median of MAPE
    results_matrix <- matrix(rep(0,12), nrow = 3, ncol = 4)
    rownames(results_matrix) <- c("Mean", "Median", "Range")
    colnames(results_matrix) <- c("SN", "NN", "LN", "LN2")
    
    for (i in 1:4) {
      results_matrix[1,i] <- round(mean(model_measures[,i]),digits = 2)
      results_matrix[2,i] <- round(median(model_measures[,i]), digits = 2)
      results_matrix[3,i] <- round(max(model_measures[,i]) - min(model_measures[,i]),digits = 2)
    }
    
    print(results_matrix) #select best model as Neural Network Model
    
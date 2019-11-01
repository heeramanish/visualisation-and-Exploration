library(plyr)
library(readr)
library(ggplot2)
library(tidyr)

########combining 2013 file from jan till dec and saving them back to disk and importing it from mongodb######

data_2013 <- list.files("C:\\data\\FIT5141DatasetMelbournePedestrianTraffic\\2013",
                        pattern = "*.csv", full.names = TRUE)

data2013<-ldply(data_2013, read_csv)

########And we have 2013 data from jan to Dec, exporting it back#############################################
write.csv(data2013,"2013.csv",row.names = FALSE)


########combining 2014 file from jan till dec and saving them back to disk and importing it from mongodb######

data_2014 <- list.files("C:\\data\\FIT5141DatasetMelbournePedestrianTraffic\\2014",
                        pattern = "*.csv", full.names = TRUE)
data2014 <- ldply(data_2014, read_csv)

########And we have 2014 data from jan to Dec, exporting it back#############################################
write.csv(data2014,"2014.csv",row.names = FALSE)

########combining 2015 file of jan and  Feb and saving them back to disk and importing it from mongodb######

data_2015 <- list.files("C:\\data\\FIT5141DatasetMelbournePedestrianTraffic\\2015",
                        pattern = "*.csv", full.names = TRUE)
data2015 <- ldply(data_2015, read_csv)

########And we have 2014 data from jan to Dec, exporting it back#############################################
write.csv(data2015,"2015.csv",row.names = FALSE)

##########################Importing from mongodb#############################################################
#In order to make connection between the collection and the R, we need mongolite package####################
library(mongolite)

################################connecting to the mongoDB##################################################
##########################collection is having data from 2009 till 2015####################################
Collection2009 = mongo(collection = "MelPedCount2009_2015", db = "fit5141_Assign_3")
##colection2009 have 2009_to_2015 data of melpedestrians###########################################################
data2009_15=Collection2009$find(fields = '{"_id":0}')
View(data2009_15)

#########################Data Wrangling#########################################################################
library("dplyr")
#The goal of tidyr is to help you create tidy data. Tidy data is data where:
#Each variable is in a column.
#Each observation is a row.
#Each value is a cell.
install.packages("tidyr",dependencies = TRUE)
library("tidyr")
####need all the places under one column(tidyr is the function for that)########################################
data2009_2015<-gather(data = data2009_15,"Location", "PedCount",`State Library`:`Spring St-Lonsdale St (South)`
       ,factor_key = FALSE)
View(data2009_2015)
#############working with date column###########################################################################
library(lubridate)
data2009_2015$Date<-dmy(data2009_2015$Date)
arrange(data2009_2015,Date)
##############Extracting Years out of date and mappoing it to new column named Year#############################
data2009_2015$Year <- lubridate::year(data2009_2015$Date)
View(data2009_2015)

########################Time Series analysis####################################################################
###dropping first threee columsn, right now focusing on pedcount and year column################################ 
newData<-select(data2009_2015,-c(1,2))

y<-ts(data = newData, start = 2009,frequency = 12)
#################time plot######################################################################################
library(ggplot2)
#autoplot(y)+
#  title("Melbourne Pedestrain count time graph analysis")+
#  xlab("Year")+
#  ylab("PedCount")
gc()
ggplot(data = data2009_2015,aes(x = data2009_2015$Year,y = data2009_2015$PedCount))+
  geom_line(stat = 'identity')+
  theme_minimal()



#################fixing date for data2009_15######################
library(lubridate)
data2009_15$Date<-dmy(data2009_15$Date)
arrange(data2009_15,Date)
##############Extracting Years out of date and mappoing it to new column named Year#############################
data2009_15$Year <- lubridate::year(data2009_15$Date)
data2009_15$Month <- lubridate::month(data2009_15$Date, label=TRUE)
View(data2009_15)

# Summarize
require(dplyr)
#DF = ddply(data2009_15, c("Day", "group"), summarize, 
#            = mean(Worry_duration, na.rm = T))


###########ggplot for time series analysis for state library##########
#data2009_15$`State Library` = as.numeric(gsub(",", "", as.character(data2009_15$`State Library`), fixed = TRUE))

#subset getting oney the count and date column and subsetting into newset######
View(data2009_15)
newset<-data2009_15[,c("Bourke Street Mall (South)","Date")]
View(newset)

newset$Year <- lubridate::year(newset$Date)

####time series object ts_statelib########################################################################
ts_BourkeStreet <- ts(newset$`Bourke Street Mall (South)`, start = min(newset$Year),end = max(newset$Year),frequency = 365)


###data is not seasonal rather there is some sort of trend######
##################Autocorelation####################################
#Just as correlation measures the extent of a linear relationship between two variables, 
#autocorrelation measures the linear relationship between lagged values of a time series.
#The correlations associated with the lag plots form what is called the autocorrelation function (ACF)
library(ggplot2)

autoplot(ts_BourkeStreet)+
  ggtitle("Pedestrain count at Bourke Street Mall (South) A time plot") +
  xlab("Year") +
  ylab("Ped Count")
View(ts_statelib)


##seasonal plot for BS################################################
ggseasonplot(x = ts_BourkeStreet,col=rainbow(12), year.labels=TRUE)+
  ylab("Pedestrain Count") +
  ggtitle("Seasonal plot: Pedestrain count at Bourke street South")

###################forecasting for bourke street###################
ts_BourkeStreet %>% ets() %>% forecast() %>% autoplot()


auto.arima(ts_BourkeStreet,stepwise = FALSE,approximation = FALSE)
####################################################################################################
###################time series and forecasting for Melbourne central#####################################
#########################################################################################################
dataset_MelbCen<-data2009_15[,c("Melbourne Central","Date")]

library(lubridate)
dataset_MelbCen$Date<-dmy(dataset_MelbCen$Date)
arrange(dataset_MelbCen,Date)
dataset_MelbCen$Year <- lubridate::year(dataset_MelbCen$Date)

View(dataset_MelbCen)


################################time series object for melbourne data##########################################
ts_MelbCen <- ts(dataset_MelbCen$`Melbourne Central`, start = min(dataset_MelbCen$Year),end = max(dataset_MelbCen$Year),
                 frequency = 365)

autoplot(ts_MelbCen)+
  ggtitle("Pedestrain count at Melbourne Central A time plot") +
  xlab("Year") +
  ylab("Ped Count")

###################################################checking for seasnoality#################################
ggseasonplot(x = ts_MelbCen,col=rainbow(12), year.labels=TRUE)+
  ylab("Pedestrain Count") +
  ggtitle("Seasonal plot: Pedestrain count at Melbourne central")

####################forecasting for MElb Central using seasonal ARIMA#############################################

MelCentral <-auto.arima(ts_MelbCen,seasonal = TRUE)


ts_MelbCen%>%diff(lag=4)%>%ggtsdisplay()

ts_MelbCen %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

forecast(ts_MelbCen,h = 4)

beer <- window(ts_MelbCen, start=2009)

xx<-naive(ts_MelbCen,h=20)

###################################################################################################################

dataset_TownH<-data2009_15[,c("Town Hall (West)","Date")]
View(dataset_TownH) 

library(lubridate)
dataset_TownH$Year <- lubridate::year(dataset_TownH$Date)

DataTH<-dataset_TownH[,c("Town Hall (West)","Year")]
View(DataTH)

ts_TH <- ts(DataTH$`Town Hall (West)`, start = min(DataTH$Year),end = max(DataTH$Year),
                 frequency = 365)

autoplot(ts_TH)+
  ggtitle("Pedestrain count at Town HAll (West) A time plot") +
  xlab("Year") +
  ylab("Ped Count")

#########forecasting#######
fitTH <- auto.arima(ts_TH, seasonal=FALSE)


coef(fitTH)

predict(fitTH,ahead = 4, se.fit = T)

TownHallForcast<-forecast(object = fitTH,  h = 4)

plot(TownHallForcast)

cdc<-forecast(ts_TH,h=4)
autoplot(cdc)



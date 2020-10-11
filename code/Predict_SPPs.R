rm(list=ls())
gc()
options(dplyr.print_max = 1e9)
options(stringsAsFactors = FALSE)
memory.limit(100000)

######## Inputs  ########
setwd("C:/Users/Rachel/Documents/UT_Grad/EDP_LAW379M/EnviroDev_Policy/data/")

######## Read in Data  ########
training_data=readRDS("training_data.RDS")
testing_data=readRDS("testing_data.RDS")

######## Fit Model  ########
mymodel <- lm(Price ~ Year + category + Zone + NG_Price + GDP + Solar_PV_Cost + Onshore_Wind_Cost, 
               data = training_data)
print(summary(mymodel))

######## Predict SPPs for Scenarios ########
#Base Case
names(testing_data)[names(testing_data) == "GasPrice_Base"]="NG_Price"
names(testing_data)[names(testing_data) == "GDP_Base"]="GDP"
testing_data$PredPrice_Base<- predict(mymodel,testing_data,type="response")
names(testing_data)[names(testing_data) == "NG_Price"]="GasPrice_Base"
names(testing_data)[names(testing_data) == "GDP"]="GDP_Base"

#High Economic Growth
names(testing_data)[names(testing_data) == "GasPrice_High_EconGrowth"]="NG_Price"
names(testing_data)[names(testing_data) == "GDP_High_EconGrowth"]="GDP"
testing_data$PredPrice_High_EconGrowth<- predict(mymodel,testing_data,type="response")
names(testing_data)[names(testing_data) == "NG_Price"]="GasPrice_High_EconGrowth"
names(testing_data)[names(testing_data) == "GDP"]="GDP_High_EconGrowth"

######## Ouptut SPPs  ########

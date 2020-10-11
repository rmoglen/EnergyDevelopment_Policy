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

######## Predict SPPs for Scenarios  ########


######## Ouptut SPPs  ########

#Author: Rachel Moglen
#Date: 10/22/20
#Perform LCA calculations

library(xlsx)
library(readxl)
rm(list=ls())
gc()
options(dplyr.print_max = 1e9)
options(stringsAsFactors = FALSE)
memory.limit(100000)

######## Inputs  ########
setwd("C:/Users/Rachel/Documents/UT_Grad/EDP_LAW379M/EnergyDevelopment_Policy/data/")

######## Read in Data  ########
SPP_h=readRDS("SPP_h.rds")
GasPrice_h=read_excel("US_GasPrice_2001-2019_monthly.xlsx")

#Some formatting
SPP_h$"Delivery Date"=as.Date(SPP_h$"Delivery Date",format = "%m/%d/%Y")
SPP_h$month=as.numeric(format(SPP_h$`Delivery Date`, format="%m"))
SPP_h$Year=as.numeric(format(SPP_h$`Delivery Date`, format="%Y"))
colnames(SPP_h)[7]="Price"
colnames(SPP_h)[5]="Zone"

##Houston CCGTs
SPP_h_1=SPP_h[SPP_h$Zone=="LZ_HOUSTON",]  #location of CCGT

#Merge
data=merge(SPP_h_1,GasPrice_h)

#Calculate Marginal Costs
data$M_7FA05=data$NG_Price*(6800/1037) #GE 7GA.05 Marginal Cost in $/MWh
data$M_501J=data$NG_Price*(6400/1037)  #Mitsubishi 501J Marginal Cost in $/MWh
data$M_501GAC=data$NG_Price*(6200/1037) #Mitsubishi 501GAC Marginal Cost in $/MWh

#Calculate Capacity Factors
data$CF_7FA05=0
data$CF_501J=0
data$CF_501GAC=0
data$CF_7FA05[data$M_7FA05<data$Price]=1
data$CF_501J[data$M_501J<data$Price]=1
data$CF_501GAC[data$M_501GAC<data$Price]=1

print(paste0("Houston GE 7GA.05 Capacity Factor (2011-2019): ", sum(data$CF_7FA05)/nrow(data)))
print(paste0("Houston Mitsubishi 501J Capacity Factor (2011-2019): ", sum(data$CF_501J)/nrow(data)))
print(paste0("Houston Mitsubishi 501GAC Capacity Factor (2011-2019): ", sum(data$CF_501GAC)/nrow(data)))

##San Antonio CCGTs
SPP_h_1=SPP_h[SPP_h$Zone=="LZ_CPS",]  #location of CCGT

#Merge
data=merge(SPP_h_1,GasPrice_h)

#Calculate Marginal Costs
data$M_7FA04=data$NG_Price*(8840/1037) #GE 7GA.04 Marginal Cost in $/MWh

#Calculate Capacity Factors
data$CF_7FA04=0
data$CF_7FA04[data$M_7FA04<data$Price]=1

print(paste0("Houston GE 7GA.04 Capacity Factor (2011-2019): ", sum(data$CF_7FA04)/nrow(data)))


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
ramp1=6 #fudge factor for CF-Houston
ramp2=10 #fudge factor for CF- San Antonio

######## Read in Data  ########
SPP_h=readRDS("SPP_h.rds")
GasPrice_h=read_excel("US_GasPrice_2001-2019_monthly.xlsx")

#Some formatting
SPP_h$"Delivery Date"=as.Date(SPP_h$"Delivery Date",format = "%m/%d/%Y")
SPP_h$month=as.numeric(format(SPP_h$`Delivery Date`, format="%m"))
SPP_h$Year=as.numeric(format(SPP_h$`Delivery Date`, format="%Y"))
colnames(SPP_h)[7]="Price"
colnames(SPP_h)[5]="Zone"
colnames(SPP_h)[2]="Hour"
colnames(SPP_h)[1]="Date"

##Houston CCGTs
SPP_h_1=SPP_h[SPP_h$Zone=="LZ_HOUSTON",]  #location of CCGT
SPP_h_1=aggregate(Price~Hour+Date+month+Year, data=SPP_h_1, FUN=mean)

#Merge
data_1=merge(SPP_h_1,GasPrice_h)

#Calculate Marginal Costs
#data_1$M_7FA05=data_1$NG_Price*(6800/1037) #GE 7GA.05 Marginal Cost in $/MWh
#data_1$M_501J=data_1$NG_Price*(6400/1037)  #Mitsubishi 501J Marginal Cost in $/MWh
#data_1$M_501GAC=data_1$NG_Price*(6200/1037) #Mitsubishi 501GAC Marginal Cost in $/MWh

data_1$M_7FA05=data_1$NG_Price*(7480/1037) #GE 7GA.05 Marginal Cost in $/MWh
data_1$M_501J=data_1$NG_Price*(7040/1037)  #Mitsubishi 501J Marginal Cost in $/MWh
data_1$M_501GAC=data_1$NG_Price*(6820/1037) #Mitsubishi 501GAC Marginal Cost in $/MWh


#Calculate Capacity Factors
data_1$CF_7FA05=0
data_1$CF_501J=0
data_1$CF_501GAC=0
data_1$CF_7FA05[data_1$M_7FA05<(data_1$Price+ramp1)]=1
data_1$CF_501J[data_1$M_501J<(data_1$Price+ramp1)]=1
data_1$CF_501GAC[data_1$M_501GAC<(data_1$Price+ramp1)]=1

print(paste0("Houston GE 7GA.05 Capacity Factor (2011-2019): ", sum(data_1$CF_7FA05)/nrow(data_1)))
print(paste0("Houston Mitsubishi 501J Capacity Factor (2011-2019): ", sum(data_1$CF_501J)/nrow(data_1)))
print(paste0("Houston Mitsubishi 501GAC Capacity Factor (2011-2019): ", sum(data_1$CF_501GAC)/nrow(data_1)))

##San Antonio CCGTs
SPP_h_2=SPP_h[SPP_h$Zone=="LZ_CPS",]  #location of CCGT
SPP_h_2=aggregate(Price~Hour+Date+month+Year, data=SPP_h_2, FUN=mean)

#Merge
data_2=merge(SPP_h_2,GasPrice_h)

#Calculate Marginal Costs
data_2$M_7FA04=data_2$NG_Price*(8536/1037) #GE 7GA.04 Marginal Cost in $/MWh

#Calculate Capacity Factors
data_2$CF_7FA04=0
data_2$CF_7FA04[data_2$M_7FA04<(data_2$Price+ramp2)]=1

print(paste0("Houston GE 7GA.04 Capacity Factor (2011-2019): ", sum(data_2$CF_7FA04)/nrow(data_2)))


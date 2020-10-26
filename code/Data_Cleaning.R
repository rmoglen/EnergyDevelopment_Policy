library(readxl)
rm(list=ls())
gc()
options(dplyr.print_max = 1e9)
options(stringsAsFactors = FALSE)
memory.limit(100000)

######## Inputs  ########
setwd("C:/Users/Rachel/Documents/UT_Grad/EDP_LAW379M/EnergyDevelopment_Policy/data/")
build_SPP=FALSE
renewables_discount=0.9  #multiply predicted renewables capital costs by this discounting factor

######## Read in Data  ########
#SPP
if (build_SPP){
  sheets <- excel_sheets("SPP_2011.xlsx")
  SPP_h=read_excel("SPP_2011.xlsx", sheet = "Jan_1")
  for (sheet in sheets[-1]){
    SPP_h=rbind(SPP_h,read_excel("SPP_2011.xlsx", sheet = sheet))
  }
  
  files =list("SPP_2012.xlsx","SPP_2013.xlsx","SPP_2014.xlsx","SPP_2015.xlsx","SPP_2016.xlsx","SPP_2017.xlsx","SPP_2018.xlsx","SPP_2019.xlsx")
  for (file in files){
    print(file)
    sheets <- excel_sheets(file)
    for (sheet in sheets){
      SPP_h=rbind(SPP_h,read_excel(file, sheet = sheet))
    }
    saveRDS(SPP_h, file = "SPP_h.rds")
  }
} else{
  SPP_h=readRDS("SPP_h.rds")
}
SPP_h$"Delivery Date"=as.Date(SPP_h$"Delivery Date",format = "%m/%d/%Y")
SPP_h$month=as.numeric(format(SPP_h$`Delivery Date`, format="%m"))
SPP_h$Year=as.numeric(format(SPP_h$`Delivery Date`, format="%Y"))
SPP_h$season="non-summer"
SPP_h$season[SPP_h$month<11 & SPP_h$month>4]="summer"
SPP_h$interval="off-peak"
SPP_h$interval[SPP_h$season=="summer" & SPP_h$'Delivery Hour'>7 & SPP_h$'Delivery Hour'<24]="peak"
SPP_h$interval[SPP_h$season=="non-summer" & (SPP_h$'Delivery Hour'<2 | SPP_h$'Delivery Hour'>23)]="peak"
SPP_h$category=paste0(SPP_h$season," ",SPP_h$interval)
colnames(SPP_h)[7]="Price"
colnames(SPP_h)[5]="Zone"

#Natural Gas Prices
GasPrice_h=read_excel("US_GasPrice_1997-2019.xlsx")
GasPrice_f=read_excel("US_GasPrice_2020-2050.xlsx")

#Temperatures
TempAvg_f=read_excel("Texas_TempAvg_2020-2039.xlsx")  #in degrees C
TempMin_f=read_excel("Texas_TempMin_2020-2039.xlsx")  #in degrees C
TempMax_f=read_excel("Texas_TempMax_2020-2039.xlsx")  #in degrees C
TempAvg_f[,2:13]=(9/5)*TempAvg_f[,2:13]+32
TempMin_f[,2:13]=(9/5)*TempMin_f[,2:13]+32
TempMax_f[,2:13]=(9/5)*TempMax_f[,2:13]+32

Temp_h=read_excel("Houston_TempHourly_2011-2019.xlsx")

#Texas GDP
GDP_h=read_excel("Texas_GDP_1997-2019.xlsx")
GDP_f=read_excel("Texas_GDP_2020-2039.xlsx")

#Renewables Capital Costs
Renewables=read_excel("Renewables_CC_2011-2039.xlsx")

######## Build Testing and Training Sets  ########

#build testing and training sets
training_set=aggregate(Price~Year+category+Zone, data=SPP_h, FUN=median)
testing_set=read_excel("testing_set.xlsx")

#Gas Prices
training_set=merge(training_set,GasPrice_h)
testing_set=merge(testing_set,GasPrice_f[,c("Year","GasPrice_Base","GasPrice_High_EconGrowth")])

#Economic Growth
training_set=merge(training_set,GDP_h)
testing_set=merge(testing_set,GDP_f)

#Renewables Costs
training_set=merge(training_set,Renewables[,c("Year","Solar_PV_Cost","Onshore_Wind_Cost")])
testing_set=merge(testing_set,Renewables[,c("Year","Solar_PV_Cost","Onshore_Wind_Cost")])
names(testing_set)[names(testing_set) == "Solar_PV_Cost"]="Solar_PV_Cost_Base"
names(testing_set)[names(testing_set) == "Onshore_Wind_Cost"]="Onshore_Wind_Cost_Base"
testing_set$Solar_PV_Cost_CheaperRenewables=testing_set$Solar_PV_Cost_Base*renewables_discount
testing_set$Onshore_Wind_Cost_CheaperRenewables=testing_set$Onshore_Wind_Cost_Base*renewables_discount

#Temperatures
#coming back to temperatures cause it will take a while to format

saveRDS(training_set,"training_data.RDS")
saveRDS(testing_set,"testing_data.RDS")
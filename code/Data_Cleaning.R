library(readxl)
rm(list=ls())
gc()
options(dplyr.print_max = 1e9)
options(stringsAsFactors = FALSE)
memory.limit(100000)

######## Inputs  ########
setwd("C:/Users/Rachel/Documents/UT_Grad/EDP_LAW379M/EnviroDev_Policy/data/")
build_SPP=FALSE

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


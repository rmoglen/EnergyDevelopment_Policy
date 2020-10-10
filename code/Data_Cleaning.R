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

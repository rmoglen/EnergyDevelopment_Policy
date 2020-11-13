#Author: Rachel Moglen
#Date: 10/22/20
#Fit linear model to predict future LMPs based on predictor forecasts

library(xlsx)
rm(list=ls())
gc()
options(dplyr.print_max = 1e9)
options(stringsAsFactors = FALSE)
memory.limit(100000)

######## Inputs  ########
setwd("C:/Users/Rachel/Documents/UT_Grad/EDP_LAW379M/EnergyDevelopment_Policy/data/")
adjust=TRUE  #hybrid model= TRUE, OLS only= FALSE

######## Read in Data  ########
training_data=readRDS("training_data.RDS")
testing_data=readRDS("testing_data.RDS")
adjustments=read_excel("adjustments.xlsx")

######## Fit Model  ########
mymodel <- lm(Price ~ Year + category + Zone + NG_Price + GDP + Solar_PV_Cost + Onshore_Wind_Cost, 
               data = training_data)
#mymodel <- lm(Price ~ NG_Price, 
#              data = training_data)
print(summary(mymodel))

######## Predict SPPs for Scenarios ########
#Base Case
names(testing_data)[names(testing_data) == "GasPrice_Base"]="NG_Price"
names(testing_data)[names(testing_data) == "GDP_Base"]="GDP"
names(testing_data)[names(testing_data) == "Solar_PV_Cost_Base"]="Solar_PV_Cost"
names(testing_data)[names(testing_data) == "Onshore_Wind_Cost_Base"]="Onshore_Wind_Cost"
testing_data$PredPrice_Base<- predict(mymodel,testing_data,type="response")
names(testing_data)[names(testing_data) == "NG_Price"]="GasPrice_Base"
names(testing_data)[names(testing_data) == "GDP"]="GDP_Base"
names(testing_data)[names(testing_data) == "Solar_PV_Cost"]="Solar_PV_Cost_Base"
names(testing_data)[names(testing_data) == "Onshore_Wind_Cost"]="Onshore_Wind_Cost_Base"

#High Economic Growth
names(testing_data)[names(testing_data) == "GasPrice_High_EconGrowth"]="NG_Price"
names(testing_data)[names(testing_data) == "GDP_High_EconGrowth"]="GDP"
names(testing_data)[names(testing_data) == "Solar_PV_Cost_Base"]="Solar_PV_Cost"
names(testing_data)[names(testing_data) == "Onshore_Wind_Cost_Base"]="Onshore_Wind_Cost"
testing_data$PredPrice_High_EconGrowth<- predict(mymodel,testing_data,type="response")
names(testing_data)[names(testing_data) == "NG_Price"]="GasPrice_High_EconGrowth"
names(testing_data)[names(testing_data) == "GDP"]="GDP_High_EconGrowth"
names(testing_data)[names(testing_data) == "Solar_PV_Cost"]="Solar_PV_Cost_Base"
names(testing_data)[names(testing_data) == "Onshore_Wind_Cost"]="Onshore_Wind_Cost_Base"

#Cheap Renewables Case
names(testing_data)[names(testing_data) == "GasPrice_Base"]="NG_Price"
names(testing_data)[names(testing_data) == "GDP_Base"]="GDP"
names(testing_data)[names(testing_data) == "Solar_PV_Cost_CheaperRenewables"]="Solar_PV_Cost"
names(testing_data)[names(testing_data) == "Onshore_Wind_Cost_CheaperRenewables"]="Onshore_Wind_Cost"
testing_data$PredPrice_CheaperRenewables<- predict(mymodel,testing_data,type="response")
names(testing_data)[names(testing_data) == "NG_Price"]="GasPrice_Base"
names(testing_data)[names(testing_data) == "GDP"]="GDP_Base"
names(testing_data)[names(testing_data) == "Solar_PV_Cost"]="Solar_PV_Cost_CheaperRenewables"
names(testing_data)[names(testing_data) == "Onshore_Wind_Cost"]="Onshore_Wind_Cost_CheaperRenewables"

#Adjustments
testing_data=merge(testing_data,adjustments)
if (adjust){
  
  #Generation mix
  testing_data$PredPrice_CheaperRenewables=testing_data$PredPrice_CheaperRenewables*testing_data$Gen_Mix
  testing_data$PredPrice_High_EconGrowth=testing_data$PredPrice_High_EconGrowth*testing_data$Gen_Mix
  testing_data$PredPrice_Base=testing_data$PredPrice_Base*testing_data$Gen_Mix
  
  #Transmission- renewables and high growth
  testing_data$PredPrice_Base=testing_data$PredPrice_Base*testing_data$Transmission
  testing_data$PredPrice_CheaperRenewables=testing_data$PredPrice_CheaperRenewables*testing_data$Transmission*0.9
  testing_data$PredPrice_High_EconGrowth=testing_data$PredPrice_High_EconGrowth*testing_data$Transmission*1.1
  }

######## Ouptut SPPs  ########


saveRDS(testing_data,"Predicted_SPPs.rds")

#Oh my god so ugly. But I'm tired and want to go to bed
testing_data$PredPrice_Base[testing_data$category=="summer peak"]=testing_data$PredPrice_Base[testing_data$category=="summer peak"]*(1/6) 
testing_data$PredPrice_Base[testing_data$category=="summer off-peak"]=testing_data$PredPrice_Base[testing_data$category=="summer off-peak"]*(1/12)
testing_data$PredPrice_Base[testing_data$category=="non-summer off-peak"]=testing_data$PredPrice_Base[testing_data$category=="non-summer off-peak"]*(11/16)  
testing_data$PredPrice_Base[testing_data$category=="non-summer peak"]=testing_data$PredPrice_Base[testing_data$category=="non-summer peak"]*(1/16)  

testing_data$PredPrice_High_EconGrowth[testing_data$category=="summer peak"]=testing_data$PredPrice_High_EconGrowth[testing_data$category=="summer peak"]*(1/6) 
testing_data$PredPrice_High_EconGrowth[testing_data$category=="summer off-peak"]=testing_data$PredPrice_High_EconGrowth[testing_data$category=="summer off-peak"]*(1/12)
testing_data$PredPrice_High_EconGrowth[testing_data$category=="non-summer off-peak"]=testing_data$PredPrice_High_EconGrowth[testing_data$category=="non-summer off-peak"]*(11/16)  
testing_data$PredPrice_High_EconGrowth[testing_data$category=="non-summer peak"]=testing_data$PredPrice_High_EconGrowth[testing_data$category=="non-summer peak"]*(1/16)  

testing_data$PredPrice_CheaperRenewables[testing_data$category=="summer peak"]=testing_data$PredPrice_CheaperRenewables[testing_data$category=="summer peak"]*(1/6) 
testing_data$PredPrice_CheaperRenewables[testing_data$category=="summer off-peak"]=testing_data$PredPrice_CheaperRenewables[testing_data$category=="summer off-peak"]*(1/12)
testing_data$PredPrice_CheaperRenewables[testing_data$category=="non-summer off-peak"]=testing_data$PredPrice_CheaperRenewables[testing_data$category=="non-summer off-peak"]*(11/16)  
testing_data$PredPrice_CheaperRenewables[testing_data$category=="non-summer peak"]=testing_data$PredPrice_CheaperRenewables[testing_data$category=="non-summer peak"]*(1/16)  

testing_data1=aggregate(PredPrice_Base~Year+Zone, data=testing_data, FUN=sum)
testing_data2=aggregate(PredPrice_High_EconGrowth~Year+Zone, data=testing_data, FUN=sum)
testing_data3=aggregate(PredPrice_CheaperRenewables~Year+Zone, data=testing_data, FUN=sum)

write.xlsx(testing_data1[,c("PredPrice_Base","Year","Zone")], file="Predicted_SPPs.xlsx", sheetName="S2_Med", row.names=FALSE)
write.xlsx(testing_data2[,c("PredPrice_High_EconGrowth","Year","Zone")], file="Predicted_SPPs.xlsx", sheetName="S2_High", append=TRUE, row.names=FALSE)
write.xlsx(testing_data3[,c("PredPrice_CheaperRenewables","Year","Zone")], file="Predicted_SPPs.xlsx", sheetName="S3_Low", append=TRUE, row.names=FALSE)

#carbon tax
testing_data3=merge(testing_data3,adjustments)
testing_data2=merge(testing_data2,adjustments)
testing_data1=merge(testing_data1,adjustments)

testing_data3$PredPrice_CheaperRenewables=testing_data3$PredPrice_CheaperRenewables*testing_data3$C_tax
testing_data2$PredPrice_High_EconGrowth=testing_data2$PredPrice_High_EconGrowth*testing_data2$C_tax
testing_data1$PredPrice_Base=testing_data1$PredPrice_Base*testing_data1$C_tax

saveRDS(testing_data1,"Predicted_SPPs_C_tax_med.rds")
saveRDS(testing_data2,"Predicted_SPPs_C_tax_high.rds")
saveRDS(testing_data3,"Predicted_SPPs_C_tax_low.rds")


write.xlsx(testing_data1[,c("PredPrice_Base","Year","Zone")], file="Predicted_SPPs.xlsx", sheetName="S1_Med_Ctax", append=TRUE, row.names=FALSE)
write.xlsx(testing_data2[,c("PredPrice_High_EconGrowth","Year","Zone")], file="Predicted_SPPs.xlsx", sheetName="S2_High_Ctax", append=TRUE, row.names=FALSE)
write.xlsx(testing_data3[,c("PredPrice_CheaperRenewables","Year","Zone")], file="Predicted_SPPs.xlsx", sheetName="S3_Low_Ctax", append=TRUE, row.names=FALSE)

#saveRDS(testing_data,"Predicted_SPPs_NG_only.rds")

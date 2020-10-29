#Author: Rachel Moglen
#Date: 10/22/20
#Graphing and Basis Risk calculation

library(xlsx)
library(readxl)
library(ggplot2)
rm(list=ls())
gc()
options(dplyr.print_max = 1e9)
options(stringsAsFactors = FALSE)
memory.limit(100000)

######## Inputs  ########
setwd("C:/Users/Rachel/Documents/UT_Grad/EDP_LAW379M/EnergyDevelopment_Policy/data/")

compute_basis_risk=FALSE
graphing_regression=TRUE
LZ_graphing="LZ_HOUSTON"


######## Basis Risk  ########
if (compute_basis_risk){
  #Some formatting
  SPP_h=readRDS("SPP_h.rds")
  SPP_h$"Delivery Date"=as.Date(SPP_h$"Delivery Date",format = "%m/%d/%Y")
  SPP_h=SPP_h[,c(1,2,3,5,7)]
  colnames(SPP_h)[5]="LZ_AEN"
  colnames(SPP_h)[4]="Zone"
  colnames(SPP_h)[2]="Hour"
  
  basis_risk=SPP_h[SPP_h$Zone=="LZ_AEN",]
  colnames(SPP_h)[5]="LZ_WEST"
  basis_risk=merge(basis_risk,SPP_h[SPP_h$Zone=="LZ_WEST",],by=c("Delivery Date", "Hour", "Delivery Interval")) #I know there's gotta be a better way but whatever
  basis_risk=basis_risk[,c(1,2,3,5,7)]
  basis_risk$price_diff=basis_risk$LZ_AEN - basis_risk$LZ_WEST
  basis_risk$month=as.numeric(format(basis_risk$`Delivery Date`, format="%m"))
  
  print(summary(basis_risk$price_diff))
  
  #Basis Risk by Hour of the Day
  diff=aggregate(price_diff~Hour, data=basis_risk, FUN=mean)
  diff$Metric="Mean"
  diff=rbind(diff, data.frame(aggregate(price_diff~Hour, data=basis_risk, FUN=quantile, probs=0.05), Metric="5th Percentile"))
  diff=rbind(diff, data.frame(aggregate(price_diff~Hour, data=basis_risk, FUN=quantile, probs=0.95), Metric="95th Percentile"))
  
  p1=ggplot(data=diff, aes(x=Hour, y=price_diff, group=Metric))+
    geom_line(aes(linetype=Metric)) +
    xlab("Hour of the Day") +
    ylab("Basis Risk (LZ_AEN-LZ_West) ($/MWh)") +
    scale_x_continuous(breaks = seq(0, 24, by = 2))+
    theme_minimal()
  
  print(p1)
  
  #Basis Risk by Month of the Year
  diff=aggregate(price_diff~month, data=basis_risk, FUN=mean)
  diff$Metric="Mean"
  diff=rbind(diff, data.frame(aggregate(price_diff~month, data=basis_risk, FUN=quantile, probs=0.05), Metric="5th Percentile"))
  diff=rbind(diff, data.frame(aggregate(price_diff~month, data=basis_risk, FUN=quantile, probs=0.95), Metric="95th Percentile"))
  
  p2=ggplot(data=diff, aes(x=month, y=price_diff, group=Metric))+
    geom_line(aes(linetype=Metric)) +
    xlab("Month of the Year") +
    ylab("Basis Risk (LZ_AEN-LZ_West) ($/MWh)") +
    scale_x_continuous(breaks = seq(1, 12, by = 1))+
    theme_minimal()
  
  print(p2)
}

######## Explore Predictive Model ########

if (graphing_regression){
  predicted_SPPs=readRDS("predicted_SPPs.RDS")
  historic_SPPs=readRDS("training_data.RDS")
  
  #Formatting
  predicted_SPPs=predicted_SPPs[predicted_SPPs$Zone==LZ_graphing,]
  historic_SPPs=historic_SPPs[historic_SPPs$Zone==LZ_graphing,]
  SPPs=data.frame(historic_SPPs[,c("Year","category","Price")],Scenario="Historic")
  
  #Base Case
  temp=data.frame(predicted_SPPs[,c("Year","category","PredPrice_Base")],Scenario="Base")
  colnames(temp)[3]="Price"
  SPPs=rbind(SPPs,temp)
  
  #High Economic Growth
  temp=data.frame(predicted_SPPs[,c("Year","category","PredPrice_High_EconGrowth")],Scenario="High GDP Growth")
  colnames(temp)[3]="Price"
  SPPs=rbind(SPPs,temp)
  
  #CheaperRenewables
  temp=data.frame(predicted_SPPs[,c("Year","category","PredPrice_CheaperRenewables")],Scenario="Low Cost Renewables")
  colnames(temp)[3]="Price"
  SPPs=rbind(SPPs,temp)
  
  #Weight LMPs 
  SPPs$Price[SPPs$category=="summer peak"]=SPPs$Price[SPPs$category=="summer peak"]*(1/6) 
  SPPs$Price[SPPs$category=="summer off-peak"]=SPPs$Price[SPPs$category=="summer off-peak"]*(1/12)
  SPPs$Price[SPPs$category=="non-summer off-peak"]=SPPs$Price[SPPs$category=="non-summer off-peak"]*(11/16)  
  SPPs$Price[SPPs$category=="non-summer peak"]=SPPs$Price[SPPs$category=="non-summer peak"]*(1/16)  
  SPPs=aggregate(Price~Year+Scenario, data=SPPs, FUN=sum)
  
  p3=ggplot(data=SPPs, aes(x=Year, y=Price, group=Scenario))+
    geom_line(aes(linetype=Scenario)) +
    xlab("Year") +
    ylab("Average LMP ($/MWh)") +
    scale_linetype_manual(values=c("twodash", "dashed","solid","dotted"))+
    ggtitle(paste0("Average LMPs in ",LZ_graphing))+
    ylim(0,30)+
    theme_minimal()
  
  print(p3)
  
}


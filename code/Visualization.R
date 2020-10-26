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

SPP_h=readRDS("SPP_h.rds")
compute_basis_risk=TRUE


######## Basis Risk  ########
if (compute_basis_risk){
  #Some formatting
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
  diff=rbind(diff, data.frame(aggregate(price_diff~Hour, data=basis_risk, FUN=quantile, probs=0.1), Metric="10th Percentile"))
  diff=rbind(diff, data.frame(aggregate(price_diff~Hour, data=basis_risk, FUN=quantile, probs=0.9), Metric="90th Percentile"))
  
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
  diff=rbind(diff, data.frame(aggregate(price_diff~month, data=basis_risk, FUN=quantile, probs=0.1), Metric="10th Percentile"))
  diff=rbind(diff, data.frame(aggregate(price_diff~month, data=basis_risk, FUN=quantile, probs=0.9), Metric="90th Percentile"))
  
  p2=ggplot(data=diff, aes(x=month, y=price_diff, group=Metric))+
    geom_line(aes(linetype=Metric)) +
    xlab("Month of the Year") +
    ylab("Basis Risk (LZ_AEN-LZ_West) ($/MWh)") +
    scale_x_continuous(breaks = seq(1, 12, by = 1))+
    theme_minimal()
  
  print(p2)
}

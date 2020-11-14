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

######## Functions  ########
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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
  basis_risk$Year=as.numeric(format(basis_risk$`Delivery Date`, format="%Y"))
  
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
  
  #Basis Risk over time
  diff=aggregate(price_diff~Year, data=basis_risk, FUN=mean)
  diff$Metric="Mean"
  diff=rbind(diff, data.frame(aggregate(price_diff~Year, data=basis_risk, FUN=quantile, probs=0.05), Metric="5th Percentile"))
  diff=rbind(diff, data.frame(aggregate(price_diff~Year, data=basis_risk, FUN=quantile, probs=0.95), Metric="95th Percentile"))
  
  p12=ggplot(data=diff, aes(x=Year, y=price_diff, group=Metric))+
    geom_line(aes(linetype=Metric)) +
    xlab("Year") +
    ylab("Basis Risk (LZ_AEN-LZ_West) ($/MWh)") +
    scale_x_continuous(breaks = seq(2011, 2019, by = 1))+
    theme_minimal()
  
  print(p12)
}

######## Explore Predictive Model ########

if (graphing_regression){
### Overall LMPs over time, historic and predicted ###
  predicted_SPPs=readRDS("predicted_SPPs.RDS")
  historic_SPPs=readRDS("training_data.RDS")
  
  #Formatting
  predicted_SPPs=predicted_SPPs[predicted_SPPs$Zone==LZ_graphing,]
  historic_SPPs=historic_SPPs[historic_SPPs$Zone==LZ_graphing,]
  SPPs=data.frame(historic_SPPs[,c("Year","category","Price")],Scenario="Historic")
  
  #Medium
  temp=data.frame(predicted_SPPs[,c("Year","category","PredPrice_Base")],Scenario="Medium")
  colnames(temp)[3]="Price"
  SPPs=rbind(SPPs,temp)
  
  #High
  temp=data.frame(predicted_SPPs[,c("Year","category","PredPrice_High_EconGrowth")],Scenario="High")
  colnames(temp)[3]="Price"
  SPPs=rbind(SPPs,temp)
  
  #Low
  temp=data.frame(predicted_SPPs[,c("Year","category","PredPrice_CheaperRenewables")],Scenario="Low")
  colnames(temp)[3]="Price"
  SPPs=rbind(SPPs,temp)
  
  #Medium C-tax
  temp=data.frame(predicted_SPPs[,c("Year","category","PredPrice_Base_Ctax")],Scenario="Medium C-tax")
  colnames(temp)[3]="Price"
  SPPs=rbind(SPPs,temp)
  
  #High C-tax
  temp=data.frame(predicted_SPPs[,c("Year","category","PredPrice_High_EconGrowth_Ctax")],Scenario="High C-tax")
  colnames(temp)[3]="Price"
  SPPs=rbind(SPPs,temp)
  
  #Low C-tax
  temp=data.frame(predicted_SPPs[,c("Year","category","PredPrice_CheaperRenewables_Ctax")],Scenario="Low C-tax")
  colnames(temp)[3]="Price"
  SPPs=rbind(SPPs,temp)
  
  #Weight LMPs 
  SPPs$Price[SPPs$category=="summer peak"]=SPPs$Price[SPPs$category=="summer peak"]*(1/6) 
  SPPs$Price[SPPs$category=="summer off-peak"]=SPPs$Price[SPPs$category=="summer off-peak"]*(1/12)
  SPPs$Price[SPPs$category=="non-summer"]=SPPs$Price[SPPs$category=="non-summer"]*(0.75)  
  SPPs=aggregate(Price~Year+Scenario, data=SPPs, FUN=sum)
  
  p3=ggplot(data=SPPs, aes(x=Year, y=Price, group=Scenario, color=Scenario))+
    geom_line(aes(linetype=Scenario),lwd=1.2) +
    xlab("Year") +
    ylab("Average LMP ($/MWh)") +
    scale_linetype_manual(values=c("dashed", "dashed","solid","dotted","dotted","twodash","twodash"))+
    #scale_color_manual(values = c("gray40", "gray70", "cadetblue2","cadetblue2","cadetblue2","cadetblue2","cadetblue2")) +
    ggtitle(paste0("Average LMPs in ",LZ_graphing))+
    ylim(0,60)+
    theme_minimal()
  
  
  print(p3)
  
### Regression explination 1: Natural Gas Prices vs. LMP ###
  
  #LMPs
  SPP_h=readRDS("SPP_h.rds")
  colnames(SPP_h)[7]="Price"
  colnames(SPP_h)[5]="Zone"
  SPP_h=SPP_h[SPP_h$Zone==LZ_graphing,]
  SPP_h$"Delivery Date"=as.Date(SPP_h$"Delivery Date",format = "%m/%d/%Y")
  SPP_h$month=as.numeric(format(SPP_h$`Delivery Date`, format="%m"))
  SPP_h$Year=as.numeric(format(SPP_h$`Delivery Date`, format="%Y"))
  SPP_h=aggregate(Price~Year+month, data=SPP_h, FUN=median)
  
  #Gas Price
  GasPrice_h=read_excel("US_GasPrice_2001-2019_monthly.xlsx")
  SPP_h=merge(SPP_h,GasPrice_h)
  
  #plot
  p4=ggplot(SPP_h, aes(x = NG_Price, y = Price)) +
    geom_point() +
    xlab("Natural Gas Price ($/ tcf)") +
    ylab("Average Monthly LMP ($/MWh)") +
    ggtitle(paste0("Gas Prices vs. LMPs in ",LZ_graphing, " (2011-2019)"))+
    theme_minimal()
  
  lm_eqn = function(df,x,y){
    m = lm(y ~ x, df);
    eq = substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
  }
  
  p5=ggplot(SPP_h, aes(x = NG_Price, y = Price)) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    geom_text(x = 3.75, y = 36, label = lm_eqn(SPP_h,SPP_h$NG_Price, SPP_h$Price), parse = TRUE) +
    xlab("Natural Gas Price ($/ tcf)") +
    ylab("Average Monthly LMP ($/MWh)") +
    ggtitle(paste0("Gas Prices vs. LMPs in ",LZ_graphing, " (2011-2019)"))+
    theme_minimal()
  
  #print(p5)
  
  #And now look at resulting forecasts
  predicted_SPPs=readRDS("predicted_SPPs_NG_only.RDS")
  historic_SPPs=readRDS("training_data.RDS")
  
  #Formatting
  predicted_SPPs=predicted_SPPs[predicted_SPPs$Zone==LZ_graphing,]
  historic_SPPs=historic_SPPs[historic_SPPs$Zone==LZ_graphing,]
  SPPs=data.frame(historic_SPPs[,c("Year","category","Price")],LMPs="Historic")
  
  #Base Case
  temp=data.frame(predicted_SPPs[,c("Year","category","PredPrice_Base")],LMPs="Predicted")
  colnames(temp)[3]="Price"
  SPPs=rbind(SPPs,temp)
  
  #Weight LMPs 
  SPPs$Price[SPPs$category=="summer peak"]=SPPs$Price[SPPs$category=="summer peak"]*(1/6) 
  SPPs$Price[SPPs$category=="summer off-peak"]=SPPs$Price[SPPs$category=="summer off-peak"]*(1/12)
  SPPs$Price[SPPs$category=="non-summer"]=SPPs$Price[SPPs$category=="non-summer"]*(0.75)  
  SPPs=aggregate(Price~Year+LMPs, data=SPPs, FUN=sum)
  
  p6=ggplot(data=SPPs, aes(x=Year, y=Price, group=LMPs))+
    geom_line(aes(linetype=LMPs)) +
    xlab("Year") +
    ylab("Average LMP ($/MWh)") +
    ggtitle(paste0("Average LMPs in ",LZ_graphing, " (2011-2039)"))+
    theme_minimal()
  
  
  #Plot gas prices on the same figure
  GasPrice_h=read_excel("US_GasPrice_1997-2019.xlsx")
  GasPrice_f=read_excel("US_GasPrice_2020-2050.xlsx")
  colnames(GasPrice_f)[2]="NG_Price"
  GasPrice=rbind(GasPrice_f[,c("Year","NG_Price")],GasPrice_h)
  SPPs=merge(SPPs,GasPrice)
  SPPs$Gas="Historic"
  SPPs$Gas[SPPs$Year>2019]="EIA Forecast"

  p6=p6 +
    geom_point( data=SPPs,aes(x = Year, y = NG_Price*9.1, shape=Gas)) +
    scale_y_continuous(
      name = "Average LMP ($/MWh)", # first axis
      sec.axis = sec_axis(~./9.1, name="Natural Gas Price ($/tcf)") #second axis
    )
  
  #print(p6)
  
  #Explore the relationship with other predictors: GDP, Year, Solar and Wind Costs
  SPP_h=aggregate(Price~Year, data=SPP_h, FUN=mean)
  GDP_h=read_excel("Texas_GDP_1997-2019.xlsx")
  Renewables=read_excel("Renewables_CC_2011-2039.xlsx")
  SPP_h=merge(SPP_h,GDP_h)
  SPP_h=merge(SPP_h,Renewables)
  
  p7=ggplot(SPP_h, aes(x = Year, y = Price)) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    geom_text(x = 2016.5, y = 28, label = lm_eqn(SPP_h,SPP_h$Year, SPP_h$Price), parse = TRUE) +
    xlab("Year") +
    ylab("Average LMP ($/MWh)") +
    scale_x_continuous(breaks=c(2011,2012,2013,2014,2015,2016,2017,2018,2019))+
    theme_minimal()
  
  p8=ggplot(SPP_h, aes(x = GDP, y = Price)) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    geom_text(x = 1700000, y = 28, label = lm_eqn(SPP_h,SPP_h$GDP, SPP_h$Price), parse = TRUE) +
    xlab("GDP (Millions of Dollars)") +
    ylab("Average LMP ($/MWh)") +
    theme_minimal()
  
  p9=ggplot(SPP_h, aes(x = Solar_PV_Cost, y = Price)) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    geom_text(x = 2500, y = 29, label = lm_eqn(SPP_h,SPP_h$Solar_PV_Cost, SPP_h$Price), parse = TRUE) +
    xlab("Solar Capital Cost ($/kW)") +
    ylab("Average LMP ($/MWh)") +
    theme_minimal()
  
  p10=ggplot(SPP_h, aes(x = Onshore_Wind_Cost, y = Price)) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    geom_text(x = 1550, y = 28, label = lm_eqn(SPP_h,SPP_h$Onshore_Wind_Cost, SPP_h$Price), parse = TRUE) +
    xlab("Onshore Wind Cost ($/kW)") +
    ylab("Average LMP ($/MWh)") +
    theme_minimal()
  
  #p11=multiplot(p7, p8, p9, p10, cols=2)
}




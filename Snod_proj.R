
rm(list = ls())
###Importing data RMBL data
library(readxl)
library(openxlsx)
library(tidyverse)
library(lubridate)
setwd("~/Documents/SNOD_data")

####Importing data and organizing headers from original .dat file
##to resemble collected data headers

snod_hc<- read_excel("SNOD_data.xlsx", sheet = 1)
snod_hc$TOA5 <- convertToDateTime(snod_hc$TOA5)
names(snod_hc)<-snod_hc[1,]
snod_hc<- snod_hc[-1,]
snod_hc<-snod_hc%>% rename("date"=`NA`)
snod_hc

snod_mc<- read_excel("SNOD_data.xlsx", sheet = 2)
snod_mc$TOA5 <- convertToDateTime(snod_mc$TOA5)
names(snod_mc)<-snod_mc[1,]
snod_mc<- snod_mc[-1,]
snod_mc<-snod_mc%>% rename("date"=`NA`)
snod_mc

snod_ma<- read_excel("SNOD_data.xlsx", sheet = 3)
snod_ma$TOA5 <- convertToDateTime(snod_ma$TOA5)
names(snod_ma)<-snod_ma[1,]
snod_ma<- snod_ma[-1,]
snod_ma<-snod_ma%>% rename("date"=`NA`)
snod_ma

snod_la<- read_excel("SNOD_data.xlsx", sheet = 4)
snod_la$TOA5 <- convertToDateTime(snod_la$TOA5)
names(snod_la)<-snod_la[1,]
snod_la<- snod_la[-1,]
snod_la<-snod_la%>% rename("date"=`NA`)
snod_la

library(tidyr)
library(dplyr)




###Goal is to visualize all data readings first, Then play with comparisons
####SNOD_LA VISUAL ANALYSIS

###CO2 probes
slac<-snod_la %>% select(date,CO2_1corr_Avg,CO2_2corr_Avg,CO2_3corr_Avg)
snod_la %>% 
  ggplot(aes(date, as.numeric(CO2_1corr_Avg)))+
  geom_point()

##unifying data so var= all co2 measurements
slac<-slac%>%
  pivot_longer(., cols = c(CO2_1corr_Avg,CO2_2corr_Avg,CO2_3corr_Avg), names_to = "depth", values_to = "co2_concentration")
slac

slac_plot<-slac %>% 
  ggplot(aes(date, as.numeric(co2_concentration),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("co2_concentration ppm")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))
slac_plot


#####VWC measure
slaw<-snod_la %>% select(date,VWC_1_Avg,VWC_2_Avg,VWC_3_Avg)

##unifying data so var= all w measurements
slaw<-slaw%>%
  pivot_longer(., cols = c(VWC_1_Avg,VWC_2_Avg,VWC_3_Avg), names_to = "depth", values_to = "VWC_m^3/m^3")
slaw

slaw_plot<-slaw %>% 
  ggplot(aes(date, as.numeric(`VWC_m^3/m^3`),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("VWC_m^3/m^3")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))
slaw_plot



###temp probe data
slat<-snod_la %>% select(date,T_1_Avg,T_2_Avg,T_3_Avg)

##unifying data so var= all co2 measurements
slat<-slat%>%
  pivot_longer(., cols = c(T_1_Avg,T_2_Avg,T_3_Avg), names_to = "depth", values_to = "Temp_C")
slat

slat_plot<-slat %>% 
  ggplot(aes(date, as.numeric(Temp_C),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("Temp_C")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))
slat_plot


#### Graphing Probe data
install.packages("ggpubr")
library(ggpubr)
figure1 <- ggarrange(slac_plot, slaw_plot,slat_plot,
                    labels = c("Soil [co2] ppm", "VWC", "Temp_C"),
                    ncol = 1, nrow = 3)
figure1


#####Meatorilogical data
metT_plot<-snod_la %>% 
  ggplot(aes(date, as.numeric(AirT_C_Avg)))+
  geom_smooth(color ="red")+
  ylab("air_Temp_C")
  
metT_plot

metR_plot<-snod_la %>% 
  ggplot(aes(date, as.numeric(Rain_mm_Tot)))+
  geom_line(color="blue")+
  ylim(0,20)+
  ylab("Rain_mm_total")
metR_plot

metRH_plot<-snod_la %>% 
  ggplot(aes(date, as.numeric(RH)))+
  geom_smooth()+
  ylab("RH")
  
metRH_plot

metVP_plot<-snod_la %>% 
  ggplot(aes(date, as.numeric(VP_mbar_Avg)))+
  geom_smooth(color="gold")+
  ylab("VP_mbar")
metVP_plot

figure2 <- ggarrange(metT_plot, metR_plot,metRH_plot,metVP_plot,
                     labels = c("Air_T_C", "Rain_mm", "Relative Humidity","Vapor Pressure"),
                     ncol = 2, nrow = 2)
figure2




#######################SNODMA  








###CO2 probes
smac<-snod_ma %>% select(date,CO2_1corr_Avg,CO2_2corr_Avg,CO2_3corr_Avg)
snod_ma %>% 
  ggplot(aes(date, as.numeric(CO2_1corr_Avg)))+
  geom_point()

##unifying data so var= all co2 measurements
smac<-smac%>%
  pivot_longer(., cols = c(CO2_1corr_Avg,CO2_2corr_Avg,CO2_3corr_Avg), names_to = "depth", values_to = "co2_concentration")
slac

smac_plot<-smac %>% 
  ggplot(aes(date, as.numeric(co2_concentration),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("co2_concentration ppm")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))
smac_plot


#####VWC measure
smaw<-snod_ma %>% select(date,VWC_1_Avg,VWC_2_Avg,VWC_3_Avg)

##unifying data so var= all w measurements
smaw<-smaw%>%
  pivot_longer(., cols = c(VWC_1_Avg,VWC_2_Avg,VWC_3_Avg), names_to = "depth", values_to = "VWC_m^3/m^3")
slaw

smaw_plot<-smaw %>% 
  ggplot(aes(date, as.numeric(`VWC_m^3/m^3`),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("VWC_m^3/m^3")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))
smaw_plot



###temp probe data
smat<-snod_ma %>% select(date,T_1_Avg,T_2_Avg,T_3_Avg)

##unifying data so var= all co2 measurements
smat<-smat%>%
  pivot_longer(., cols = c(T_1_Avg,T_2_Avg,T_3_Avg), names_to = "depth", values_to = "Temp_C")
slat

smat_plot<-smat %>% 
  ggplot(aes(date, as.numeric(Temp_C),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("Temp_C")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))
smat_plot


#### Graphing Probe data
install.packages("ggpubr")
library(ggpubr)
figure3 <- ggarrange(smac_plot, smaw_plot,smat_plot,
                     labels = c("Soil [co2] ppm", "VWC", "Temp_C"),
                     ncol = 1, nrow = 3)
figure3


#####Meatorilogical data
mametT_plot<-snod_ma %>% 
  ggplot(aes(date, as.numeric(AirT_C_Avg)))+
  geom_smooth(color ="red")+
  ylab("air_Temp_C")

mametT_plot

mametR_plot<-snod_ma %>% 
  ggplot(aes(date, as.numeric(Rain_mm_Tot)))+
  geom_line(color="blue")+
  ylim(0,20)+
  ylab("Rain_mm_total")
mametR_plot

mametRH_plot<-snod_ma %>% 
  ggplot(aes(date, as.numeric(RH)))+
  geom_smooth()+
  ylab("RH")

mametRH_plot

mametVP_plot<-snod_ma %>% 
  ggplot(aes(date, as.numeric(VP_mbar_Avg)))+
  geom_smooth(color="gold")+
  ylab("VP_mbar")
mametVP_plot

figure4 <- ggarrange(mametT_plot, mametR_plot,mametRH_plot,mametVP_plot,
                     labels = c("Air_T_C", "Rain_mm", "Relative Humidity","Vapor Pressure"),
                     ncol = 2, nrow = 2)
figure4







##########SNOD_MC





###CO2 probes
smcc<-snod_mc %>% select(date,CO2_1corr_Avg,CO2_2corr_Avg,CO2_3corr_Avg)
snod_ma %>% 
  ggplot(aes(date, as.numeric(CO2_1corr_Avg)))+
  geom_point()

##unifying data so var= all co2 measurements
smcc<-smcc%>%
  pivot_longer(., cols = c(CO2_1corr_Avg,CO2_2corr_Avg,CO2_3corr_Avg), names_to = "depth", values_to = "co2_concentration")


smcc_plot<-smcc %>% 
  ggplot(aes(date, as.numeric(co2_concentration),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("co2_concentration ppm")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))
smcc_plot


#####VWC measure
smcw<-snod_mc %>% select(date,VWC_1_Avg,VWC_2_Avg,VWC_3_Avg)

##unifying data so var= all w measurements
smcw<-smcw%>%
  pivot_longer(., cols = c(VWC_1_Avg,VWC_2_Avg,VWC_3_Avg), names_to = "depth", values_to = "VWC_m^3/m^3")


smcw_plot<-smcw %>% 
  ggplot(aes(date, as.numeric(`VWC_m^3/m^3`),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("VWC_m^3/m^3")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))
smcw_plot


###################




#####VWC measure
smcw<-snod_m %>% select(date,VWC_1_Avg,VWC_2_Avg,VWC_3_Avg)

smcw2_plot<-snod_ma %>% 
  ggplot(aes(date, as.numeric(VWC_1_Avg)))+
  geom_point(position="jitter",size=.5)+
  ylab("VWC2_15cm")
smcw2_plot

smcw1_plot<-snod_mc %>% 
  ggplot(aes(date, as.numeric(VWC_1_Avg)))+
  geom_point(position="jitter",size=.5)+
  ylab("VWC3_50cm")
smcw1_plot



###temp probe data
smct<-snod_mc %>% select(date,T_1_Avg,T_2_Avg,T_3_Avg)

##unifying data so var= all co2 measurements
smct<-smct%>%
  pivot_longer(., cols = c(T_1_Avg,T_2_Avg,T_3_Avg), names_to = "depth", values_to = "Temp_C")
slat

smct_plot<-smct %>% 
  ggplot(aes(date, as.numeric(Temp_C),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("Temp_C")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))
smct_plot


#### Graphing Probe data
install.packages("ggpubr")
library(ggpubr)
figure5 <- ggarrange(smcc_plot, smcw_plot,smct_plot,
                     labels = c("Soil [co2] ppm", "VWC", "Temp_C"),
                     ncol = 1, nrow = 3)
figure5


#####Meatorilogical data
mcmetT_plot<-snod_mc %>% 
  ggplot(aes(date, as.numeric(AirT_C_Avg)))+
  geom_smooth(color ="red")+
  ylab("air_Temp_C")

mcmetT_plot

mcmetR_plot<-snod_mc %>% 
  ggplot(aes(date, as.numeric(Rain_mm_Tot)))+
  geom_line(color="blue")+
  ylim(0,20)+
  ylab("Rain_mm_total")
mcmetR_plot

mcmetRH_plot<-snod_mc %>% 
  ggplot(aes(date, as.numeric(RH)))+
  geom_smooth()+
  ylab("RH")

mcmetRH_plot

mcmetVP_plot<-snod_mc %>% 
  ggplot(aes(date, as.numeric(VP_mbar_Avg)))+
  geom_smooth(color="gold")+
  ylab("VP_mbar")
mcmetVP_plot

figure6 <- ggarrange(mcmetT_plot, mcmetR_plot,mcmetRH_plot,mcmetVP_plot,
                     labels = c("Air_T_C", "Rain_mm", "Relative Humidity","Vapor Pressure"),
                     ncol = 2, nrow = 2)
figure6






#############SNODHC



###CO2 probes
shcc<-snod_hc %>% select(date,CO2_1corr_Avg,CO2_2corr_Avg,CO2_3corr_Avg)
snod_ma %>% 
  ggplot(aes(date, as.numeric(CO2_1corr_Avg)))+
  geom_point()

##unifying data so var= all co2 measurements
shcc<-shcc%>%
  pivot_longer(., cols = c(CO2_1corr_Avg,CO2_2corr_Avg,CO2_3corr_Avg), names_to = "depth", values_to = "co2_concentration")


shcc_plot<-shcc %>% 
  ggplot(aes(date, as.numeric(co2_concentration),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("co2_concentration ppm")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))+
  ylim(0,7000)
shcc_plot


#####VWC measure
shcw<-snod_hc %>% select(date,VWC_1_Avg,VWC_2_Avg,VWC_3_Avg)

##unifying data so var= all w measurements
shcw<-shcw%>%
  pivot_longer(., cols = c(VWC_1_Avg,VWC_2_Avg,VWC_3_Avg), names_to = "depth", values_to = "VWC_m^3/m^3")


shcw_plot<-shcw %>% 
  ggplot(aes(date, as.numeric(`VWC_m^3/m^3`),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("VWC_m^3/m^3")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))
shcw_plot



###temp probe data
shct<-snod_hc %>% select(date,T_1_Avg,T_2_Avg,T_3_Avg)

##unifying data so var= all co2 measurements
shct<-shct%>%
  pivot_longer(., cols = c(T_1_Avg,T_2_Avg,T_3_Avg), names_to = "depth", values_to = "Temp_C")
slat

shct_plot<-shct %>% 
  ggplot(aes(date, as.numeric(Temp_C),color=depth))+
  geom_point(position="jitter",size=.5)+
  ylab("Temp_C")+
  scale_color_discrete(name="Depth", labels=c("50cm","15cm","5cm"))
shct_plot


#### Graphing Probe data
install.packages("ggpubr")
library(ggpubr)
figure5 <- ggarrange(shcc_plot, shcw_plot,shct_plot,
                     labels = c("Soil [co2] ppm", "VWC", "Temp_C"),
                     ncol = 1, nrow = 3)
figure5


#####Meatorilogical data
hcmetT_plot<-snod_hc %>% 
  ggplot(aes(date, as.numeric(AirT_C_Avg)))+
  geom_smooth(color ="red")+
  ylab("air_Temp_C")

hcmetT_plot

hcmetR_plot<-snod_hc %>% 
  ggplot(aes(date, as.numeric(Rain_mm_Tot)))+
  geom_line(color="blue")+
  ylim(0,20)+
  ylab("Rain_mm_total")
hcmetR_plot

hcmetRH_plot<-snod_hc %>% 
  ggplot(aes(date, as.numeric(RH)))+
  geom_smooth()+
  ylab("RH")

hcmetRH_plot

hcmetVP_plot<-snod_hc %>% 
  ggplot(aes(date, as.numeric(VP_mbar_Avg)))+
  geom_smooth(color="gold")+
  ylab("VP_mbar")
mcmetVP_plot

figure6 <- ggarrange(hcmetT_plot, hcmetR_plot,hcmetRH_plot,hcmetVP_plot,
                     labels = c("Air_T_C", "Rain_mm", "Relative Humidity","Vapor Pressure"),
                     ncol = 2, nrow = 2)
figure6




######Wind..
windla<-snod_la %>% 
  ggplot(aes(date, as.numeric(WS_ms_Avg)))+
  geom_smooth()+
  ylab("wind_speed LA")
windla

windma<-snod_ma %>% 
  ggplot(aes(date, as.numeric(WS_ms_Avg)))+
  geom_smooth()+
  ylab("wind_speed mA")
windma


windmc<-snod_mc %>% 
  ggplot(aes(date, as.numeric(WS_ms_Avg)))+
  geom_smooth()+
  ylab("wind_speed mc")
windmc


windhc<-snod_hc %>% 
  ggplot(aes(date, as.numeric(WS_ms_Avg)))+
  geom_smooth()+
  ylab("wind_speed hc")
windhc

figure8 <- ggarrange(windla, windma,windmc,windhc,
                     labels = ("m/sec"),
                     ncol = 2, nrow = 2)
figure8





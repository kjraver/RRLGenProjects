library(dplyr)
library(ggplot2)
library(googlesheets4)
library(ggridges)

IA<-read_sheet('https://docs.google.com/spreadsheets/d/1Dazv10GXyhBLlo3Ql-tcvQBdP_uoW73VNfc-Dq0TxkY/edit?usp=sharing', ".")
IAmin<- read_sheet('https://docs.google.com/spreadsheets/d/1Dazv10GXyhBLlo3Ql-tcvQBdP_uoW73VNfc-Dq0TxkY/edit?usp=sharing', "Sheet1")


ggplot(data=IA, aes(x=NRC2001_Lignin_tdn, y=fdProductCode))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 3)


ggplot(data=IA, aes(x=TDN, y=fdProductCode))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 3)

ggplot(data=IA, aes(x=NEG_kg, y=fdProductCode))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 3)

ggplot(data=IA, aes(x=Beef_Ton, y=fdProductCode))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 3)





ggplot(data=IAmin, aes(x=fdsnMg, y=MaterialType))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 3)

ggplot(data=IAmin, aes(x=fdsngCa, y=MaterialType))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 3)

ggplot(data=IAmin, aes(x=fdsngk, y=MaterialType))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 3)
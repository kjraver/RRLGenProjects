sales<-read.csv(file.choose())
salesref<- read.csv(file.choose())

library(dplyr)
library(ggplot2)
library(meantables)
library(plotly)
library(googlesheets4)

sales%>%
  select(Year, Month, N, TotalCharge, AccountName, AccountNumber) %>% 
  # gather(fdDateReceived, fdSampleDesc, TotalDM, AcctNo, na.rm = TRUE) %>% 
  inner_join(salesref, by = c("AccountName", "AccountNumber")) -> CombinedTable


CPN<-CombinedTable%>%
  group_by(Year)%>%
  mutate(ChargeperN=TotalCharge/N)%>%
  summarise(ChargeperNS=mean(ChargeperN))%>%
  as.data.frame()


ggplot(CPN, aes(x=Year, y=ChargeperNS))+
  geom_bar(stat="identity")+
  theme_classic()

CPN2<-CPN%>%
  filter(Business.Type=="Legacy"|Business.Type=="Driver"|Business.Type=="Allied Industry"|Business.Type=="Legacy/Mill"|Business.Type=="Mill"|Business.Type=='Farm Direct')%>%
  ggplot(., aes(x=Year, y=ChargeperNS, fill=Business.Type))+
  geom_bar(position="dodge", stat="identity")
  
CPN2

CT1<-CombinedTable%>%
  group_by(Year, AccountNumber, Business.Type, AccountName)%>%
  summarise(across(c(TotalCharge, N),sum))%>%
  mutate(sizebyN= case_when(N < 100 ~ "Small",
                          N >= 100 ~"Medium",
                          N >= 400 ~ 'Large'))%>%
  mutate(sizebyCharge= case_when(TotalCharge < 5000 ~ "Small",
                            TotalCharge <= 40000 ~"Medium",
                            .default = "Large"))
CT1

CombinedTable%>%
  group_by(Year, Business.Type)%>%
  summarise(Sum_N=sum(N))%>%
  filter(Business.Type=="Legacy"|Business.Type=="Driver"|Business.Type=="Allied Industry"|Business.Type=="Legacy/Mill"|Business.Type=="Mill"|Business.Type=='Farm Direct')%>%
  mutate(percentage=Sum_N/sum(Sum_N))%>%
  ggplot(., aes(x=Year, y=percentage, fill=Business.Type, text= Business.Type))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  ylab("Percentage of Sample N")->MyPlot

MyPlot

ggplotly(MyPlot,tooltip="text")

CombinedTablePlay<-CombinedTable%>%
  group_by(Year, Business.Type)%>%
  summarise(across(where(is.numeric),~ sum(.x, na.rm=TRUE)))


CombinedTableP2<-CombinedTablePlay%>%
  arrange(Business.Type)%>%
  group_by(Business.Type)%>%
  filter(Business.Type=="Legacy"|Business.Type=="Driver"|Business.Type=="Allied Industry"|Business.Type=="Legacy/Mill"|Business.Type=="Mill"|Business.Type=='Farm Direct'| Business.Type=="Seed Company"| Business.Type=="University")%>%
mutate(growthN = ((N - dplyr::lag(N, order_by=Year))/dplyr::lag(N, order_by=Year)) * 100)%>%
  mutate(growthTC= ((TotalCharge - dplyr::lag(TotalCharge))/dplyr::lag(TotalCharge)) * 100)
  # filter(Business.Type=="Legacy"|Business.Type=="Driver"|Business.Type=="Allied Industry"|Business.Type=="Legacy/Mill"|Business.Type=="Mill"|Business.Type=='Farm Direct'| Business.Type=="Seed Company"| Business.Type=="University")


ggplot(CombinedTableP2, aes(x=as.factor(Year), y=growthN, fill=Business.Type))+
  geom_bar(position="dodge", stat='identity')



CombinedTable%>%
  group_by(Year, Business.Type)%>%
  summarise(Sum_N=sum(N))%>%
  filter(Business.Type=="Legacy"|Business.Type=="Driver"|Business.Type=="Allied Industry"|Business.Type=="Legacy/Mill"|Business.Type=="Mill"|Business.Type=='Farm Direct')%>%
  # mutate(percentage=Sum_N/sum(Sum_N))%>%
  ggplot(., aes(x=Year, y=Sum_N, fill=Business.Type, text= Business.Type))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  ylab("Total Sample Numbers")->MyPlot

MyPlot


ByN<-CombinedTable%>%
  group_by(Year, Business.Type)%>%
  summarise(Sum_N=sum(N))

ByN

CombinedTable%>%
  group_by(Year, Business.Type)%>%
  summarise(Sum_TC=sum(TotalCharge))%>%
  ggplot(., aes(x=Year, y=Sum_TC, fill=Business.Type))+
  geom_area(alpha=0.6, size=.5, colour="white")+
  theme_classic()

CT2<-CT1%>%
  group_by(sizebyCharge, Year)%>%
  mutate(percentage=TotalCharge/sum(TotalCharge))%>%
  mean_table(TotalCharge)%>%
  group_by(group_2_cat)%>%
  mutate(percentage2=n/sum(n))%>%
  ggplot(., aes(x=group_2_cat, y=percentage2, fill=group_1_cat))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  xlab("Year")+
  ylab("% Of total Customers In each Category")

CT2

CT3<-CT1%>% group_by(Year, sizebyCharge) %>% mean_table(TotalCharge)
CT3

CT4<-CT1%>%
  group_by(sizebyCharge, Year)%>%
  summarise(SumTC=sum(TotalCharge))%>%
  # mutate(percentage=TotalCharge/sum(TotalCharge))%>%
  #mutate(percentage2=n/sum(n))%>%
  ggplot(., aes(x=Year, y=SumTC, fill=sizebyCharge))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  xlab("Year")+
  ylab("Total Charge by Customer Size")

CT4

CT5<- CT1 %>%
  
  filter(sizebyCharge=="Large")%>%
  group_by(AccountName)%>%
  do(PLOTS={ggplot(.,aes(x=Year, y=TotalCharge), color=Year)+
      geom_bar(stat="identity")+
      ggtitle(unique(.$AccountName))})->Mypl

Mypl$PLOTS

CT6<- CT1%>%
  group_by(Year, AccountNumber, Business.Type, AccountName)%>%
  summarise(across(c(TotalCharge, N),sum))%>%
  filter(TotalCharge> 100000| Year<2023)%>%
  ggplot(.,aes(x=Year, y=TotalCharge, fill=AccountName))+
      geom_bar(position="dodge", stat="identity")

CT6


Assays<-read_sheet("https://docs.google.com/spreadsheets/d/1CUWHfD5ifyulHJyZaJ3FB8m3fYDAYBd5ygFZrok4T8M/edit?usp=drive_link", "WFArchive Counts")

Assays%>%
  group_by(Year, Month)%>%
  ggplot(.,aes(x=Month, y=ADF))+geom_point()+facet_grid(.~Year)

Assays%>%
  group_by(Year, Month)%>%
  ggplot(.,aes(x=Month, y=ADF_ICP))+geom_point()+facet_grid(.~Year)

Assays%>%
  group_by(Year, Month)%>%
  ggplot(.,aes(x=Month, y=Ash))+geom_point()+facet_grid(.~Year)

Assays%>%
  group_by(Year, Month)%>%
  ggplot(.,aes(x=Month, y=CF))+geom_point()+facet_grid(.~Year)

Assays%>%
  group_by(Year, Month)%>%
  ggplot(.,aes(x=Month, y=Enterobacteria))+geom_point()+facet_grid(.~Year)


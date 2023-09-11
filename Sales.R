sales<- read_sheet("https://docs.google.com/spreadsheets/d/1FSbQCQnDwg_V20G4lXnkYLzrCr35P7Ym52-HdYccVvA/edit?usp=sharing")
salesref<- read_sheet("https://docs.google.com/spreadsheets/d/1JXD9MLP1tL66m-tZtf_xYRVc3y5rYuQQCsTujeqX-n8/edit?usp=sharing")

gs4_auth(email=raverkatie0@gmail.com)
devtools::install_github("tidyverse/googlesheets4")

drive_auth(email = "raverkatie0@gmail.com")


library(dplyr)
library(ggplot2)
library(meantables)
library(plotly)
library(googlesheets4)
library(flexdashboard)
library(googledrive)

sales%>%
  select(Year, Month, N, TotalCharge, AccountName, AccountNumber) %>% 
  # gather(fdDateReceived, fdSampleDesc, TotalDM, AcctNo, na.rm = TRUE) %>% 
  inner_join(salesref, by = c("AccountName", "AccountNumber")) -> CombinedTable


CPN<-CombinedTable%>%
  group_by(Year)%>%
  mutate(ChargeperN=TotalCharge/N)%>%
  summarise(ChargeperNS=mean(ChargeperN))%>%
  as.data.frame()

CPN1<-CombinedTable%>%
  group_by(Year, Business.Type)%>%
  mutate(ChargeperN=TotalCharge/N)%>%
  summarise(ChargeperNS=mean(ChargeperN))%>%
  as.data.frame()

ggplot(CPN, aes(x=Year, y=ChargeperNS, fill=Year))+
  geom_bar(stat="identity")+
  theme_classic()

CPN2<-CPN1%>%
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

CombinedTable%>%
  group_by(Year, Business.Type)%>%
  summarise(Sum_TotalCharge=sum(TotalCharge))%>%
  filter(Business.Type=="Legacy"|Business.Type=="Driver"|Business.Type=="Allied Industry"|Business.Type=="Legacy/Mill"|Business.Type=="Mill"|Business.Type=='Farm Direct')%>%
  mutate(percentage=Sum_TotalCharge/sum(Sum_TotalCharge))%>%
  ggplot(., aes(x=Year, y=percentage, fill=Business.Type, text= Business.Type))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  ylab("Percentage of TotalCharge")->MyPlot2

MyPlot2

ggplotly(MyPlot,tooltip="text")

CombinedTablePlay<-CombinedTable%>%
  group_by(Year, Business.Type)%>%
  summarise(across(where(is.numeric),~ sum(.x, na.rm=TRUE)))


CombinedTableP2<-CombinedTablePlay%>%
  arrange(Business.Type)%>%
  group_by(Business.Type)%>%
  filter(Business.Type=="Legacy"|Business.Type=="Driver"|Business.Type=="Allied Industry"|Business.Type=="Legacy/Mill"|Business.Type=="Mill"|Business.Type=='Farm Direct'| Business.Type=="Seed Company"| Business.Type=="University")%>%
mutate(growthN = ((N - dplyr::lag(N, order_by=Year))/dplyr::lag(N, order_by=Year)) * 100)%>%
  mutate(growthTC= ((TotalCharge - dplyr::lag(TotalCharge, order_by= Year))/dplyr::lag(TotalCharge, order_by=Year)) * 100)

ggplot(CombinedTableP2, aes(x=Year, y=growthN, fill=Business.Type))+
  geom_bar(position="dodge", stat='identity')+
  ggtitle("Growth Percent in Sample N by Customer Type")

ggplot(CombinedTableP2, aes(x=Year, y=growthTC, fill=Business.Type))+
  geom_bar(position="dodge", stat='identity')+
  ggtitle("Growth Percent in Total Charge by Customer Type")

YTDgrowth<-CombinedTableP2%>%
  filter(Year<2023)

ggplot(YTDgrowth, aes(x=Year, y=growthN, fill=Business.Type))+
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

Tree<-CT1%>%
  group_by(sizebyCharge, Business.Type)%>%
  summarize(SumTC= sum(TotalCharge))%>%
           collapsibleTreeSummary(hierarchy=c("sizebyCharge", "Business.Type"), root=CT1, attribute="SumTC")
Tree

CT5<- CT1 %>%
  
  filter(sizebyCharge=="Large")%>%
  group_by(AccountName)%>%
  do(PLOTS={ggplot(.,aes(x=Year, y=TotalCharge), color=Year)+
      geom_bar(stat="identity")+
      ggtitle(unique(.$AccountName))})->Mypl

Mypl$PLOTS

CT22<- CT1%>%
  group_by(Year, AccountNumber, Business.Type, AccountName)%>%
  summarise(across(c(TotalCharge, N),sum))%>%
  filter(TotalCharge > 100000 & Year > 2019 & Year <2023)%>%
  ggplot(.,aes(x=Year, y=TotalCharge, fill=AccountName))+
      geom_bar(position="dodge", stat="identity")

CT22

CT7<- CT1 %>%
  filter(sizebyCharge=="Large")%>%
  arrange(AccountName)%>%
  group_by(AccountName)%>%
  mutate(growthN = ((N - dplyr::lag(N, order_by=Year))/dplyr::lag(N, order_by=Year)) * 100)%>%
  mutate(growthTC= ((TotalCharge - dplyr::lag(TotalCharge, order_by= Year))/dplyr::lag(TotalCharge, order_by=Year)) * 100)%>%
  filter(AccountName=="ADM ALLIANCE NUTRITION, INC."| AccountName=="Alcivia Cooperative"| AccountName=="Big Gain"| AccountName=="CHR Hansen Inc."| AccountName=="Edge Dairy Consulting"| AccountName=="Heritage Cooperative"| AccountName=="Holtz-Nelson Dairy Consulants LLC"|AccountName=="Lallemand Animal Nutrition"| AccountName=="Milk Source"| AccountName=="NutriQuest, LLC"| AccountName=="Papillon Agricultural Company, Inc."| AccountName=="Phibro"| AccountName=="PREMIER COOPERATIVE"| AccountName=="Progressive Dairy Solutions"| AccountName=="Sensenigs Feed Mill-70000840"| AccountName== "Syngenta Enogen"| AccountName=="VITA PLUS CORPORATION")


CT8<-CT7%>%  
group_by(AccountName)%>%
  do(PLOTS={ggplot(.,aes(x=Year, y=growthTC, fill=Year))+
      geom_bar(position='dodge', stat="identity")+
      ggtitle(unique(.$AccountName))})->Mypl2

Mypl2$PLOTS

ggplot(CT7, aes(x=Year, y=growthTC, fill=Year))+
      geom_bar(position='dodge', stat="identity")+
      facet_wrap(~AccountName, ncol=5)

ggplot(CT7, aes(x=Year, y=growthN, fill=Year))+
  geom_bar(position='dodge', stat="identity")+
  facet_wrap(~AccountName, ncol=5)

CostPerSampleTopAct<-CT7%>%
  mutate(CostperN=TotalCharge/N)

ggplot(CostPerSampleTopAct, aes(x=Year, y=CostperN, fill=Year))+
  geom_bar(position='dodge', stat="identity")+
  facet_wrap(~AccountName, ncol=5)

Assays<-read_sheet("https://docs.google.com/spreadsheets/d/1CUWHfD5ifyulHJyZaJ3FB8m3fYDAYBd5ygFZrok4T8M/edit?usp=drive_link", "WFArchive Counts")

str(Assays)

Assays[] <- lapply(Assays, function(x) as.numeric(as.list(x)))

TotalAssays<- Assays%>%
  rowwise()%>%
  mutate(totalassays = sum(c_across(c(4:59)), na.rm = T))

TotalAssays1<-TotalAssays%>%
  group_by(Year, Month)%>%
  summarise(totalassayswc=sum(totalassays))%>%
  as.data.frame()

TotalAssays2<-TotalAssays1%>%
  group_by(Year, Month)%>%
  ggplot(.,aes(x=Month, y=totalassayswc))+geom_point()+facet_grid(.~Year)
TotalAssays2

SampleNo<-read_sheet("https://docs.google.com/spreadsheets/d/1CUWHfD5ifyulHJyZaJ3FB8m3fYDAYBd5ygFZrok4T8M/edit?usp=drive_link", "Total Samples")

SampleNo[] <- lapply(SampleNo, function(x) as.numeric(as.list(x)))

SampleNoChem <- SampleNo[seq_len(nrow(SampleNo)) %% 2 == 1, ]   # Extracting even rows
head(SampleNoChem)

SampleNoChem%>%
  rowwise()%>%
  mutate(TotalN= sum(c_across(c(3:48)), na.rm = T))%>%
  as.data.frame()

Assays1<- TotalAssays1%>%
  group_by(Year, Month)%>%
  summarise(totalassayswc)%>%
  na.omit(.)


Assays1%>%
  select(Year, Month, totalassayswc) %>% 
  # gather(fdDateReceived, fdSampleDesc, TotalDM, AcctNo, na.rm = TRUE) %>% 
  inner_join(SampleNoChem, by = c("Year", "Month")) -> APS

APS1<-APS%>%
  rowwise()%>%
  mutate(TotalN= sum(c_across(c(5:48)), na.rm = T))%>%
  mutate(AssaysPerSample=totalassayswc/TotalN)

print(APS)

ggplot(APS1,aes(x=Month, y=AssaysPerSample))+geom_point()+ geom_smooth()+facet_grid(.~Year)

ggplot(APS1,aes(x=Month, y=AssaysPerSample))+geom_point()+geom_smooth()




APSmeans<-APS1%>%
  mean_table(AssaysPerSample)

APSmeans

library(reshape)
library(reshape2)

AssaysL<- melt(TotalAssays, id=c("WFLocation", "Year", "Month"))

AssaysL%>%na.omit()%>%
  filter(value>1)

ggplot(AssaysL, aes(x=Year, y=value))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ variable, scales="free", ncol=8)


ggplot(AssaysL, aes(x=Month, y=value+ color=Year))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ variable, scales="free", ncol=8)

Assaysgoingup<- AssaysL%>%
  filter(variable=="FatAcidHydrolysis"| variable=="isRUP"| variable=="VFAWeights"| variable=='SEQF'| variable=="CF"| variable=="NDF_NDFICP")%>%
  ggplot(., aes(x=Year, y=value))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ variable, scales="free", ncol=3)
Assaysgoingup
  
FHAssays<- AssaysL%>%
  filter(variable=="Clostridium"| variable=="Enterobacteria"|variable=="HT2Toxin"| variable=='MS_Aflatoxin'| variable=="MS_Deoxynivalenol"| variable=="MS_Fumonisin"| variable=="MS_OchratoxinA"| variable=="MS_T2"| variable=="MS_Zearalenone"| variable=="YeastMold")%>%
  ggplot(., aes(x=Year, y=value))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ variable, scales="free", ncol=4)
FHAssays

AssaysSTD<-AssaysL%>%
  filter(variable=="Ash"| variable=="ADF"| variable=="CP"| variable=="Starch"| variable=="NDF"| variable=="DM"| variable=='SD'| variable=="Mineral"| variable=="SugarWSC")%>%
ggplot(., aes(x=Year, y=value))+
  geom_point()+
  geom_smooth()+
facet_wrap(~ variable, scales="free", ncol=3)
AssaysSTD

Assayseasonal<-AssaysL%>%
  filter(variable=="ADF"| variable=="MS_OchratoxinA"| variable=="MS_Fumonisin"| variable=="ADF_ICP"| variable=="totalassays"| variable=="SD"| variable=="mineral")%>%
  ggplot(., aes(x=Month, y=value, color=Year))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ variable, scales="free", ncol=3)
Assayseasonal
# data loader for ITS 
# Updated model summary 
library(tidyverse)
library(tsModel)
library(sandwich)
library(readxl)
library(stargazer)
library(dplyr)
library(tsModel)
library(sandwich)
library(lubridate)
library(ggthemes)
library(cowplot)
library(MASS)

msm2 = read_excel("/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/MSM_CDC.xlsx")
library(zoo)
msm2$month=as.Date(as.yearmon(msm2$Month))

msm2$High = ifelse(msm2$newlocklevel%in%c("Level 5-3"),1,0)
msm2$Low = ifelse(msm2$newlocklevel%in%c("Level3b","Level1","Level3b-1","Level2-Level3","Level4","Level3","Level3-Level2","No lockdown"),1,0)
msm2=msm2%>%dplyr::select(month,HTS_TST_TOTAL,HTS_TST_POS_TOTAL,TX_NEW_TOTAL,PREP_NEW_TOTAL,High,Low)
# data 2 emh 
msm = read_excel("~/Desktop/updated_msm.xlsx") # Oct to Mar2 22


msm = msm[1:37,] # exclude july





# deal with date 
library(zoo)
msm$month=as.Date(as.yearmon(msm$month))

msm$High = ifelse(msm$newlocklevel%in%c("Level 5-3"),1,0)
msm$Low = ifelse(msm$newlocklevel%in%c("Level3b","Level1","Level3b-1","Level2-Level3","Level4","Level3","Level3-Level2","No lockdown"),1,0)

msm=msm%>%dplyr::select(month,HTS_TST_TOTAL,HTS_TST_POS_TOTAL,TX_NEW_TOTAL,PREP_NEW_TOTAL,High,Low)


# combined 
sum_msm=rbind(msm,msm2)
tab=sum_msm%>%group_by(month)%>%
  summarise(HTS_TST_TOTAL=sum(HTS_TST_TOTAL),
            HTS_TST_POS_TOTAL=sum(HTS_TST_POS_TOTAL),
            TX_NEW_TOTAL=sum(TX_NEW_TOTAL),
            PREP_NEW_TOTAL= sum(PREP_NEW_TOTAL),across())%>%
  distinct(month,.keep_all = TRUE)
sum_msm=tab
dim(sum_msm)
# 
sum_msm$time=c(1:37)
which(sum_msm$High==1)
sum_msm$inter1 = ifelse(sum_msm$High==1,sum_msm$time-9,0) # center at lock down 
sum_msm$inter2 = ifelse(sum_msm$Low==1,sum_msm$time-15,0)
# 
library(xlsx)

write.xlsx(as.data.frame(sum_msm), "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/Combined_MSM2022.xlsx")
save(sum_msm, file = "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/final_msm.RData")

# For FSW -----------------------------------------------
#--------------------------------------------------------




fsw2 = read_excel("/Users/hereagain/JHU/RA_JHU/COVID19ITS/Data/FSW_CDC_his.xlsx")
fsw2$month=format(as.Date(fsw2$Week), "%Y-%m")
fsw2$month=as.Date(as.yearmon(fsw2$month))


# CDC historic to Mar 21
tab1=fsw2%>% dplyr::select(month,High,Low,HTS_TST_TOTAL,HTS_TST_POS_TOTAL,TX_NEW_TOTAL,PREP_NEW_TOTAL)%>%
  group_by(month)%>%
  summarise(HTS_TST_TOTAL=sum(HTS_TST_TOTAL),
            HTS_TST_POS_TOTAL=sum(HTS_TST_POS_TOTAL),
            TX_NEW_TOTAL=sum(TX_NEW_TOTAL),
            PREP_NEW_TOTAL= sum(PREP_NEW_TOTAL),across())%>%
  distinct(month,.keep_all = TRUE)
as.data.frame(tab1)

fsw2=tab1
which(fsw2$month=="2020-03-01")
fsw2 = fsw2[c(1:26),]

# combine: 
fsw = read_excel("/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/FSW_CDC.xlsx")
dim(fsw)
library(zoo)
fsw$month=as.Date(as.yearmon(fsw$Month))

fsw$High = ifelse(fsw$newlocklevel%in%c("Level 5-3"),1,0)
unique(fsw$newlocklevel)
fsw$Low = ifelse(fsw$newlocklevel%in%c("Level3b","Level1","Level3b-1","Level2-Level3","Level4","Level3","Level3-Level2","No lockdown"),1,0)
fsw= fsw%>%dplyr::select(month,HTS_TST_TOTAL,HTS_TST_POS_TOTAL,TX_NEW_TOTAL,PREP_NEW_TOTAL,High,Low)


fsw3=data.frame(rbind(fsw2,fsw)) #2018 Jan to 2022 mar
dim(fsw)

wrhi2 = read_excel("/Users/hereagain/JHU/RA_JHU/COVID19ITS/Data/WRHI_FSW.xlsx",sheet="FSW")
wrhi2$month=format(as.Date(wrhi2$Week), "%Y-%m")
wrhi2$month=as.Date(as.yearmon(wrhi2$month))



tab1=wrhi2%>% dplyr::select(month,High,Low,HTS_TST_TOTAL,HTS_TST_POS_TOTAL,TX_NEW_TOTAL,PREP_NEW_TOTAL)%>%
  group_by(month)%>%
  summarise(HTS_TST_TOTAL=sum(HTS_TST_TOTAL),
            HTS_TST_POS_TOTAL=sum(HTS_TST_POS_TOTAL),
            TX_NEW_TOTAL=sum(TX_NEW_TOTAL),
            PREP_NEW_TOTAL= sum(PREP_NEW_TOTAL),across())%>%
  distinct(month,.keep_all = TRUE)

wrhi2=tab1


# updated data
wrhi3 = read_excel("/Users/hereagain/JHU/RA_JHU/COVID19ITS/Data/FSW_WRHI2.xlsx")
wrhi3$month=format(as.Date(wrhi3$Week), "%Y-%m")
wrhi3$month=as.Date(as.yearmon(wrhi3$month))

names(wrhi2)

tab2=wrhi3%>% dplyr::select(HTS_TST_TOTAL,HTS_TST_POS_TOTAL,TX_NEW_TOTAL,PREP_NEW_TOTAL,month,High,Low,)%>%
  group_by(month)%>%
  summarise(HTS_TST_TOTAL=sum(HTS_TST_TOTAL),
            HTS_TST_POS_TOTAL=sum(HTS_TST_POS_TOTAL),
            TX_NEW_TOTAL=sum(TX_NEW_TOTAL),
            PREP_NEW_TOTAL= sum(PREP_NEW_TOTAL),across())%>%
  distinct(month,.keep_all = TRUE)

wrhi3=tab2
wrhi2= wrhi2[-c(39),]

as.data.frame(tab2)

wrhi3=rbind(wrhi2,wrhi3)




# combine with WRHI 

sum_fsw= rbind(wrhi3,fsw3)
sum_fsw=sum_fsw%>%group_by(month)%>%
  summarise(HTS_TST_TOTAL=sum(HTS_TST_TOTAL),
            HTS_TST_POS_TOTAL=sum(HTS_TST_POS_TOTAL),
            TX_NEW_TOTAL=sum(TX_NEW_TOTAL),
            PREP_NEW_TOTAL= sum(PREP_NEW_TOTAL),across())%>%
  distinct(month,.keep_all = TRUE)
as.data.frame(sum_fsw)

dim(sum_fsw)

sum_fsw$time=c(1:51)
which(sum_fsw$Low==1)
sum_fsw$inter1 = ifelse(sum_fsw$High==1,sum_fsw$time-26,0) # center at lock down 
sum_fsw$inter2 = ifelse(sum_fsw$Low==1,sum_fsw$time-32,0)
sum_fsw$HTS_TST_TOTAL=as.integer(sum_fsw$HTS_TST_TOTAL)

sum_fsw$HTS_TST_POS_TOTAL=as.integer(sum_fsw$HTS_TST_POS_TOTAL)
sum_fsw$TX_NEW_TOTAL=as.integer(sum_fsw$TX_NEW_TOTAL)
sum_fsw$PREP_NEW_TOTAL=as.integer(sum_fsw$PREP_NEW_TOTAL)



#
write.xlsx(as.data.frame(sum_fsw), "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/Combined_FSW2022.xlsx")
save(sum_fsw, file = "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/final_fsw.RData")


# For TG ------------------------------
#--------------------------------------
# clean the WRHI dataset for tg 
TG = read_excel("/Users/hereagain/JHU/RA_JHU/COVID19ITS/Data/WRHI_TG.xlsx")
TG$month=format(as.Date(TG$Week), "%Y-%m")
TG$month=as.Date(as.yearmon(TG$month))

names(TG)

tab1=TG%>% dplyr::select(month,High,Low,HTS_TST_TOTAL,HTS_TST_POS_TOTAL,TX_NEW_TOTAL,PREP_NEW_TOTAL)%>%
  group_by(month)%>%
  summarise(HTS_TST_TOTAL=sum(HTS_TST_TOTAL),
            HTS_TST_POS_TOTAL=sum(HTS_TST_POS_TOTAL),
            TX_NEW_TOTAL=sum(TX_NEW_TOTAL),
            PREP_NEW_TOTAL= sum(PREP_NEW_TOTAL),across())%>%
  distinct(month,.keep_all = TRUE)

TG=tab1
TG=TG[-c(31),]


TG=TG%>%filter(month>"2018-10-01")
dim(TG)
TG$time=c(1:29)
which(TG$Low==1)
TG$inter1 = ifelse(TG$High==1,TG$time-16,0) # center at lock down 
which(TG$High==1)
TG$inter2 = ifelse(TG$Low==1,TG$time-22,0)

# update data

TG2 = read_excel("/Users/hereagain/JHU/RA_JHU/COVID19ITS/Data/WRHI_TG2.xlsx")
TG2$month=format(as.Date(TG2$Week), "%Y-%m")
TG2$month=as.Date(as.yearmon(TG2$month))

names(TG)

tab2=TG2%>% dplyr::select(month,High,Low,HTS_TST_TOTAL,HTS_TST_POS_TOTAL,TX_NEW_TOTAL,PREP_NEW_TOTAL)%>%
  group_by(month)%>%
  summarise(HTS_TST_TOTAL=sum(HTS_TST_TOTAL),
            HTS_TST_POS_TOTAL=sum(HTS_TST_POS_TOTAL),
            TX_NEW_TOTAL=sum(TX_NEW_TOTAL),
            PREP_NEW_TOTAL= sum(PREP_NEW_TOTAL),across())%>%
  distinct(month,.keep_all = TRUE)
# deal with date 
TG2 =tab2

dim(TG2)
TG2 = rbind(TG,TG2)
dim(TG2)
library(zoo)
as.data.frame(TG2)
TG2=TG2%>%filter(month>"2019-02-01")
dim(TG2)
TG2$time=c(1:39)
which(TG2$Low==1)
TG2$inter1 = ifelse(TG2$High==1,TG2$time-12,0) # center at lock down 
which(TG2$High==1)

TG2$inter2 = ifelse(TG2$Low==1,TG2$time-17,0)
library(xlsx)
write.xlsx(as.data.frame(sum_fsw), "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/Combined_TG2022.xlsx")
save(TG2, file = "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/final_tg.RData")



# with Newey_west standard errors 
library(biostat3)
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
library(zoo)
load("/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/final_msm.RData")
load( "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/final_fsw.RData")
load( "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/final_tg.RData")

model1_init  = glm.nb(HTS_TST_TOTAL~time+ High+Low+High:inter1+Low:inter2, sum_msm)
sum_msm$res1 <- residuals(model1_init, type = "deviance") 
ggplot(data = sum_msm, mapping = aes(x = time, y = res1,)) +
  geom_point() 
acf(sum_msm$res2)
pacf(sum_msm$res2)
nb1 =  glm.nb(HTS_TST_POS_TOTAL~time+ High+ Low+High:inter1+ Low:inter2,sum_msm)
sum_msm$res2<- residuals(nb1, type = "deviance") 
summary(model1_init)

lincom(model = model1_init,c("time + High:inter1"),eform = T)[2]

## Calculate Newey-West standard errors using lag 3
help("NeweyWest")

est <- exp(c(coef(model1_init)["High"],coef(model1_init)["Low"],coef(model1_init)["time"],coef(model1_init)["High:inter1"], coef(model1_init)["Low:inter2"] )) 
se1 <- sqrt(diag(NeweyWest(model1_init, prewhite = F, lag = 3)))["High"]
se2 <- sqrt(diag(NeweyWest(model1_init, prewhite = F, lag = 3)))["Low"]
# for trend 
#se4 <- sqrt(diag(NeweyWest(model1_init, prewhite = F, lag = 3)))["High:inter1"]
#se5 <- sqrt(diag(NeweyWest(model1_init, prewhite = F, lag = 3)))["Low:inter2"]
se3 <- sqrt(diag(NeweyWest(model1_init, prewhite = F, lag = 3)))["time"]

lb <- est[c(1:3)] * exp(-1.96 * c(se1, se2, se3))
trend_lb <- as.numeric(c(lincom(model = model1_init,c("time + High:inter1"),eform = T)[2],lincom(model = model1_init,c("time + Low:inter2"),eform = T)[2]))
lb<- c(lb,trend_lb)
ub <- est [c(1:3)]* exp(1.96 * c(se1, se2, se3))
trend_ub <- as.numeric(c(lincom(model = model1_init,c("time + High:inter1"),eform = T)[3],lincom(model = model1_init,c("time + Low:inter2"),eform = T)[3]))
ub<- c(ub,trend_ub)
table <- cbind(round(est, digits = 3), round(lb, digits = 3), round(ub, digits = 3)) 


Newey_result <- function(model1){
  est <- exp(c(coef(model1)["High"],coef(model1)["Low"],coef(model1)["time"],coef(model1)["High:inter1"]+coef(model1)["time"], coef(model1)["Low:inter2"]+coef(model1)["time"] ))
  # For adjusted 
  # for se 
  se1 <- sqrt(diag(NeweyWest(model1, prewhite = F, lag = 3)))["High"]
  se2 <- sqrt(diag(NeweyWest(model1, prewhite = F, lag = 3)))["Low"]
 # se4 <- sqrt(diag(NeweyWest(model1, prewhite = F, lag = 3)))["High:inter1"]
  #se5 <- sqrt(diag(NeweyWest(model1, prewhite = F, lag = 3)))["Low:inter2"]
  se3 <- sqrt(diag(NeweyWest(model1, prewhite = F, lag = 3)))["time"]
  
  lb <- round(est[c(1:3)] * exp(-1.96 * c(se1, se2, se3)),2)
  trend_lb <- as.numeric(c(lincom(model = model1,c("time + High:inter1"),eform = T)[2],lincom(model = model1,c("time + Low:inter2"),eform = T)[2]))
  lb<- round(c(lb,trend_lb),2)
  ub <- est [c(1:3)]* exp(1.96 * c(se1, se2, se3))
  trend_ub <- as.numeric(c(lincom(model = model1,c("time + High:inter1"),eform = T)[3],lincom(model = model1,c("time + Low:inter2"),eform = T)[3]))
  ub<- round(c(ub,trend_ub),2)
  est2 <- round(est,2)
  
  irr1 = paste(est2[1]," (",lb[1],",",ub[1],")")
  irr2 = paste(est2[2]," (",lb[2],",",ub[2],")")
  beta_1 =  paste(est2[3]," (",lb[3],",",ub[3],")")
  
  trend1 = paste(est2[4]," (",lb[4],",",ub[4],")")
  trend2 = paste(est2[5]," (",lb[5],",",ub[5],")")
  result = as.data.frame(cbind(irr1,irr2,beta_1,trend1,trend2))
  names(result)<- c("IRR1","IRR2","Pre-lock down Trend","Post lock down1 Trend","Post lock down2 Trend")
  return(result)
  
}
exp(coef(model1_init )["High:inter1"]+coef(model1_init )["time"])

combine_nwresults = function(mod1,mod2,mod3,mod4,mod5,mod6){
  hiv =Newey_result(mod1)
  pos = Newey_result(mod2)
  adjusted_pos = Newey_result(mod3)
  prep = Newey_result(mod4)
  tx = Newey_result(mod5)
  adjusted_art = Newey_result(mod6)
  result <- rbind(hiv,pos,adjusted_pos,prep,tx,adjusted_art)
  row.names(result)<- c("HIV test",'POS tests',"Adjusted Pos","Prep","ART","Adjusted ART")
  return(result)
}
# get P 

# seasonal adjusted 
# for seanonal adjusted model 


msm_re1=combine_nwresults(model1_init,nb1,nb2,model3,tx1,tx2)

# seasonal adjusted 
msm_seas=combine_nwresults(model_seas1,nb_seas1,nb_seas2,model_seas3,tx1_season,tx2_season)
summary(model1_init)
coef(model_seas1)
coef(model1_init)
summary(model_seas1)

msm_result=rbind(msm_re1,msm_seas)  

#
fsw_re1=combine_nwresults(sum_fsw_mod1,sum_fsw_pos1,sum_fsw_pos2,sum_fsw_prep22,sum_fsw_art1,sum_fsw_art2)
# seasonal adjusted 
fsw_seas=combine_nwresults(sum_fsw_seas1,sum_fsw_pos_sea,sum_fsw_pos_sea,sum_fsw_nb_sea3,sum_fsw_nb_sea4,sum_fsw_nb_sea5)

fsw_result=rbind(fsw_re1,fsw_seas)

tg_re1=combine_nwresults(TG2_test1,TG2_pos_sea ,TG2_pos_sea2,TG2_prep2 ,TG2_art1,TG2_art2)
# seasonal adjusted 
tg_seas=combine_nwresults(TG2_seas1,TG2_pos_sea,TG2_pos_sea2 ,TG2_prep_sea,TG2_sea4,TG2_nb_sea4)

tg_result=rbind(tg_re1,tg_seas)


library(xlsx)

write.xlsx(msm_result, "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Final_NWadjuested_Summary_1015_v3.xlsx", sheetName = "MSM", 
           col.names = TRUE, row.names = TRUE, append = FALSE,showNA = FALSE)
write.xlsx(fsw_result, "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Final_NWadjuested_Summary_1015_v3.xlsx", sheetName = "FSW", 
           col.names = TRUE, row.names = TRUE, showNA = FALSE,append=TRUE)
write.xlsx(tg_result, "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Final_NWadjuested_Summary_1015_v3.xlsx", sheetName = "TG", 
           col.names = TRUE, row.names = TRUE, showNA = FALSE,append=TRUE)


# ITS project figures update 
library(ggplot2)
# load data 
load("/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/final_msm.RData")
load( "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/final_fsw.RData")
load( "/Users/hereagain/JHU/RA_JHU/COVID19ITS/Updated_data/final_tg.RData")
TG2
# for MSM 
Get_data = function(mod1,mod2,mod3,dt){
  dat = data.frame(month=rep(sum_msm$month,3),
                   Models= rep(c("Model predicted" ,"Adjusted for seasonality","Counterfactual"),each=37),
                   values= c(mod1$fitted.values,predict(loess(mod2$fitted.values ~ sum_msm$time) ),exp(predict(mod3, dt) )))
  
  return(dat)
  
}


Get_figure <- function(dat1,dat2){
  figure=dat2%>%
    ggplot(aes(x=month,y=values,color=Models,linetype=Models))+
    #geom_line(color = "steelblue")+#linetype="dotdash")+
    geom_point(aes(x=month,y=rep(dat1,3),group=1),color = "grey")+#"#FC4E07" red
    geom_line(size=1)+
    theme_bw()+
    scale_color_manual(
      values =c( "steelblue","darkorange","darkgreen"))+
    scale_linetype_manual(values = c(2,3,1))+
    
    
    xlab("Calendar time")+
    
    scale_x_continuous(breaks=c(as.numeric(sum_msm$month[1]),as.numeric(sum_msm$month[10]),as.numeric(sum_msm$month[15]),as.numeric(sum_msm$month[28]),as.numeric(sum_msm$month[37])), label=c("Jun-2019", "Mar-2020","Aug-2020","Sep-2021","Jun-2022"))+
    geom_vline(xintercept = as.numeric(sum_msm$month[10]),color="darkblue",linetype=2)+
    geom_vline(xintercept = as.numeric(sum_msm$month[15]),color="darkblue",linetype=2)+
    geom_vline(xintercept = as.numeric(sum_msm$month[28]),color='darkgrey',linetype='dashed')+
    theme_stata()
  # chan
  return(figure)
}

# HIV 
model1_init  = glm.nb(HTS_TST_TOTAL~time+ High+Low+High:inter1+Low:inter2, sum_msm)

model_seas1 =  glm.nb(HTS_TST_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,37),sum_msm)
summary(model1_init)
 

# counterfactual 

sum_msm_new<- as.data.frame(cbind(time = sum_msm$time, High = rep(0),Low=rep(0),inter1=rep(0),inter2=rep(0)))

dat = Get_data(model1_init,model_seas1,model1_init,sum_msm_new)
test = Get_figure(sum_msm$HTS_TST_TOTAL,dat)
test+annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.4,fill="#E7B800")+
  ggtitle("HIV TOTAL TESTS Among MSM June 2019-June 2022")+
  annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.4,fill="#E7B800")+
  ggtitle("HIV TOTAL TESTS Among MSM June 2019-June 2022")+
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-06-01"),ymin = 0,ymax=Inf,alpha=.2,fill="#E7B800")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-06-01"),y=3500,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-10-01"),y=3500,label="After High level",size=5)+
  scale_y_continuous(limits = c(0,10000))+
  geom_hline(yintercept = 9496,color="darkblue",size=0.5,linetype="dashed")+
  geom_text(aes( as.Date("2019-06-01"),9496, label = 9500, vjust = -1), size = 3,color="black")
  

test=test+annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.4,fill="#E7B800")+
  ggtitle("HIV TOTAL TESTS Among MSM June 2019-June 2022")+
  annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.4,fill="#E7B800")+
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-06-01"),ymin = 0,ymax=Inf,alpha=.2,fill="#E7B800")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-06-01"),y=3500,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-10-01"),y=3500,label="After High level",size=5)+
  scale_y_continuous(limits = c(0,10000))+
  geom_hline(yintercept = 9496,color="darkblue",size=0.5,linetype="dashed")+#+
  annotate("text",x=as.Date("2021-09-01"),y=9650,label="KPIF End Date",size=3,color='black')+
  ylab('HIV Total Tests')
  #geom_text(aes( as.Date("2019-06-01"),9496, label = 9500, vjust = -1), size = 3,color="black")

ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/msm_test.pdf',test,height=5,width=8)
# case finding 

nb1 =  glm.nb(HTS_TST_POS_TOTAL~time+ High+ Low+High:inter1+ Low:inter2,sum_msm)
sum_msm = sum_msm%>%group_by(month)%>%
  summarise(positivity= HTS_TST_POS_TOTAL/HTS_TST_TOTAL,across())
sum_msm$positivity
nb11=  glm.nb(positivity~time+ High+ Low+High:inter1+ Low:inter2,sum_msm)

# add offset 
nb2 = glm.nb(HTS_TST_POS_TOTAL~time+ High+ Low+High:inter1+ Low:inter2+ offset(log(HTS_TST_TOTAL)),sum_msm)

AIC(nb1,nb2)
nb_seas1 =  glm.nb(HTS_TST_POS_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,37), sum_msm)
nb_seas2 =  glm.nb(HTS_TST_POS_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,37)+offset(log(HTS_TST_TOTAL)), sum_msm)


sum_msm_new<- as.data.frame(cbind(time = sum_msm$time, High = rep(0),Low=rep(0),inter1=rep(0),inter2=rep(0),HTS_TST_TOTAL=sum_msm$HTS_TST_TOTAL) )

nbpred2 <- exp(predict(nb2, sum_msm_new) )


dat = Get_data(nb1,nb_seas1,nb1,sum_msm_new)

# 

# plot 

pos = Get_figure(sum_msm$HTS_TST_POS_TOTAL,dat)
pos=pos+ annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.6,fill="#E7B800")+
  ggtitle("HIV TOTAL POSITIVE TESTS Among MSM June 2019-June 2022")+
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-06-01"),ymin = 0,ymax=Inf,alpha=.2,fill="#E7B800")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-06-01"),y=600,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-10-01"),y=600,label="After High level",size=5)+
  geom_hline(yintercept = 764,color="red",size=0.5,linetype="dashed")+
  ylab("HIV Case Finding")+
  scale_y_continuous(limits = c(0,800))+
  geom_hline(yintercept = 764,color="darkblue",size=0.5,linetype="dashed")+
  geom_text(aes( as.Date("2019-06-01"),764, label = 770, vjust = -1), size = 3,color="black")+
  annotate("text",x=as.Date("2021-09-01"),y=780,label="KPIF End Date",size=3,color='black')

#geom_text(aes( as.Date("2019-06-01"),9496, label = 9500, vjust = -1), size = 3,color="black")

ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/msm_positve.pdf',pos,height=5,width=8)

# TX

tx1 = glm.nb(TX_NEW_TOTAL~time+ High+ Low+High:inter1+ Low:inter2,sum_msm)
# add offset 
tx2 = glm.nb(TX_NEW_TOTAL~time+ High+ Low+High:inter1+ Low:inter2+ offset(log(HTS_TST_POS_TOTAL)),sum_msm)


tx_seas1 =  glm.nb(TX_NEW_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,37), sum_msm)
tx_seas2 =  glm.nb(TX_NEW_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,37)+offset(log(HTS_TST_POS_TOTAL)), sum_msm)
#tx_seas3 =  glm.nb(ART_pro~time+ High+ Low+High:inter1+ Low:inter2+harmonic(time,2,37), sum_msm)


# counterfactual 

sum_msm_new2<- as.data.frame(cbind(time = sum_msm$time, High = rep(0),Low=rep(0),inter1=rep(0),inter2=rep(0),HTS_TST_POS_TOTAL=sum_msm$HTS_TST_POS_TOTAL) )

tx_dat1 = Get_data(tx1,tx_seas1,tx1,sum_msm_new2)
tx_dat2 = Get_data(tx2,tx_seas2,tx2,sum_msm_new2)
tx_dat11 = Get_data(tx11,tx_seas3,tx11,sum_msm_new2)

art1 = Get_figure(sum_msm$TX_NEW_TOTAL,tx_dat1)
art1=art1 + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.4,fill="purple")+
  ggtitle("TX NEW Among MSM June 2019-June 2022")+
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-06-01"),ymin = 0,ymax=Inf,alpha=.2,fill="purple")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-06-01"),y=300,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-10-01"),y=300,label="After High level",size=5)+
  geom_hline(yintercept = 428,color="darkblue",size=0.5,linetype="dashed")+
  ylab("ART Newly Initiated")+
  scale_y_continuous(limits = c(0,450),breaks = seq(0,450,by=50))  +
  geom_text(aes( as.Date("2019-07-01"),428,label = 428, vjust = -1), size = 3,color="black") +
  annotate("text",x=as.Date("2021-09-01"),y=435,label="KPIF End Date",size=3,color='black')
  
ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/msm_art.pdf',art1,height=5,width=8)
#geom_text(aes( as.Date("2019-06-01"),9496, label = 9500, vjust = -1), size = 3,color="black")



# PreP

model3_init  = glm.nb(PREP_NEW_TOTAL~time+ High+ Low+High:inter1+ Low:inter2,sum_msm)

summary(model3_init)
model_seas3 =  glm.nb(PREP_NEW_TOTAL~time+ High+ Low+High:inter1+ Low:inter2+harmonic(time,2,36), sum_msm)


curve_values3 <- loess(model_seas3$fitted.values ~ sum_msm$time) 



prep_dat = Get_data(model3_init,model_seas3,model3_init,sum_msm_new2)

prep1 = Get_figure(sum_msm$PREP_NEW_TOTAL,prep_dat)

prep1=prep1+annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.6,fill="lightgreen")+
  ggtitle("PREP NEW Among MSM June 2019-June 2022")+
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-06-01"),ymin = 0,ymax=Inf,alpha=.2,,fill="lightgreen")+
  theme(plot.title = element_text(hjust=0.5,face="bold"))+
  annotate("text",x=as.Date("2020-06-15"),y=1200,label="High Level",size=5)+
  annotate("text",x=as.Date("2020-11-15"),y=1200,label="After High level",size=5)+
  
  scale_y_continuous(limits = c(0,2500))+
  geom_hline(yintercept = 2265,color="darkblue",size=0.5,linetype="dashed")+
  geom_text(aes( as.Date("2019-06-01"),2265,label = 2200, vjust = -1), size = 3,color="black")+
  annotate("text",x=as.Date("2021-09-01"),y=2300,label="KPIF End Date",size=3,color='black')

ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/msm_prep.pdf',prep1,height=5,width=8)
#geom_text(aes( as.Date("2019-06-01"),9496, label = 9500, vjust = -1), size = 3,color="black")


# ----------------------------------------
# FSW-------------------------------------
#-----------------------------------------
Get_data = function(mod1,mod2,mod3,dt){
  dat = data.frame(month=rep(sum_fsw$month,3),
                   Models= rep(c("Model predicted" ,"Adjusted for seasonality","Counterfactual"),each=51),
                   values= c(mod1$fitted.values,predict(loess(mod2$fitted.values ~ sum_fsw$time) ),exp(predict(mod3, dt) )))
  
  return(dat)
  
}

sum_fsw$month
Get_figure <- function(dat1,dat2){
  figure=dat2%>%
    ggplot(aes(x=month,y=values,color=Models,linetype=Models))+
    #geom_line(color = "steelblue")+#linetype="dotdash")+
    geom_point(aes(x=month,y=rep(dat1,3),group=1),color = "grey")+#"#FC4E07" red
    geom_line(size=1)+
    theme_bw()+
    scale_color_manual(
      values =c( "steelblue","darkorange","darkgreen"))+
    scale_linetype_manual(values = c(2,3,1))+
    
    
    xlab("Calendar time")+
    
    scale_x_continuous(breaks=c(as.numeric(sum_fsw$month[1]),as.numeric(sum_fsw$month[27]),as.numeric(sum_fsw$month[32]),as.numeric(sum_fsw$month[45]),as.numeric(sum_fsw$month[51])), label=c("Jan-2018", "Mar-2020","Aug-2020","Sep-2021","Mar-2022"))+
    geom_vline(xintercept = as.numeric(sum_fsw$month[27]),color="darkblue",linetype=2)+
    geom_vline(xintercept = as.numeric(sum_fsw$month[32]),color="darkblue",linetype=2)+
    geom_vline(xintercept = as.numeric(sum_fsw$month[45]),color='grey',linetype='dashed')+
    
    theme_stata()
  # chan
  return(figure)
}

fsw_test1  = glm.nb(HTS_TST_TOTAL~time+ High+Low+High:inter1+Low:inter2, sum_fsw)

fsw_seas1 =  glm.nb(HTS_TST_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,51), sum_fsw)


cdc_curve1 <- loess(fsw_seas1$fitted.values ~ sum_fsw$time)
cdc_new<- as.data.frame(cbind(time = sum_fsw$time, High = rep(0),Low=rep(0),inter1=rep(0),inter2=rep(0))) 

cdc_pred1 <- exp(predict(fsw_test1, cdc_new) )

dat1 = Get_data(fsw_test1,fsw_seas1,fsw_test1,cdc_new)

test1 = Get_figure(sum_fsw$HTS_TST_TOTAL,dat1)




test1=test1 + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = -Inf,ymax=Inf,alpha=.3,fill="darkorange")+
  
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = -Inf,ymax=Inf,alpha=.1,fill="darkorange")+
  ggtitle("HIV Testing Among FSW Jan 2018-Mar 2022 (WRHI+CDC)")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  
  ylab("HIV Total Tests")+
  annotate("text",x=as.Date("2020-05-15"),y=1200,label="High Level",size=6)+
  annotate("text",x=as.Date("2021-09-15"),y=1200,label="After High level",size=6)+
  annotate("text",x=as.Date("2021-09-01"),y=4500,label="KPIF End Date",size=3,color='black')
  


ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/fsw_test.pdf',test1,height=5,width=8)

  

# POS 

fsw_pos1  = glm(HTS_TST_POS_TOTAL~time+ High+Low+High:inter1+Low:inter2,family = "poisson",  sum_fsw)

fsw_sea2 =  glm(HTS_TST_POS_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,53),family = "poisson",  sum_fsw)


cdc_curve1 <- loess(fsw_sea2$fitted.values ~ sum_fsw$time)
cdc_new<- as.data.frame(cbind(time = sum_fsw$time, High = rep(0),Low=rep(0),inter1=rep(0),inter2=rep(0))) 

cdc_pred1 <- exp(predict(fsw_pos1, cdc_new) )

dat2 = Get_data(fsw_pos1,fsw_sea2,fsw_pos1,cdc_new)
pos1 = Get_figure(sum_fsw$HTS_TST_POS_TOTAL,dat2)
pos1 + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = -Inf,ymax=Inf,alpha=.3,fill="darkorange")+
  
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = -Inf,ymax=Inf,alpha=.1,fill="darkorange")+
  ggtitle("HIV Positive cases Among FSW Jan 2018-Mar 2022 (WRHI+CDC)")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-05-15"),y=300,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-09-15"),y=300,label="After High level",size=5)+
  ylab("HIV Case Finding")+
  scale_y_continuous(limits = c(0,400))+
  annotate("text",x=as.Date("2021-09-01"),y=380,label="KPIF End Date",size=3,color='black')



ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/fsw_pos.pdf',pos1,height=5,width=8)



  
which(sum_fsw$month=="2021-08-01")


# positivity 
sum_fsw=sum_fsw%>%group_by(month)%>%
  summarise(positivity=HTS_TST_POS_TOTAL/HTS_TST_TOTAL,across())
sum_fsw$positivity
fsw_pos  = glm(positivity~time+ High+Low+High:inter1+Low:inter2,family = "poisson", sum_fsw)

fsw_sea =  glm(positivity~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,53),family = "poisson", sum_fsw)


dat3 = Get_data(fsw_pos,fsw_sea,fsw_pos,cdc_new)
pos3=Get_figure(sum_fsw$positivity,dat3)
pos3 + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = -Inf,ymax=Inf,alpha=.3,fill="darkorange")+
  
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = -Inf,ymax=Inf,alpha=.1,fill="darkorange")+
  ggtitle("HIV Positivity Among FSW Jan 2018-Mar 2022 (WRHI+CDC)")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-05-15"),y=0.6,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-09-15"),y=0.6,label="After High level",size=5)+
  ylab("HIV Positivity (%)")+
  scale_y_continuous(limits = c(0,0.8))
geom_hline(yintercept = 0.2,color="darkblue",size=0.5,linetype="dashed")+
  

# Prep 
fsw_prep1  = glm(PREP_NEW_TOTAL~time+ High+Low+High:inter1+Low:inter2,family = "poisson", sum_fsw)
fsw_prep2  = glm.nb(PREP_NEW_TOTAL~time+ High+Low+High:inter1+Low:inter2, sum_fsw)
summary(fsw_prep1)
fsw_sea3 =  glm(PREP_NEW_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,53),family = "poisson", sum_fsw)

fsw_nb_sea3 =  glm.nb(PREP_NEW_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,53),sum_fsw)

cdc_curve3 <- loess(fsw_sea3$fitted.values ~ sum_fsw$time)
cdc_new<- as.data.frame(cbind(time = sum_fsw$time, High = rep(0),Low=rep(0),inter1=rep(0),inter2=rep(0))) 

cdc_pred3 <- exp(predict(fsw_prep1, cdc_new) )
dat3 = Get_data(fsw_prep2,fsw_sea3,fsw_prep2,cdc_new)
prep1 = Get_figure(sum_fsw$PREP_NEW_TOTAL,dat3)
prep1=prep1 + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = -Inf,ymax=Inf,alpha=.3,fill="lightgreen")+
  
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = -Inf,ymax=Inf,alpha=.1,fill="lightgreen")+
  ggtitle("PREP New Among FSW Jan 2018-Mar 2022 (WRHI+CDC)")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-05-15"),y=450,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-09-15"),y=450,label="After High level",size=5)+
  ylab("PREP NEW TOTAL")+geom_hline(yintercept = 1050,color="darkblue",size=0.5,linetype="dashed")+
  scale_y_continuous(limits = c(0,1200))+
  geom_text(aes( as.Date("2018-01-01"),1050, label = 1050, vjust = -1), size = 3,color="black")+
  annotate("text",x=as.Date("2021-09-01"),y=1080,label="KPIF End Date",size=3,color='black')



ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/fsw_prep.pdf',prep1,height=5,width=8)



sum_fsw$month
sum_fsw$PREP_NEW_TOTAL
#prep1+scale_y_continuous(limits = c(0,1000),breaks = sort(c(ggplot_build(prep1)$layout$panel_params[[1]]$y.major, 930))) # add value to the hline 

library(AER)
dispersiontest(fsw_prep1)

# very dispersed 

# ART 

fsw_art1  = glm(TX_NEW_TOTAL~time+ High+Low+High:inter1+Low:inter2,family = "poisson", sum_fsw)
fsw_art2  = glm.nb(TX_NEW_TOTAL~time+ High+Low+High:inter1+Low:inter2, sum_fsw)

fsw_sea4 =  glm(TX_NEW_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,52),family = "poisson", sum_fsw)

fsw_nb_sea4 =  glm.nb(TX_NEW_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,52),sum_fsw)

cdc_curve4 <- loess(fsw_sea4$fitted.values ~ sum_fsw$time)

cdc_new<- as.data.frame(cbind(time = sum_fsw$time, High = rep(0),Low=rep(0),inter1=rep(0),inter2=rep(0))) 

cdc_pred4 <- exp(predict(fsw_art1, cdc_new) )


dat4 = Get_data(fsw_art2,fsw_sea4,fsw_art2,cdc_new)
art1 = Get_figure(sum_fsw$TX_NEW_TOTAL,dat4)
art1=art1 + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = -Inf,ymax=Inf,alpha=.3,fill="purple")+
  
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = -Inf,ymax=Inf,alpha=.1,fill="purple")+
  ggtitle("ART NEW Among FSW Jan 2018-Mar 2022 (WRHI+CDC)")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-05-15"),y=450,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-09-15"),y=450,label="After High level",size=5)+
  ylab("ART NEW Numbers")+
  geom_text(aes( as.Date("2018-01-01"),1050, label = 1050, vjust = -1), size = 3,color="black")+
  annotate("text",x=as.Date("2021-09-01"),y=1000,label="KPIF End Date",size=3,color='black')

  
ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/fsw_art.pdf',art1,height=5,width=8)

#------------------------------------------------
#----------------------Transgender people--------
#-----------------------------------------------
# from March -2019 


Get_data = function(mod1,mod2,mod3,dt){
  dat = data.frame(month=rep(TG2$month,3),
                   Models= rep(c("Model predicted" ,"Adjusted for seasonality","Counterfactual"),each=39),
                   values= c(mod1$fitted.values,predict(loess(mod2$fitted.values ~ TG2$time) ),exp(predict(mod3, dt) )))
  
  return(dat)
  
}


Get_figure <- function(dat1,dat2){
  figure=dat2%>%
    ggplot(aes(x=month,y=values,color=Models,linetype=Models))+
    #geom_line(color = "steelblue")+#linetype="dotdash")+
    geom_point(aes(x=month,y=rep(dat1,3),group=1),color = "grey")+#"#FC4E07" red
    geom_line(size=1)+
    theme_bw()+
    scale_color_manual(
      values =c( "steelblue","darkorange","darkgreen"))+
    scale_linetype_manual(values = c(2,3,1))+
    
    
    xlab("Calendar time")+
    
    scale_x_continuous(breaks=c(as.numeric(TG2$month[1]),as.numeric(TG2$month[17]),as.numeric(TG2$month[21]),as.numeric(TG2$month[33]),as.numeric(TG2$month[43])), label=c("Nov-2018", "Mar-2020","Aug-2020","Sep-2021","Mar-2022"))+
    geom_vline(xintercept = as.numeric(TG2$month[17]),color="darkblue",linetype=2)+
    geom_vline(xintercept = as.numeric(TG2$month[21]),color="darkblue",linetype=2)+
    theme_stata()
  # chan
  return(figure)
}

TG2_test1  = glm.nb(HTS_TST_TOTAL~time+ High+Low+High:inter1+Low:inter2, TG2)

TG2_seas1 =  glm.nb(HTS_TST_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,43), TG2)


cdc_curve1 <- loess(TG2_seas1$fitted.values ~ TG2$time)
cdc_new<- as.data.frame(cbind(time = TG2$time, High = rep(0),Low=rep(0),inter1=rep(0),inter2=rep(0))) 

cdc_pred1 <- exp(predict(TG2_test1, cdc_new) )


dat1 = Get_data(TG2_test1,TG2_seas1,TG2_test1,cdc_new)

test1 = Get_figure(TG2$HTS_TST_TOTAL,dat1)




test1=test1 + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = -Inf,ymax=Inf,alpha=.3,fill="darkorange")+
  
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = -Inf,ymax=Inf,alpha=.1,fill="darkorange")+
  ggtitle("HIV Testing Among TG Nov 2018-Mar 2022 (WRHI)")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  
  ylab("HIV Testing Numbers")+
  annotate("text",x=as.Date("2020-05-15"),y=1200,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-09-15"),y=1200,label="After High level",size=5)+
  geom_hline(yintercept = 1682,color="darkblue",size=0.5,linetype="dashed")+
  scale_y_continuous(limits = c(0,2000))+
  #https://stackoverflow.com/questions/12876501/r-ggplot2-labelling-a-horizontal-line-on-the-y-axis-with-a-numeric-value/46851686#46851686
  #scale_y_continuous(limits = c(0,1000))+
  geom_text(aes( as.Date("2018-11-01"),1682, label = 1680, vjust = -1), size = 3,color="black")+
  annotate("text",x=as.Date("2021-09-01"),y=1720,label="KPIF End Date",size=3,color='black')


ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/tg_art.pdf',test1,height=5,width=8)

  

# postive 
TG2_pos1  = glm.nb(HTS_TST_POS_TOTAL~time+ High+Low+High:inter1+Low:inter2,TG2)

TG2_sea2 =  glm.nb(HTS_TST_POS_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,43),TG2)


cdc_curve1 <- loess(TG2_sea2$fitted.values ~ TG2$time)
cdc_new<- as.data.frame(cbind(time = TG2$time, High = rep(0),Low=rep(0),inter1=rep(0),inter2=rep(0))) 

cdc_pred1 <- exp(predict(TG2_pos1, cdc_new) )
TG2$month
TG2$HTS_TST_POS_TOTAL
dat2 = Get_data(TG2_pos1,TG2_sea2,TG2_pos1,cdc_new)
pos1 = Get_figure(TG2$HTS_TST_POS_TOTAL,dat2)
pos1=pos1 + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = -Inf,ymax=Inf,alpha=.3,fill="darkorange")+
  
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = -Inf,ymax=Inf,alpha=.1,fill="darkorange")+
  ggtitle("HIV Positive cases Among TG Nov 2018-Mar 2022 (WRHI)")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-05-15"),y=300,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-11-15"),y=300,label="After High level",size=5)+
  ylab("HIV Case Finding")+
  scale_y_continuous(limits = c(0,400))+
  geom_hline(yintercept = 358,color="darkblue",size=0.5,linetype="dashed")+
  #https://stackoverflow.com/questions/12876501/r-ggplot2-labelling-a-horizontal-line-on-the-y-axis-with-a-numeric-value/46851686#46851686
  #scale_y_continuous(limits = c(0,1000))+
  geom_text(aes( as.Date("2018-11-01"),358, label = 360, vjust = -1), size = 3,color="black")+
  annotate("text",x=as.Date("2021-09-01"),y=365,label="KPIF End Date",size=3,color='black')


ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/tg_pos.pdf',pos1,height=5,width=8)
  


# posivity 

TG2_pos  = glm(positivity~time+ High+Low+High:inter1+Low:inter2,family = "poisson", TG2)

TG2_sea =  glm(positivity~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,43),family = "poisson", TG2)
TG2=TG2%>%group_by(month)%>%
  summarise(positivity=HTS_TST_POS_TOTAL/HTS_TST_TOTAL,across())
TG2$positivity[2]=0
dat3 = Get_data(TG2_pos,TG2_sea,TG2_pos,cdc_new)
pos3=Get_figure(TG2$positivity,dat3)
pos3 + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = -Inf,ymax=Inf,alpha=.3,fill="darkorange")+
  
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = -Inf,ymax=Inf,alpha=.1,fill="darkorange")+
  ggtitle("HIV Positivity Among TG Nov 2018-Mar 2022 (WRHI)")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-05-15"),y=0.6,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-11-15"),y=0.6,label="After High level",size=5)+
  ylab("HIV Positivity (%)")+
  scale_y_continuous(limits = c(0,0.8))+
  geom_hline(yintercept = 0,color="darkblue",size=0.5,linetype="dashed")

# Prep  
TG2_prep1  = glm(PREP_NEW_TOTAL~time+ High+Low+High:inter1+Low:inter2,family = "poisson", TG2)
TG2_prep2  = glm.nb(PREP_NEW_TOTAL~time+ High+Low+High:inter1+Low:inter2, TG2)
summary(TG2_prep1)
TG2_sea3 =  glm(PREP_NEW_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,43),family = "poisson", TG2)

TG2_nb_sea3 =  glm.nb(PREP_NEW_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,30),TG2)

cdc_curve3 <- loess(TG2_nb_sea3$fitted.values ~ TG2$time)
cdc_new<- as.data.frame(cbind(time = TG2$time, High = rep(0),Low=rep(0),inter1=rep(0),inter2=rep(0))) 

cdc_pred3 <- exp(predict(TG2_prep2, cdc_new) )
dat3 = Get_data(TG2_prep2,TG2_nb_sea3,TG2_prep2,cdc_new)
prep1 = Get_figure(TG2$PREP_NEW_TOTAL,dat3)
prep1=prep1 + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = -Inf,ymax=Inf,alpha=.3,fill="lightgreen")+
  
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = -Inf,ymax=Inf,alpha=.1,fill="lightgreen")+
  ggtitle("PREP New Among TG Nov 2018-Mar 2022 (WRHI)")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-05-15"),y=430,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-11-15"),y=430,label="After High level",size=5)+
  ylab("PREP NEW TOTAL")+
  scale_y_continuous(limits = c(0,500))+
  geom_hline(yintercept = 450,color="darkblue",size=0.5,linetype="dashed")+
  #https://stackoverflow.com/questions/12876501/r-ggplot2-labelling-a-horizontal-line-on-the-y-axis-with-a-numeric-value/46851686#46851686
  #scale_y_continuous(limits = c(0,1000))+
  geom_text(aes( as.Date("2018-11-01"),450, label = 450, vjust = -1), size = 3,color="black")+
  annotate("text",x=as.Date("2021-09-01"),y=460,label="KPIF End Date",size=3,color='black')


ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/tg_prep.pdf',prep1,height=5,width=8)
  




TG2$month
TG2$PREP_NEW_TOTAL
#prep1+scale_y_continuous(limits = c(0,1000),breaks = sort(c(ggplot_build(prep1)$layout$panel_params[[1]]$y.major, 930))) # add value to the hline 

library(AER)
dispersiontest(TG2_prep1)
TG2[which(TG2$month>="2019-03-01"),]
# very dispersed 

# ART 

TG2_art1  = glm(TX_NEW_TOTAL~time+ High+Low+High:inter1+Low:inter2,family = "poisson", TG2)
TG2_art2  = glm.nb(TX_NEW_TOTAL~time+ High+Low+High:inter1+Low:inter2, TG2)

TG2_sea4 =  glm(TX_NEW_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,43),family = "poisson", TG2)

TG2_nb_sea4 =  glm.nb(TX_NEW_TOTAL~time+High+Low+High:inter1+Low:inter2+harmonic(time,2,43),TG2)

cdc_curve4 <- loess(TG2_sea4$fitted.values ~ TG2$time)

cdc_new<- as.data.frame(cbind(time = TG2$time, High = rep(0),Low=rep(0),inter1=rep(0),inter2=rep(0))) 

cdc_pred4 <- exp(predict(TG2_art2, cdc_new) )
dat4 = Get_data(TG2_art2,TG2_nb_sea4,TG2_art2,cdc_new)
art1 = Get_figure(TG2$TX_NEW_TOTAL,dat4)
art1=art1 + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = -Inf,ymax=Inf,alpha=.3,fill="purple")+
  
  annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = -Inf,ymax=Inf,alpha=.1,fill="purple")+
  ggtitle("ART NEW Among TG Nov 2018-Mar 2022 (WRHI)")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  annotate("text",x=as.Date("2020-05-15"),y=150,label="High Level",size=5)+
  annotate("text",x=as.Date("2021-11-15"),y=150,label="After High level",size=5)+
  ylab("ART NEW Numbers")+
  scale_y_continuous(limits = c(0,200))+
  geom_hline(yintercept = 180,color="darkblue",size=0.5,linetype="dashed")+
  #https://stackoverflow.com/questions/12876501/r-ggplot2-labelling-a-horizontal-line-on-the-y-axis-with-a-numeric-value/46851686#46851686
  #scale_y_continuous(limits = c(0,1000))+
  geom_text(aes( as.Date("2018-11-01"),180, label = 180, vjust = -1), size = 3,color="black")+
  annotate("text",x=as.Date("2021-09-01"),y=185,label="KPIF End Date",size=3,color='black')


ggsave('/Users/hereagain/JHU/RA_JHU/COVID19ITS/results/final_figure/tg_art.pdf',art1,height=5,width=8)





  











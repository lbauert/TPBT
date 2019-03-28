######## installing the correct packages
install.packages("psych")
install.packages("ggplot2")
install.packages("reshape")
install.packages("dplyr")
install.packages("lme4")
install.packages("nlme")
install.packages("Hmisc")
library(psych)
library(ggplot2)
library(reshape)
library(dplyr)
library(lme4)
library(nlme)

####### Check dataset
dim(bbx3)
names(bbx3)
#######renameing Participant ID to PID, helps later
colnames(bbx3)[colnames(bbx3)=="Participant ID (bbx_###)"] <- "PID"
names(bbx3)
####### Make new datasets per construct
AA <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_aa', 
         'b5_tpbq_usb_aa', 'w2_tpbq_usb_aa')
dim(AA)
names(AA)

IA <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
        'age', 'ethnicity', 'race', 
        'gender', 'w1_tpbq_usb_ia', 
        'b5_tpbq_usb_ia', 'w2_tpbq_usb_ia')
dim(IA)
names(IA)

SN <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_sn', 
         'b5_tpbq_usb_sn', 'w2_tpbq_usb_sn')
dim(SN)
names(SN)

PBC <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_pbc', 
         'b5_tpbq_usb_pbc', 'w2_tpbq_usb_pbc')
dim(PBC)
names(PBC)

BI <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_bi', 
         'b5_tpbq_usb_bi', 'w2_tpbq_usb_bi')
dim(BI)
names(BI)
######## Combining all the split datasets
DL <- list('aa' = AA, 'ia' = IA, 'sn' = SN, 'pbc' = PBC, 'bi'= BI)
head(DL)
######## Function to turn all long datasets to long datasets
lengthen <- function(x){
  x <- as.data.frame(x)
  names(x)<-c('Sweet', 'PID', 'w1_BMI', 'w1_BMI_status', 'age', 'ethnicity', 'race', 'gender', 'T1', 'T2', 'T3')
  x_long<-melt(data = x,
               id.vars = c('PID', 'Sweet','w1_BMI', 'w1_BMI_status', 'age', 'ethnicity', 'race', 'gender'),
               measure.vars = c('T1', 'T2', 'T3'))
  names(x_long)<-c('PID', 'Sweet','w1_BMI', 'w1_BMI_status', 'age', 'ethnicity', 'race', 'gender', 'time','measure')
  return(x_long)
}
######## Applying the long function to the dataset
DL_long <- lapply(DL, lengthen)
DL_long
######## Function for fitting the data to a model
fitter<-function(x){
  x$Sweet<-as.factor(x$Sweet)
  intercept<-gls(measure~1, data=x, method="ML", na.action=na.exclude)
  randomIntercept<-lme(measure~1, data=x, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  timeRI<-update(randomIntercept, .~.+time)
  timeRS<-update(timeRI, random=~time|PID)
  timeCov<-update(timeRS, .~.+Sweet)
  results <-list("basic"=summary(timeCov), "better?"= anova(intercept, randomIntercept, timeRI, timeRS, timeCov))
  return(results)
}
lapply(DL_long, fitter)
######## Graphing the data
plotter<-function(x){
  x$Sweet<-as.factor(x$Sweet)
  plot1 <- ggplot(x, aes(time, measure, colour = Sweet))
  plot2<-plot1 + stat_summary(
    fun.y = mean, geom = "point") + stat_summary(
      fun.y =mean, geom = "line", aes(group= Sweet)) + stat_summary(
        fun.data = mean_cl_boot, geom   = "errorbar", width = 0.2) + labs(
          x = "time", y = "measure" , colour = "Sweet")    
  return(plot2)
}
lapply(DL_long, plotter)
####### test normality
AA <- as.data.frame(AA)
AA_long<-melt(data = AA,
              id.vars = c('PID', 'Sweet','w1_BMI', 'w1_BMI_status', 'age', 'ethnicity', 'race', 'gender'),
              measure.vars = c('w1_tpbq_ssb_aa', 'b5_tpbq_ssb_aa', 'w2_tpbq_ssb_aa'))
names(AA_long)<-c(
  'PID', 'Sweet','w1_BMI', 'w1_BMI_status', 'age', 'ethnicity', 'race', 'gender', 'time_bev','attitude')
dim(AA_long)


intercept<-gls(attitude~1, data=AA_long, method="ML", na.action=na.exclude)
randomIntercept<-lme(
  attitude~1, data=AA_long, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-update(randomIntercept, .~.+time_bev)
timeRS<-update(timeRI, random=~time_bev|PID)
timeCov<-update(timeRS, .~.+Sweet)
qqnorm(timeCov, ~ resid(., type = "p"))
qqnorm(timeCov, ~ranef(.))
plot.lme(timeCov)

######## installing the correct packages

# install.packages("psych")
# install.packages("ggplot2")
# install.packages("reshape")
# install.packages("dplyr")
# install.packages("lme4")
# install.packages("nlme")
# install.packages("Hmisc")
# install.packages("reghelper")

require("reghelper")
require("lattice")
require("Hmisc")
library(psych)
library(ggplot2)
library(reshape)
library(dplyr)
library(lme4)
library(nlme)
library(readxl)

# please make sure to read in your data table!
# other wise I can't really replicate your code


 bbx3<-read.table("~/Google Drive/Lias_stuff/data/variables_interest.csv", sep=",", header=T, fill = T)
 bbx3$Sweet<-as.factor(bbx3$Sweet)


#what the computer had been running for the data
#bbx3<- read_excel("~/Google Drive/Lias_stuff/data/lia_bbxdata_020119_full.xlsx", sheet = "calculated_data3")
#####I tried to link it directly to the drive sheet but had issues so if you know how to get this sheet from google 
#####than it would be the same data I am using.  

####### Check dataset
dim(bbx3)
names(bbx3)
#######renameing Participant ID to PID, helps later
#colnames(bbx3)[colnames(bbx3)=="Participant ID (bbx_###)"] <- "PID"
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
# DL_long
######## Function for fitting the data to a model
fitter<-function(x){
  x$Sweet<-as.factor(x$Sweet)
  intercept<-gls(measure~1, data=x, method="ML", na.action=na.exclude)
  randomIntercept<-lme(measure~1, data=x, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  timeRI<-lme(measure~1+time, data=x, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  timeRS<-lme(measure~1+time, data=x, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  timeCov<-lme(measure~1+time*Sweet, data=x, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  results <-list("model"=timeCov, "basic"=summary(timeCov), "better?"= anova(intercept, randomIntercept, timeRI, timeRS, timeCov))
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
########testing potential significant##########
plotter(DL_long$bi)
fitter(DL_long$bi)

plotter(DL_long$aa)
fitter(DL_long$aa)
###### test normality
model_list2<-fitter(DL_long$bi)
model_list2[1]
Plot.Model.F.Linearity <-plot(resid(model_list2$model), DL_long$bi$measure)

DL_long$bi$timeCov.Res<- residuals(model_list2$model) #extracts the residuals and places them in a new column in our original data table
DL_long$bi$Abs.timeCov.Res <-abs(DL_long$bi$timeCov.Res) #creates a new column with the absolute value of the residuals
DL_long$bi$timeCov.Res2 <- DL_long$bi$Abs.timeCov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(timeCov.Res2 ~ PID, data=DL_long$bi) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

Plot.timeCov <- plot(model_list2$model) #creates a fitted vs residual plot
Plot.timeCov

outcome<-DL_long$bi$measure[complete.cases(DL_long$bi$measure)]
length(outcome)
plot(model_list2$model)

qqnorm(resid(model_list2$model))
qqline(resid(model_list2$model))
####### getting standardized betas

beta (model_list2$model)


###### BMI analysis
mean (bbx3$w1_BMI, na.rm=TRUE)
sd(bbx3$w1_BMI, na.rm=TRUE)
t.test(bbx3$w1_BMI~bbx3$Sweet)

fitter_BMI<-function(x){
  x$Sweet<-as.factor(x$Sweet)
  intercept<-gls(measure~1, data=x, method="ML", na.action=na.exclude)
  randomIntercept<-lme(measure~1, data=x, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  timeRI<-lme(measure~1+time, data=x, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  timeRS<-lme(measure~1+time, data=x, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  timeCov<-lme(measure~1+time*Sweet, data=x, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  time_BMICov<-lme(measure~1+time*Sweet*w1_BMI, data=x, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  results <-list("model"=time_BMICov, "basic"=summary(time_BMICov), "better?"= anova(timeCov, time_BMICov))
  return(results)
}

lapply(DL_long, fitter_BMI)
####### interesting results
fit<-fitter_BMI(DL_long$bi)
fit
beta(fit$model)

plotter_bmi<-function(x){
  x$Sweet<-as.factor(x$Sweet)
  plot1<-ggplot(x, aes(w1_BMI,measure, color=Sweet)) +
    geom_point(shape=1)+
    facet_grid(~time)+
    geom_smooth(method=lm,   # Add linear regression lines
                se=T)    # Don't add shaded confidence region
  return(plot1)
}
names(DL_long)
plotter_bmi(DL_long$bi)


DL_long$bi$Sweet<-as.factor(DL_long$bi$Sweet)
plot1<-ggplot(DL_long$bi, aes(time, measure, color=Sweet)) + geom_boxplot()
plot1
describeBy(DL_long$bi$measure, DL_long$bi$time)
describeBy(DL_long$bi$measure, DL_long$bi$Sweet)

######packages

install.packages("psych")
install.packages("ggplot2")
install.packages("reshape")
install.packages("dplyr")
install.packages("lme4")
install.packages("nlme")
install.packages("Hmisc")
install.packages("reghelper")
install.packages("bestNormalize")
install.packages("car")

require("reghelper")
require("lattice")
require("Hmisc")
library(psych)
library(ggplot2)
library(reshape)
library(dplyr)
library(lme4)
library(nlme)
library (bestNormalize)
library(car)


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
colnames(bbx3)[colnames(bbx3)=="Participant ID (bbx_###)"] <- "PID"
names(bbx3)
####### Make new datasets per construct
AA <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_ssb_aa', 
          'w2_tpbq_ssb_aa')
dim(AA)
names(AA)

UAA <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_aa', 
         'w2_tpbq_usb_aa')
dim(AA)
names(AA)


IA <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_ssb_ia', 
          'w2_tpbq_ssb_ia')
dim(IA)
names(IA)

UIA <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_ia', 
         'w2_tpbq_usb_ia')

SN <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_ssb_sn', 
          'w2_tpbq_ssb_sn')
dim(SN)
names(SN)
USN <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_sn', 
         'w2_tpbq_usb_sn')

PBC <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_ssb_pbc', 
          'w2_tpbq_ssb_pbc')
dim(PBC)
names(PBC)
UPBC <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_pbc', 
         'w2_tpbq_usb_pbc')

BI <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_ssb_bi', 
          'w2_tpbq_ssb_bi')
dim(BI)
names(BI)
UBI <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_bi', 
         'w2_tpbq_usb_bi')
######## Combining all the split datasets
DL <- list('aa' = AA, 'uaa'=UAA, 'ia' = IA,'uia'=UIA, 'sn' = SN,'usn'= USN, 'pbc' = PBC, 'upbc'=UPBC, 'bi'= BI, 'ubi'=UBI)
head(DL)
######## Function to turn all long datasets to long datasets
lengthen <- function(x){
  x <- as.data.frame(x)
  names(x)<-c('Sweet', 'PID', 'w1_BMI', 'w1_BMI_status', 'age', 'ethnicity', 'race', 'gender', 'T1', 'T2')
  x_long<-melt(data = x,
               id.vars = c('PID', 'Sweet','w1_BMI', 'w1_BMI_status', 'age', 'ethnicity', 'race', 'gender'),
               measure.vars = c('T1', 'T2'))
  names(x_long)<-c('PID', 'Sweet','w1_BMI', 'w1_BMI_status', 'age', 'ethnicity', 'race', 'gender', 'time','measure')
  return(x_long)
}
######## Applying the long function to the dataset
DL_long <- lapply(DL, lengthen)
DL_long

AA_long<-lengthen(AA)
AA_long

UAA_long<-lengthen(UAA)
UAA_long

##### linear mixed model
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
fitter(AA_long)

lapply(DL_long, fitter)
####### Plotter function
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
####### Interesting data: pbc, ubc, and ubi
####### normality check
model_list2<-fitter(DL_long$ubi)
model_list2[1]
Plot.Model.F.Linearity <-plot(resid(model_list2$model), DL_long$ubi$measure)

DL_long$ubi$timeCov.Res<- residuals(model_list2$model) #extracts the residuals and places them in a new column in our original data table
DL_long$ubi$Abs.timeCov.Res <-abs(DL_long$ubi$timeCov.Res) #creates a new column with the absolute value of the residuals
DL_long$ubi$timeCov.Res2 <- DL_long$ubi$Abs.timeCov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(timeCov.Res2 ~ PID, data=DL_long$ubi) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

Plot.timeCov <- plot(model_list2$model) #creates a fitted vs residual plot
Plot.timeCov

outcome<-DL_long$ubi$measure[complete.cases(DL_long$ubi$measure)]
length(outcome)
plot(model_list2$model)

qqnorm(resid(model_list2$model))
qqline(resid(model_list2$model))

###### transformations for... upbc
DL_long$upbc$measure.1 <- DL_long$upbc$measure+1
DL_long$upbc$measure.log <- log10(DL_long$upbc$measure.1)
DL_long$upbc$measure.ln <- log(DL_long$upbc$measure.1)
DL_long$upbc$measure.exp <- exp(DL_long$upbc$measure) 
DL_long$upbc$measure.sqrt <- sqrt(DL_long$upbc$measure)
DL_long$upbc$measure.cb <- abs(DL_long$upbc$measure)^(1/3)
DL_long$upbc$measure.in <- abs(DL_long$upbc$measure)^(-1)
DL_long$upbc$measure_tuk = 
  transformTukey(DL_long$upbc$measure,
                 plotit=FALSE)
yeojohnson(DL_long$upbc$measure)
powerTransform(DL_long$upbc$measure, family = "yjPower")
DL_long$upbc$measure.yj <- yjPower(DL_long$upbc$measure, -0.4723849 )
bestNormalize(DL_long$upbc$measure)

DL_long$upbc$measure<-na.omit(DL_long$upbc$measure)
DL_long$upbc$measure.bc <-boxcox(DL_long$upbc$measure)
yeojohnson(DL_long$upbc$measure)
powerTransform(DL_long$upbc$measure, family = "yjPower")
DL_long$upbc$measure.yj <- yjPower(DL_long$upbc$measure, -3.0632 )
model_upbc <- fitter(DL_long$upbc)

intercept<-gls(measure.in~1, data=DL_long$upbc, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.in~1, data=DL_long$upbc, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.in~1+time, data=DL_long$upbc, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.in~1+time, data=DL_long$upbc, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.in~1+time*Sweet, data=DL_long$upbc, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
results <-list("model"=timeCov, "basic"=summary(timeCov), "better?"= anova(intercept, randomIntercept, timeRI, timeRS, timeCov))
summary(timeCov)

qqnorm(resid(timeCov))
qqline(resid(timeCov))

r.squaredGLMM(model_upbc$model)
r.squaredGLMM(timeCov)

DL_long$upbc$timeCov.Res<- residuals(timeCov) #extracts the residuals and places them in a new column in our original data table
DL_long$upbc$Abs.timeCov.Res <-abs(DL_long$upbc$timeCov.Res) #creates a new column with the absolute value of the residuals
DL_long$upbc$timeCov.Res2 <- DL_long$upbc$Abs.timeCov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(timeCov.Res2 ~ PID, data=DL_long$upbc) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

DL_long$bi$measure.1 <- DL_long$bi$measure+1
DL_long$bi$measure.log <- log10(DL_long$bi$measure.1)
DL_long$bi$measure.ln <- log(DL_long$bi$measure.1)
DL_long$bi$measure.exp <- exp(DL_long$bi$measure) 
DL_long$bi$measure.sqrt <- sqrt(DL_long$bi$measure)
DL_long$bi$measure.cb <- abs(DL_long$bi$measure)^(1/3)
DL_long$bi$measure.in <- abs(DL_long$bi$measure)^(-1)
yeojohnson(DL_long$bi$measure)
DL_long$bi$measure.yj <- yjPower(DL_long$bi$measure, 1.353742)

intercept<-gls(measure.yj~1, data=DL_long$bi, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.yj~1, data=DL_long$bi, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.yj~1+time, data=DL_long$bi, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.yj~1+time, data=DL_long$bi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.yj~1+time*Sweet, data=DL_long$bi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
results <-list("model"=timeCov, "basic"=summary(timeCov), "better?"= anova(intercept, randomIntercept, timeRI, timeRS, timeCov))
summary(timeCov)

qqnorm(resid(timeCov))
qqline(resid(timeCov))

r.squaredGLMM(model_list2$model)
r.squaredGLMM(timeCov)

DL_long$bi$timeCov.Res<- residuals(timeCov) #extracts the residuals and places them in a new column in our original data table
DL_long$bi$Abs.timeCov.Res <-abs(DL_long$bi$timeCov.Res) #creates a new column with the absolute value of the residuals
DL_long$bi$timeCov.Res2 <- DL_long$bi$Abs.timeCov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(timeCov.Res2 ~ PID, data=DL_long$bi) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

DL_long$ubi$measure.1 <- DL_long$ubi$measure+1
DL_long$ubi$measure.log <- log10(DL_long$ubi$measure.1)
DL_long$ubi$measure.ln <- log(DL_long$ubi$measure.1)
DL_long$ubi$measure.exp <- exp(DL_long$ubi$measure) 
DL_long$ubi$measure.sqrt <- sqrt(DL_long$ubi$measure)
DL_long$ubi$measure.cb <- abs(DL_long$ubi$measure)^(1/3)
DL_long$ubi$measure.in <- abs(DL_long$ubi$measure)^(-1)
yeojohnson(DL_long$ubi$measure)
DL_long$bi$measure.yj <- yjPower(DL_long$bi$measure, 1.353742)

intercept<-gls(measure.in~1, data=DL_long$ubi, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.in~1, data=DL_long$ubi, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.in~1+time, data=DL_long$ubi, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.in~1+time, data=DL_long$ubi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.in~1+time*Sweet, data=DL_long$ubi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
results <-list("model"=timeCov, "basic"=summary(timeCov), "better?"= anova(intercept, randomIntercept, timeRI, timeRS, timeCov))
summary(timeCov)

qqnorm(resid(timeCov))
qqline(resid(timeCov))

r.squaredGLMM(model_list2$model)
r.squaredGLMM(timeCov)

DL_long$bi$timeCov.Res<- residuals(timeCov) #extracts the residuals and places them in a new column in our original data table
DL_long$bi$Abs.timeCov.Res <-abs(DL_long$bi$timeCov.Res) #creates a new column with the absolute value of the residuals
DL_long$bi$timeCov.Res2 <- DL_long$bi$Abs.timeCov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(timeCov.Res2 ~ PID, data=DL_long$bi) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results
###### BMI
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
DL_long_fit<-lapply(DL_long, fitter_BMI)
DL_long_fit

plotter_bmi<-function(x){
  x$Sweet<-as.factor(x$Sweet)
  plot1<-ggplot(x, aes(w1_BMI,measure, color=Sweet)) +
    geom_point(shape=1)+
    facet_grid(~time)+
    geom_smooth(method=lm,   # Add linear regression lines
                se=T)    # Don't add shaded confidence region
  return(plot1)
}
lapply(DL_long, plotter_bmi)
######interesting BMI, aa and bi
model_list2<-fitter_BMI(DL_long$aa)
model_list2[1]
Plot.Model.F.Linearity <-plot(resid(model_list2$model), DL_long$pbc$measure)

DL_long$pbc$timeCov.Res<- residuals(model_list2$model) #extracts the residuals and places them in a new column in our original data table
DL_long$pbc$Abs.timeCov.Res <-abs(DL_long$pbc$timeCov.Res) #creates a new column with the absolute value of the residuals
DL_long$pbc$timeCov.Res2 <- DL_long$pbc$Abs.timeCov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(timeCov.Res2 ~ PID, data=DL_long$pbc) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

Plot.timeCov <- plot(model_list2$model) #creates a fitted vs residual plot
Plot.timeCov

outcome<-DL_long$pbc$measure[complete.cases(DL_long$pb$measure)]
length(outcome)
plot(model_list2$model)

qqnorm(resid(model_list2$model))
qqline(resid(model_list2$model))

DL_long$aa$measure.in <- abs(DL_long$aa$measure)^(-1) 
intercept<-gls(measure.in~1, data=DL_long$aa, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.in~1, data=DL_long$aa, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.in~1+time, data=DL_long$aa, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.in~1+time, data=DL_long$aa, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.in~1+time*Sweet, data=DL_long$aa, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
time_BMICov<-lme(measure.in~1+time*Sweet*w1_BMI, data=DL_long$aa, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
summary(time_BMICov)

beta(time_BMICov, y=F) #### loss significance with transformation

DL_long$aa$time_BMICov.Res<- residuals(time_BMICov) #extracts the residuals and places them in a new column in our original data table
DL_long$aa$Abs.time_BMICov.Res <-abs(DL_long$aa$time_BMICov.Res) #creates a new column with the absolute value of the residuals
DL_long$aa$time_BMICov.Res2 <- DL_long$aa$Abs.time_BMICov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(time_BMICov.Res2 ~ PID, data=DL_long$aa) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

qqnorm(resid(time_BMICov))
qqline(resid(time_BMICov))

DL_long$bi$measure.1 <- DL_long$bi$measure+1
DL_long$bi$measure.log <- log10(DL_long$bi$measure.1)
DL_long$bi$measure.ln <- log(DL_long$bi$measure.1)
DL_long$bi$measure.exp <- exp(DL_long$bi$measure) 
DL_long$bi$measure.sqrt <- sqrt(DL_long$bi$measure)
DL_long$bi$measure.cb <- abs(DL_long$bi$measure)^(1/3)
DL_long$bi$measure.in <- abs(DL_long$bi$measure)^(-1)
bestNormalize(DL_long$bi$measure)

intercept<-gls(measure.in~1, data=DL_long$bi, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.in~1, data=DL_long$bi, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.in~1+time, data=DL_long$bi, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.in~1+time, data=DL_long$bi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.in~1+time*Sweet, data=DL_long$bi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
time_BMICov<-lme(measure.in~1+time*Sweet*w1_BMI, data=DL_long$bi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
summary(time_BMICov)

DL_long$bi$time_BMICov.Res<- residuals(time_BMICov) #extracts the residuals and places them in a new column in our original data table
DL_long$bi$Abs.time_BMICov.Res <-abs(DL_long$bi$time_BMICov.Res) #creates a new column with the absolute value of the residuals
DL_long$bi$time_BMICov.Res2 <- DL_long$bi$Abs.time_BMICov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(time_BMICov.Res2 ~ PID, data=DL_long$bi) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results
###### standardized betas
model1 <- fitter(DL_long$ubi)
beta(model1$model, y=F)

model2 <- fitter_BMI(DL_long$ubi)
beta(model2$model, y=F)

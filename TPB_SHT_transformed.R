######## Goal: transform SSB_ia (unknown), SSB_sn (inverse), SSB_bi (cubic) & USB_aa (exp), USB_ia(exp), USB_pbc (unknown)

######## installing the correct packages

install.packages("psych")
install.packages("ggplot2")
install.packages("reshape")
install.packages("dplyr")
install.packages("lme4")
install.packages("nlme")
install.packages("Hmisc")
install.packages("reghelper")
install.packages("MuMIn")
install.packages("rcompanion")
install.packages("MASS")
install.packages ("car")
install.packages ("bestNormalize")

require("reghelper")
require("lattice")
require("Hmisc")
library(psych)
library(ggplot2)
library(reshape)
library(dplyr)
library(lme4)
library(nlme)
library (MuMIn)
library (rcompanion)
library (MASS)
library (car)
library(bestNormalize)

# please make sure to read in your data table!
# other wise I can't really replicate your code


bbx3<-read.table("~/Google Drive/Lias_stuff/data/variables_interest.csv", sep=",", header=T, fill = T)
bbx3$Sweet<-as.factor(bbx3$Sweet)


#what the computer had been running for the data
#bbx3<- read_excel("~/Google Drive/Lias_stuff/data/lia_bbxdata_020119_full.xlsx", sheet = "calculated_data3")
#####I tried to link it directly to the drive sheet but had issues so if you know how to get this sheet from google 
#####than it would be the same data I am using.  

####### Check dataset
dim(bbx)
names(bbx)
#######renameing Participant ID to PID, helps later
colnames(bbx3)[colnames(bbx3)=="Participant ID (bbx_###)"] <- "PID"
names(bbx3)
####### Make new datasets per construct
uAA <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_aa', 
         'b5_tpbq_usb_aa', 'w2_tpbq_usb_aa')
dim(uAA)
names(uAA)

AA <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_ssb_aa', 
         'b5_tpbq_ssb_aa', 'w2_tpbq_ssb_aa')
dim(AA)
names(AA)

IA <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_ssb_ia', 
         'b5_tpbq_ssb_ia', 'w2_tpbq_ssb_ia')
dim(IA)
names(IA)

uIA <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_ia', 
         'b5_tpbq_usb_ia', 'w2_tpbq_usb_ia')
dim(uIA)
names(uIA)

SN <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_ssb_sn', 
         'b5_tpbq_ssb_sn', 'w2_tpbq_ssb_sn')
dim(SN)
names(SN)

uSN <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_sn', 
         'b5_tpbq_usb_sn', 'w2_tpbq_usb_sn')
dim(uSN)
names(uSN)

uPBC <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_pbc', 
         'b5_tpbq_usb_pbc', 'w2_tpbq_usb_pbc')
dim(uPBC)
names(uPBC)

PBC <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_ssb_pbc', 
         'b5_tpbq_ssb_pbc', 'w2_tpbq_ssb_pbc')

BI <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_ssb_bi', 
         'b5_tpbq_ssb_bi', 'w2_tpbq_ssb_bi')
dim(BI)
names(BI)

uBI <- bbx3 %>% group_by(Sweet) %>% 
  select('PID','w1_BMI', 'w1_BMI_status', 
         'age', 'ethnicity', 'race', 
         'gender', 'w1_tpbq_usb_bi', 
         'b5_tpbq_usb_bi', 'w2_tpbq_usb_bi')
######## Combining all the split datasets
DL <- list('uaa' = uAA, 'aa' = AA,  'ia' = IA, 'uia' = uIA, 'sn' = SN, 'usn' = uSN, 'upbc' = uPBC, 'pbc' = PBC, 'bi'= BI, 'ubi'= uBI)
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
  timeRI<-lme(measure~1+time, data=x, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  timeRS<-lme(measure~1+time, data=x, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  timeCov<-lme(measure~1+time*Sweet, data=x, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
  results <-list("model"=timeCov, "basic"=summary(timeCov), "better?"= anova(intercept, randomIntercept, timeRI, timeRS, timeCov))
  return(results)
}
lapply(DL_long, fitter)

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


####### transforming variables that failed normality test
DL_long$ia$measure.1 <- DL_long$ia$measure+1
DL_long$ia$measure.log <- log10(DL_long$ia$measure.1)
DL_long$ia$measure.ln <- log(DL_long$ia$measure.1) 
DL_long$ia$measure.sqrt <- sqrt(DL_long$ia$measure)
DL_long$ia$measure.cb <- abs(DL_long$ia$measure)^(1/3)
DL_long$ia$measure.in <- abs(DL_long$ia$measure)^(-1)
DL_long$uia$measure.exp <- exp(DL_long$uia$measure) #####USB
hist(DL_long$bi$measure)
DL_long$ia$measure_tuk = 
  transformTukey(DL_long$ia$measure,
                 plotit=FALSE)
yeojohnson(DL_long$ia$measure)
powerTransform(DL_long$ia$measure, family = "yjPower")
DL_long$ia$measure.yj <- yjPower(DL_long$upbc$measure, 2.123436)
model_ia <- fitter(DL_long$ia)

intercept<-gls(measure.exp~1, data=DL_long$uia, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.exp~1, data=DL_long$uia, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.exp~1+time, data=DL_long$uia, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.exp~1+time, data=DL_long$uia, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.exp~1+time*Sweet, data=DL_long$uia, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
results <-list("model"=timeCov, "basic"=summary(timeCov), "better?"= anova(intercept, randomIntercept, timeRI, timeRS, timeCov))
summary(timeCov)

qqnorm(resid(timeCov))
qqline(resid(timeCov))

r.squaredGLMM(model_ia$model)
r.squaredGLMM(timeCov)


DL_long$sn$measure.1 <- DL_long$sn$measure+1
DL_long$sn$measure.log <- log10(DL_long$sn$measure.1)
DL_long$sn$measure.ln <- log(DL_long$sn$measure.1)
DL_long$sn$measure.sqrt <- sqrt(DL_long$sn$measure)
DL_long$sn$measure.cb <- abs(DL_long$sn$measure)^(1/3)
DL_long$sn$measure.in <- (DL_long$sn$measure)^(-1) ##### corrected normality
DL_long$sn$measure.exp <- exp(DL_long$sn$measure)
DL_long$sn$measure_tuk = 
  transformTukey(DL_long$sn$measure,
                 plotit=FALSE)
model_sn <-fitter(DL_long$sn)


intercept<-gls(measure.in~1, data=DL_long$sn, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.in~1, data=DL_long$sn, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.in~1+time, data=DL_long$sn, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.in~1+time, data=DL_long$sn, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.in~1+time*Sweet, data=DL_long$sn, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
results <-list("model"=timeCov, "basic"=summary(timeCov), "better?"= anova(intercept, randomIntercept, timeRI, timeRS, timeCov))
summary(timeCov)



qqnorm(resid(timeCov))
qqline(resid(timeCov))

r.squaredGLMM(model_sn$model)
r.squaredGLMM(timeCov)

beta(timeCov, y=F)
DL_long$sn$timeCov.Res<- residuals(timeCov) #extracts the residuals and places them in a new column in our original data table
DL_long$sn$Abs.timeCov.Res <-abs(DL_long$sn$timeCov.Res) #creates a new column with the absolute value of the residuals
DL_long$sn$timeCov.Res2 <- DL_long$sn$Abs.timeCov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(timeCov.Res2 ~ PID, data=DL_long$sn) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results



DL_long$bi$measure.1 <- DL_long$bi$measure+1
DL_long$bi$measure.log <- log10(DL_long$bi$measure.1)
DL_long$bi$measure.sqrt <- sqrt(DL_long$bi$measure)
DL_long$bi$measure.cb <- abs(DL_long$bi$measure)^(1/3) ##### corrected for levene test
model_list2<- fitter(DL_long$bi)

intercept<-gls(measure.cb~1, data=DL_long$bi, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.cb~1, data=DL_long$bi, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.cb~1+time, data=DL_long$bi, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.cb~1+time, data=DL_long$bi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.cb~1+time*Sweet, data=DL_long$bi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
results <-list("model"=timeCov, "basic"=summary(timeCov), "better?"= anova(intercept, randomIntercept, timeRI, timeRS, timeCov))
View(results)
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


DL_long$uaa$measure.1 <- DL_long$uaa$measure+1
DL_long$uaa$measure.log <- log10(DL_long$uaa$measure.1)
DL_long$uaa$measure.sqrt <- sqrt(DL_long$uaa$measure)
DL_long$uaa$measure.cb <- abs(DL_long$uaa$measure)^(1/3)
DL_long$uaa$measure.exp <- exp(DL_long$uaa$measure) ###### fixed levene's test
DL_long$uaa$measure_tuk = 
  transformTukey(DL_long$uaa$measure,
                 plotit=FALSE)
model_uaa <- fitter(DL_long$uaa)

intercept<-gls(measure.exp~1, data=DL_long$uaa, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.exp~1, data=DL_long$uaa, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.exp~1+time, data=DL_long$uaa, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.exp~1+time, data=DL_long$uaa, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.exp~1+time*Sweet, data=DL_long$uaa, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
results <-list("model"=timeCov, "basic"=summary(timeCov), "better?"= anova(intercept, randomIntercept, timeRI, timeRS, timeCov))
summary(timeCov)

qqnorm(resid(timeCov))
qqline(resid(timeCov))

r.squaredGLMM(model_uaa$model)
r.squaredGLMM(timeCov)

DL_long$uaa$timeCov.Res<- residuals(timeCov) #extracts the residuals and places them in a new column in our original data table
DL_long$uaa$Abs.timeCov.Res <-abs(DL_long$uaa$timeCov.Res) #creates a new column with the absolute value of the residuals
DL_long$uaa$timeCov.Res2 <- DL_long$uaa$Abs.timeCov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(timeCov.Res2 ~ PID, data=DL_long$uaa) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

DL_long$uia$measure.1 <- DL_long$uia$measure+1
DL_long$uia$measure.log <- log10(DL_long$uia$measure.1)
DL_long$uia$measure.ln <- log(DL_long$uia$measure.1)
DL_long$uia$measure.exp <- exp(DL_long$uia$measure) ###### corrected the normality
DL_long$uia$measure.sqrt <- sqrt(DL_long$uia$measure)
DL_long$uia$measure.cb <- abs(DL_long$uia$measure)^(1/3)
DL_long$uia$measure_tuk = 
  transformTukey(DL_long$uia$measure,
                 plotit=FALSE)
model_uia <- fitter(DL_long$uia)

intercept<-gls(measure.exp~1, data=DL_long$uia, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.exp~1, data=DL_long$uia, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.exp~1+time, data=DL_long$uia, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.exp~1+time, data=DL_long$uia, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.exp~1+time*Sweet, data=DL_long$uia, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
results <-list("model"=timeCov, "basic"=summary(timeCov), "better?"= anova(intercept, randomIntercept, timeRI, timeRS, timeCov))
summary(timeCov)

qqnorm(resid(timeCov))
qqline(resid(timeCov))

r.squaredGLMM(model_uia$model)
r.squaredGLMM(timeCov)

DL_long$uia$timeCov.Res<- residuals(timeCov) #extracts the residuals and places them in a new column in our original data table
DL_long$uia$Abs.timeCov.Res <-abs(DL_long$uia$timeCov.Res) #creates a new column with the absolute value of the residuals
DL_long$uia$timeCov.Res2 <- DL_long$uia$Abs.timeCov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(timeCov.Res2 ~ PID, data=DL_long$uia) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

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
model_upbc <- fitter(DL_long$upbc)

intercept<-gls(measure.yj~1, data=DL_long$upbc, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.yj~1, data=DL_long$upbc, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.yj~1+time, data=DL_long$upbc, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.yj~1+time, data=DL_long$upbc, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.yj~1+time*Sweet, data=DL_long$upbc, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
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

#######BMI checks
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

DL_long$ubi$measure.1 <- DL_long$ubi$measure+1
DL_long$ubi$measure.log <- log10(DL_long$ubi$measure.1)
DL_long$ubi$measure.ln <- log(DL_long$ubi$measure.1)
DL_long$ubi$measure.exp <- exp(DL_long$ubi$measure) 
DL_long$ubi$measure.sqrt <- sqrt(DL_long$ubi$measure)
DL_long$ubi$measure.cb <- abs(DL_long$ubi$measure)^(1/3) 
DL_long$ubi$measure.in <- abs(DL_long$ubi$measure)^(-1)
DL_long$ubi$measure_tuk = 
  transformTukey(DL_long$bi$measure,
                 plotit=FALSE)
yeojohnson(DL_long$ubi$measure)
powerTransform(DL_long$bi$measure, family = "yjPower")
DL_long$ubi$measure.yj <- yjPower(DL_long$ubi$measure, -4.658621 ) ###### adjusted normality, SSB 
model_list2<- fitter_BMI(DL_long$ubi)

intercept<-gls(measure.yj~1, data=DL_long$bi, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.yj~1, data=DL_long$bi, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.yj~1+time, data=DL_long$bi, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.yj~1+time, data=DL_long$bi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.yj~1+time*Sweet, data=DL_long$bi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
time_BMICov<-lme(measure.yj~1+time*Sweet*w1_BMI, data=DL_long$bi, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
results <-list("model"=time_BMICov, "basic"=summary(time_BMICov), "better?"= anova(timeCov, time_BMICov))
summary(time_BMICov)

qqnorm(resid(timeCov))
qqline(resid(timeCov))

r.squaredGLMM(model_list2$model)
r.squaredGLMM(time_BMICov)

beta(time_BMICov, y=F)

DL_long$ubi$time_BMICov.Res<- residuals(time_BMICov) #extracts the residuals and places them in a new column in our original data table
DL_long$ubi$Abs.time_BMICov.Res <-abs(DL_long$ubi$time_BMICov.Res) #creates a new column with the absolute value of the residuals
DL_long$ubi$time_BMICov.Res2 <- DL_long$ubi$Abs.time_BMICov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(time_BMICov.Res2 ~ PID, data=DL_long$ubi) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

DL_long$aa$measure.1 <- DL_long$aa$measure+1
DL_long$aa$measure.log <- log10(DL_long$aa$measure.1)
DL_long$aa$measure.ln <- log(DL_long$aa$measure.1)
DL_long$uaa$measure.exp <- exp(DL_long$uaa$measure) ###### corrected normality issue USB
DL_long$aa$measure.sqrt <- sqrt(DL_long$aa$measure)
DL_long$aa$measure.cb <- abs(DL_long$aa$measure)^(1/3) 
DL_long$aa$measure.in <- abs(DL_long$aa$measure)^(-1) ######corrected normality issue SSB
DL_long$aa$measure_tuk = 
  transformTukey(DL_long$aa$measure,
                 plotit=FALSE)
yeojohnson(DL_long$aa$measure)
powerTransform(DL_long$aa$measure, family = "yjPower")
DL_long$aa$measure.yj <- yjPower(DL_long$aa$measure, -0.5344996 ) #-0.2226905  or  -0.01212761  
model_list2 <- fitter_BMI(DL_long$uaa)

intercept<-gls(measure.exp~1, data=DL_long$uaa, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.exp~1, data=DL_long$uaa, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.exp~1+time, data=DL_long$uaa, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.exp~1+time, data=DL_long$uaa, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.exp~1+time*Sweet, data=DL_long$uaa, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
time_BMICov<-lme(measure.exp~1+time*Sweet*w1_BMI, data=DL_long$uaa, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
summary(time_BMICov)

qqnorm(resid(time_BMICov))
qqline(resid(time_BMICov))

r.squaredGLMM(model_list2$model)
r.squaredGLMM(time_BMICov)

beta(time_BMICov, y=F)

DL_long$uaa$time_BMICov.Res<- residuals(time_BMICov) #extracts the residuals and places them in a new column in our original data table
DL_long$uaa$Abs.time_BMICov.Res <-abs(DL_long$uaa$time_BMICov.Res) #creates a new column with the absolute value of the residuals
DL_long$uaa$time_BMICov.Res2 <- DL_long$uaa$Abs.time_BMICov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(time_BMICov.Res2 ~ PID, data=DL_long$uaa) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

DL_long$ia$measure.1 <- DL_long$ia$measure+1
DL_long$ia$measure.log <- log10(DL_long$ia$measure.1)
DL_long$ia$measure.ln <- log(DL_long$ia$measure.1)
DL_long$uia$measure.exp <- exp(DL_long$uia$measure) ######corrected normality issue USB
DL_long$ia$measure.sqrt <- sqrt(DL_long$ia$measure)
DL_long$ia$measure.cb <- abs(DL_long$ia$measure)^(1/3) 
DL_long$ia$measure.in <- abs(DL_long$ia$measure)^(-1) 
DL_long$ia$measure_tuk = 
  transformTukey(DL_long$ia$measure,
                 plotit=FALSE)
yeojohnson(DL_long$ia$measure)
powerTransform(DL_long$ia$measure, family = "yjPower")
DL_long$ia$measure.yj <- yjPower(DL_long$aa$measure, 1.176395) #1.176395  or 2.123436
model_list2 <- fitter_BMI(DL_long$uia)

intercept<-gls(measure.exp~1, data=DL_long$uia, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.exp~1, data=DL_long$uia, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.exp~1+time, data=DL_long$uia, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.exp~1+time, data=DL_long$uia, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.exp~1+time*Sweet, data=DL_long$uia, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
time_BMICov<-lme(measure.exp~1+time*Sweet*w1_BMI, data=DL_long$uia, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
summary(time_BMICov)

qqnorm(resid(time_BMICov))
qqline(resid(time_BMICov))

r.squaredGLMM(model_list2$model)
r.squaredGLMM(time_BMICov)

beta(time_BMICov, y=F)

DL_long$uia$time_BMICov.Res<- residuals(time_BMICov) #extracts the residuals and places them in a new column in our original data table
DL_long$uia$Abs.time_BMICov.Res <-abs(DL_long$uia$time_BMICov.Res) #creates a new column with the absolute value of the residuals
DL_long$uia$time_BMICov.Res2 <- DL_long$uia$Abs.time_BMICov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(time_BMICov.Res2 ~ PID, data=DL_long$uia) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results


DL_long$usn$measure.1 <- DL_long$usn$measure+1
DL_long$usn$measure.log <- log10(DL_long$usn$measure.1)
DL_long$usn$measure.ln <- log(DL_long$usn$measure.1)
DL_long$usn$measure.exp <- exp(DL_long$usn$measure) 
DL_long$usn$measure.sqrt <- sqrt(DL_long$usn$measure)
DL_long$usn$measure.cb <- abs(DL_long$usn$measure)^(1/3) 
DL_long$sn$measure.in <- abs(DL_long$sn$measure)^(-1) ######corrected normality issue SSB 
DL_long$sn$measure_tuk = 
  transformTukey(DL_long$sn$measure,
                 plotit=FALSE)
yeojohnson(DL_long$usn$measure)
powerTransform(DL_long$sn$measure, family = "yjPower")
DL_long$usn$measure.yj <- yjPower(DL_long$usn$measure, -2.12813 ) #-2.12813  or -1.845137 
model_list2 <- fitter_BMI(DL_long$sn)

intercept<-gls(measure.in~1, data=DL_long$sn, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.in~1, data=DL_long$sn, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.in~1+time, data=DL_long$sn, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.in~1+time, data=DL_long$sn, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.in~1+time*Sweet, data=DL_long$sn, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
time_BMICov<-lme(measure.in~1+time*Sweet*w1_BMI, data=DL_long$sn, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
summary(time_BMICov)

qqnorm(resid(time_BMICov))
qqline(resid(time_BMICov))

r.squaredGLMM(model_list2$model)
r.squaredGLMM(time_BMICov)

beta(time_BMICov, y=F)

DL_long$sn$time_BMICov.Res<- residuals(time_BMICov) #extracts the residuals and places them in a new column in our original data table
DL_long$sn$Abs.time_BMICov.Res <-abs(DL_long$sn$time_BMICov.Res) #creates a new column with the absolute value of the residuals
DL_long$sn$time_BMICov.Res2 <- DL_long$sn$Abs.time_BMICov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(time_BMICov.Res2 ~ PID, data=DL_long$sn) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

DL_long$pbc$measure.1 <- DL_long$pbc$measure+1
DL_long$pbc$measure.log <- log10(DL_long$pbc$measure.1) ####### solves for ssb
DL_long$pbc$measure.ln <- log(DL_long$pbc$measure.1)
DL_long$pbc$measure.exp <- exp(DL_long$pbc$measure) 
DL_long$pbc$measure.sqrt <- sqrt(DL_long$pbc$measure)
DL_long$pbc$measure.cb <- abs(DL_long$pbc$measure)^(1/3) 
DL_long$pbc$measure.in <- abs(DL_long$pbc$measure)^(-1) 
DL_long$pbc$measure_tuk = 
  transformTukey(DL_long$pbc$measure,
                 plotit=FALSE)
yeojohnson(DL_long$upbc$measure)
powerTransform(DL_long$pbc$measure, family = "yjPower")
DL_long$upbc$measure.yj <- yjPower(DL_long$upbc$measure,-0.4723849  ) #-0.4723849   or -0.1012562
model_list2 <- fitter_BMI(DL_long$pbc)

intercept<-gls(measure.log~1, data=DL_long$pbc, method="ML", na.action=na.exclude)
randomIntercept<-lme(measure.log~1, data=DL_long$pbc, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRI<-lme(measure.log~1+time, data=DL_long$pbc, random=~1|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeRS<-lme(measure.log~1+time, data=DL_long$pbc, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
timeCov<-lme(measure.log~1+time*Sweet, data=DL_long$pbc, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
time_BMICov<-lme(measure.log~1+time*Sweet*w1_BMI, data=DL_long$pbc, random=~time|PID, method="ML", na.action=na.exclude, control=list(opt="optim"))
summary(time_BMICov)

qqnorm(resid(timeCov))
qqline(resid(timeCov))

r.squaredGLMM(model_list2$model)
r.squaredGLMM(time_BMICov)

beta(time_BMICov, y=F)

DL_long$pbc$timeCov.Res<- residuals(timeCov) #extracts the residuals and places them in a new column in our original data table
DL_long$pbc$Abs.timeCov.Res <-abs(DL_long$pbc$timeCov.Res) #creates a new column with the absolute value of the residuals
DL_long$pbc$timeCov.Res2 <- DL_long$pbc$Abs.timeCov.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.timeCov <- lm(timeCov.Res2 ~ PID, data=DL_long$pbc) #ANOVA of the squared residuals
anova(Levene.timeCov) #displays the results

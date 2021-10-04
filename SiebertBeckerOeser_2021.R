# R-Script for analyses carried out in manuscript: Siebert, Becker, Oeser 2021
# "Making a good career choice: A decision-analytical intervention to train complex decision-making and enhance proactive decision-making and career choice self-efficacy in high school students"

rm(list = ls())
################### 0)  load libraries  #############
library(lme4)
#library(xlsx)
library(MASS)
library(psych)
library(lmerTest)
library(tidyverse)
library(dplyr)
library(glmmTMB)
library(ggplot2)
library(scales)                                                                                      
library(sjlabelled)      
library(sjmisc)                                                                                    
library(sjstats)
library(sjPlot)
library(tidyr)
library(ggeffects)
library(performance)
library(parameters)
library(emmeans)
library(gridExtra)

########## 1) read in data ####################
setwd("C:/Users/Maxi/Dropbox/Bayreuth/data/")
data <- read.table("SiebertBeckerOeser_2021.csv", sep = ";", dec = ",", header=TRUE, na.strings=c(""," ","NA", "999", "88"))

#data$time_fac = as.factor(data$time)
data$time2 = data$time
data$time_fac = as.factor(data$time2)
data$sex[data$sex == "d"]= NA
data$sex_fac = as.factor(data$sex)
data$group[data$group == "K"] = "C"
data$group[data$group == "E"] = "I"
data$group = as.factor(data$group)
data$time[data$time == 1] = "pre"
data$time[data$time == 2] = "post"

data$time = factor(data$time, levels = c("pre", "post", "FU"))
data$CCSE = data$berufswahlSW_Gesamt
data$OSE = data$berufSW_Gesamt 
data$SE = data$Selbstwert_Gesamt
data$GR = data$allgResilienz_Gesamt


##### Descriptives of sample ####################
KLUG_age <- data[data$time == "pre" & data$raus != 1,] %>%
  group_by(group) %>%
  summarise( mean_age = mean(age, na.rm = T), sd_age = sd(age, na.rm= T),
             max_age = max(age), min_age = min(age))

KLUG_nationality= data[data$time == "pre" & data$raus != 1,] %>% group_by(group) %>% count(nationality)
KLUG_sex= data[data$time == "pre" & data$raus != 1, ] %>% group_by(group) %>% count(sex)

library(MKinfer)
#library(rcompanion)
data$gender = data$sex
data$gender[data$gender == "m"] = 0
data$gender[data$gender == "w"] = 1
data$gender = as.integer(data$gender)
#chisq.test(data[data$time == "pre" & data$raus != 1 & data$group == "I",]$gender, 
#            data[data$time == "pre" & data$raus != 1 & data$group == "C",]$gender)

data$German = data$nationality
data$German[data$nationality == "Germany"] = 1
data$German[data$nationality == "bosn/Germany"] = 1
data$German[data$nationality == "serb/Germany"] = 1  
data$German[data$nationality == "Germany, amerikanisch"] = 1
data$German[data$nationality == "Germany, russich"] = 1
data$German[data$nationality != "Germany"] = 0
data$German = as.integer(data$German)
perm.t.test(data[data$time == "pre" & data$raus != 1 & data$group == "I",]$German, 
            data[data$time == "pre" & data$raus != 1 & data$group == "C",]$German)


###### relevant Outcome Variables: 
hist(data$PDM_Ini_Gesamt)
hist(data$PDM_Improve_Gesamt)
hist(data$PDM_Obj_Gesamt)
hist(data$PDM_Info_Gesamt)
hist(data$PDM_Alternative_Gesamt)
hist(data$PDM_Radar_Gesamt)
hist(data$allgResilienz_Gesamt)
hist(data$Selbstwert_Gesamt)
hist(data$berufswahlSW_Gesamt)
hist(data$berufSW_Gesamt)
hist(data$Confidence_gesamt)

#################################################################
#########  SKILLS  ###########################

PDM_SKILLS_LMER <- lmer(SKILLS  ~  time*group + sex_fac+ (1|ID),data=data[data$raus != 1,], na.action= na.omit)

#check distributions of UV & AV  -> normal
plot(check_distribution(PDM_SKILLS_LMER))
hist(residuals(PDM_SKILLS_LMER))

#summarize results
PDMStable1 = tab_model(PDM_SKILLS_LMER)
summary(PDM_SKILLS_LMER)

# get mean values and ci's
#confint(PDM_SKILLS_LMER, method = "boot") #-> bootstrapped CIs but dont work
SKILLspic <- ggpredict(PDM_SKILLS_LMER, c("time", "group"))  %>% plot(show.title = F)+ ggplot2::theme_classic()
emmeans(PDM_SKILLS_LMER, c("time", "group")) %>% contrast(method = "pairwise")

#########  TRAITS  ###########################
data$TRAITS = (data$PDM_Ini_Gesamt+ data$PDM_Improve_Gesamt) / 2
PDM_TRAITS_LMER <- lmer(TRAITS  ~  time*group + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)

#check distributions of UV & AV  -> normal
plot(check_distribution(PDM_TRAITS_LMER))
hist(residuals(PDM_TRAITS_LMER))

#summarize results
tab_model(PDM_SKILLS_LMER, PDM_TRAITS_LMER)
summary(PDM_TRAITS_LMER)

# get mean values and ci's
#confint(PDM_TRAITS_LMER, method = "boot") #-> bootstrapped CIs but dont work
TRAITSpic <- ggpredict(PDM_TRAITS_LMER, c("time", "group"))  %>% plot(show.title = F)+ ggplot2::theme_classic()
emmeans(PDM_TRAITS_LMER, c("time", "group")) %>% contrast(method = "pairwise")

#equivalence test
#standard deviation berechnen f|r ROPE (region of practical equivalence)
SD.1 = sd(data[ data$raus == 0,]$TRAITS,na.rm = T) * 0.5 #0.5 for point estimator

#eigentlicher equivalence test
equivalence_test(PDM_TRAITS_LMER, range = c(-SD.1 , SD.1))


PDMStable2 = tab_model(PDM_SKILLS_LMER,PDM_TRAITS_LMER)


library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
#hight 500/ width427
fig1 <- ggarrange(SKILLspic, TRAITSpic, 
                  common.legend = TRUE, legend = "bottom",
                  #labels = c("A", "B", "C", 'D'),
                  ncol = 2, nrow = 1)
fig1
######### 4l) allgResilienz_Gesamt  - general resilience ###########################

allgResilienz_LMER <- lmer(GR ~ time*group + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)

#check distributions of UV & AV  -> normal
plot(check_distribution(allgResilienz_LMER))
hist(residuals(allgResilienz_LMER))

#summarize results
tab_model(allgResilienz_LMER)
summary(allgResilienz_LMER)

# get mean values and ci's
#confint(allgResilienz_LMER, method = "boot") #-> bootstrapped CIs but dont work
GRpic<-ggpredict(allgResilienz_LMER, c("time", "group")) %>% plot(show.title = F)+ ggplot2::theme_classic()
emmeans(allgResilienz_LMER, c("time"))%>% contrast(method = "pairwise")

######### 4m) Selbstwert_Gesamt - Self esteem (SE)  ###########################

colMeans(subset(data[data$raus != 1,], time == 1 , select = Selbstwert_Gesamt    ), na.rm = T)
colMeans(subset(data[data$raus != 1,], time == 2 , select = Selbstwert_Gesamt    ), na.rm = T)
hist(data$Selbstwert_Gesamt)
Selbstwert_LMER <- lmer(SE ~  time*group + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)

#check distributions of UV & AV  -> normal
plot(check_distribution(Selbstwert_LMER))
hist(residuals(Selbstwert_LMER))

#summarize results
tab_model(Selbstwert_LMER)
summary(Selbstwert_LMER)

# get mean values and ci's
#confint(Selbstwert_LMER, method = "boot") #-> bootstrapped CIs but dont work
SEpic<- ggpredict(Selbstwert_LMER, c("time", "group"))  %>% plot(show.title = F)+ ggplot2::theme_classic()
emmeans(Selbstwert_LMER, c("time"))%>% contrast(method = "pairwise")

######### 4n) berufswahlSW_Gesamt - CCSE carrer choice self-efficacy    ###########################

colMeans(subset(data[data$raus != 1,], time == 1 , select = berufswahlSW_Gesamt), na.rm = T)
colMeans(subset(data[data$raus != 1,], time == 2 , select = berufswahlSW_Gesamt), na.rm = T)

berufswahlSW_LMER <- lmer(CCSE  ~  time*group + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)

#check distributions of UV & AV  -> normal
plot(check_distribution(berufswahlSW_LMER))
hist(residuals(berufswahlSW_LMER))

#summarize results
tab_model(berufswahlSW_LMER)
summary(berufswahlSW_LMER)

# get mean values and ci's
#confint(berufswahlSW_LMER, method = "boot") #-> bootstrapped CIs but dont work
CCSEpic <-ggpredict(berufswahlSW_LMER, c("time", "group")) %>% plot(show.title = F) + ggplot2::theme_classic()
emmeans(berufswahlSW_LMER, c("time", "group"))%>% contrast(method = "pairwise")

######### 4o) berufSW_Gesamt - OSE occupational self-efficacy  ###########################

#colMeans(subset(data, time == 1 , select = berufSW_Gesamt      ), na.rm = T)
#colMeans(subset(data, time == 2 , select = berufSW_Gesamt      ), na.rm = T)

# sehr schiefe Verteilung
hist(data$berufSW_Gesamt)
data$berufSW_Gesamt1 = log(10-data$berufSW_Gesamt)
data$berufSW_Gesamt2 = max(data$berufSW_Gesamt1, na.rm = T)- data$berufSW_Gesamt1
hist(data$berufSW_Gesamt1)

berufSW_LMER <- lmer(OSE ~ time*group + sex_fac+ (1|ID),data=data[data$raus != 1,], na.action= na.omit)

#check distributions of UV & AV  -> normal
plot(check_distribution(berufSW_LMER))
hist(residuals(berufSW_LMER))

#summarize results
 tab_model(berufSW_LMER)
summary(berufSW_LMER)

# get mean values and ci's
#confint(berufSW_LMER, method = "boot") #-> bootstrapped CIs but dont work
OSEpic <-ggpredict(berufSW_LMER, c("time", "group"))  %>% plot(show.title = F)+ ggplot2::theme_classic()
emmeans(berufSW_LMER, c("time"))%>% contrast(method = "pairwise")


library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
fig2 <- ggarrange(CCSEpic, OSEpic, SEpic, GRpic, 
                  #Selbstwert_LMER, allgResilienz_LMER,
                  common.legend = TRUE, legend = "bottom",
                  #labels = c("A", "B", "C", 'D'),
                  ncol = 2, nrow = 2)
fig2



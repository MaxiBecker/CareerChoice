# R-Script for analyses carried out in manuscript: Siebert, Becker, Oeser 2022
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
library(MKinfer)

########## 1) read in data ####################
setwd("C:/Users/Maxi/Dropbox/Bayreuth/data/")
data <- read.table("SiebertBeckerOeser_2021r.csv", sep = ";", dec = ",", header=TRUE, na.strings=c(""," ","NA", "999", "88"))

#data$time_fac = as.factor(data$time)
data$time2 = data$time
data$time_fac = as.factor(data$time2)
data$sex[data$sex == "d"]= NA
data$sex_fac = as.factor(data$sex)
data$group[data$group == "K"] = "C"
data$group[data$group == "E"] = "I"
data$group = as.factor(data$group)
data$time[data$time == 1] = "t1"
data$time[data$time == 2] = "t2"
data$time[data$time == 'FU'] = NaN

data$time = factor(data$time, levels = c("t1", "t2"))
data$CCSE = data$berufswahlSW_Gesamt
data$OSE = data$berufSW_Gesamt 
data$SE = data$Selbstwert_Gesamt
data$GR = data$allgResilienz_Gesamt

data$gender = data$sex
data$gender[data$gender == "m"] = 0
data$gender[data$gender == "w"] = 1
data$gender = as.integer(data$gender)
data$school = as.factor(data$school)

##### Descriptives of sample ####################

# compare German to Non-German proportion between both groups
data$German = data$nationality
data$German[data$nationality == "Germany"] = 1
data$German[data$nationality == "bosn/Germany"] = 1
data$German[data$nationality == "serb/Germany"] = 1  
data$German[data$nationality == "Germany, amerikanisch"] = 1
data$German[data$nationality == "Germany, russich"] = 1
data$German[data$German != "1" ] = 0
data$German = as.integer(data$German)
perm.t.test(data[data$time == "t1" & data$raus != 1 & data$group == "I",]$German, 
            data[data$time == "t1" & data$raus != 1 & data$group == "C",]$German)
KLUG_nationality= data[data$time == "t1" & data$raus != 1,] %>% group_by(group) %>% count(German)

# compare age between both groups
KLUG_age <- data[data$time == "t1" & data$raus != 1,] %>%
  group_by(group) %>%
  summarise( mean_age = mean(age, na.rm = T), sd_age = sd(age, na.rm= T),
             max_age = max(age), min_age = min(age))

# compare sex between both groups
KLUG_sex= data[data$time == "t1" & data$raus != 1, ] %>% group_by(group) %>% count(sex)

##################################################
# compare age, sex, proportion Germans between both schools

perm.t.test(data[data$time == "t1" & data$raus != 1 & data$school == "A" & data$group == "C",]$gender, 
            data[data$time == "t1" & data$raus != 1 & data$school == "A" & data$group == "I",]$gender)

perm.t.test(data[data$time == "t1" & data$raus != 1 & data$school == "A",]$age, 
            data[data$time == "t1" & data$raus != 1 & data$school == "B",]$age)

perm.t.test(data[data$time == "t1" & data$raus != 1 & data$school == "A",]$German, 
            data[data$time == "t1" & data$raus != 1 & data$school == "B",]$German)


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

PDM_SKILLS_LMER0 <- lmer(SKILLS  ~  time+group + school + sex_fac+ (1|ID),data=data[data$raus != 1,], na.action= na.omit)
PDM_SKILLS_LMER <- lmer(SKILLS  ~  time*group + school + sex_fac+ (1|ID),data=data[data$raus != 1,], na.action= na.omit)

anova(PDM_SKILLS_LMER0, PDM_SKILLS_LMER)

#check distributions of UV & AV  -> normal
plot(check_distribution(PDM_SKILLS_LMER))
hist(residuals(PDM_SKILLS_LMER))

#summarize results
PDMStable1 = tab_model(PDM_SKILLS_LMER)
summary(PDM_SKILLS_LMER, ddf = "Satterthwaite")

# get mean values and ci's
#confint(PDM_SKILLS_LMER, method = "boot") #-> bootstrapped CIs but dont work
SKILLspic <- ggpredict(PDM_SKILLS_LMER, c("time", "group"))  %>% plot(show.title = F)+ ggplot2::theme_classic()
emmeans(PDM_SKILLS_LMER, c("time", "group")) %>% contrast(method = "pairwise")

#########  TRAITS  ###########################
data$TRAITS = (data$PDM_Ini_Gesamt+ data$PDM_Improve_Gesamt) / 2
PDM_TRAITS_LMER0 <- lmer(TRAITS  ~  time+group + school + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)
PDM_TRAITS_LMER <- lmer(TRAITS  ~  time*group + school + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)
anova(PDM_TRAITS_LMER0, PDM_TRAITS_LMER)

#check distributions of UV & AV  -> normal
plot(check_distribution(PDM_TRAITS_LMER))
hist(residuals(PDM_TRAITS_LMER))

#summarize results
tab_model(PDM_SKILLS_LMER, PDM_TRAITS_LMER)
summary(PDM_TRAITS_LMER, ddf = "Satterthwaite")

# get mean values and ci's
#confint(PDM_TRAITS_LMER, method = "boot") #-> bootstrapped CIs but dont work
TRAITSpic <- ggpredict(PDM_TRAITS_LMER, c("time", "group"))  %>% plot(show.title = F)+ ggplot2::theme_classic()
emmeans(PDM_TRAITS_LMER, c("time", "group")) %>% contrast(method = "pairwise")


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

allgResilienz_LMER0 <- lmer(GR ~ time+group + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)
allgResilienz_LMER <- lmer(GR ~ time*group + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)
anova(allgResilienz_LMER0, allgResilienz_LMER)

#check distributions of UV & AV  -> normal
plot(check_distribution(allgResilienz_LMER))
hist(residuals(allgResilienz_LMER))

#summarize results
tab_model(allgResilienz_LMER)
summary(allgResilienz_LMER, ddf = "Satterthwaite")

# get mean values and ci's
#confint(allgResilienz_LMER, method = "boot") #-> bootstrapped CIs but dont work
GRpic<-ggpredict(allgResilienz_LMER, c("time", "group")) %>% plot(show.title = F)+ ggplot2::theme_classic()
emmeans(allgResilienz_LMER, c("time"))%>% contrast(method = "pairwise")

######### 4m) Selbstwert_Gesamt - Self esteem (SE)  ###########################

colMeans(subset(data[data$raus != 1,], time == 1 , select = Selbstwert_Gesamt    ), na.rm = T)
colMeans(subset(data[data$raus != 1,], time == 2 , select = Selbstwert_Gesamt    ), na.rm = T)
hist(data$Selbstwert_Gesamt)

Selbstwert_LMER0 <- lmer(SE ~  time+group + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)
Selbstwert_LMER <- lmer(SE ~  time*group + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)
anova(Selbstwert_LMER0, Selbstwert_LMER)

#check distributions of UV & AV  -> normal
plot(check_distribution(Selbstwert_LMER))
hist(residuals(Selbstwert_LMER))

#summarize results
tab_model(Selbstwert_LMER)
summary(Selbstwert_LMER, ddf = "Satterthwaite")

# get mean values and ci's
#confint(Selbstwert_LMER, method = "boot") #-> bootstrapped CIs but dont work
SEpic<- ggpredict(Selbstwert_LMER, c("time", "group"))  %>% plot(show.title = F)+ ggplot2::theme_classic()
emmeans(Selbstwert_LMER, c("time"))%>% contrast(method = "pairwise")

######### 4n) berufswahlSW_Gesamt - CCSE carrer choice self-efficacy    ###########################

colMeans(subset(data[data$raus != 1,], time == 1 , select = berufswahlSW_Gesamt), na.rm = T)
colMeans(subset(data[data$raus != 1,], time == 2 , select = berufswahlSW_Gesamt), na.rm = T)

berufswahlSW_LMER0 <- lmer(CCSE  ~  time+group + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)
berufswahlSW_LMER <- lmer(CCSE  ~  time*group + sex_fac + (1|ID),data=data[data$raus != 1,], na.action= na.omit)
anova(berufswahlSW_LMER0, berufswahlSW_LMER)

#check distributions of UV & AV  -> normal
plot(check_distribution(berufswahlSW_LMER))
hist(residuals(berufswahlSW_LMER))

#summarize results
TMberufswahlSW_LMER = tab_model(berufswahlSW_LMER)
summary(berufswahlSW_LMER, ddf = "Satterthwaite")

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

berufSW_LMER0 <- lmer(OSE ~ time+group + sex_fac+ (1|ID),data=data[data$raus != 2,], na.action= na.omit)
berufSW_LMER <- lmer(OSE ~ time*group + sex_fac+ (1|ID),data=data[data$raus != 2,], na.action= na.omit)
anova(berufSW_LMER0, berufSW_LMER)

#check distributions of UV & AV  -> normal
plot(check_distribution(berufSW_LMER))
hist(residuals(berufSW_LMER))

#summarize results
tab_model(berufSW_LMER)
summary(berufSW_LMER, ddf = "Satterthwaite")

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

#summarize model estimates of career choice resilience
tab_model(berufswahlSW_LMER, berufSW_LMER )
tab_model(Selbstwert_LMER, allgResilienz_LMER)
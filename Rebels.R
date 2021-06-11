# Syrian Rebel Fragmentation script

# rm(list=ls(all=TRUE)) 

# Read packages
library(tidyverse)
library(MLmetrics)
library(rcompanion)
library(sjPlot)
library(xlsx)

# Import dataset
Rebels <- read_excel("C:/Users/Asus/Desktop/Thesis/R Project/Syrians.xlsx")

View(Rebels)

# Clarify data?
names(Rebels)[names(Rebels) == "Martyr cult"] <- "Martyr_cult"
names(Rebels)[names(Rebels) == "Survived/ended"] <- "Survived"
names(Rebels)[names(Rebels) == "Local/National"] <- "Geography"
names(Rebels)[names(Rebels) == "Social media"] <- "Social_media"
names(Rebels)[names(Rebels) == "Agent-Self"] <- "Agent_self"
names(Rebels)[names(Rebels) == "Identity Synergy"] <- "Identity_synergy"
names(Rebels)[names(Rebels) == "Relational Ties"] <- "Relational_ties"
Rebels[Rebels$Ideology == 1,]$Ideology <- "Salafi-Jihadi"
Rebels[Rebels$Ideology == 2,]$Ideology <- "Sunni Islamist"
Rebels[Rebels$Ideology == 3,]$Ideology <- "Other"
Rebels[Rebels$Ethnicity == 1,]$Ethnicity <- "Arab"
Rebels[Rebels$Ethnicity == 2,]$Ethnicity <- "Other"
Rebels[Rebels$Geography == 1,]$Geography <- "National"
Rebels[Rebels$Geography == 2,]$Geography <- "Local"
Rebels$Ideology <- as.factor(Rebels$Ideology)
Rebels$Ethnicity <- as.factor(Rebels$Ethnicity)
Rebels$Geography <- as.factor(Rebels$Geography)

#GLM model Logistic Regressions

# Martyr cult model
Log_Martyr_cult <- glm(Survived ~ Ideology+ Ethnicity + Geography + Martyr_cult, data =Rebels, family = "binomial")
summary(Log_Martyr_cult)

# Agent-self principle model
Log_Agent_self<- glm(Survived ~ Ideology + Ethnicity + Geography + Agent_self, data =Rebels, family = "binomial")
summary(Log_Agent_self)

# Identity synergy principle model
Log_Identity_synergy <- glm(Survived ~ Ideology+ Ethnicity + Geography + Identity_synergy, data =Rebels, family = "binomial")
summary(Log_Identity_synergy)

# Relational Ties prinicple model
Log_Relational_ties <- glm(Survived ~ Ideology+ Ethnicity + Geography + Relational_ties, data =Rebels, family = "binomial")
summary(Log_Relational_ties)

# Failed Irrevocability because of perfect correlation
# Log_Irrevocability <- glm(Survived ~ Ideology+ Ethnicity + Geography + Irrevocability, data =Rebels, family = "binomial")
# summary(Log_Irrevocability)

# Create summary variable of all Identity fusion IV's in order to measure overall effect of increased IF IV's on survival
Martyr_sum <- Rebels$Agent_self + Rebels$Identity_synergy + Rebels$Relational_ties + Rebels$Irrevocability

# Add variable to dataset as 'Identity fusion'
Rebels$Identity_fusion <- Martyr_sum

# Identity fusion log model
Log_Identity_fusion <- glm(Survived ~ Ideology+ Ethnicity + Geography + Identity_fusion, data =Rebels, family = "binomial")
summary(Log_Identity_fusion)

# Pseudo R square for models
nagelkerke(Log_Agent_self)
nagelkerke(Log_Identity_synergy)
nagelkerke(Log_Relational_ties)
nagelkerke(Log_Martyr_cult)
nagelkerke(Log_Identity_fusion)

# PRAUC and AUC for models
PRAUC(predict(Log_Agent_self), Rebels$Survived)
PRAUC(predict(Log_Identity_synergy), Rebels$Survived)
PRAUC(predict(Log_Relational_ties), Rebels$Survived)
PRAUC(predict(Log_Martyr_cult), Rebels$Survived)
PRAUC(predict(Log_Identity_fusion), Rebels$Survived)


AUC(predict(Log_Agent_self), Rebels$Survived)
AUC(predict(Log_Identity_synergy), Rebels$Survived)
AUC(predict(Log_Relational_ties), Rebels$Survived)
AUC(predict(Log_Martyr_cult), Rebels$Survived)
AUC(predict(Log_Identity_fusion), Rebels$Survived)

# Create table
tab_model(Log_Agent_self, Log_Identity_synergy, Log_Relational_ties, Log_Martyr_cult, Log_Identity_fusion, show.ci = FALSE, auto.label = FALSE)

#Selective descriptives for analysis
summary(Rebels$Ideology)
summary(Rebels$Ethnicity)
summary(Rebels$Geography)
summary(Rebels$Relational_ties)
Rebels$Relations <- as.factor(Rebels$Relational_ties)
summary(Rebels$Relations)
Rebels$Agents <- as.factor(Rebels$Agent_self)
summary(Rebels$Agents)
Rebels$Identities <- as.factor(Rebels$Identity_synergy)
summary(Rebels$Identities)

#Residuals template  
res1 <- resid(Log_Identity_fusion)
plot(fitted(Log_Identity_fusion), res1)
par("mar")
par(mar=c(3,2,2,3))
abline(0,0)
qqnorm(res1)    
qqline(res1)
plot(density(res1))


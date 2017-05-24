---
title: "ANOVA_R"
author: "Priyanka Angadi"
date: "May 7, 2017"
output: html_document
---
To investigate demographic data,
we will use ANOVAs to see if different demographic groups
gender, race geographic locations perform differently on individual subject scores, 
overall scores


```{r}
#Load Packages
rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
library(dplyr)
library(car)
library(corrplot)
library(survey)
library(ggplot2)
setwd("/Users/pangadi/Desktop/stats150/ANOVA")
load("Subset.RData")
#Check your dataset
#Number of male and female students
Subset %>% group_by(gender) %>% summarise(Count=length(unique(randomized_id)))
#Check to see if one student has been assigned 2 genders (data entry errors)
Subset %>% group_by(randomized_id) %>% summarise(Count=length(unique(gender)))
```
```{r}
#Get Data ready for analysis
ForANOVA_M <- Subset[Subset$subject_desc=="Mathematics" & Subset$gender=="M"
                   ,c("randomized_id","gender","subject_desc","scale_score")]
ForANOVA_F<- Subset[Subset$subject_desc=="Mathematics" & Subset$gender=="F"
                     ,c("randomized_id","gender","subject_desc","scale_score")]
#random sampling of 4000 male students
M <- sample(unique(ForANOVA_M$randomized_id),size = 4000,replace=FALSE) 
#random sampling of female students
Female <- sample(unique(ForANOVA_F$randomized_id),size = 4000,replace=FALSE)

ForANOVA_M <- ForANOVA_M[ForANOVA_M$randomized_id %in% M,]
ForANOVA_F <- ForANOVA_F[ForANOVA_F$randomized_id %in% Female,]

ForANOVA <- rbind(ForANOVA_F,ForANOVA_M)
```
```{r}
#Building the model
modl <- aov(scale_score~gender,data=ForANOVA)
modl

summary(modl)
```
```{r}
#Making plots to check assumptions for model
plot(modl,which =1,main = "Residual PLot")
qqnorm(rstandard(modl),ylab="Standardized Residuals", xlab="Normal Scores", 
       main="NPP plot") 
qqline(rstandard(modl))

TukeyHSD(modl, conf.level = 0.95)

pairwise.t.test(ForANOVA$scale_score, 
                ForANOVA$gender, p.adj = "bonferroni")

pairwise.t.test(ForANOVA$scale_score, 
                ForANOVA$gender, p.adj = "fdr")

#rule of thumb
thumb <- as.data.frame(ForANOVA %>% group_by(gender) %>% summarize(SD=sd(scale_score)))
ruleofthumb <- max(thumb[,2])/min(thumb[,2])

#Levene's test
suppressWarnings(leveneTest(scale_score~gender,data = ForANOVA))

#boxplot
boxplot(cbind(M=ForANOVA[ForANOVA$gender=="M",4],ForANOVA[ForANOVA$gender=="F",4]),main=
          "Boxplot of Mathematics scores in male and female students")
```

Running the model for race
```{r}
ForANOVA_MRace <- Subset[Subset$subject_desc=="Mathematics" 
                 ,c("randomized_id","race_ethnicity","subject_desc","scale_score")]
#removing data for who we don't have race information
ForANOVA_MRace <-ForANOVA_MRace[ForANOVA_MRace$race_ethnicity!= " ",]
#First few rows of data used to build model
head(ForANOVA_MRace)
modl <- aov(scale_score~race_ethnicity,data=ForANOVA_MRace)
modl
#check the model
summary(modl)

#Make plots to check assumptions of model
plot(modl,which =1,main = "Residual PLot")
qqnorm(rstandard(modl),ylab="Standardized Residuals", xlab="Normal Scores", 
       main="NPP plot") 
qqline(rstandard(modl))

TukeyHSD(modl, conf.level = 0.95)

pairwise.t.test(ForANOVA_MRace$scale_score, 
                ForANOVA_MRace$race_ethnicity, p.adj = "bonferroni")

pairwise.t.test(ForANOVA_MRace$scale_score, 
                ForANOVA_MRace$race_ethnicity, p.adj = "fdr")

#Levene's test
suppressWarnings(leveneTest(scale_score~race_ethnicity,data = ForANOVA_MRace,center = mean))

#rule of thumb
thumb <- as.data.frame(ForANOVA_MRace %>% group_by(race_ethnicity) %>% summarize(SD=sd(scale_score)))
ruleofthumb <- max(thumb[,2])/min(thumb[,2])


#boxplot
ForANOVA_MRace$race_ethnicity <- unlist(lapply(strsplit(ForANOVA_MRace$race_ethnicity," "),function(x) x[1]))
par(mar = c(9,4,4,2) + 0.1)
ForANOVA_MRace[ForANOVA_MRace$race_ethnicity=="American","race_ethnicity"] <- "American-Indian"
boxplot(formula = scale_score~race_ethnicity,data=ForANOVA_MRace,cex=0.4,las=2)

```



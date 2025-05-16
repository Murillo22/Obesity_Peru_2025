#app.R

library(shinyPredict)
library(readxl)
library(Hmisc)
library(tidyverse)
library(dplyr)
library(rms)

#Loading data
read_excel('Input File/BASE_OF.xlsx')->data

#Formatting data
data%>%
  mutate(SEX=factor(SEX,levels=c('1','2')),
         Age_Group=factor(Age_Group,levels=c('1','2','3')),
         Age_months=as.numeric(Age_months),
         EDI=factor(EDI,levels=c('1','2','3')),
         Obesity=factor(Obesity,levels=c('0','1')),
         Anemia=factor(Anemia,levels=c('0','1')),
         PhysActiv=factor(PhysActiv,levels=c('0','1')),
         KnowFeeding=factor(KnowFeeding,levels=c('1','2','3')),
         MotherAge=as.numeric(MotherAge),
         EDULevel=factor(EDULevel,levels=c('1','2','3','4','5'))
  )->data
# Setting labels

data$SEX <- factor(data$SEX, levels = c(1,2), labels = c("Male","Female"))
data$Age_Group <- factor(data$Age_Group, levels = c(1, 2, 3), 
                         labels = c("<1yo", "1-2yo", "3-4yo"))
data$Obesity <- factor(data$Obesity, levels = c(0, 1), labels = c("No", "Yes"))
data$PhysActiv <- factor(data$PhysActiv, levels = c(0, 1), labels = c("No", "Yes"))
data$KnowFeeding <- factor(data$KnowFeeding, levels = c(1,2,3), labels = c("Low", "Intermediate","High"))

data->dataset
rm(data)
dd <- rms::datadist(dataset)
options(datadist = "dd")


model <- glm(EDI ~ SEX + Age_Group+
               Obesity+PhysActiv+KnowFeeding,data=dataset,family = 'binomial')

print(shinyPredict(models=list("Simple model"=model),
                   data=dataset[,c('SEX','Age_Group','Obesity',
                                   'PhysActiv','KnowFeeding')],
                   path = "./Chainy",
                   title="Neurodevelopmental disorder in Children",shinytheme="paper"))

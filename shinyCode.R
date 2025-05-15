#app.R

library(shiny)
library(DynNom)
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

lookup <- c(Sex = "SEX", 'Age_groups' = "Age_Group",
            "Physical_Activity"='PhysActiv',
            "Knowledge_on_Feeding"='KnowFeeding')

rename(data, all_of(lookup))->data


dd <- datadist(data)
options(datadist = "dd")


model <- lrm(EDI ~ Sex + Age_groups+Obesity+Physical_Activity+Knowledge_on_Feeding,data=data)


DNbuilder(
  model, 
  data = data, 
  clevel = 0.95,
  DNtitle = 'Neurodevelopmental Disorders in Children'
)

# Using shinylive and 
shinylive::export(appdir = "DynNomapp", destdir = "app_site")

#httpuv::runStaticServer("docs/", port=8008)
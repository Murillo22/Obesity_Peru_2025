library(ggplot2)
library(shiny)
library(plotly)
library(stargazer)
library(compare)
library(rms)

#######################################################
#### Before publishing your dynamic nomogram:
####
#### - You may need to edit the following lines if
#### data or model objects are not defined correctly
#### - You could modify ui.R or server.R for
#### making any required changes to your app
#######################################################

load('data.RData')
source('functions.R')
t.dist <- datadist(data)
options(datadist = 't.dist')
m.summary <- 'raw'
covariate <- 'slider'
clevel <- 0.95

### Please cite the package in publications if it is used.
# Read more on:
# Jalali, A., Alvarez-Iglesias, A., Roshan, D., & Newell, J. (2019). Visualising statistical models using dynamic nomograms. PLoS one, 14(11), e0225253.



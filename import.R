options(warn=-1)

library(Rnalytica)
library(ggplot2)
library(car)
library(effects)
library(ScottKnottESD)
library(gridExtra)
library(effsize)
library(dplyr)
library(rms)
library(Rnalytica)
library(C50)
library(randomForest)
library(FFTrees)
library(gridExtra)
library(caret)
library(DMwR)

eclipse <- loadDefectDataset("eclipse-2.0")

data <- eclipse$data
indep <- eclipse$indep
dep <- eclipse$dep
data[,dep] <- factor(data[,dep])

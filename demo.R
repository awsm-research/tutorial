setwd("~/Research/softwareanalytics101")
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
library(rsq)
library(C50)
library(randomForest)
library(FFTrees)
library(gridExtra)
library(caret)
library(DMwR)


lucene <- loadDefectDataset("lucene-2.3.0")
data <- lucene$data
indep <- lucene$indep
dep <- lucene$dep

table(data[,dep])/nrow(data)

################################################
# Intro to logistic regression

m <- glm(RealBug ~ SumCyclomatic, data = data, family="binomial")


pdf("intro-regression.pdf",width=4, height=3)
plot(allEffects(m))
dev.off()

################################################

# STEP1: INCLUDE OTHER METRICS

m1 <- glm(RealBug ~ SumCyclomatic, data = data, family="binomial")
Anova(m1)

m2 <- glm(RealBug ~ CountLine + SumCyclomatic, data = data, family="binomial")
Anova(m2)

# STEP2: REMOVE HIGHLY-CORRELATED METRICS


pdf("2-varclus.pdf", width=12, height=7)
plot(varclus(as.matrix(data[,indep]), similarity="spear", trans="abs"))
abline(h=0.3, col="red")
dev.off()

# Auomated Feature Selection (AutoSpearman)


pdf("2-autospearman.pdf", width=12, height=7)
filter <- AutoSpearman(data, indep)
plot(varclus(as.matrix(data[,indep]), similarity="spear", trans="abs"))
abline(h=0.3, col="red")
dev.off()

f <- as.formula(paste( "RealBug", '~', paste(indep, collapse = "+")))
m3 <- glm(f, data = data, family="binomial")
rsq(m3)
f <- as.formula(paste( "RealBug", '~', paste(filter, collapse = "+")))
m4 <- glm(f, data = data, family="binomial")
rsq(m4)
rsq <- function(model){return(1-(model$deviance/model$null.deviance))}


# STEP3: Explore different learning algorithms


indep <- c("OWN_COMMIT","MaxInheritanceTree", "CountDeclInstanceVariable")
tree.model <- C5.0(x = data[, indep], y = data[,dep])
summary(tree.model)
pdf("3-tree.pdf", width=8, height=7)
plot(tree.model)
dev.off()

rule.model <- C5.0(x = data[, indep], y = data[,dep], rules = TRUE)
summary(rule.model)


f <- as.formula(paste( "RealBug", '~', paste(indep, collapse = "+")))
rf.model <- randomForest(f, data = data, importance = TRUE, keep.forest=TRUE)
print(rf.model)
pdf("3-rf.pdf", width=8, height=6)
varImpPlot(rf.model)
dev.off()

fft.model <- FFTrees(formula = f, data = data)
print(fft.model)

pdf("3-fftrees.pdf", width=8, height=8)
plot(fft.model)
dev.off()

############################################################

# STEP4: Explore different parameter settings

results <- list()


for(i in seq(1,100)){
  set.seed(i)
  indices <- sample(nrow(data), replace=TRUE)
  training <- data[indices,]
  testing <- data[-indices,]
  indep <- AutoSpearman(training, lucene$indep)
  f <- as.formula(paste( "RealBug", '~', paste(indep, collapse = "+")))
  
  glm.model <- glm(f, data = training, family="binomial")
  defaulttree.model <- C5.0(x = training[, indep], y = training[,dep], rules=TRUE, trials=1)
  optimaltree.model <- C5.0(x = training[, indep], y = training[,dep], rules=TRUE, trials=100)
  rf10trees.model <- randomForest(f, data = training, importance = TRUE, ntree=10)
  rf100trees.model <- randomForest(f, data = training, importance = TRUE, ntree=100)
  fft.model <- FFTrees(formula=f, data = training)
  
  predictions <- data.frame(
    GLM = predict(glm.model, testing, type="response"),
    C50.1trial = predict(defaulttree.model, testing, type="prob")[,"TRUE"],
    C50.100trials = predict(optimaltree.model, testing, type="prob")[,"TRUE"],
    RF.10trees = predict(rf10trees.model, testing, type="prob")[,"TRUE"],
    RF.100trees = predict(rf100trees.model, testing, type="prob")[,"TRUE"],
    FFTrees = predict(fft.model, testing, type="prob")[,2]
  )
  performance <- apply(predictions, 2, function(x) performance.calculation(testing[,dep], x))
  results[["AUC"]] <- rbind(results[["AUC"]], performance["AUC",])
  results[["Fmeasure(0.5)"]] <- rbind(results[["Fmeasure(0.5)"]], performance["Fmeasure",])
  
  performance <- apply(predictions, 2, function(x) performance.calculation(testing[,dep], x, threshold = 0.3))
  results[["Fmeasure(0.3)"]] <- rbind(results[["Fmeasure(0.3)"]], performance["Fmeasure",])
  
  performance <- apply(predictions, 2, function(x) performance.calculation(testing[,dep], x, threshold = 0.7))
  results[["Fmeasure(0.7)"]] <- rbind(results[["Fmeasure(0.7)"]], performance["Fmeasure",])
}
saveRDS(results, "step4.rds")

levels <- c("C50.100trials","RF.100trees","C50.1trial","RF.10trees","FFTrees","GLM")
g1<- ggplot(melt(data.frame(results[["Fmeasure(0.5)"]])), aes(x=factor(variable, levels=levels), y=value)) + geom_boxplot() + scale_y_continuous(labels=0:40*0.2, breaks=0:40*0.2, limit=c(0.4,1)) + ylab("F-measure (0.5)") + xlab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

g2<- ggplot(melt(data.frame(results[["Fmeasure(0.7)"]]),na.rm=TRUE), aes(x=factor(variable, levels=levels), y=value)) + geom_boxplot() + scale_y_continuous(labels=0:40*0.2, breaks=0:40*0.2, limit=c(0.4,1)) + ylab("F-measure (0.7)") + xlab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

g3<- ggplot(melt(results[["AUC"]]), aes(x=reorder(Var2, -value, median), y=value)) + geom_boxplot() + scale_y_continuous(labels = 7:10*0.1, limit=c(0.7,1)) + ylab("AUC") + xlab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

pdf("4-5-parameter-and-thresholds.pdf",width=10,height=4)
grid.arrange(g1,g2,g3,ncol=3)
dev.off()       


############################################################

# STEP4: AUC is not sensitive to imbalanced data (especially, for high EPV values)

results <- list()
indep <- AutoSpearman(data, lucene$indep)
f <- as.formula(paste( "RealBug", '~', paste(indep, collapse = "+")))
for(i in seq(1,100)){
  set.seed(1234+i)
  indices <- sample(nrow(data), replace=TRUE)
  training <- data[indices,]
  testing <- data[-indices,]
  undersample <- downSample(training[,indep], training[,dep], yname = "RealBug")
  oversample <- upSample(training[,indep], training[,dep], yname = "RealBug")
  smotesample <- SMOTE(f, data=training)
  
  original.m <- glm(f, data = training, family="binomial")
  under.m <- glm(f, data = undersample, family="binomial")
  over.m <- glm(f, data = oversample, family="binomial")
  smote.m <- glm(f, data = smotesample, family="binomial")
  
  predictions <- data.frame(
    Original = predict(original.m, testing, type="response"),
    Under = predict(under.m, testing, type="response"),
    Over = predict(over.m, testing, type="response"),
    SMOTE = predict(smote.m, testing, type="response")
  )
  
  performance <- apply(predictions, 2, function(x) performance.calculation(testing[,dep], x))
  results[["AUC"]] <- rbind(results[["AUC"]], performance["AUC",])
  results[["Fmeasure"]] <- rbind(results[["Fmeasure"]], performance["Fmeasure",])
  results[["Precision"]] <- rbind(results[["Precision"]], performance["Precision",])
  results[["Recall"]] <- rbind(results[["Recall"]], performance["Recall",])
}  

finalresults <- data.frame(rbind(results[["AUC"]], results[["Precision"]], results[["Recall"]], results[["Fmeasure"]]))
finalresults$measure <- c(rep("AUC",100),rep("Precision",100),rep("Recall",100),rep("Fmeasure",100))

ggplot(melt(finalresults), aes(x=variable, y=value)) + geom_boxplot() + facet_wrap(~measure)  + ylab("") + xlab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("class-imbalance.pdf",width=5,height=5)


############################################################

# BOOT VS CV

boot <- NULL
indep <- AutoSpearman(data, lucene$indep)
f <- as.formula(paste( "RealBug", '~', paste(indep, collapse = "+")))
for(i in seq(1,100)){
  set.seed(1234+i)
  indices <- sample(nrow(data), replace=TRUE)
  training <- data[indices,]
  testing <- data[-indices,]
  
  m <- C5.0(x = training[, indep], y = training[,dep], rules=TRUE, trials=100)
  predictions <- predict(m, testing, type="prob")[,"TRUE"]
  performance <- performance.calculation(testing[,dep], predictions)
  boot <- rbind(boot, performance)
}

# 10x10-folds CV
cv <- NULL
for(i in seq(1,10)){
  set.seed(1234+i)
  indices <- createFolds(data[, dep], k = 10, list = TRUE, returnTrain = TRUE)
  for(i in seq(1,10)){
    training <- data[indices[[i]],]
    testing <- data[-indices[[i]],]
    m <- C5.0(x = training[, indep], y = training[,dep], rules=TRUE, trials=100)
    predictions <- predict(m, testing, type="prob")[,"TRUE"]
    performance <- performance.calculation(testing[,dep], predictions)
    cv <- rbind(cv, performance)
  }
}

results <- data.frame(rbind(cv,boot))
results$mvt <- c(rep("10x10Folds",100),rep("OOS-Bootstrap",100))

ggplot(melt(results[,c("AUC","Fmeasure","mvt")]), aes(x=mvt, y=value)) + geom_boxplot() + ylab("value") + xlab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~variable) + scale_y_continuous(labels=5:10*0.1, breaks=5:10*0.1, limit=c(0.5,1))
ggsave("step7-fulldatasets.pdf",width=4,height=5)

############################################################

# STEP8: USE ANOVA TYPE-II, INSTEAD OF TYPE-I

indep <- AutoSpearman(data, lucene$indep)
f <- as.formula(paste( "RealBug", '~', paste(indep, collapse = "+")))
m <- glm(f, data=data, family="binomial")
importance1 <- data.frame(M1.Type1=anova(m)$Deviance[-1], M1.Type2=Anova(m,type="2",test="LR")$"LR Chisq")
rownames(importance1) <- indep

indep <- sample(indep)
f <- as.formula(paste( "RealBug", '~', paste(indep, collapse = "+")))
m <- glm(f, data=data, family="binomial")
importance2 <- data.frame(M2.Type1=anova(m)$Deviance[-1], M2.Type2=Anova(m,type="2",test="LR")$"LR Chisq")
rownames(importance2) <- indep

importance <- data.frame(importance1[indep,],importance2[indep,])
importance <- data.frame(apply(importance, 2, function(x){x/sum(abs(x))}))

round(importance[order(-importance$M1.Type2),], digit=2)*100


############################################################

# STEP9: SUMMARIZE BY A ScottKnott-ESD TEST : goal is to look at confidence interval
importance <- NULL
indep <- AutoSpearman(data, lucene$indep)
f <- as.formula(paste( "RealBug", '~', paste(indep, collapse = "+")))
for(i in seq(1,100)){
  set.seed(1234+i)
  indices <- sample(nrow(data), replace=TRUE)
  training <- data[indices,]
  m <- glm(f, data = training, family="binomial")
  importance <- rbind(importance, Anova(m,type="2",test="LR")$"LR Chisq")
}

importance <- data.frame(importance)
colnames(importance) <- indep

plot(sk_esd(importance))

############################################################

m <- glm(f, data = data, family="binomial")
plot(allEffects(m))
plot(effect("OWN_COMMIT",m))
plot(effect("MAJOR_LINE",m))

# Model Diagnostic
# https://ms.mcmaster.ca/~bolker/R/misc/modelDiag.html









# 2.1 Check data type
summary(data$SumCyclomatic)

# 2.1 Test of normality
shapiro.test(data$SumCyclomatic)

# 2.1 Hypothesis Testing (CC)
wilcox.test(data[data$RealBug==TRUE,]$SumCyclomatic, data[data$RealBug==FALSE,]$SumCyclomatic)

# 2.1 Generate a density plot
ggplot(data, aes(x=SumCyclomatic, fill=RealBug)) + geom_density(alpha=.3) + scale_x_log10() + theme_bw()
ggsave("2.1-density-plot.pdf", width=4, height=2)

# 2.1 Hypothesis Testing (CC) / alternative
wilcox.test(data[data$RealBug==TRUE,]$SumCyclomatic, data[data$RealBug==FALSE,]$SumCyclomatic, alternative = "greater")
wilcox.test(data[data$RealBug==TRUE,]$SumCyclomatic, data[data$RealBug==FALSE,]$SumCyclomatic, alternative = "less")

# 2.2 Effect size analysis
cliff.delta(data[data$RealBug==TRUE,]$SumCyclomatic, data[data$RealBug==FALSE,]$SumCyclomatic)
cliff.delta(data[data$RealBug==TRUE,]$CountLine, data[data$RealBug==FALSE,]$CountLine)

# 2.3 Multivariate Analysis

g1 <- ggplot(data, aes(x=SumCyclomatic, fill=RealBug)) + geom_density(alpha=.3) + scale_x_log10() + theme_bw() + theme(legend.position="top")
g2 <- ggplot(data, aes(x=ADEV, fill=RealBug)) + geom_density(alpha=.3) + scale_x_log10() + theme_bw() + theme(legend.position="top")
g3 <- ggplot(data, aes(x=CountLine, fill=RealBug)) + geom_density(alpha=.3) + scale_x_log10() + theme_bw() + theme(legend.position="top")
g4 <- ggplot(data, aes(x=Del_lines, fill=RealBug)) + geom_density(alpha=.3) + scale_x_log10() + theme_bw() + theme(legend.position="top")
pdf("2.3-multivariate.pdf", width=5, height=5)
grid.arrange(g1,g2,g3,g4, ncol=2)
dev.off()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Only when using RStudio
getwd()

source("import.R")

library(Hmisc)
describe(data)

table(data[,dep])/nrow(data)

################################################
# INTRO: BASIC REGRESSION ANALYSIS (Slide 8)
################################################

m <- glm(post ~ CC_max, data = data, family="binomial")
summary(m)
pdf("figures/0-intro-regression.pdf",width=4, height=3)
plot(allEffects(m))
dev.off()

sp <- spearman2(formula(paste("post" ," ~ ",paste0(indep, collapse=" + "))), data= data, p=2)
plot(sp)
################################################
# INTRO: PRELIMINARY DATA ANALYSIS
################################################

# Check data distribution
summary(data$CC_sum)

# Hypothesis Testing (CC)
wilcox.test(data[data$post==TRUE,]$CC_sum, data[data$post==FALSE,]$CC_sum)

# Generate a density plot
ggplot(data, aes(x=CC_sum, fill=post)) + geom_density(alpha=.3) + scale_x_log10() + theme_bw()
ggsave("2.1-density-plot.pdf", width=4, height=2)

# Hypothesis Testing (CC) / alternative
wilcox.test(data[data$post==TRUE,]$CC_sum, data[data$post==FALSE,]$CC_sum, alternative = "greater")
wilcox.test(data[data$post==TRUE,]$CC_sum, data[data$post==FALSE,]$CC_sum, alternative = "less")

# Effect size analysis
cliff.delta(data[data$post==TRUE,]$CC_sum, data[data$post==FALSE,]$CC_sum)

################################################
# STEP1: INCLUDE CONTROL METRICS (Slide 12)
################################################

m1 <- glm(post ~ CC_max + PAR_max + FOUT_max, data = data, family="binomial")
anova(m1)

m2 <- glm(post ~ TLOC + CC_max + PAR_max + FOUT_max, data = data, family="binomial")
anova(m2)

importance <- data.frame(m1=c(0,anova(m1)$Deviance[-1]), m2=anova(m2)$Deviance[-1])
importance <- data.frame(apply(importance, 2, function(x){x/sum(abs(x))}))
rownames(importance) <- c("TLOC","CC_max","PAR_max","FOUT_max")
round(importance,digit=2)*100

################################################
# STEP2: REMOVE CORRELATED METRICS
################################################

# THE RISKS OF NOT REMOVING CORRELATED METRICS  (Slide 14)

m1 <- glm(post ~ CC_max + + CC_avg + PAR_max + FOUT_max, data = data, family="binomial")
anova(m1)

m2 <- glm(post ~ CC_avg + CC_max + PAR_max + FOUT_max, data = data, family="binomial")
anova(m2)

importance <- data.frame(m1=anova(m1)$Deviance[-1], m2=anova(m2)$Deviance[c(3,2,4,5)])
importance <- data.frame(apply(importance, 2, function(x){x/sum(abs(x))}))
rownames(importance) <- c("CC_max","CC_avg","PAR_max","FOUT_max")
round(importance,digit=2)*100

# SPEARMAN'S CORRELATION ANALYSIS  (Slide 15)
indep <- eclipse$indep
pdf("figures/2-varclus.pdf", width=10, height=5)
plot(varclus(as.matrix(data[,indep]), similarity="spear", trans="abs"))
abline(h=0.3, col="red")
dev.off()

# AutoSpearman: AUTOMATICALLY REMOVE CORRELATED METRICS
library(Rnalytica)
pdf("figures/2-autospearman.pdf", width=5, height=5)
filterindep <- AutoSpearman(data, indep)
plot(varclus(as.matrix(data[, filterindep]), similarity="spear", trans="abs"))
abline(h=0.3, col="red")
dev.off()

################################################
# STEP3: BUILD EXPLAINABLE MODELS
################################################

indep <- AutoSpearman(data, eclipse$indep)

# DECISION TREE-BASED MODEL
tree.model <- C5.0(x = data[,indep], y = data[,dep])
summary(tree.model)
pdf("figures/3-tree.pdf", width=30, height=15)
plot(tree.model)
dev.off()

# RULE-BASED MODEL
rule.model <- C5.0(x = data[, indep], y = data[,dep], rules = TRUE)
summary(rule.model)

# RANDOM FOREST MODEL
f <- as.formula(paste( "post", '~', paste(indep, collapse = "+")))
rf.model <- randomForest(f, data = data, importance = TRUE, keep.forest=TRUE)
print(rf.model)
pdf("figures/3-rf.pdf", width=8, height=6)
varImpPlot(rf.model, type="1")
dev.off()

# FAST-AND-FRUGAL TREE MODEL
f <- as.formula(paste( "post", '~', paste(indep, collapse = "+")))
fft.model <- FFTrees(formula = f, data = data)
print(fft.model)

pdf("figures/3-fftrees.pdf", width=8, height=8)
plot(fft.model)
dev.off()

################################################
# STEP4: Explore different parameter settings
################################################

results <- list()
for(i in seq(1,100)){
  set.seed(i)
  indices <- sample(nrow(data), replace=TRUE)
  training <- data[indices,]
  testing <- data[-indices,]
  indep <- AutoSpearman(training, eclipse$indep)
  f <- as.formula(paste( "post", '~', paste(indep, collapse = "+")))

  glm.model <- glm(f, data = training, family="binomial")
  defaulttree.model <- C5.0(x = training[, indep], y = training[,dep], rules=TRUE, trials=1)
  optimaltree.model <- C5.0(x = training[, indep], y = training[,dep], rules=TRUE, trials=100)
  rf10trees.model <- randomForest(f, data = training, importance = TRUE, ntree=10)
  rf100trees.model <- randomForest(f, data = training, importance = TRUE, ntree=100)

  predictions <- data.frame(
    GLM = predict(glm.model, testing, type="response"),
    C50.1trial = predict(defaulttree.model, testing, type="prob")[,"TRUE"],
    C50.100trials = predict(optimaltree.model, testing, type="prob")[,"TRUE"],
    RF.10trees = predict(rf10trees.model, testing, type="prob")[,"TRUE"],
    RF.100trees = predict(rf100trees.model, testing, type="prob")[,"TRUE"]
  )
  performance <- apply(predictions, 2, function(x) performance.calculation(testing[,dep], x))
  results[["AUC"]] <- rbind(results[["AUC"]], performance["AUC",])
  results[["Fmeasure(0.5)"]] <- rbind(results[["Fmeasure(0.5)"]], performance["Fmeasure",])

  performance <- apply(predictions, 2, function(x) performance.calculation(testing[,dep], x, threshold = 0.2))
  results[["Fmeasure(0.2)"]] <- rbind(results[["Fmeasure(0.2)"]], performance["Fmeasure",])

  performance <- apply(predictions, 2, function(x) performance.calculation(testing[,dep], x, threshold = 0.8))
  results[["Fmeasure(0.8)"]] <- rbind(results[["Fmeasure(0.8)"]], performance["Fmeasure",])
}
saveRDS(results, "figures/parameter-settings.rds")

results <- readRDS("figures/parameter-settings.rds")
df <- melt(results[["AUC"]])
df$Var2 <- factor(df$Var2, levels=c("C50.1trial","C50.100trials","RF.10trees","RF.100trees","GLM"))
ggplot(df, aes(x=Var2, y=value)) + geom_boxplot()  + ylab("AUC") + xlab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(breaks = 10:18*0.05, labels = 10:18*0.05, limit=c(0.5,0.9))
ggsave("figures/4-parameter-settings.pdf",width=4,height=4)

############################################################
# STEP5: USE OUT-OF-SAMPLE BOOTSTRAP
############################################################

results <- NULL
indep <- AutoSpearman(data, eclipse$indep)
f <- as.formula(paste( "post", '~', paste(indep, collapse = "+")))

for(i in seq(1,100)){
  set.seed(1234+i)
  indices <- sample(nrow(data), replace=TRUE)
  training <- data[indices,]
  testing <- data[-indices,]
  
  m <- glm(f, data = training, family="binomial")
  predictions <- predict(m, testing, type="response")
  performance <- performance.calculation(testing[,dep], predictions)
  results <- rbind(results, c(method="100 Bootstrap",performance["AUC"]))
}

# 10x10-folds CV
for(i in seq(1,10)){
  set.seed(1234+i)
  indices <- createFolds(data[, dep], k = 10, list = TRUE, returnTrain = TRUE)
  for(i in seq(1,10)){
    training <- data[indices[[i]],]
    testing <- data[-indices[[i]],]
    
    m <- glm(f, data = training, family="binomial")
    predictions <- predict(m, testing, type="response")
    performance <- performance.calculation(testing[,dep], predictions)
    results <- rbind(results, c(method="10X10-Fold CV",performance["AUC"]))
  }
}

results <- data.frame(results)
results$AUC <- as.numeric(as.character(results$AUC))

ggplot(melt(results), aes(x=method, y=value)) + geom_boxplot() + ylab("value") + xlab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~variable, scales="free_y") 
ggsave("figures/5-model-validation.pdf",width=4,height=5)

############################################################
# STEP6: SUMMARIZE BY A ScottKnott-ESD TEST 
############################################################

importance <- NULL
indep <- AutoSpearman(data, eclipse$indep)
f <- as.formula(paste( "post", '~', paste(indep, collapse = "+")))
for(i in seq(1,100)){
  indices <- sample(nrow(data), replace=TRUE)
  training <- data[indices,]
  m <- glm(f, data = training, family="binomial")
  importance <- rbind(importance, Anova(m,type="2",test="LR")$"LR Chisq")
}

importance <- data.frame(importance)
colnames(importance) <- indep

df <- melt(importance)
df$rank <- sk_esd(importance)$groups[as.character(df$variable)]

ggplot(df, aes(x=variable, y=value)) + geom_boxplot() + facet_grid(~rank, scales = "free", drop = TRUE) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/6-scottknott.pdf",width=5,height=5)

############################################################
# STEP7: VISUALIZE THE RELATIONSHIPS
############################################################
indep <- AutoSpearman(data, eclipse$indep)
f <- as.formula(paste( "post", '~', paste(indep, collapse = "+")))
m <- glm(f, data = data, family="binomial")
pdf("figures/7-relationship.pdf",width=4,height=4)
plot(effect("pre",m))
dev.off()

############################################################
# DON'T USE ANOVA TYPE-I
############################################################

indep1 <- c("TLOC","NSF_max","NSM_max","NOF_max")
f <- as.formula(paste0(dep, " ~ ", paste0(indep1,collapse = "+")))
m <- glm(f, data=data, family="binomial")
anova(m)
Anova(m)

# ANOVA Explaination

### The Deviance of MaxInheritanceTree for ANOVA Type-I
glm(post ~  1, data=data, family="binomial")$deviance - glm(post ~  MaxInheritanceTree, data=data, family="binomial")$deviance

### The Deviance of MaxInheritanceTree for ANOVA Type-II
glm(post ~  CountClassDerived + AvgLineBlank  +  MINOR_LINE + CountClassCoupled, data=data, family="binomial")$deviance-glm(post ~  MaxInheritanceTree + CountClassDerived + AvgLineBlank  +  MINOR_LINE + CountClassCoupled, data=data, family="binomial")$deviance

# The risks of reordering model specification

indep1 <- c("NSF_max","NSM_max","NOF_max","ACD")
f <- as.formula(paste0(dep, " ~ ", paste0(indep1,collapse = "+")))
m <- glm(f, data=data, family="binomial")

importance1 <- data.frame(Type1.1=anova(m)$Deviance[-1], Type2.1=Anova(m,type="2",test="LR")$"LR Chisq")
rownames(importance1) <- indep1

indep2 <- c("NSM_max","ACD","NSF_max","NOF_max")
f <- as.formula(paste0(dep, " ~ ", paste0(indep2,collapse = "+")))
m <- glm(f, data=data, family="binomial")
importance2 <- data.frame(Type1.2=anova(m)$Deviance[-1], Type2.2=Anova(m,type="2",test="LR")$"LR Chisq")
rownames(importance2) <- indep2

importance <- data.frame(importance1[indep1,],importance2[indep1,])
importance <- data.frame(apply(importance, 2, function(x){x/sum(abs(x))}))

round(importance[order(-importance$Type2.1),], digit=2)*100

################################################
# DON’T CHANGE PROBABILITY THRESHOLD
################################################

results <- readRDS("figures/parameter-settings.rds")
levels <- c("C50.100trials","RF.100trees","C50.1trial","RF.10trees","GLM")
g1<- ggplot(melt(data.frame(results[["Fmeasure(0.5)"]])), aes(x=factor(variable, levels=levels), y=value)) + geom_boxplot()  + ylab("F-measure (0.5)") + xlab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(labels=0:7*0.1, breaks=0:7*0.1, limit=c(0, 0.7))

g2<- ggplot(melt(data.frame(results[["Fmeasure(0.8)"]]),na.rm=TRUE), aes(x=factor(variable, levels=levels), y=value)) + geom_boxplot() + ylab("F-measure (0.8)") + xlab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(labels=0:7*0.1, breaks=0:7*0.1, limit=c(0, 0.7))

g3<- ggplot(melt(data.frame(results[["Fmeasure(0.2)"]]),na.rm=TRUE), aes(x=factor(variable, levels=levels), y=value)) + geom_boxplot()  + ylab("F-measure (0.2)") + xlab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(labels=0:7*0.1, breaks=0:7*0.1, limit=c(0, 0.7))

pdf("figures/Dont2-probability-thresholds.pdf",width=10,height=4)
grid.arrange(g1,g2,g3,ncol=3)
dev.off()  

################################################
# DON’T REBALANCE THE DATA
################################################

indep <- c("TLOC","PAR_max",'NOI',"NOF_max","FOUT_max","NSM_max","NSF_max","ACD","NOM_max")

original.m <- fit(data, dep, indep, classifier="lr", rebalance="no", validation="boot")
down.m <- fit(data, dep, indep, classifier="lr", rebalance="down", validation="boot")
up.m <- fit(data, dep, indep, classifier="lr", rebalance="up", validation="boot")

auc <- data.frame(Original=original.m$performance$AUC, 
                  UnderSampling=down.m$performance$AUC,
                  OverSampling=up.m$performance$AUC)
g1 <- ggplot(melt(auc), aes(x=variable, y=value)) + geom_boxplot() + theme_bw() + ylab("AUC Performance") + xlab("") + scale_y_continuous(breaks=12:20*0.05, limits = c(0.6,0.9)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

fmeasure <- data.frame(Original=original.m$performance$Fmeasure, 
                       UnderSampling=down.m$performance$Fmeasure,
                       OverSampling=up.m$performance$Fmeasure)
g2 <- ggplot(melt(fmeasure), aes(x=variable, y=value)) + geom_boxplot() + theme_bw() + ylab("F-Measure Performance") + xlab("") + scale_y_continuous(breaks=4:10*0.05, limits = c(0.2,0.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(g1,g2, ncol=2)

ggsave("figures/3-class-imbalance.pdf",width=5,height=5)

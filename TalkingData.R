#setwd("~/Rockhurst/BIA 6301 B/Lab/Project")

library(anytime)
options(scipen = 999)
library(cluster)
library(fpc)
library(klaR)
library(clustMixType)
library(dplyr)
library(caret)
library(rpart.plot)
devtools::install_github('ashata/StatsUtil')
library(StatsUtil)
library(lubridate)
library(dummies)
fraudTrainComplete.df <- read.csv('train_sample.csv', stringsAsFactors=FALSE)

#Get time in seconds between click time and download time
fraudTrainComplete.df$click_time<-mdy_hm(fraudTrainComplete.df$click_time)
fraudTrainComplete.df$attributed_time<-mdy_hm(fraudTrainComplete.df$attributed_time)

#fraudTrainComplete.df$Interval <- as.numeric(difftime(fraudTrainComplete.df$attributed_time, fraudTrainComplete.df$click_time, units = "min"))

fraudTrainComplete.df <- fraudTrainComplete.df %>% 
  mutate(day = day(click_time),
         hour = hour(click_time),
         minute = minute(click_time),
         timeClickDur = as.integer(difftime(fraudTrainComplete.df$click_time, as.POSIXct("2017-11-06 00:00:00"), units = "min")))

#bring target variable to first column
fraudTrainComplete.df <- fraudTrainComplete.df[, c(8,1:7,9:12)]

#drop click time and attributed time as more than 90% of attr time is NA and is attributed is a duplicate of attributed time
#we will use click dur for EDA
fraudTrain.df <- fraudTrainComplete.df[, -c(7, 8)]

##EDA
library(glmnet)
options(scipen = 999)

fraudTrain.df$is_attributed = as.factor(fraudTrain.df$is_attributed)

#convert 
#factorial data will be used to find characteristics of the clusters
fraudTrainFactors.df<-fraudTrain.df
fraudTrainFactors.df$ip<- ifelse(is.na(fraudTrainFactors.df$ip), "Unknown IP", paste("IP",fraudTrainFactors.df$ip))
fraudTrainFactors.df$device<- ifelse(is.na(fraudTrainFactors.df$device), "Unknown Device", paste("Device",fraudTrainFactors.df$device))
fraudTrainFactors.df$os<- ifelse(is.na(fraudTrainFactors.df$os), "Unknown OS", paste("OS",fraudTrainFactors.df$os))
fraudTrainFactors.df$channel<- ifelse(is.na(fraudTrainFactors.df$channel), "Unknown Channel", paste("Channel",fraudTrainFactors.df$channel))
fraudTrainFactors.df$app<- ifelse(is.na(fraudTrainFactors.df$ip), "Unknown App", paste("App",fraudTrainFactors.df$app))

fraudTrainFactors.df$is_attributed<-
  ifelse(is.na(fraudTrainFactors.df$is_attributed), "No", 
         ifelse(fraudTrainFactors.df$is_attributed == '0', "No", "Yes"))
fraudTrainFactors.df$is_attributed<-as.factor(fraudTrainFactors.df$is_attributed)
fraudTrainFactors.df <- cbind(dfToFactors(fraudTrainFactors.df[,c(2:6)]), fraudTrainFactors.df[,c(1,7:10)])
fraudTrainFactors.df <- fraudTrainFactors.df[, c(6,1:5,8:10)]
write.csv(fraudTrainFactors.df, file = "CleanTrain.csv")

##EDA
#library(plotly)
#devtools::install_github('hadley/ggplot2')
library(ggplot2)
library(psych)

#pairs.panels(fraudTrainFactors.df[c("ip", "device", "channel", "os", "app")])
#pairs.panels(fraudTrainFactors.df[c("minute", "hour", "timeClickDur", "Interval")])

topIP<-fraudTrainFactors.df%>%
  group_by(ip, is_attributed)%>%
  summarise(total=n())%>%
  arrange(desc(total))

topRealIP<-fraudTrainFactors.df %>% subset(is_attributed == "Yes")%>%
  group_by(ip, is_attributed)%>%
  summarise(total=n())%>%
  arrange(desc(total))


#some of the top fraud ips have also downloaded the app, how to distinguish from fraud to lack of interest in app
ip1 <- ggplot(topIP[1:10,], aes(x = reorder(ip, -total), y = total, fill = is_attributed))+
geom_bar(stat="identity", position = position_dodge2())+
  theme(axis.text = element_text(face = "bold"))+
  scale_fill_brewer(palette="Set1")+
  labs(title="Top IP - All", x="IP",
       y="Total clicks", fill = "Is attributed")

#some of the top fraud ips have also downloaded the app, how to distinguish from fraud to lack of interest in app
ip2 <- ggplot(topRealIP[1:10,], aes(x = reorder(ip, -total), y = total, fill = is_attributed))+
  geom_bar(stat="identity", position = position_dodge2())+
  theme(axis.text = element_text(face = "bold"))+
  scale_fill_brewer(palette="Dark2")+
  labs(title="Top IP - Real", x="IP",
       y="Total clicks", fill = "Is attributed")

library(gridExtra)
requireNamespace("ggplot2", quietly = TRUE)
grid.arrange(ip1, ip2)

topDevice<-fraudTrainFactors.df%>%
  group_by(device, is_attributed)%>%
  summarise(total=n())%>%
  arrange(desc(total))

ggplot(topDevice[1:10,], aes(x = reorder(device, -total), y = total, fill = is_attributed))+
  geom_bar(stat="identity")+
  theme(axis.text = element_text(face = "bold"))+
  scale_fill_brewer(palette="Set1")+
  labs(title="Top Devices", x="Devices",
       y="Total clicks", fill = "Is attributed")

set.seed(235)
#some of the top fraud devices have also downloaded the app, how to distinguish from fraud to lack of interest in app
device1 <- ggplot(topDevice[1:5,], aes(x=reorder(device, -total), y=total, fill=is_attributed)) + 
  geom_bar (stat="identity", position = position_dodge2())+
  #geom_label(aes(label=total), position = position_identity())+
  theme(axis.text = element_text(face = "bold"))+
  scale_fill_brewer(palette="Set1")+
  labs(title="Top Devices - All", x="Devices",
       y="Total clicks", fill = "Is attributed")

topRealDevice <- topDevice %>% subset(is_attributed == "Yes")
device2 <- ggplot(topRealDevice[1:5,], aes(x=reorder(device, -total), y=total, fill=is_attributed)) + 
  geom_bar (stat="identity", position = position_stack())+
  #geom_label(aes(label=total), position = position_identity())+
  theme(axis.text = element_text(face = "bold"))+
  scale_fill_brewer(palette="Dark2")+
  labs(title="Top Devices - Real", x="Devices",
       y="Total clicks", fill = "Is attributed")

#require(gridExtra)
grid.arrange(device1, device2)

ggplot (fraudTrainFactors.df, aes(x=is_attributed, fill=is_attributed)) + 
  geom_bar(stat="count", position = position_dodge2())+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+
  theme(axis.text = element_text(face = "bold"))+
  scale_fill_brewer(palette="Set1")+
  labs(title="Groups of clicks", x="Is Attributed",
       y="Total clicks", fill = "Is attributed")

##All Association rules
library(arules)
allMatrix <- read.transactions("cleanTrain.csv", sep = ",")

image(sample(allMatrix, 100))
inspect(allMatrix[1:5])

itemFrequencyPlot(allMatrix, support = 0.2)
itemFrequencyPlot(allMatrix, topN = 10)

#apriori
allRules <- apriori(allMatrix, parameter = list(support =
                                                  0.1, confidence = 0.70, minlen =2))
inspect(allRules)
summary(allRules)

allrules.sorted <- sort(allRules, by = "lift")
inspect(allrules.sorted)

allRules.pruned<-allRules[!is.redundant(allrules.sorted)] #keeps only non-redundant rules
allRules.pruned <- sort(allRules.pruned, by = c("lift", "support"))
inspect(allRules.pruned)
summary(allRules.pruned)

library(arulesViz)
plot(allRules.pruned, method="grouped matrix", control=list(type="itemsets"))


#EDA real vs fraud clicks
realClicks.df <- fraudTrain.df %>% subset(is_attributed == "1")

pca_real <- prcomp(realClicks.df[, -c(1)], scale. = T)
pca_real
summary(pca_real)

pcsreal.variance.explained <-(pca_real$sdev^2 / sum(pca_real$sdev^2))*100
barplot(pcsreal.variance.explained, las=2, xlab="Principal Component", ylab="% Variance Explained", main="Principal Components versus Percent of Variance Explained")

screeplot(pca_real, type="line")

#use factorial data for meaningful rules, use k=4 based on scaled PCA
realClicks.df <- fraudTrainFactors.df %>% subset(is_attributed == "Yes")

realClicks_kProto <- kproto(realClicks.df[,-c(1)], k=4, nstart = 7) #you should try setting nstart > 1. 
realClicks_kProto
clprofiles(realClicks_kProto, realClicks.df)

write.csv(realClicks.df, file = "realclicks.csv", row.names = FALSE)

##Real user Association rules
library(arules)
realMatrix <- read.transactions("realclicks.csv", sep = ",")

image(sample(realMatrix, 100))
inspect(realMatrix[1:5])

itemFrequencyPlot(realMatrix, support = 0.2)
itemFrequencyPlot(realMatrix, topN = 10)

#apriori
realRules <- apriori(realMatrix, parameter = list(support =
                                                    0.1, confidence = 0.7, minlen =2))
inspect(realRules)
summary(realRules)

frules.sorted <- sort(realRules, by = "lift")
inspect(frules.sorted)

realRules.sorted <-sort(realRules, by = c("lift", "support"))
inspect(realRules.sorted)
summary(realRules.sorted)

realRules.pruned<-realRules[!is.redundant(realRules.sorted)] #keeps only non-redundant rules
realRules.pruned <- sort(realRules.pruned, by = c("lift", "support"))
inspect(realRules.pruned)
summary(realRules.pruned)

library(arulesViz)
plot(realRules.pruned, method="grouped matrix", control=list(type="itemsets"))


#EDA fraud clicks

fraudClicks.df <- fraudTrain.df %>% subset(is_attributed == "0")
fraudClicks.df <- fraudClicks.df[, c(2:6,8:10)]
pca_fraud <- prcomp(fraudClicks.df, scale. = T)
pca_fraud
summary(pca_fraud)

pcafraud.variance.explained <-(pca_fraud$sdev^2 / sum(pca_fraud$sdev^2))*100
barplot(pcafraud.variance.explained, las=2, xlab="Principal Component", ylab="% Variance Explained", main="Principal Components versus Percent of Variance Explained")

screeplot(pca_fraud, type="line")

fraudClicks.df <- fraudTrainFactors.df %>% subset(is_attributed == "No")
fraudClicks.df <- fraudTrainFactors.df[,-c(10)]

fraudClicks_kProto <- kproto(fraudClicks.df, k=2, nstart = 7) #you should try setting nstart > 1. 
fraudClicks_kProto
clprofiles(fraudClicks_kProto, fraudClicks.df)

write.csv(fraudClicks.df, file = "fraudclicks.csv", row.names = FALSE)

##Fraud Association rules
library(arules)
library("rlang", lib.loc="~/R/win-library/3.4")
fraudMatrix <- read.transactions("fraudclicks.csv", sep = ",")

image(sample(fraudMatrix, 100))
inspect(fraudMatrix[1:5])

itemFrequencyPlot(fraudMatrix, support = 0.2)
itemFrequencyPlot(fraudMatrix, topN = 10)

#apriori
apriori(fraudMatrix)
fraudRules <- apriori(fraudMatrix, parameter = list(support =
                                                      0.1, confidence = 0.7, minlen =2)) 
inspect(fraudRules)
summary(fraudRules)

frules.sorted <- sort(fraudRules, by = "lift")
inspect(frules.sorted)

fraudRules.sorted <-sort(fraudRules, by = c("lift", "support"))
fraudRules.pruned<-fraudRules[!is.redundant(fraudRules.sorted)] #keeps only non-redundant rules
fraudRules.pruned <- sort(fraudRules.pruned, by = c("lift", "support"))
inspect(fraudRules.pruned)
summary(fraudRules.pruned)

plot(fraudRules.pruned, method="grouped matrix", control=list(type="itemsets"))

##Predict

##There is over 300 levels in each of the factor variables
##We run in to problems with memory or "curse of dimensionality" problem if we use factorial data to train the models
##So we will use the plain integer dataset in the next part. 
library(caret)
#summary(fraudTrain.df$Interval)

data.df <- fraudTrain.df[,-c(11)]
data.df$is_attributed <- recode_factor(data.df$is_attributed,'0' = 'No', '1' = 'Yes')

trainIndex <- caret::createDataPartition(data.df$is_attributed, p = .7,list = FALSE,times = 1)
train.df <- data.df[trainIndex,]
test.df <- data.df[-trainIndex,] #notice the minus sign

#Kaggle feature engineering
#gc()
library(caret)
#model tuning
options(scipen = 999)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     selectionFunction = "best",
                     allowParallel = TRUE,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)
grid <- expand.grid(cp = 10^seq(-10, -2, by = 1))
fit.rpart <- train(is_attributed ~ ., 
                   data = train.df, 
                   method = "rpart", 
                   metric="ROC", trControl=ctrl, 
                   parms = list(split="gini"),
                   tuneGrid = grid)

#?make.names
registerDoSEQ()
print(fit.rpart)
plot(fit.rpart)
plot(varImp(fit.rpart))
library(rattle)

#not very meanigful Decision Tree as the features are considered as integers
fancyRpartPlot(fit.rpart$finalModel, palettes=c("Reds", "Greens"), main = "Is it a fraud click?", caption = "Decision Tree for clicks",
               cex=0.5, xcompact=FALSE, ycompact=FALSE, type=1)

set.seed(123)

dt_pred <- predict(fit.rpart, test.df, type="raw")

dt_tabs<-confusionMatrix(dt_pred, test.df$is_attributed,positive="Yes", dnn = c("Predicted", "Actual"))

fourfoldplot(dt_tabs$table, color = c("#ff3300", "#00e600"),
             conf.level = 0, margin = 1, main = "Model 2 - Prediction", space = 0.1)

library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations
library(caret)
library(dplyr)

##This dataset has an unbalanced predictor. Therefore the accuracy of No is better than Yes. In order to improve
#the accuracy of Yes which is what we need in this problem, let's try out different sampling methods to help improve the models
#Create model weights (they sum to one)
model_weights <- ifelse(train.df$is_attributed == "No",
                        (1/table(train.df$is_attributed)[1]) * 0.5,
                        (1/table(train.df$is_attributed)[2]) * 0.5)

ctrl1 <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     selectionFunction = "best",
                     allowParallel = TRUE,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

#Use the same seed to ensure same cross-validation splits
#ctrl$seeds <- fit.rpart$control$seeds

#Build weighted model NB is not a weighted model so weights may not do anything
ctrl$sampling <- "none"
weightedNB_fit <- train(is_attributed ~ .,
                      data = train.df,
                      method = "naive_bayes",
                      weights = model_weights,
                      metric="ROC", trControl=ctrl1)
#Build down-sampled model
ctrl1$sampling <- "down"

downNB_fit <- train(is_attributed ~ .,
                  data = train.df,
                  method = "naive_bayes",
                  metric = "ROC",
                  trControl = ctrl1)

#Build up-sampled model
ctrl1$sampling <- "up"

upNB_fit <- train(is_attributed ~ .,
                data = train.df,
                method = "naive_bayes",
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl1)

# Build smote model
ctrl1$sampling <- "smote"

smoteNB_fit <- train(is_attributed ~ .,
                   data = train.df,
                   method = "naive_bayes",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl1)

#Build down-sampled model
ctrl1$sampling <- "down"

downDT_fit <- train(is_attributed ~ .,
                  data = train.df,
                  method = "rpart",
                  metric = "ROC",
                  trControl = ctrl1)

#ROC
library(ROCR)
#Create a ROC curve
# Examine results for the 2 selected models. We can add all the models we trained above to model_list. 
#I did that to select the best performing version for each model but only showing the 2 for clean visual
model_list <- list(Model1a = weightedNB_fit,
                   Model1b = downNB_fit,
                   Model1c = upNB_fit,
                   Model1d = smoteNB_fit,
                   Model2a = fit.rpart,
                   Model2b = downDT_fit)

test_roc <- function(model, data) {
  
  roc(data$is_attributed,
      predict(model, data, type = "prob")[, "Yes"])
  
}

model_list_roc <- model_list %>%
  map(test_roc, data = test.df)

model_list_roc %>%
  map(auc)


results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for the models

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_colour_brewer(palette = "Accent") +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)+
  labs(title="Performance measure - Area under curve", x="False positive rate",
       y="True positive rate")

#confusion matrix
library(rattle)

confusionMatrixFun <- function(model, text) {
  plot(varImp(model), main="Model 2 Significant features", cex.main=1.5, cex.lab=2, cex.axis=1.5)
  
  set.seed(123)
  
  dt_pred1 <- predict(model, test.df, type="raw")
  
  dt_tabs1 <-confusionMatrix(dt_pred1, test.df$is_attributed,positive="Yes", dnn = c("Predicted", "Actual"))
  
  fourfoldplot(dt_tabs1$table, color = c("#ff3300", "#00e600"),
               conf.level = 0, margin = 1, main = text, space = 0.1)
  
  prop.table(dt_tabs1$table)
}

confusionMatrixFun(downDT_fit, "Model 2 prediction")
confusionMatrixFun(upNB_fit, "Model 1 prediction")



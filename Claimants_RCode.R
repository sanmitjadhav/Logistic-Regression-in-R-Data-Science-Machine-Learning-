claimants <- read.csv(file.choose())# Choose the claimants Data set
View(claimants)
data=claimants[,-1]
View(data)
claimants <- na.omit(data)
View(claimants)
model <- glm(ATTORNEY~.,data=claimants,family = "binomial")
summary(model)
# Confusion matrix table 
prob <- predict(model,type=c("response"),claimants)
View(prob)
confusion<-table(prob>0.5,claimants$ATTORNEY)
confusion

confusion# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy#70.52
1-Accuracy
sum(confusion[cbind(2:1, 1:2)])/sum(confusion)
?diag

##
pred_values <- NULL
pred_values
yes_no <- NULL
for (i in 1:1096){
  pred_values[i] <- ifelse(prob[i]>=0.5,1,0)
  yes_no[i] <- ifelse(prob[i]>=0.5,"yes","no")
}

claimants[,"prob"] <- prob
View(prob)
View(claimants)
claimants[,"pred_values"] <- pred_values
View(pred_values)
View(claimants)
claimants[,"yes_no"] <- yes_no
View(claimants)

View(claimants[,c(2,7,8,9)])

# Accuracy 
acc <- table(claimants$ATTORNEY,pred_values)
acc
Accuracy<-sum(diag(acc)/sum(acc))
Accuracy # 70.62

# ROC Curve 
#install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,claimants$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(1000,0))



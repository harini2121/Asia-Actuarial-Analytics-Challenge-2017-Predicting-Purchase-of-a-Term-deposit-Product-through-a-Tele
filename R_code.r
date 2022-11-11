auc <- function(actual, predicted)
{
  r <- rank(predicted)
  n_pos <- sum(actual==1)*1.0
  n_neg <- length(actual) - n_pos
  auc <- (sum(r[actual==1]) - n_pos*(n_pos+1)/2) / (n_pos*n_neg)
  auc
}
require(data.table)
train.data <- fread("C:/Users/HariniG/Documents/SAS/SAS_Train_Data_v3.xls",stringsAsFactors = T)	
test.data <- fread("C:/Users/HariniG/Documents/SAS/SAS_Test_Data_v3.xls",stringsAsFactors = T)

train.data[train.data==""]<- NA
sapply(train.data, function(x) sum(is.na(x)))

submissionData <- subset(test.data,select=c(CustomerID))

train.data <- subset(train.data,select=-c(CustomerID))
test.data <- subset(test.data,select=-c(CustomerID))


set.seed(2389)

indexes = sample(1:nrow(train.data), size=0.8*nrow(train.data))
subtrain.data = train.data[indexes,]

validation.data = train.data[-indexes,]

#library(dummies)
#data.ind=subset(train.data,select =c(1:21) )
#data.ind$Sex=dummy(train.data$Sex)
#cor(train.data$Contact_Month,train.data$Outcome)

data.small <-na.omit(subset(subtrain.data,select = c(2,9,10,12,15,16,17,18,20,21)))
glm0 <- glm(Outcome ~ .,data=data.small,family=binomial())

summary(glm0)


Pred_glm0 <- matrix(0, nrow(data.small))
Pred_glm0 = predict(glm0, newdata=data.small,type="response")

auc.train <-auc(data.small$Outcome,Pred_glm0)
auc.train


Pred_glm1 <- matrix(0, nrow(validation.data))
Pred_glm1 = predict(glm0, newdata=validation.data,type="response")
summary(Pred_glm1)
# check AUC of validation dataset
auc.validation <-auc(validation.data$Outcome,Pred_glm1)
auc.validation


# Now use modeled results to predict on test dataset
Pred_glm2 <- matrix(0, nrow(test.data))
Pred_glm2 = predict(glm0, newdata=test.data,type="response")


# Prepare final prediction results 
submissionData$Outcome <- Pred_glm2

# Write prediction results to a file
test.names <- c('CustomerID', 'Outcome')
write.csv(submissionData[,test.names, with=FALSE], file='FinalResults.csv',row.names = FALSE)

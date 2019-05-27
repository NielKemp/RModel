#Logistic Regression Example Code



#Url no longer working
inputData <- read.csv("http://rstatistics.net/wp-content/uploads/2015/09/adult.csv")
head(inputData)

#summarize target variable ABOVE50K to see "response rate"
table(inputData$ABOVE50K) # 24720 0's and 7841 1's

# Create Training Data
input_ones <- inputData[which(inputData$ABOVE50K == 1), ]  # all 1's
input_zeros <- inputData[which(inputData$ABOVE50K == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
#sample 70%
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ] #select the inverse of the training indexes
test_zeros <- input_zeros[-input_zeros_training_rows, ] #select the inverse of the training indexes
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 




library(smbinning)
# segregate continuous and factor variables
factor_vars <- c ("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")
continuous_vars <- c("AGE", "FNLWGT","EDUCATIONNUM", "HOURSPERWEEK", "CAPITALGAIN", "CAPITALLOSS")

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(14))  # init for IV results

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="ABOVE50K", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="ABOVE50K", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df


#Build Logit models and generate some predictions
logitMod <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=trainingData, family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores

#model diagnostics
summary(logitMod)

#check for multicolinnearity
vif(logitMod)


library(InformationValue)
optCutOff <- optimalCutoff(testData$ABOVE50K, predicted)[1] #find optimal cutoff
misClassError(testData$ABOVE50K, predicted, threshold = optCutOff)

#plot ROC Curve
plotROC(testData$ABOVE50K, predicted)

#Check concordance
Concordance(testData$ABOVE50K, predicted)


#run on validation data (check bias and variance)
sensitivity(testData$ABOVE50K, predicted, threshold = optCutOff)
specificity(testData$ABOVE50K, predicted, threshold = optCutOff)
confusionMatrix(testData$ABOVE50K, predicted, threshold = optCutOff)

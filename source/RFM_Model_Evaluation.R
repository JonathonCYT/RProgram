library(rfm)
DT <- read.csv("data/OnlineRetail.csv")
str(DT)

DT[, 2] <- as.Date(as.character(DT[,2]),"%Y%m%d")
str(DT)

DT <- DT[order(DT$Date,decreasing = TRUE), ]

#a) Reset the time frame
History_endDate <- as.Date("2018-03-30")
Outcome_endDate <- as.Date("2018-06-30")

################################ 
## Split the data into history and Outcome period
################################
#b)Split and develop logistic regression model
HistDT <- DT[DT$Date <= History_endDate, ]
OutcomeDT <- DT[DT$Date > History_endDate, ]

Hist_RFM <- rfm_table_order(data = HistDT,
                           customer_id = ID,
                           order_date = Date,
                           revenue = Amount,
                           analysis_date = History_endDate)

RFMDT <- Hist_RFM$rfm
View(RFMDT)
OutcomeID <- OutcomeDT$ID[!duplicated(OutcomeDT$ID)]
# Generate the indicator whether the customer buy anything from the website.
RFMDT$Buy <- ifelse(RFMDT$customer_id %in% OutcomeID, 1, 0)

## Evaluating Accuracy, Precision, and Recall
## Install "caret" package if you use the package for the first time
# install.packages("caret")
library(caret)
set.seed(42)

##########################################   
## Split the data into training and testing
##########################################
RFMDT$Buy = as.factor(RFMDT$Buy)
inTrain = createDataPartition(RFMDT$Buy, p = 2/3, list = FALSE)
inTrain = as.vector(inTrain)  
Train = RFMDT[inTrain, ]
Test = RFMDT[-inTrain, ]

################################ 
## Train the model
################################
LogitReg = glm(Buy~recency_days + transaction_count + amount,
            family=quasibinomial(link='logit'),
            data=Train)
summary(LogitReg)

############################################ 
## Predict the outcome using the test data
############################################
#c)Predict the retention probability for the customers
BuyProb <- predict(LogitReg, Test, type = "response")
print(BuyProb)

# If p exceeds threshold of 0.5, 1 else 0
#d)Classify the customer
PredBuy <- ifelse(BuyProb > 0.5, 1, 0)
# Convert to factor: p_class
PredBuy <- factor(PredBuy, levels = levels(Test$Buy))
print(PredBuy)

# Create confusion matrix
#e)Generate confusion matrix
LogitConfuse = confusionMatrix(PredBuy, Test$Buy, positive = "1")
print(LogitConfuse)

#2)To improve precision increase threshold
# If p exceeds threshold of 0.9, 1 else 0
PredBuy2 <- ifelse(BuyProb > 0.99, 1, 0)

# Convert to factor: p_class
PredBuy2 <- factor(PredBuy2, levels = levels(Test$Buy))

# Create confusion matrix
LogitConfuse = confusionMatrix(PredBuy2, Test$Buy, positive = "1")
print(LogitConfuse)

#3)To improve sensitivity
# If p exceeds threshold of 0.1, 1 else 0
PredBuy3 <- ifelse(BuyProb > 0.1, 1, 0)

# Convert to factor: p_class
PredBuy3 <- factor(PredBuy2, levels = levels(Test$Buy))

# Create confusion matrix
LogitConfuse = confusionMatrix(PredBuy2, Test$Buy, positive = "1")
print(LogitConfuse)

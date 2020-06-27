churn_raw = read.csv("dataset_churn.csv", header = TRUE)
str(churn_raw)
sum(is.na(churn_raw))
sapply(churn_raw, function(x) sum(is.na(x)))
churn_raw[is.na(churn_raw$TotalCharges),1:6]

###General Discussion #####
### Gender wise Churn #####
ggplot(churn_raw) +
geom_bar(aes(x = gender, fill = Churn), position = "dodge")

churn_raw %>%
  group_by(gender,Churn) %>%
  summarise(n=n())

##### Senior citizen Churn ####
ggplot(churn_raw) +
  geom_bar(aes(x = SeniorCitizen, fill = Churn), position = "dodge")

churn_raw %>%
  group_by(SeniorCitizen, Churn) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

##### Contract wise Churn ####
ggplot(churn_raw) +
  geom_bar(aes(x = Contract, fill = Churn), position = "dodge")

churn_raw %>%
  group_by(Contract, Churn) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

##### Payment Mode wise Churn ####
ggplot(churn_raw) +
  geom_bar(aes(x = PaymentMethod, fill = Churn), position = "dodge")

churn_raw %>%
  group_by(PaymentMethod, Churn) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

library(dplyr)
churn_raw %>%
  filter(tenure == 0) %>%
  summarize("Zero Tenure" = n())

churnnoNAs <- churn_raw[complete.cases(churn_raw),]
dim(churnnoNAs)

#####Cleaning the Data###
churn_neat <- churnnoNAs %>%
  select(-customerID, -TotalCharges) %>%
  rename(Gender = gender, Tenure = tenure)
table(churn_neat$SeniorCitizen)

churn_neat$SeniorCitizen <- as.factor(ifelse(churn_neat$SeniorCitizen == 1, "Yes", "No"))
table(churn_neat$SeniorCitizen)
str(churn_neat)

factorrenames <- names(churn_neat[9:14])
data <- churn_neat %>% 
  mutate_at(.vars=factorrenames,
            .funs=~recode_factor(., `No internet service`="No")) %>%
  mutate_at(.vars="MultipleLines",
            .funs=~recode_factor(., `No phone service`="No"))
str(data)

#####Data Exploration ####
churnrate <- table(data$Churn) / nrow(data)
churnrate

set.seed(1)
rowindices <- sample(nrow(data))
data_shuffled <- data[rowindices,]

split <- round(nrow(data_shuffled) * 0.7)
split

train <- data_shuffled[1:split,]
test <- data_shuffled[(split+1):nrow(data_shuffled),]
dim(train)
dim(test)

library(caret)
control <- trainControl(method = "cv",number = 10,summaryFunction = twoClassSummary,classProbs = TRUE,verboseIter = FALSE)

#### KNNNl #####
knn_model <- train(Churn ~ ., data = train,method = "knn", trControl = control,preProcess = c("center","scale"), tuneLength = 50)
knn_model

####Predictive Capability#####
knn_pred <- predict(knn_model, newdata = test)
test <- data_shuffled[(split+1):nrow(data_shuffled),]
confusionMatrix(knn_pred, test)
knnaccuracy <- knncm$overall[c(1,3,4)]
knncm
confusionMatrix(knn_pred$Churn, churn_raw$Churn)

### Logistic Regression ####
glm_model <- train(Churn ~ ., data = train,method="glm", trControl = control)
glm_model
glm_pred <- predict(glm_model, newdata = test)
glmcm <- confusionMatrix(glm_pred, test[["Churn"]])
glmaccuracy <- glmcm$overall[c(1,3,4)]
glmcm

#### Identifying attributes of customers likely to churn #####
summary(glm_model)

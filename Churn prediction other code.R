churn = read.csv("Churn1.csv", header = TRUE)
summary(churn)
read.csv(churn)
summary(churn)

##Missing data ###
library(VIM)
aggr(churn)

set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
churn = read.csv("Churn.csv", header = TRUE)

# calculate correlation matrix
correlationMatrix <- cor(churn[,1:18])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
churn = read.csv("Churn.csv", header = TRUE)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model1
model <- train(Churn~., data=churn, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
churn = read.csv("Churn.csv", header = TRUE)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(churn[,2:18], churn[,1], sizes=c(1:18), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


#Association rule using Chi-Squared test to identify association between features and churn
library(MASS)       # load the MASS package 

#Churn vs gender
chu = table(churn$Churn, churn$gender) 
chu   
chisq.test(chu) #Dependant variable

#Churn vs senior citizen
chu1 = table(churn$Churn, churn$SeniorCitizen) 
chu1   
chisq.test(chu1) #Idependant variable

#Churn vs partner
chu2 = table(churn$Churn, churn$Partner) 
chu2   
chisq.test(chu2) #Idependant variable

#Churn vs Dependants
chu3 = table(churn$Churn, churn$Dependents) 
chu3   
chisq.test(chu3) #Idependant variable

#Churn vs tensure
chu4 = table(churn$Churn, churn$tenure) 
chu4   
chisq.test(chu4) #Idependant variable

#Churn vs phone service
chu5 = table(churn$Churn, churn$PhoneService) 
chu5   
chisq.test(chu5) #Dependant variable

#Churn vs Multiline
chu6 = table(churn$Churn, churn$MultipleLines) 
chu6   
chisq.test(chu6) #Idependant variable

#Churn vs InternetService
chu7 = table(churn$Churn, churn$InternetService) 
chu7   
chisq.test(chu7) #Idependant variable

#Churn vs OnlineSecurity
chu8 = table(churn$Churn, churn$OnlineSecurity) 
chu8   
chisq.test(chu8) #Idependant variable

#Churn vs OnlineBackup
chu9 = table(churn$Churn, churn$OnlineBackup) 
chu9   
chisq.test(chu9) #Idependant variable

#Churn vs DeviceProtection
chu10 = table(churn$Churn, churn$DeviceProtection) 
chu10   
chisq.test(chu10) #Idependant variable

#Churn vs techsupport
chu11 = table(churn$Churn, churn$TechSupport) 
chu11   
chisq.test(chu11) #Idependant variable

#Churn vs streaming TV
chu12 = table(churn$Churn, churn$StreamingTV) 
chu12   
chisq.test(chu12) #Idependant variable

#Churn vs streaming movies
chu13 = table(churn$Churn, churn$StreamingMovies) 
chu13
chisq.test(chu13) #Idependant variable

#Churn vs contract
chu14 = table(churn$Churn, churn$Contract) 
chu14
chisq.test(chu14) #Idependant variable

#Churn vs PaperlessBilling
chu15 = table(churn$Churn, churn$PaperlessBilling) 
chu15
chisq.test(chu15) #Idependant variable

#Churn vs PaymentMethod
chu16 = table(churn$Churn, churn$PaymentMethod) 
chu16
chisq.test(chu16) #Idependant variable

#Churn vs MonthlyCharges
chu17 = table(churn$Churn, churn$MonthlyCharges) 
chu17
chisq.test(chu17) #Idependant variable

#Churn vs Total charges
chu18 = table(churn$Churn, churn$TotalCharges) 
chu18
chisq.test(chu18) #Dependant variable
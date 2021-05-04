setwd("C:/Users/djdit/OneDrive - University of South Florida/Statistical Data Mining/assignments/telco-churn")

library(readxl)
df <- read_excel("TelcoChurn.xlsx")
View(df)
str(df)

colSums(is.na(df))                  #11 rows with NA on total charges
dim(df)                             #Check dimensions before cleaning

#Cleaning and Preprocessing
df <- df[complete.cases(df), ]      #Drop NAs
dim(df)                             #Make sure only 11 rows dropped

#Set non-integers to "Yes"/"No" factors
df$Churn <- as.numeric(ifelse(df$Churn=="No",0,1))
df$SeniorCitizen <- factor(df$SeniorCitizen, labels=c("No", "Yes"))  #Change from dummy var to factor
df$InternetService <- ifelse(df$InternetService=="DSL" | df$InternetService=="Fiber optic", "Yes", "No")
df$MultipleLines <- ifelse(df$MultipleLines=="No" | df$MultipleLines=="No phone service", "No", "Yes")
df$OnlineSecurity <- ifelse(df$OnlineSecurity=="No" | df$OnlineSecurity=="No internet service", "No", "Yes")
df$OnlineBackup <- ifelse(df$OnlineBackup=="No" | df$OnlineBackup=="No internet service", "No", "Yes")
df$DeviceProtection <- ifelse(df$DeviceProtection=="No" | df$DeviceProtection=="No internet service", "No", "Yes")
df$TechSupport <- ifelse(df$TechSupport=="No" | df$TechSupport=="No internet service", "No", "Yes")
df$StreamingTV <- ifelse(df$StreamingTV=="No" | df$StreamingTV=="No internet service", "No", "Yes")
df$StreamingMovies <- ifelse(df$StreamingMovies=="No" | df$StreamingMovies=="No internet service", "No", "Yes")
df$Contract <- ifelse(df$Contract=="One year" | df$Contract=="Two year", "Yes", "No")


cols <- c("gender", "Partner", "Dependents", "PhoneService", "MultipleLines", "InternetService",
          "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", 
          "StreamingMovies", "Contract", "PaymentMethod", "PaperlessBilling")

df[cols] <- lapply(df[cols], factor)
str(df)

df$PaymentMethod <- relevel(df$PaymentMethod, ref="Mailed check")

#Create Phone service only subset

phone <- subset(df, PhoneService=="Yes" & InternetService=="No" , 
                select=c("gender", "SeniorCitizen", "Partner", "Dependents", "tenure", 
                         "MultipleLines", "Contract", "PaymentMethod",  "Churn", 
                         "PaperlessBilling","PhoneService", "InternetService", "MonthlyCharges"))

table(phone$PhoneService, phone$InternetService)
str(phone)

#Create internet only subset

internet <- subset(df, InternetService=="Yes" & PhoneService=="No", 
                   select=c("gender", "SeniorCitizen", "Partner", "Dependents", "tenure",
                            "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport",
                            "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling",
                            "PaymentMethod", "MonthlyCharges", "Churn", "PaperlessBilling", 
                            "InternetService", "PhoneService"))
table(internet$PhoneService, internet$InternetService)
str(internet)

#Create internet + phone service subset

ph_int <- subset(df, InternetService=="Yes" & PhoneService=="Yes", 
                 select=c("gender", "SeniorCitizen", "Partner", "Dependents", "tenure",  "MultipleLines",
                          "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", 
                          "StreamingMovies","Contract", "PaymentMethod", "Churn", "PaperlessBilling",
                          "MonthlyCharges", "PhoneService","InternetService"))

table(ph_int$PhoneService, ph_int$InternetService)
str(ph_int)

# lm models to compare as baseline for logit models

# Phone only model
lmp <- lm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + 
            MultipleLines + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, data=phone)         

summary(lmp)

# Internet only model

lmi <- lm(Churn ~ gender + SeniorCitizen + Partner + Dependents +
           tenure +  OnlineBackup + OnlineSecurity + DeviceProtection + TechSupport + 
           StreamingMovies + StreamingTV + Contract + PaperlessBilling + PaymentMethod + 
           MonthlyCharges, data=internet)

summary(lmi)

# Phone + Internet model 

lmpi <- lm(Churn ~ gender + SeniorCitizen + Partner + Dependents +
           tenure + MultipleLines + OnlineBackup + OnlineSecurity + 
           DeviceProtection + TechSupport + StreamingMovies + StreamingTV + Contract + 
           PaperlessBilling + PaymentMethod + MonthlyCharges, data=ph_int)

summary(lmpi)

library(stargazer)
stargazer(lmp, lmi, lmpi, type="text", single.row=TRUE)

# phone only logit
set.seed(1024)
p_trainIndex <- sample(1:nrow(df), size=round(0.75*nrow(phone)), replace=FALSE)
p_train <- phone[p_trainIndex,]
p_test  <- phone[-p_trainIndex,]
dim(p_train); dim(p_test)

p_logit <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + MultipleLines + 
                 Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, data=p_train, family=binomial(link=logit))
summary(p_logit)
p_test_x <- p_test[ , c(1:13)]
p_predlogit <-predict(p_logit, newdata=p_test_x, type="response")
p_predlogit <- ifelse(p_predlogit>0.5, 1, 0)

p_ClassificationError <- mean(p_predlogit != p_test$Churn)
print(paste("Accuracy = ", 1-p_ClassificationError))        
table(p_test$Churn, p_predlogit)    

exp(p_logit$coefficients)  #convert betas to exp(beta) for interpretation

# internet only logit
i_trainIndex <- sample(1:nrow(df), size=round(0.75*nrow(internet)), replace=FALSE)
i_train <- internet[i_trainIndex,]
i_test  <- internet[-i_trainIndex,]
dim(i_train); dim(i_test)

i_logit <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents +
                 tenure +  OnlineBackup + OnlineSecurity + DeviceProtection + TechSupport + 
                 StreamingMovies + StreamingTV + Contract + PaperlessBilling + PaymentMethod + 
                 MonthlyCharges, data=i_train, family=binomial(link=logit))
summary(i_logit)
i_test_x <- i_test[ , c(1:19)]
i_predlogit <-predict(i_logit, newdata=i_test_x, type="response")
i_predlogit <- ifelse(i_predlogit>0.5, 1, 0)

i_ClassificationError <- mean(i_predlogit != i_test$Churn)
print(paste("Accuracy = ", 1-i_ClassificationError))        
table(i_test$Churn, i_predlogit)     

exp(i_logit$coefficients)  #convert betas to exp(beta) for interpretation


# phone and internet logit
pi_trainIndex <- sample(1:nrow(df), size=round(0.75*nrow(ph_int)), replace=FALSE)
pi_train <- ph_int[pi_trainIndex,]
pi_test  <- ph_int[-p_trainIndex,]
dim(pi_train); dim(pi_test)


pi_logit <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents +
                  tenure + MultipleLines + OnlineBackup + OnlineSecurity + 
                  DeviceProtection + TechSupport + StreamingMovies + StreamingTV + Contract + 
                  PaperlessBilling + PaymentMethod + MonthlyCharges, data=pi_train, family=binomial(link=logit))
summary(pi_logit)
pi_test_x <- pi_test[ , c(1:19)]
pi_predlogit <-predict(pi_logit, newdata=pi_test_x, type="response")
pi_predlogit <- ifelse(pi_predlogit>0.5, 1, 0)

pi_ClassificationError <- mean(pi_predlogit != pi_test$Churn)
print(paste("Accuracy = ", 1-pi_ClassificationError))        
table(pi_test$Churn, pi_predlogit) 

exp(pi_logit$coefficients)  #convert betas to exp(beta) for interpretation


stargazer(p_logit, i_logit, pi_logit, type="text", single.row=TRUE)


#Recall, precision, F1, and AUC for each model

#install.packages("caret")
library(caret)            #Computes all classification evaluation metrics from the confusion matrix

#p_logit model
p_logitCM <- confusionMatrix(reference = as.factor(p_test$Churn), data=as.factor(p_predlogit), mode="everything")
p_logitCM

pr <- prediction(p_predlogit, p_test_x$Churn)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#i_logit model
i_logitCM <- confusionMatrix(reference = as.factor(i_test$Churn), data=as.factor(i_predlogit), mode="everything")
i_logitCM

pr <- prediction(i_predlogit, i_test_x$Churn)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#pi_logit model
pi_logitCM <- confusionMatrix(reference = as.factor(pi_test$Churn), data=as.factor(pi_predlogit), mode="everything")
pi_logitCM

pr <- prediction(pi_predlogit, pi_test_x$Churn)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

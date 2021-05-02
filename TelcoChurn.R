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

#Set non-integers to factors
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
          "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies",
          "Contract", "PaperlessBilling", "Churn")

df[cols] <- lapply(df[cols], factor)
str(df)



# How to handle payment method? - comibine auto/manual, make ordered factor??



#Create Phone service only subset

phone <- subset(df, PhoneService=="Yes" & InternetService=="No" , select=c("gender", "SeniorCitizen", "Partner", "Dependents", "tenure", 
                                                  "MultipleLines", "Contract", "PaymentMethod", "MonthlyCharges", 
                                                  "Churn", "PhoneService", "InternetService"))
table(phone$PhoneService, phone$InternetService)
str(phone)

#Create internet only subset

internet <- subset(df, InternetService=="Yes" & PhoneService=="No", select=c("gender", "SeniorCitizen", "Partner", "Dependents", "tenure", 
                                                                             "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", 
                                                                             "StreamingTV", "StreamingMovies", "Contract",  
                                                                             "PaymentMethod", "MonthlyCharges", "Churn", 
                                                                             "InternetService", "PhoneService"))
table(internet$PhoneService, internet$InternetService)
str(internet)


#Create internet + phone service subset

ph_int <- subset(df, InternetService=="Yes" & PhoneService=="Yes", select=c("gender", "SeniorCitizen", "Partner", "Dependents", "tenure",  "MultipleLines", 
                                                                            "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", 
                                                                            "StreamingMovies","Contract", "PaymentMethod", "Churn", 
                                                                            "MonthlyCharges", "PhoneService","InternetService"))


table(ph_int$PhoneService, ph_int$InternetService)
str(ph_int)


# Phone only model

lmp <- lm(Churn ~ gender + SeniorCitizen + Partner + Dependents +
           tenure + MultipleLines + Contract + MonthlyCharges, data=df)


summary(lmp)



# Interent only model

lmi <- lm(Churn ~ gender + SeniorCitizen + Partner + Dependents +
           tenure + PhoneService + MultipleLines + InternetService + OnlineBackup +
           OnlineSecurity + DeviceProtection + TechSupport + StreamingMovies + 
           StreamingTV + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, data=df)


summary(lmi)

# Phone + Interent model

lmpi <- lm(Churn ~ gender + SeniorCitizen + Partner + Dependents +
           tenure + PhoneService + MultipleLines + InternetService + OnlineBackup +
           OnlineSecurity + DeviceProtection + TechSupport + StreamingMovies + 
           StreamingTV + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, data=df)


summary(lmpi)
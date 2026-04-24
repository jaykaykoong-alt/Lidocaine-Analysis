data <- read.csv(file.choose())

# 2. Convert categorical variables to factors
data$TR_GENDER <- as.factor(data$TR_GENDER)
data$TR_CASE_TYPE <- as.factor(data$TR_CASE_TYPE)
data$HOSPITAL_LOC_ID <- as.factor(data$HOSPITAL_LOC_ID)
data$TR_SERVICE_LINE <- as.factor(data$TR_SERVICE_LINE)
data$INTRAOP_PAINMED_NOLIDOCAINE_FLAG <- as.factor(data$INTRAOP_PAINMED_NOLIDOCAINE_FLAG)

# 3. Decide how to handle ASA (I will factor it)
data$TR_ASA <- as.factor(data$TR_ASA)   # safest choice

# 4. Ensure continuous variables are numeric
data$BMI <- as.numeric(data$BMI)
data$case_duration <- as.numeric(data$case_duration)
data$PT_AGE_SURG_DT <- as.numeric(data$PT_AGE_SURG_DT)
data$tr_year <- as.numeric(data$tr_year)

vars <- c(
  "TR_GENDER",
  "TR_CASE_TYPE",
  "HOSPITAL_LOC_ID",
  "TR_SERVICE_LINE",
  "INTRAOP_PAINMED_NOLIDOCAINE_FLAG" ,
  "TR_ASA",
  "BMI",
  "case_duration",
  "PT_AGE_SURG_DT",
  "tr_year")

model <- glm(
  TR_Lidocaine.Infusion ~ TR_GENDER + TR_CASE_TYPE + HOSPITAL_LOC_ID +
    TR_SERVICE_LINE + TR_ASA + BMI +
    case_duration + PT_AGE_SURG_DT + tr_year + INTRAOP_PAINMED_NOLIDOCAINE_FLAG,
  family = binomial,
  data = data
)



exp(coef(model))

#Now we have ranked variables by effect size,now we will run Nagelkerke to see which varibales to keep
#Close to 10% increase we keep

install.packages("fmsb")
library(fmsb)

# Fit model as before
model <- glm(TR_Lidocaine.Infusion ~ HOSPITAL_LOC_ID, data = data, family = binomial)

# Compute Nagelkerke R²
NagelkerkeR2(model)

#Value is 0.2162, now we add INTRAOP_PAINMED_NOLIDOCAINE_FLAG 
model_pm <- glm(TR_Lidocaine.Infusion ~ HOSPITAL_LOC_ID + INTRAOP_PAINMED_NOLIDOCAINE_FLAG, data = data, family = binomial)
NagelkerkeR2(model_pm)
#New value is 0.2372, which increased by roughly 10%, so we keep INTRAOP_PAINMED_NOLIDOCAINE_FLAG
#Now we keep if there is a 10% increase from 0.2372
#Now we add ASA 

model_asa <- glm(TR_Lidocaine.Infusion ~ HOSPITAL_LOC_ID +INTRAOP_PAINMED_NOLIDOCAINE_FLAG + TR_ASA, data = data, family = binomial)
NagelkerkeR2(model_asa)

#Value is 0.2486 and does not increase by 10% so we drop ASA

#Now we do Case Type
model_case_type <- glm(TR_Lidocaine.Infusion ~ HOSPITAL_LOC_ID + INTRAOP_PAINMED_NOLIDOCAINE_FLAG + TR_CASE_TYPE, data = data, family = binomial)
NagelkerkeR2(model_case_type)
#This is roughly 10% so we round up and include case type.
#New Nagelkerke is 0.2532

#Now we add gender 
model_gender <- glm(TR_Lidocaine.Infusion ~ HOSPITAL_LOC_ID + INTRAOP_PAINMED_NOLIDOCAINE_FLAG+ TR_CASE_TYPE + TR_GENDER, data = data, family = binomial)
NagelkerkeR2(model_gender)

#Nagelkerke value is 0.2533, so not 10% increase, so drop gender

#Now we add year
model_year <- glm(TR_Lidocaine.Infusion ~ HOSPITAL_LOC_ID + INTRAOP_PAINMED_NOLIDOCAINE_FLAG + TR_CASE_TYPE + tr_year, data = data, family = binomial)
NagelkerkeR2(model_year)
#Nagelkerke value is 0.2553, so not 10% increase, so drop year

#Add BMI
model_bmi <- glm(TR_Lidocaine.Infusion ~ HOSPITAL_LOC_ID + INTRAOP_PAINMED_NOLIDOCAINE_FLAG + TR_CASE_TYPE + BMI, data = data, family = binomial)
NagelkerkeR2(model_bmi)
#Value is 0.252, so not 10% increase, so drop BMI

#Add Age
model_age <- glm(TR_Lidocaine.Infusion ~ HOSPITAL_LOC_ID + INTRAOP_PAINMED_NOLIDOCAINE_FLAG + TR_CASE_TYPE + PT_AGE_SURG_DT, data = data, family = binomial)
NagelkerkeR2(model_age)
#Value is 0.25503, so drop age, not 10%

#Add Case Duration 
model_case_duration <- glm(TR_Lidocaine.Infusion ~ HOSPITAL_LOC_ID + INTRAOP_PAINMED_NOLIDOCAINE_FLAG + TR_CASE_TYPE + case_duration, data = data, family = binomial)
NagelkerkeR2(model_case_duration)

#Value is 0.2584, so drop Case Duration because it is not a 10% increase

#Now we will run the logistic regression model with the kept variables
#ID 12216806= TR_KADLEC (Name of hospital)
#ID 12216279= TR_SWEDISH (Name of hospital)
#Other hospitals didn't have large enough effect to include 


data$TR_KADLEC <- as.factor(data$TR_KADLEC)
data$TR_SWEDISH <- as.factor(data$TR_SWEDISH)

data$TR_CASE_TYPE <- as.factor(data$TR_CASE_TYPE)

data$TR_Lidocaine.Infusion <- as.factor(data$TR_Lidocaine.Infusion)

data$INTRAOP_PAINMED_NOLIDOCAINE_FLAG <- as.factor(data$INTRAOP_PAINMED_NOLIDOCAINE_FLAG)

model <- glm(TR_Lidocaine.Infusion ~ TR_KADLEC + TR_SWEDISH + TR_CASE_TYPE + INTRAOP_PAINMED_NOLIDOCAINE_FLAG, 
             data = data, 
             family = binomial)

summary(model)


exp(cbind(OddsRatio = coef(model),
          confint(model)))


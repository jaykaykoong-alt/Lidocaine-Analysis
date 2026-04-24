# This will open a standard Mac 'Open File' window
library(readxl)
data <- read_excel(file.choose())

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

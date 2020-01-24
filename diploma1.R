dataDirectory <- "/Users/lulu/Desktop/"

data <- read.csv(paste(dataDirectory, 'white_mid.csv', sep=""), header = TRUE)
#mlr
model1 <- lm(Dose ~ Age+Height+Weight+Aspirin+Lovastatin+Amiodarone+Carbamazepine+Phenytoin+tINR+Smoker+Cyp2C9+VKORC1, data)

rmse <- function(error)
{ sqrt(mean(error^2))
}
mae <- function(error1)
{ mean(error1)
}  

summary(model1)
predDose <- predict(model1,data)
error <- data$Dose - predDose
mlrPredictionRMSE <- rmse(error)
error1 <- abs(predDose - data$Dose)
mlrPredictionMAE <- mae(error1)
exp(model1$coefficients)

library(e1071)
#svr
model2 <- svm(Dose ~ Age+Height+Weight+Aspirin+Lovastatin+Amiodarone+Carbamazepine+Phenytoin+tINR+Smoker+Cyp2C9+VKORC1,data)
summary(model2)
predictedDose <- predict(model2, data)
error <- data$Dose - predictedDose
svrPredictionRMSE <- rmse(error)
error1 <- abs(predictedDose - data$Dose)
svrPredictionMAE <- mae(error1)
W = t(model2$coefs) %*% model2$SV
b=model2$rho
#tuning
tuneResult <- tune(svm, Dose ~ Age+Height+Weight+Lovastatin+Amiodarone+Carbamazepi ne+Phenytoin+Smoker+Cyp2C9+VKORC1, data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 1:100))
print(tuneResult)
plot(tuneResult)
#saving plot
png(file="white_mid.png", bg="transparent") plot(tuneResult)
dev.off()
35

#best results
BestModel=tuneResult$best.model
PredDoseBest=predict(BestModel,data)
error <- data$Dose - PredDoseBest
RMSEbest <- rmse(error)
error1 <- abs(PredDoseBest - data$Dose)
MAEbest <- mae(error1)
W1 = t(BestModel$coefs) %*% BestModel$SV
b1 = BestModel$rho
library(xtable)
library(stargazer)
white_mod_mlr_tabel <- xtable(model1)
print(white_mod_mlr_tabel, type="html",
      file="white_mod_mlr_tabel.html")
white_W_tabel <- xtable(W)
print(white_W_tabel, type="html",
      file="white_W_tabel.html")
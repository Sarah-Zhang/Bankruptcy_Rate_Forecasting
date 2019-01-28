library(tseries)
library(forecast)
library(lawstat)
library(stats)
library(vars)

train <- read.csv("/Users/sarahzhang/Desktop/604_Time_Series/Final_Project/train.csv")
test <- read.csv("/Users/sarahzhang/Desktop/604_Time_Series/Final_Project/test.csv")

# devide training set based on test set mirror principle to get new train and test set for further calculate RMSE
train.new <- train[1:264,]
test.new <- train[265:288,]
train.UNE <- ts(train.new$Unemployment_Rate, start = c(1987, 1), frequency=12)
#test.UNE <- ts(test.new$Unemployment_Rate, start = c(2006, 1), frequency=12)
train.POP <- ts(train.new$Population, start = c(1987, 1), frequency=12)
#test.POP <- ts(test.new$Population, start = c(2006, 1), frequency=12)
train.HPI <- ts(train.new$House_Price_Index, start = c(1987, 1), frequency=12)
#test.HPI <- ts(test.new$House_Price_Index, start = c(2006, 1), frequency=12)
train.BR <- ts(train.new$Bankruptcy_Rate, start = c(1987, 1), frequency=12)
#test.BR <- ts(test.new$Bankruptcy_Rate, start = c(2006, 1), frequency=12)

# plot explanatory series
par(mfrow=c(3,1))
plot(train.UNE)
plot(train.POP)
plot(train.HPI)
# plot responce series
par(mfrow=c(1,1))
plot(train.BR)

# Try VAR(p)

# Account all four series
# select p
VARselect(y = data.frame(train.BR, train.UNE, train.POP, train.HPI))
# From above result, we have P = 6 or 8 or 10 optimal
# Fit Model with p = 6
m1.var <- VAR(y = data.frame(train.BR, train.UNE, train.POP, train.HPI), p = 6)
#plot(m1.var)
# Model forecasting and RMSE
pred1 <- predict(m1.var, n.ahead = 24, ci = 0.95)
plot(pred1)
rmse1.var <- sqrt(mean((pred1$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse1.var
# Fit Model with p = 8
m1.var.1 <- VAR(y = data.frame(train.BR, train.UNE, train.POP, train.HPI), p = 8)
#plot(m1.var.1)
# Model forecasting and RMSE
pred1.1 <- predict(m1.var.1, n.ahead = 24, ci = 0.95)
plot(pred1.1)
rmse1.var.1 <- sqrt(mean((pred1.1$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse1.var.1
# Fit Model with p = 10
m2.var <- VAR(y = data.frame(train.BR, train.UNE, train.POP, train.HPI), p = 10)
#plot(m2.var)
# Model forecasting and RMSE
pred2 <- predict(m2.var, n.ahead = 24, ci = 0.95)
plot(pred2)
rmse2.var <- sqrt(mean((pred2$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse2.var

# Account only BR, UNE and POP series
# select p
VARselect(y = data.frame(train.BR, train.UNE, train.POP))
# From above result, we have P = 8 or 10 optimal
# Fit Model with p = 8
m3.var <- VAR(y = data.frame(train.BR, train.UNE, train.POP), p = 8)
#plot(m3.var)
# Model forecasting and RMSE
pred3 <- predict(m3.var, n.ahead = 24, ci = 0.95)
plot(pred3)
rmse3.var <- sqrt(mean((pred3$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse3.var
# Fit Model with p = 10
m4.var <- VAR(y = data.frame(train.BR, train.UNE, train.POP), p = 10)
#plot(m4.var)
# Model forecasting and RMSE
pred4 <- predict(m4.var, n.ahead = 24, ci = 0.95)
plot(pred4)
rmse4.var <- sqrt(mean((pred4$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse4.var

# Account only BR, UNE and HPI series
# select p
VARselect(y = data.frame(train.BR, train.UNE, train.HPI))
# From above result, we have P = 3 or 5 or 6 optimal
# Fit Model with p = 3
m5.var <- VAR(y = data.frame(train.BR, train.UNE, train.HPI), p = 3)
#plot(m5.var)
# Model forecasting and RMSE
pred5 <- predict(m5.var, n.ahead = 24, ci = 0.95)
plot(pred5)
rmse5.var <- sqrt(mean((pred5$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse5.var
# Fit Model with p = 5
m6.var <- VAR(y = data.frame(train.BR, train.UNE, train.HPI), p = 5)
#plot(m6.var)
# Model forecasting and RMSE
pred6 <- predict(m6.var, n.ahead = 24, ci = 0.95)
plot(pred6)
rmse6.var <- sqrt(mean((pred6$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse6.var
# Fit Model with p = 6
m6.var.1 <- VAR(y = data.frame(train.BR, train.UNE, train.HPI), p = 6)
#plot(m6.var.1)
# Model forecasting and RMSE
pred6.1 <- predict(m6.var.1, n.ahead = 24, ci = 0.95)
plot(pred6.1)
rmse6.var.1 <- sqrt(mean((pred6.1$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse6.var.1

# Account only BR, POP and HPI series
# select p
VARselect(y = data.frame(train.BR, train.POP, train.HPI))
# From above result, we have P = 8 or 10 optimal
# Fit Model with p = 8
m7.var <- VAR(y = data.frame(train.BR, train.POP, train.HPI), p = 8)
#plot(m7.var)
# Model forecasting and RMSE
pred7 <- predict(m7.var, n.ahead = 24, ci = 0.95)
plot(pred7)
rmse7.var <- sqrt(mean((pred7$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse7.var
# Fit Model with p = 10
m8.var <- VAR(y = data.frame(train.BR, train.POP, train.HPI), p = 10)
#plot(m8.var)
# Model forecasting and RMSE
pred8 <- predict(m8.var, n.ahead = 24, ci = 0.95)
plot(pred8)
rmse8.var <- sqrt(mean((pred8$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse8.var

# Account only BR and UNE series
# select p
VARselect(y = data.frame(train.BR, train.UNE))
# From above result, we have P = 5 or 6 optimal
# Fit Model with p = 5
m9.var <- VAR(y = data.frame(train.BR, train.UNE), p = 5)
#plot(m9.var)
# Model forecasting and RMSE
pred9 <- predict(m9.var, n.ahead = 24, ci = 0.95)
plot(pred9)
rmse9.var <- sqrt(mean((pred9$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse9.var
# Fit Model with p = 6
m10.var <- VAR(y = data.frame(train.BR, train.UNE), p = 6)
#plot(m10.var)
# Model forecasting and RMSE
pred10 <- predict(m10.var, n.ahead = 24, ci = 0.95)
plot(pred10)
rmse10.var <- sqrt(mean((pred10$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse10.var

# Account only BR and POP series
# select p
VARselect(y = data.frame(train.BR, train.POP))
# From above result, we have P = 10 optimal
# Fit Model with p = 10
m11.var <- VAR(y = data.frame(train.BR, train.POP), p = 10)
#plot(m11.var)
# Model forecasting and RMSE
pred11 <- predict(m11.var, n.ahead = 24, ci = 0.95)
plot(pred11)
rmse11.var <- sqrt(mean((pred11$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse11.var

# Account only BR and HPI series
# select p
VARselect(y = data.frame(train.BR, train.HPI))
# From above result, we have P = 5 or 6 optimal
# Fit Model with p = 5
m12.var <- VAR(y = data.frame(train.BR, train.HPI), p = 5)
#plot(m12.var)
# Model forecasting and RMSE
pred12 <- predict(m12.var, n.ahead = 24, ci = 0.95)
plot(pred12)
rmse12.var <- sqrt(mean((pred12$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse12.var
# Fit Model with p = 6
m12.var.1 <- VAR(y = data.frame(train.BR, train.HPI), p = 6)
#plot(m12.var.1)
# Model forecasting and RMSE
pred12.1 <- predict(m12.var.1, n.ahead = 24, ci = 0.95)
plot(pred12.1)
rmse12.var.1 <- sqrt(mean((pred12.1$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse12.var.1

# "optimal" model:
# Based on the principle of getting the minimum RMSE, rmse11.var =  0.003846416 is the smallest among 
# all 12 RMSEs. That is, model m11.var, VAR(p) model account only BR and POP series with p = 10 is the "optimal"
# model for Vector Autoregressive process.

train.final <- train[1:288,]
BR <- ts(train.final$Bankruptcy_Rate, start = c(1987, 1), frequency=12)
POP <- ts(train.final$Population, start = c(1987, 1), frequency=12)
# Fit Model with p = 10
m11.var.final <- VAR(y = data.frame(BR, POP), p = 10)
#plot(m11.var.final)
# Model forecasting and RMSE
pred11.final <- predict(m11.var.final, n.ahead = 24, ci = 0.95)
plot(pred11.final)

par(mfrow=c(1,1))
t.new <- seq(2009,2011,length=25)[1:24]
plot(BR, xlim=c(1987,2011),ylab = "Bankruptcy_Rate", main = "Forecast of Bankruptcy_Rate using VAR(p) approach")
abline(v=2009,col='blue',lty=2) 
lines(pred11.final$fcst$BR[,1]~t.new,type='l',col='red')
lines(pred11.final$fcst$BR[,2]~t.new,col='green') 
lines(pred11.final$fcst$BR[,3]~t.new,col='green') 
legend("topleft", legend=c("Test Set data 2009-2010", "Prediction", "95% Prediction Interval"),col=c("black", "red", "green"), lty=1, cex=0.8)

result <- pred11.final$fcst$BR[,1:3]
write.csv(result, file = "result_test_2009_2010.csv", row.names=FALSE)

# Residual Diagnostics
# Variable Bankruptcy_Rate
e <- test.new$Bankruptcy_Rate - pred11.final$fcst$BR[,1]
# Zero-Mean
t.test(e)
# Since p-value = 0.0431, which is way bigger than the 0.01 significent level, we fail to reject H0 that the residuals have zero mean. Thus, Zero-Mean assumption is satisfied.
# Homoscedasticity
# Formally: Levene Test
plot(e, main="Residuals vs t", ylab="")
abline(v=c(5,10,15,20), lwd=3, col="red")
group <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,4))
levene.test(e,group)
bartlett.test(e,group) 
# Since p-value = 0.7392 > 0.01 for the Levene Test and p-value = 0.596 > 0.01 for the Bartlett test, we fail to reject H0 that the resisuals have constant variance. Thus, Homoscedasticity assumption is satisfied.
# Zero-Correlation
# Formally: Ljung-Box Test
Box.test(e,type = "Ljung-Box")
# Since p-value = 0.004989, which is smaller than the 0.01 significent level, we reject H0 that the residuals have zero correlation, based on 1 autocorrelation coefficients (lag = 1 by default). Thus, Zero-Correlation assumption is NOT satisfied.
# Normality
# Formally: Shapiro-Wilk Test
shapiro.test(e)
# Since p-value = 0.3787, which is bigger than the 0.01 significent level, we fail to reject H0 that the residuals are nornally distributed. Thus, the Normality assumption is satisfied.
## Zero Correlation not satisfied

# Variable Population
e <- test.new$Population - pred11.final$fcst$POP[,1]
# Zero-Mean
t.test(e)
# Since p-value < 2.2e-16, which is way smaller than the 0.01 significent level, we reject H0 that the residuals have zero mean. Thus, Zero-Mean assumption is NOT satisfied.
# Homoscedasticity
# Formally: Levene Test
plot(e, main="Residuals vs t", ylab="")
abline(v=c(5,10,15,20), lwd=3, col="red")
group <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,4))
levene.test(e,group)
bartlett.test(e,group) 
# Since p-value = 0.001661 < 0.01 for the Levene Test and p-value = 5.168e-07 < 0.01 for the Bartlett test, we reject H0 that the resisuals have constant variance. Thus, Homoscedasticity assumption is NOT satisfied.
# Zero-Correlation
# Formally: Ljung-Box Test
Box.test(e,type = "Ljung-Box")
# Since p-value = 1.956e-06, which is smaller than the 0.01 significent level, we reject H0 that the residuals have zero correlation, based on 1 autocorrelation coefficients (lag = 1 by default). Thus, Zero-Correlation assumption is NOT satisfied.
# Normality
# Formally: Shapiro-Wilk Test
shapiro.test(e)
# Since p-value = 0.0001277, which is smaller than the 0.01 significent level, we reject H0 that the residuals are nornally distributed. Thus, the Normality assumption is NOT satisfied.
## All four assumption not satisfied


# split the training set to have test.new.1 is the last year in the training set
train.new <- train[1:276,]
test.new <- train[277:288,]
train.UNE <- ts(train.new$Unemployment_Rate, start = c(1987, 1), frequency=12)
train.POP <- ts(train.new$Population, start = c(1987, 1), frequency=12)
train.HPI <- ts(train.new$House_Price_Index, start = c(1987, 1), frequency=12)
train.BR <- ts(train.new$Bankruptcy_Rate, start = c(1987, 1), frequency=12)

VARselect(y = data.frame(train.BR, train.UNE, train.POP, train.HPI))
# From above result, we have P = 6 or 8 or 10 optimal
# Fit Model with p = 6
m1.var <- VAR(y = data.frame(train.BR, train.UNE, train.POP, train.HPI), p = 6)
#plot(m1.var)
# Model forecasting and RMSE
pred1 <- predict(m1.var, n.ahead = 12, ci = 0.95)
plot(pred1)
rmse1.var <- sqrt(mean((pred1$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse1.var
# Fit Model with p = 8
m1.var.1 <- VAR(y = data.frame(train.BR, train.UNE, train.POP, train.HPI), p = 8)
#plot(m1.var.1)
# Model forecasting and RMSE
pred1.1 <- predict(m1.var.1, n.ahead = 12, ci = 0.95)
plot(pred1.1)
rmse1.var.1 <- sqrt(mean((pred1.1$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse1.var.1
# Fit Model with p = 10
m2.var <- VAR(y = data.frame(train.BR, train.UNE, train.POP, train.HPI), p = 10)
#plot(m2.var)
# Model forecasting and RMSE
pred2 <- predict(m2.var, n.ahead = 12, ci = 0.95)
plot(pred2)
rmse2.var <- sqrt(mean((pred2$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse2.var

# Account only BR, UNE and POP series
# select p
VARselect(y = data.frame(train.BR, train.UNE, train.POP))
# From above result, we have P = 8 or 10 optimal
# Fit Model with p = 8
m3.var <- VAR(y = data.frame(train.BR, train.UNE, train.POP), p = 8)
#plot(m3.var)
# Model forecasting and RMSE
pred3 <- predict(m3.var, n.ahead = 12, ci = 0.95)
plot(pred3)
rmse3.var <- sqrt(mean((pred3$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse3.var
# Fit Model with p = 10
m4.var <- VAR(y = data.frame(train.BR, train.UNE, train.POP), p = 10)
#plot(m4.var)
# Model forecasting and RMSE
pred4 <- predict(m4.var, n.ahead = 12, ci = 0.95)
plot(pred4)
rmse4.var <- sqrt(mean((pred4$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse4.var

# Account only BR, UNE and HPI series
# select p
VARselect(y = data.frame(train.BR, train.UNE, train.HPI))
# From above result, we have P = 2 or 5 or 6 optimal
# Fit Model with p = 3
m5.var <- VAR(y = data.frame(train.BR, train.UNE, train.HPI), p = 2)
#plot(m5.var)
# Model forecasting and RMSE
pred5 <- predict(m5.var, n.ahead = 12, ci = 0.95)
plot(pred5)
rmse5.var <- sqrt(mean((pred5$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse5.var
# Fit Model with p = 5
m6.var <- VAR(y = data.frame(train.BR, train.UNE, train.HPI), p = 5)
#plot(m6.var)
# Model forecasting and RMSE
pred6 <- predict(m6.var, n.ahead = 12, ci = 0.95)
plot(pred6)
rmse6.var <- sqrt(mean((pred6$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse6.var
# Fit Model with p = 6
m6.var.1 <- VAR(y = data.frame(train.BR, train.UNE, train.HPI), p = 6)
#plot(m6.var.1)
# Model forecasting and RMSE
pred6.1 <- predict(m6.var.1, n.ahead = 12, ci = 0.95)
plot(pred6.1)
rmse6.var.1 <- sqrt(mean((pred6.1$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse6.var.1

# Account only BR, POP and HPI series
# select p
VARselect(y = data.frame(train.BR, train.POP, train.HPI))
# From above result, we have P = 8 or 10 optimal
# Fit Model with p = 8
m7.var <- VAR(y = data.frame(train.BR, train.POP, train.HPI), p = 8)
#plot(m7.var)
# Model forecasting and RMSE
pred7 <- predict(m7.var, n.ahead = 12, ci = 0.95)
plot(pred7)
rmse7.var <- sqrt(mean((pred7$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse7.var
# Fit Model with p = 10
m8.var <- VAR(y = data.frame(train.BR, train.POP, train.HPI), p = 10)
#plot(m8.var)
# Model forecasting and RMSE
pred8 <- predict(m8.var, n.ahead = 12, ci = 0.95)
plot(pred8)
rmse8.var <- sqrt(mean((pred8$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse8.var

# Account only BR and UNE series
# select p
VARselect(y = data.frame(train.BR, train.UNE))
# From above result, we have P = 5 or 6 optimal
# Fit Model with p = 5
m9.var <- VAR(y = data.frame(train.BR, train.UNE), p = 5)
#plot(m9.var)
# Model forecasting and RMSE
pred9 <- predict(m9.var, n.ahead = 12, ci = 0.95)
plot(pred9)
rmse9.var <- sqrt(mean((pred9$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse9.var
# Fit Model with p = 6
m10.var <- VAR(y = data.frame(train.BR, train.UNE), p = 6)
#plot(m10.var)
# Model forecasting and RMSE
pred10 <- predict(m10.var, n.ahead = 12, ci = 0.95)
plot(pred10)
rmse10.var <- sqrt(mean((pred10$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse10.var

# Account only BR and POP series
# select p
VARselect(y = data.frame(train.BR, train.POP))
# From above result, we have P = 10 optimal
# Fit Model with p = 10
m11.var <- VAR(y = data.frame(train.BR, train.POP), p = 10)
#plot(m11.var)
# Model forecasting and RMSE
pred11 <- predict(m11.var, n.ahead = 12, ci = 0.95)
plot(pred11)
rmse11.var <- sqrt(mean((pred11$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse11.var

# Account only BR and HPI series
# select p
VARselect(y = data.frame(train.BR, train.HPI))
# From above result, we have P = 5 or 6 optimal
# Fit Model with p = 5
m12.var <- VAR(y = data.frame(train.BR, train.HPI), p = 5)
#plot(m12.var)
# Model forecasting and RMSE
pred12 <- predict(m12.var, n.ahead = 12, ci = 0.95)
plot(pred12)
rmse12.var <- sqrt(mean((pred12$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse12.var
# Fit Model with p = 6
m12.var.1 <- VAR(y = data.frame(train.BR, train.HPI), p = 6)
#plot(m12.var.1)
# Model forecasting and RMSE
pred12.1 <- predict(m12.var.1, n.ahead = 12, ci = 0.95)
plot(pred12.1)
rmse12.var.1 <- sqrt(mean((pred12.1$fcst$train.BR[,1] - test.new$Bankruptcy_Rate)^2))
rmse12.var.1

# "optimal" model:
# Based on the principle of getting the minimum RMSE, rmse8.var =  0.001674185 is the smallest among 
# all 12 RMSEs. That is, model m8.var, VAR(p) model account only BR, POP and HPI series with p = 10 is the "optimal"
# model for Vector Autoregressive process.

train.final <- train[1:288,]
BR <- ts(train.final$Bankruptcy_Rate, start = c(1987, 1), frequency=12)
POP <- ts(train.final$Population, start = c(1987, 1), frequency=12)
HPI <- ts(train.final$House_Price_Index, start = c(1987, 1), frequency=12)
# Fit Model with p = 10
m8.var.final <- VAR(y = data.frame(BR, POP, HPI), p = 10)
#plot(m8.var)
# Model forecasting and RMSE
pred8.final <- predict(m8.var.final, n.ahead = 12, ci = 0.95)
plot(pred8.final)

par(mfrow=c(1,1))
t.new <- seq(2010,2011,length=13)[1:12]
plot(BR, xlim=c(1987,2011),ylab = "Bankruptcy_Rate", main = "Forecast of Bankruptcy_Rate using VAR(p) approach")
abline(v=2010,col='blue',lty=2) 
lines(pred8.final$fcst$BR[,1]~t.new,type='l',col='red')
lines(pred8.final$fcst$BR[,2]~t.new,col='green') 
lines(pred8.final$fcst$BR[,3]~t.new,col='green') 
legend("topleft", legend=c("Test Set data 2010", "Prediction", "95% Prediction Interval"),col=c("black", "red", "green"), lty=1, cex=0.8)

result <- pred8.final$fcst$BR[,1:3]
write.csv(result, file = "result_test_2010.csv", row.names=FALSE)

# Residual Diagnostics
# Variable Bankruptcy_Rate
e <- test.new$Bankruptcy_Rate - pred8.final$fcst$BR[,1]
# Zero-Mean
t.test(e)
# Since p-value = 0.1578, which is way bigger than the 0.01 significent level, we fail to reject H0 that the residuals have zero mean. Thus, Zero-Mean assumption is satisfied.
# Homoscedasticity
# Formally: Levene Test
plot(e, main="Residuals vs t", ylab="")
abline(v=c(4,8), lwd=3, col="red")
group <- c(rep(1,4),rep(2,4),rep(3,4))
levene.test(e,group)
bartlett.test(e,group) 
# Since p-value = 0.675 > 0.01 for the Levene Test and p-value = 0.831 > 0.01 for the Bartlett test, we fail to reject H0 that the resisuals have constant variance. Thus, Homoscedasticity assumption is satisfied.
# Zero-Correlation
# Formally: Ljung-Box Test
Box.test(e,type = "Ljung-Box")
# Since p-value = 0.2158, which is bigger than the 0.01 significent level, we fail to reject H0 that the residuals have zero correlation, based on 1 autocorrelation coefficients (lag = 1 by default). Thus, Zero-Correlation assumption is satisfied.
# Normality
# Formally: Shapiro-Wilk Test
shapiro.test(e)
# Since p-value = 0.924, which is bigger than the 0.01 significent level, we fail to reject H0 that the residuals are nornally distributed. Thus, the Normality assumption is satisfied.
## Four assumptions all satisfied

# Variable Population
e <- test.new$Population - pred8.final$fcst$POP[,1]
# Zero-Mean
t.test(e)
# Since p-value < 2.2e-16, which is way smaller than the 0.01 significent level, we reject H0 that the residuals have zero mean. Thus, Zero-Mean assumption is NOT satisfied.
# Homoscedasticity
# Formally: Levene Test
plot(e, main="Residuals vs t", ylab="")
abline(v=c(4,8), lwd=3, col="red")
group <- c(rep(1,4),rep(2,4),rep(3,4))
levene.test(e,group)
bartlett.test(e,group) 
# Since p-value = 0.6685 > 0.01 for the Levene Test and p-value = 0.6685 > 0.01 for the Bartlett test, we fail to reject H0 that the resisuals have constant variance. Thus, Homoscedasticity assumption is satisfied.
# Zero-Correlation
# Formally: Ljung-Box Test
Box.test(e,type = "Ljung-Box")
# Since p-value = 0.003919, which is smaller than the 0.01 significent level, we reject H0 that the residuals have zero correlation, based on 1 autocorrelation coefficients (lag = 1 by default). Thus, Zero-Correlation assumption is NOT satisfied.
# Normality
# Formally: Shapiro-Wilk Test
shapiro.test(e)
# Since p-value = 0.4176, which is bigger than the 0.01 significent level, we fail to reject H0 that the residuals are nornally distributed. Thus, the Normality assumption is satisfied.
## Zero-Mean, Zero Correlation not satisfied

# Variable House_Price_Index
e <- test.new$House_Price_Index - pred8.final$fcst$HPI[,1]
# Zero-Mean
t.test(e)
# Since p-value = 8.265e-11, which is way smaller than the 0.01 significent level, we reject H0 that the residuals have zero mean. Thus, Zero-Mean assumption is NOT satisfied.
# Homoscedasticity
# Formally: Levene Test
plot(e, main="Residuals vs t", ylab="")
abline(v=c(4,8), lwd=3, col="red")
group <- c(rep(1,4),rep(2,4),rep(3,4))
levene.test(e,group)
bartlett.test(e,group) 
# Since p-value = 0.5712 > 0.01 for the Levene Test and p-value = 0.9585 > 0.01 for the Bartlett test, we fail to reject H0 that the resisuals have constant variance. Thus, Homoscedasticity assumption is satisfied.
# Zero-Correlation
# Formally: Ljung-Box Test
Box.test(e,type = "Ljung-Box")
# Since p-value = 0.03164, which is bigger than the 0.01 significent level, we fail to reject H0 that the residuals have zero correlation, based on 1 autocorrelation coefficients (lag = 1 by default). Thus, Zero-Correlation assumption is satisfied.
# Normality
# Formally: Shapiro-Wilk Test
shapiro.test(e)
# Since p-value = 0.3125, which is bigger than the 0.01 significent level, we fail to reject H0 that the residuals are nornally distributed. Thus, the Normality assumption is satisfied.
## Zero-Mean not satisfied


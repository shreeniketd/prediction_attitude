# Question 1
library(datasets)
library(plotly)
library(psych)
head(attitude)
View(attitude)
rm(list=ls())
summary <- describe(attitude)
summary

setwd("C:\\Users\\shree\\OneDrive\\Documents\\Quarter 2\\Intermediate Anaytics\\Assignment 3")
dir()
write.csv(summary, "attitude.csv")
getwd()
dir()

boxplot(attitude, las = 2, main = "Box Plot")


#Linear Regression using lm function(attitude)
y <- attitude[, 1]
x <- attitude[, -1]
y
x
m <- lm(y ~ as.matrix(x))
sink("lmsummary.csv")
summary(m)
sink()
write.csv(as.data.frame(summary(m)$coef), file="lmsummary.csv")

yhat1 <- predict(m)
yhat1
attitude$yhat1 <- yhat1
head(attitude)
res <- resid(m)
res


# RMSE for lm function
# difference = yhat1 - y
# difference
# final_rmse <- sqrt(mean(difference^2))
# final_rmse


RMSE = function(m, o){ 
  sqrt(mean((m - o)^2))
}

RMSE_lm <-RMSE(yhat1,y)
RMSE_lm

par(mfrow = c(5, 7), mar = c(2, 0, 0.25, 0.25), srt = 45)
plot.new()
plot.window(c(0, 1), c(0, 1), asp = 1)
text("Density", cex = 2.5, x = 0.5, y = 0.5)

invisible(lapply(colnames(x), function(i) {
  plot(density(x[[i]]), yaxt = "n", main = "", ylab = "", xlab = "")
}))

plot.new()
plot.window(c(0, 1), c(0, 1), asp = 1)
text("Rating", cex = 2.5, x = 0.5, y = 0.5)

invisible(lapply(colnames(x), function(i) {
  plot(x = x[[i]], y = y, yaxt = "n", main = "", ylab = "", xlab = "")
  abline(lm(y ~ x[[i]]))
}))

plot.new()
plot.window(c(0, 1), c(0, 1), asp = 1)
text("Fitted", cex = 2.5, x = 0.5, y = 0.5)

invisible(lapply(colnames(x), function(i) {
  plot(x = x[[i]], y = yhat1, yaxt = "n", main = "", ylab = "", xlab = "")
}))

plot.new()
plot.window(c(0, 1), c(0, 1), asp = 1)
text("Residuals", cex = 2.5, x = 0.5, y = 0.5)

invisible(lapply(colnames(x), function(i) {
  plot(x = x[[i]], y = res, yaxt = "n", main = "", ylab = "", xlab = "")
  abline(h = 0)
}))

invisible(lapply(c("", colnames(x)), function(i) {
  plot.new()
  plot.window(c(0, 1), c(0, 1), asp = 1)
  text(i, x = 0.5, y = 0.5, cex = 2.5)
}))

#Regression using PCA
head(attitude)
pca_attitude <- princomp(attitude[-1])
summary(pca_attitude)
pca_attitude$loadings
pca_attitude$scores

attitude$pca <- pca_attitude$scores[,1:2]
head(attitude)
#View(attitude)
first_prediction <- lm(rating~pca, data = attitude)
summary(first_prediction)
attitude$yhat2 <- predict.lm(first_prediction)
head(attitude)

# RMSE for PCA
RMSE = function(m1, o1){
  sqrt(mean((m1 - o1)^2))
}

RMSE_pca <- RMSE(attitude$yhat2,y)
RMSE_pca

second_prediction <- lm(rating ~ complaints+ privileges+ learning+ raises+critical+ advance, data = attitude)
attitude$yhat3 <- predict.lm(second_prediction)
head(attitude)

#RMSE = function(m2, o2){
#  sqrt(mean((m2 - o2)^2))
#}

rsq1<-summary(m)$r.squared
rsq1
rsq2<-summary(first_prediction)$r.squared
rsq2
#RMSE(attitude$yhat3,y)
rmse_table <- matrix(c(RMSE_lm,RMSE_pca,rsq1,rsq2), ncol = 2, byrow = TRUE)
colnames(rmse_table) <- c("lm()", "PCA")
rownames(rmse_table) <- c("RMSE", "R-Sq")

rmse_table <- as.table(rmse_table)
rmse_table
write.csv(rmse_table,"RMSE_TABLE.csv")
dir()





#Question 2
#Exercise 1
#install package lars using  install.packages("lars")
##install package lars using  install.packages("glmnet")
dev.off() 
rm(list=ls()) 
library(lars)
library(glmnet)
data(diabetes)

attach(diabetes) 
head(diabetes)
View(diabetes)

#Exercise 2
par(mfrow=c(2,5)) 
for(i in 1:10){
  plot(x[,i], y)
  abline(lm(y~x[,i]))
}
dev.off() 

#Exercise 3
model_ols <- lm(y ~ x) 
summary(model_ols)

#Exercise 4
model_lasso <- glmnet(x	,  y)
plot.glmnet(model_lasso, xvar = "norm", label = TRUE)

coef(model_lasso)

#Exercise 5
cv_fit <- cv.glmnet(x=x, y=y, alpha = 1, nlambda = 1000) #cross validate
plot.cv.glmnet(cv_fit)

#Exercise 6
cv_fit$lambda.min

fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.min)
fit$beta

#Exercise 7
cv_fit$lambda.1se

fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.1se)
fit$beta

#Exercise 8
model_ols1 <- lm(y ~ x2) 
summary(model_ols1)

#Exercise 9
model_lasso1 <- glmnet(x2	,  y)
dev.off() 
plot.glmnet(model_lasso1, xvar = "norm", label = TRUE)
coef(model_lasso)
#Exercise 10



#Exercise 5 on new model
cv_fit <- cv.glmnet(x=x2, y=y, alpha = 1, nlambda = 1000) 
plot.cv.glmnet(cv_fit)

#Exercise 6 on new model
cv_fit$lambda.min

fit <- glmnet(x=x2, y=y, alpha = 1, lambda=cv_fit$lambda.min)
fit$beta











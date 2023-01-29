library("ggplot2")
library("tidyverse")
library("reshape2")
library("broom")

x_data <- read.csv("/Users/alanliu/RStudio/Datathon-2023/train_data.csv")
x_data <- x_data[order(x_data$Year), ]
# thousands of barrels to btu conversion
x_data$BDPRP <- x_data$BDPRP * 5.46
x_data$ENPRP <- x_data$ENPRP * 3.192

prelim_data <- read.csv("/Users/alanliu/RStudio/Datathon-2023/investment_train_data.csv")
prelim_data <- prelim_data[order(prelim_data$Year), ]
y_data <- unique(prelim_data$TotalAmountofAssistance)
y_data <- y_data[!is.na(y_data)]

# hytcb <- x_data$HYTCB
# plot(hytcb, y_data, xlab="hytcb")
# ncprb <- x_data$NCPRB
# reprb <- x_data$REPRB
# teprb <- x_data$TEPRB

x_data[x_data == 0] <- NA
col_means <- colMeans(x_data[3:24], na.rm=TRUE)
for (i in 3:ncol(x_data)) {
  for (j in 1:nrow(x_data)) {
    if (is.na(x_data[j, i])) {
      x_data[j, i] <- col_means[i-2]
    }
  }
}

training_data <- x_data[3:24]
training_data <- as.data.frame(training_data)
#training_data <- as.data.frame(scale(training_data))
mlr <- lm(y_data ~ BDPRP + CLPRB + ENPRP + GETCB + HYTCB + NCPRB + NGMPB + NUETB +
            PAPRB + REPRB + SOTCB + TEPRB + TETCB + WDEXB + WDPRB + WDTCB + WSTCB +
            WWPRB + WYTCB + emissions + numInvestments, data=training_data)

summary(mlr)
print(unlist(training_data$ENPRP))

test_data <- read.csv("/Users/alanliu/RStudio/Datathon-2023/test_data.csv")
test_data <- test_data[3:24]

test_data[test_data == 0] <- NA
col_means_test <- colMeans(test_data, na.rm=TRUE)
for (i in 1:ncol(test_data)) {
  for (j in 1:nrow(test_data)) {
    if (is.na(test_data[j, i])) {
      test_data[j, i] <- col_means[i]
    }
  }
}
#test_data <- as.data.frame(scale(test_data))
test_data <- as.data.frame(test_data)

#predictions <- predict(lm(test_data$assistance ~ unlist(test_data$BDPRP) + unlist(test_data$CLPRB) + 
#                            unlist(test_data$ENPRP) + unlist(test_data$GETCB) + unlist(test_data$HYTCB) + 
#                            unlist(test_data$NCPRB) + unlist(test_data$NGMPB)	+ unlist(test_data$NUETB)	+ 
#                            unlist(test_data$PAPRB) +	unlist(test_data$REPRB) + unlist(test_data$SOTCB) +	
#                            unlist(test_data$TEPRB) + unlist(test_data$TETCB) + unlist(test_data$WDEXB) + 
#                            unlist(test_data$WDPRB) + unlist(test_data$WDTCB) +	unlist(test_data$WSTCB) + 
#                            unlist(test_data$WWPRB) + unlist(test_data$WYTCB), test_data))
#summary(predictions)

predicted_rmse <- function(y, yhat, n) {
#  return(sqrt(sum((predict(mlr, y[i,]) - yhat[i])^2)))
#  return(sqrt(sum((predict(mlr, y[i,]) - yhat[i]))^2))
  mse = 0
  for (i in 1:n) {
    print(predict(mlr, y[i,]))
    diff = predict(mlr, y[i,]) - yhat[i]
    mse = mse + diff^2
  }
  return(sqrt(mse/n))
}

#predicted_rmse <- function(y, yhat) {
#  return(sqrt(sum((y - yhat)^2)))
#}
rmse <- predicted_rmse(test_data[1:21], test_data$assistance, 50)

model %>%
  augment() %>%
  melt(measure.vars = c("hytcb", "ncprb", "reprb", "teprb"), variable.name = c("y_data")) %>%
  ggplot(., aes(value, foss)) +
  geom_smooth(method = "lm") +
  facet_wrap(~IV, scales = "free_x")
library("ggplot2")
library("tidyverse")
library("reshape2")
library("broom")

# read in training data
training_data <- read.csv("/Users/alanliu/RStudio/Datathon-2023/train_data.csv")
training_data <- training_data[order(training_data$Year), ]
# thousands of barrels to btu conversion
training_data$BDPRP <- training_data$BDPRP * 5.46
training_data$ENPRP <- training_data$ENPRP * 3.192

prelim_data <- read.csv("/Users/alanliu/RStudio/Datathon-2023/investment_train_data.csv")
prelim_data <- prelim_data[order(prelim_data$Year), ]
y_data <- unique(prelim_data$TotalAmountofAssistance)
y_data <- y_data[!is.na(y_data)]

training_data <- training_data[3:24]

# setting zero entries to the mean of its respective column
# assumption: zeros were either not entered into the database or not tracked
training_data[training_data == 0] <- NA
col_means <- colMeans(training_data, na.rm=TRUE)
for (i in 1:ncol(training_data)) {
  for (j in 1:nrow(training_data)) {
    if (is.na(training_data[j, i])) {
      training_data[j, i] <- col_means[i]
    }
  }
}

# normalize training data
training_data <- as.data.frame(scale(training_data))
# train model
mlr <- lm(y_data ~ BDPRP + CLPRB + ENPRP + GETCB + HYTCB + NCPRB + NGMPB + NUETB +
            PAPRB + REPRB + SOTCB + TEPRB + TETCB + WDEXB + WDPRB + WDTCB + WSTCB +
            WWPRB + WYTCB + emissions + numInvestments, data=training_data)

summary(mlr)

# read in test data
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

# normalize test data
test_data <- as.data.frame(scale(test_data))

# calculate rmse
predicted_rmse <- function(y, yhat, n) {
  mse <- 0
  for (i in 1:n) {
    diff <- predict(mlr, y[i,]) - yhat[i]
    mse <- mse + diff^2
  }
  return(sqrt(mse/n))
}
rmse <- predicted_rmse(test_data[1:21], test_data$assistance, 50)

mlr %>%
  augment() %>%
  melt(measure.vars=c("HYTCB", "NCPRB", "REPRB", "TEPRB"), variable.name=c("IV")) %>%
  ggplot(., aes(value, y_data)) + geom_smooth(method="lm") + facet_wrap(~IV, scales="free_x")

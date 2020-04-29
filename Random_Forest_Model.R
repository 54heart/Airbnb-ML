library(randomForest)
require(caTools)

setwd("~/MSBAbeforeFeb/ClarkR/course_files_export/Project")
library(ROCR)
set.seed(12345)
# Original model accuracy : 0.8320499


df = read.csv("train_cleaned.csv")

df_competition = read.csv("test_cleaned.csv")

nrow(df_competition)
## Randomly partition the data into 30% testing data and the remaining 70% data.
test_instn = sample(nrow(df), 0.25*nrow(df))
df_test <- df[test_instn,]
## Save the rest of the data as the data that isn't testing
df_rest <- df[-test_instn,]
## e. Randomly partition the remaining data into 75% training data and 25% validation data.
valid_instn = sample(nrow(df_rest), 0.3*nrow(df_rest))
df_valid <- df_rest[valid_instn,]
df_train <- df_rest[-valid_instn,]
summary(df_train)


df_train$high_booking_rate <- as.factor(df_train$high_booking_rate)


rf <- randomForest(
  df_train$high_booking_rate ~ .-high_booking_rate,
  data=df_train
)
pred = predict(rf, newdata=df_valid)

mc = table(df_valid$high_booking_rate, pred)
acc = (mc[1] + mc[4])/sum(mc)
acc


pred = predict(rf, newdata=df_competition)


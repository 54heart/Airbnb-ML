library(randomForest)
require(caTools)

setwd("~/MSBAbeforeFeb/ClarkR/Project")
library(ROCR)
set.seed(12345)


df = read.csv("data/train_cleaned.csv")
df_competition = read.csv("data/test_cleaned.csv")



## Randomly partition the data into 30% testing data and the remaining 70% data.
test_instn = sample(nrow(df), 0.30*nrow(df))
df_test <- df[test_instn,]
## Save the rest of the data as the data that isn't testing
df_rest <- df[-test_instn,]
## e. Randomly partition the remaining data into 75% training data and 25% validation data.
valid_instn = sample(nrow(df_rest), 0.25*nrow(df_rest))
df_valid <- df_rest[valid_instn,]
df_train <- df_rest[-valid_instn,]
summary(df_train)


# convert target variable to factor
df_train$high_booking_rate <- as.factor(df_train$high_booking_rate)


# rf model
rf <- randomForest(
  df_train$high_booking_rate ~.,
  data=df_train
)

pred = predict(rf, newdata=df_competition)


# drop insignificant variables "roomEntire.home.apt", "require_guest_profile_picture","require_guest_phone_verification", "propertyHotel", "host_has_profile_pic")

# 
# mc = table(df_test$high_booking_rate, pred)
# acc = (mc[1] + mc[4])/sum(mc)
# acc
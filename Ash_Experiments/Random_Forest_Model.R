library(randomForest)
require(caTools)

setwd("~/GitHub/Airbnb-ML/Ash_Experiments")
library(ROCR)
set.seed(12345)
# Original model accuracy : 0.8320499
#0.8302618

df = read.csv("train_cleaned.csv")


View(df)

df_competition = read.csv("test_cleaned.csv")

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
  df_train$high_booking_rate ~ df_train$accommodates+df_train$amenities_count+
    df_train$bathrooms+ df_train$Real_Bed+ df_train$bedrooms+ df_train$beds+ 
    df_train$price+
    df_train$cancellation_policy+df_train$require_guest_profile_picture+
    df_train$guests_included+ df_train$host_about+df_train$host_has_profile_pic+
    df_train$host_identity_verified+df_train$cleaning_fee+
    df_train$host_is_superhost+df_train$availability_365+
  df_train$availability_60+df_train$availability_90+df_train$availability_30+
    df_train$host_response_rate+ df_train$host_response_time+
    df_train$instant_bookable+ df_train$is_business_travel_ready+ 
    df_train$is_location_exact+ df_train$long_stay+
    df_train$propertyApartment+ df_train$propertyCommon_house+
    df_train$propertySide_house+ df_train$propertyHotel+ df_train$propertySpecial+
    df_train$require_guest_phone_verification+ 
    df_train$requires_license+
    df_train$roomPrivate.room+
    df_train$roomShared.room+df_train$security_deposit,data = df_train,
  na.action=na.roughfix
)

pred = predict(rf, newdata=df_valid)

mc = table(df_valid$high_booking_rate, pred)
acc = (mc[1] + mc[4])/sum(mc)
acc


pred = predict(rf, newdata=df_competition)


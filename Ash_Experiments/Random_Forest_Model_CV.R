library(randomForest)
require(caTools)

setwd("~/GitHub/Airbnb-ML/Ash_Experiments")

library(ROCR)
set.seed(12345)

df = read.csv("train_cleaned.csv")
df_competition = read.csv("test_cleaned.csv")

# Randomly partition the data into 30% testing data and the remaining 70% data.
test_instn = sample(nrow(df), 0.25*nrow(df))
df_test <- df[test_instn,]
## Save the rest of the data as the data that isn't testing
df_rest <- df[-test_instn,]
## e. Randomly partition the remaining data into 75% training data and 25% validation data.
valid_instn = sample(nrow(df_rest), 0.3*nrow(df_rest))
df_valid <- df_rest[valid_instn,]
df_train <- df_rest[-valid_instn,]

df_train$high_booking_rate <- as.factor(df_train$high_booking_rate)

library(caret)
rf_model <- train( as.factor(high_booking_rate)~ 
                     accommodates+
                     amenities_count+
                     availability_30+
                     availability_365+availability_60+
                     availability_90+
                     bathrooms+Real_Bed+
                     bedrooms+
                     beds+
                     cancellation_policy+
                     cleaning_fee+
                     extra_people+first_review+
                     guests_included+
                     host_about+host_identity_verified+
                     host_is_superhost+
                     host_listings_count+host_response_rate+
                     host_response_time+experience+instant_bookable+
                     is_business_travel_ready+
                     is_location_exact+long_stay+minimum_nights+
                     price+propertyApartment+
                     propertyCommon_house+propertySide_house+
                     propertySpecial+requires_license+
                     roomPrivate.room+roomShared.room+
                     security_deposit+flexible
                
                , family='binomial', data=df_train, method='rf', trControl=trainControl(method='cv', number=10), tuneLength=10)

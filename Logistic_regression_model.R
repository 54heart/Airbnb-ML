setwd("/Users/khanhhuyen4523/Desktop")
set.seed(12345)

df = read.csv("train_cleaned.csv")

df_competition = read.csv("test2.csv")
View(df_competition)
## Randomly partition the data into 30% testing data and the remaining 70% data.
test_instn = sample(nrow(df), 0.3*nrow(df))
df_test <- df[test_instn,]
## Save the rest of the data as the data that isn't testing
df_rest <- df[-test_instn,]
## e. Randomly partition the remaining data into 75% training data and 25% validation data.
valid_instn = sample(nrow(df_rest), 0.25*nrow(df_rest))
df_valid <- df_rest[valid_instn,]
df_train <- df_rest[-valid_instn,]

model_log <- glm(high_booking_rate~accommodates+availability_60+availability_90
                 +bathrooms+bed_type+bedrooms+beds+
                   cancellation_policy+cleaning_fee+extra_people+
                   first_review+guests_included+host_about+host_has_profile_pic+
                   host_identity_verified+host_is_superhost+host_listings_count+
                   host_response_rate+host_response_time+instant_bookable+
                   is_business_travel_ready+is_location_exact+maximum_nights+
                   minimum_nights+price+require_guest_phone_verification+
                   require_guest_profile_picture+requires_license+
                   experience, data = df,family="binomial")
summary(model_log)

log_probs <- predict(model_log, newdata = df_valid, type = "response")
pred <- prediction(log_probs, df_valid$high_booking_rate)

tpr.perf = performance(pred, measure = "tpr")
tnr.perf = performance(pred, measure = "tnr")
acc.perf = performance(pred, measure = "acc")
plot(tpr.perf,ylim=c(0,1))
plot(tnr.perf, add=T,col='green')
plot(acc.perf,add=T,col = 'red')




#getting the accuracy against best cutoff; best stores the cutoff index
best = which.max(slot(acc.perf,"y.values")[[1]])
max.acc = slot(acc.perf,"y.values")[[1]][best] 
max.cutoff = slot(acc.perf,"x.values")[[1]][best]
print(c(accuracy= max.acc, cutoff = max.cutoff))

confusion_matrix <- function(preds, actuals, cutoff){
  classifications <- ifelse(preds>cutoff,1,0)
  ##careful with positives and negatives here!
  confusion_matrix <- table(actuals,classifications)
}

log_valid_preds = predict(model_log,newdata=df_valid,type="response")
log_matrix <- confusion_matrix(log_valid_preds, df_valid$high_booking_rate,0.46807)

acc_log = (log_matrix[1] + log_matrix[4])/sum(log_matrix)
acc_log

#Trying to improve the model
log_matrix

log_com_preds = predict(model_log,newdata=df_competition,type="response")
#log_matrix <- confusion_matrix(log_test_preds, df_competition$high_booking_rate,.5)
library(ROCR)
pred <- prediction(log_com_preds,df_test$high_booking_rate)
#acc_log = (log_matrix[1] + log_matrix[4])/sum(log_matrix)
#acc_log

#test$probs <- predict(movies.pruned, newdata=movies_test)[,2]
classpred <- ifelse(log_com_preds>.5,1,0)
length(classpred)
length(which(classpred==1))
#length(which(df$high_booking_rate==0))

#submission_probs <- dataframe(movies_test[,c(1,10)])
#submission_class <- movies_test[,c(1,11)]

#write.csv(submission_probs, "team0_probs.csv")
write.csv(classpred, "team7_class.csv")

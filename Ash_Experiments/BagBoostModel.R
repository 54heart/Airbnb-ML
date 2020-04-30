#Bagging Boosting Algorithm

set.seed(12345)

library(class)
library(ipred)
df = read.csv("train_cleaned.csv")

df_competition = read.csv("test_cleaned.csv")


## Randomly partition the data into 30% testing data and the remaining 70% data.
test_instn = sample(nrow(df), 0.3*nrow(df))
df_valid <- df[test_instn,]
## Save the rest of the data as the data that isn't testing
df_train <- df[-test_instn,]

bag_model <- bagging(formula = df_train$high_booking_rate~.,
                     data = df_train, coob = TRUE)

pred <- predict(object = bag_model,newdata = df_valid[,2:43], type = 'prob')

#pred
pred_y <- ifelse(pred>0.5,1,0)

pred_y

mc = table(df_valid[,1], pred_y)
acc = (mc[1] + mc[4])/sum(mc)
acc # 0.7783528

#Using cross validation along with bagging
cross_bag_model <- train(df_train$high_booking_rate~.
                         ,data = df_train,
                         method = "treebag",
                         #metric="Accuracy",
                         trControl = trainControl(
                           method = "cv",number = 10#,classProbs = TRUE,
                           #summaryFunction = twoClassSummary
                           
                         ))


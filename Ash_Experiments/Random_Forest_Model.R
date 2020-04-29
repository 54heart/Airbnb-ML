library(randomForest)
require(caTools)

setwd("~/GitHub/Airbnb-ML/Ash_Experiments")
library(ROCR)
set.seed(12345)
# Original model accuracy : 0.8320499
#0.8302618 -> Accuracy after normalisation

df = read.csv("train_cleaned.csv")


View(df)

df_competition = read.csv("test_cleaned.csv")

## Randomly partition the data into 30% testing data and the remaining 70% data.
test_instn = sample(nrow(df), 0.3*nrow(df))
df_test <- df[test_instn,]
## Save the rest of the data as the data that isn't testing
df_rest <- df[-test_instn,]
## e. Randomly partition the remaining data into 75% training data and 25% validation data.
valid_instn = sample(nrow(df_rest), 0.25*nrow(df_rest))
df_valid <- df_rest[valid_instn,]
df_train <- df_rest[-valid_instn,]
summary(df_train)


df_train$high_booking_rate <- as.factor(df_train$high_booking_rate)


rf<- randomForest(df_train2$high_booking_rate~.,data = df_train2,
                  n_estimators = 20, min_sample_leaf = 80)

View(df_train)
View(df_test)
View(df_valid)

# drop insignificant variables
drop<- c("roomEntire.home.apt", "require_guest_profile_picture","require_guest_phone_verification","host_has_profile_pic", "propertyHotel")
df_valid2 <- df_valid[ , -which(names(df_valid) %in% drop)]
df_train2 <- df_train[ , -which(names(df_train) %in% drop)]
View(df_valid2)
pred = predict(rf, newdata=df_valid2)

mc = table(df_valid$high_booking_rate, pred)
acc = (mc[1] + mc[4])/sum(mc)
acc


pred = predict(rf, newdata=df_competition)

#---Incorporating a k-means clustering feature

k.max <- 10

wss <- sapply(1:k.max, 
              function(k){kmeans(df_train, k, nstart=50,iter.max = 15 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



df.X <-df[,-1] 

df.y <- df[,1]

which(is.na(df.X), arr.ind=TRUE)
df$availability_30[is.na(df$availability_30)] <- round(mean(df$availability_30, na.rm = TRUE))


View(df.X)

km.out = kmeans(x=df.X,centers=5,nstart=10)

df.clusters <- cbind(df_train.y, km.out$cluster)

df_train$clustercategory <- df.clusters[,2]

km.out = kmeans(x=df_valid.X,centers=5,nstart=10)


## Randomly partition the data into 30% testing data and the remaining 70% data.
test_instn = sample(nrow(df), 0.25*nrow(df))
df_test <- df[test_instn,]
## Save the rest of the data as the data that isn't testing
df_rest <- df[-test_instn,]
## e. Randomly partition the remaining data into 75% training data and 25% validation data.
valid_instn = sample(nrow(df_rest), 0.3*nrow(df_rest))
df_valid <- df_rest[valid_instn,]
df_train <- df_rest[-valid_instn,]

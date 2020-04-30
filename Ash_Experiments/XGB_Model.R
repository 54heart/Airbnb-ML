library(xgboost)
library(readr)
library(stringr)
library(caret)

set.seed(12345)
library(class)

df = read.csv("train_cleaned.csv")

df_competition = read.csv("test_cleaned.csv")

## Randomly partition the data into 30% testing data and the remaining 70% data.
test_instn = sample(nrow(df), 0.3*nrow(df))
df_valid <- df[test_instn,]
## Save the rest of the data as the data that isn't testing
df_train <- df[-test_instn,]
## e. Randomly partition the remaining data into 75% training data and 25% validation data.
#valid_instn = sample(nrow(df_rest), 0.3*nrow(df_rest))
#df_valid <- df_rest[valid_instn,]
#df_train <- df_rest[-valid_instn,]


#View(df_train)

df.y <- df_train[,1]

df.X<-df_train[,2:43]

xgb <- xgboost(data = data.matrix(df.X), 
               label = df.y, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               #subsample = 0.5,
               #colsample_bytree = 0.5,
               seed = 1,
               #eval_metric = "merror",
               objective = "binary:logistic",
               #num_class = 12,
               nthread = 3
)

col(df_train)

df_v.X <-df_valid[,2:43]

nrow(df_v.X) # 29953

df_v.y <- df_valid[,1]
length(df_v.y) # 29953
dim(df_v.X)

#pred <- predict(xgb, newdata=df_v.X)
pred <- predict(xgb, newdata=data.matrix(df_v.X))
pred_2 <- ifelse(pred>.5,1,0)

#pred <- predict(xgb, df_v.X, predleaf = TRUE)

length(pred)

mc = table(df_valid[,1], pred_2)
length(df_valid[,1])
length(pred_2)
acc = (mc[1] + mc[4])/sum(mc)
acc #0.8284646

dim(newdata)
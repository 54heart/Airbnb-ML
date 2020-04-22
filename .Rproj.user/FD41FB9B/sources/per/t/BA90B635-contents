#load libraries
# check
# colnames(df)[colSums(is.na(df)) > 0]

# 0. Load Libraries
library(readr)
library(tm)
library(SnowballC)
library(dplyr)

#read data
train_x <- read_csv("data/airbnb_train_x.csv")
train_y <- read_csv("data/airbnb_train_y.csv")
df <- merge(train_x, train_y, by.x = 'X1', by.y = 'X1')


# 1. custom functions for convenience
# check function: to check for na values, unique values, and data type
check <- function(x){
  uni <- unique(x)
  na <- sum(is.na(x))
  dtype <- class(x)
  cat("NA:", na, "\n")
  cat("unique Values:", uni, "\n")
  cat(dtype)
}

# def function to remove NA, returns the dataframe df
remove <- function(x){
  df <- df[which(is.na(x) == FALSE),]
  return(df)
}

# def function to impute missing values with mean(can choose to round off the decimal digits), or choose to impute a custom "value"
# this returns the input dataframe column
impute<- function(x, roundup= FALSE, value = NULL){
  if (roundup == TRUE){
    x[is.na(x)==TRUE] <- round(mean(x, na.rm = TRUE), 0)
  }
  else{
    if (missing(value)){
      x[is.na(x)==TRUE] <- mean(x, na.rm = TRUE)
    }
    else{
      x[is.na(x)==TRUE] <- value
    }
  }
  return(x)
}



# 2. Preprocess Variables

# var2 accommodates---------------
### Remove 9 bad recoreds
df <- remove(df$accommodates)

# var3 amenities---------------
#creating the variable amenities_count from the variable amenities
corpus <- (VectorSource(df$amenities))
corpus <- Corpus(corpus)
#summary(corpus)
vectored <-c()
#length(corpus)
#i=0
for (i in 1:100000){
  if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 0){
    vectored <- c(vectored,0) }
  else if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 1){
    vectored <- c(vectored,0)}
  else{
    vectored <- c(vectored,length(unlist(strsplit(corpus[[i]]$content, "[,]"))))
  }
}

df$amenities_count <- vectored





# 5 availability_365---------------
#making changes to availability_60 and avail_90 to make them independent of availabi_30 and so on
df$availability_365 <- df$availability_365-df$availability_90

# var6 availability_60
df$availability_60 <- df$availability_60 - df$availability_30

# var7 availability_90
df$availability_90 <- df$availability_90 - df$availability_60 - df$availability_30

# var8 bathrooms---------------
##replacing all character data in bathrooms column with the mean(rounded)
# check the mean
mean(df$bathrooms, na.rm = TRUE) # mean is 1.28, and we will round to 1.5
df$bathrooms <- impute(df$bathrooms, value = 1.5)


# var9 bed_type---------------
# create new var70 : Real_Bed
df['Real_Bed'] <- ifelse(df$bed_type=='Real Bed',1,0)

# var10 bedrooms---------------
## impute 92 NA with mean, and roundup to no decimals
df$bedrooms <- impute(df$bedrooms, roundup = TRUE)

# 11 beds---------------
## impute 83 NA with mean, and roundup to no decimals
df$beds <- impute(df$beds, roundup = TRUE)

# 12 cancellation_policy---------------
## map different policy category to integers (1, 2, 5, 6, 8, 10)
df$cancellation_policy[df$cancellation_policy == 'flexible']<-1
df$cancellation_policy[df$cancellation_policy == 'moderate']<-2
df$cancellation_policy[df$cancellation_policy == 'no_refunds']<-5
df$cancellation_policy[df$cancellation_policy == 'strict']<-6
df$cancellation_policy[df$cancellation_policy == 'super_strict_30']<-8
df$cancellation_policy[df$cancellation_policy == 'super_strict_60']<-10




# 15 cleaning_fee (originally 18325 NA)---------------
## impute NA with mean
df$cleaning_fee = as.numeric(substring(as.character(df$cleaning_fee),2))
df$cleaning_fee<- impute(df$cleaning_fee)

# 17 country_code---------------
## removed 3 rows where country not US
df <- df[which(df$country_code == 'US'),]


# 20 extra_people---------------
## remove dollar sign
df$extra_people <- as.numeric(substring(as.character(df$extra_people), first = 2))

# 21 first_review---------------
## calculate time difference between first_review and now
df$first_review <- as.Date(df$first_review)
df$first_review <- difftime(Sys.Date(), df$first_review)

# 22 guests_included---------------
## very clean but have to think about relationship with extra fee and accommodates


# 23 host_about---------------
## if the hosts didn't write anything about themselves, take 0, otherwise 1
df$host_about <- ifelse(is.na(df$host_about)== TRUE, 0, 1)

# 25 host_has_profile_pic---------------
### impute most common class because almost everyon has profile pic
df$host_has_profile_pic <- impute(df$host_has_profile_pic, value = TRUE)
df$host_has_profile_pic <- ifelse(df$host_has_profile_pic == TRUE, 1, 0)


# 26 host_identity_verified---------------
## impute 142 instances with commono class
df$host_identity_verified <- impute(df$host_identity_verified, value = TRUE)
df$host_identity_verified <- ifelse(df$host_identity_verified == TRUE, 1, 0)


# 27 host_is_superhost---------------
## IMPUTE 142 NA with common class 0
df$host_is_superhost <- as.character(df$host_is_superhost)
df$host_is_superhost <- ifelse(df$host_is_superhost ==TRUE,1,0)
df$host_is_superhost<- impute(df$host_is_superhost, value = 0)

# 28 host_listings_count---------------
### impute na with mean(round)
df$host_listings_count <- impute(df$host_listings_count, roundup = TRUE)

# 32 host_response_rate (15793 NA)---------------
## remove % sign, and impute rounded mean
df$host_response_rate <- as.numeric(gsub("\\%", '', df$host_response_rate)) # remove % sign
df$host_response_rate <- impute(df$host_response_rate, roundup = TRUE)

# 33 host_response_time---------------
df$host_response_time <- as.character(df$host_response_time)
df$host_response_time[df$host_response_time == 'within an hour'] <- 4
df$host_response_time[df$host_response_time =='within a few hours'] <- 3
df$host_response_time[df$host_response_time =='within a day'] <- 2
df$host_response_time[df$host_response_time == 'a few days or more'] <- 1
df$host_response_time <- as.numeric(df$host_response_time)
### impute na with the mode
#### define mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

md <- Mode(df$host_response_time)
df$host_response_time[is.na(df$host_response_time)==TRUE] <- md


# 34 host_since---------------
## New var 71 experience
## Transform the "host_since" column to time difference
## remove 142 NA in df$experience, this also might affect other columns with 142 NAs
df$host_since <- as.Date(df$host_since, origin="1960-10-01")
df$experience <- difftime(Sys.Date(), df$host_since)
df<- remove(df$experience)




# 37 house_rules---------------
## no party no pets, no smoking


# 38 instant_bookable---------------
## convert to numeric 1,0 
df$instant_bookable <- ifelse(df$instant_bookable == TRUE, 1, 0)

# 40 is_business_travel_ready---------------
## convert to 1, 0
## impute 44529 NAs as 0 (not business travel ready)
df$is_business_travel_ready<-ifelse(df$is_business_travel_ready==TRUE,1,0)
df$is_business_travel_ready <- impute(df$is_business_travel_ready, value = 0)

# 41 is_location_exact---------------
## convert to 1, 0
df$is_location_exact<- ifelse(df$is_location_exact== TRUE, 1, 0)


# 47 maximum_nights---------------
## maybe can create new variable: allow long-stay or not


# 54 price---------------
## remove dollar sign and impute 573 NAs with mean
df$price = as.numeric(substring(as.character(df$price),2))
df$price <- impute(df$price)

# 55 property_type---------------
##? todo 


# 59 room_type---------------
## map category to 0 1 2
df$room_type <- ifelse(df$room_type == 'Entire home/apt', 0,
                       ifelse(df$room_type == 'Private room', 1, ifelse(df$room_type == 'Shared room',2, 3)))


# 60 security_deposit---------------
df$security_deposit <- as.character(df$security_deposit)
df$security_deposit <- as.numeric(substring(as.character(df$security_deposit),2))
df$security_deposit <- ifelse(df$security_deposit>0,1,0)


# 67 transit---------------
df$transit <- as.character(df$transit)
matrix <- create_matrix(df$transit, language="english", removeSparseTerms = 0.95, removeStopwords=TRUE, removeNumbers=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)
mat <- as.matrix(matrix)

flexible <- data.frame(mat)
df$flexible <- (flexible$bike+flexible$bus+flexible$buses
                +flexible$metro+flexible$line+flexible$lines+flexible$subway
                +flexible$train+flexible$trains+flexible$transportation)



#creating the variable -> amenities_count from the variableamenities
corpus <- (VectorSource(df$amenities))
corpus <- Corpus(corpus)
#summary(corpus)
vectored <-c()
#length(corpus)
#i=0
for (i in 1:100000){
  if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 0){
    vectored <- c(vectored,0) }
  else if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 1){
    vectored <- c(vectored,0)}
  else{
    vectored <- c(vectored,length(unlist(strsplit(corpus[[i]]$content, "[,]"))))
  }
}

df$amenities_count <- vectored








# Convert the "security_deposit" column to Binary column
df$security_deposit <- as.character(df$security_deposit)
df$security_deposit <- as.numeric(substring(as.character(df$security_deposit),2))
df$security_deposit <- ifelse(df$security_deposit>0,1,0)

                             
# Grade the "transit" column
df$transit <- as.character(df$transit)
matrix <- create_matrix(df$transit, language="english", removeSparseTerms = 0.95, removeStopwords=TRUE, removeNumbers=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)
mat <- as.matrix(matrix)

flexible <- data.frame(mat)
df$flexible <- (flexible$bike+flexible$bus+flexible$buses
                      +flexible$metro+flexible$line+flexible$lines+flexible$subway
                      +flexible$train+flexible$trains+flexible$transportation)



## Huyen


# Translate the "room_type" column
df$room_type <- as.character(df$room_type)
df$room_type <- ifelse(df$room_type == 'Entire home/apt', 0,
                             ifelse(df$room_type == 'Private room', 1, ifelse(df$room_type == 'Shared room',2, 3)))

# the "cleaning_fee" column
df$cleaning_fee = as.numeric(substring(as.character(df$cleaning_fee),2))
df$cleaning_fee[is.na(df$cleaning_fee)] <- 66.98



df_export <- merge(df, flexible, by.x= 'X', by.y = )
select <- c("accommodates", "availability_30", "availability_60", "availability_90", "bathrooms", "bedrooms", "beds", "cancellation_policy", "cleaning_fee", "guests_included", "host_is_superhost", "host_listings_count", "host_response_rate","is_business_travel_ready", "minimum_nights", "price", "room_type", "security_deposit", "high_booking_rate", "Real_Bed", "Num_amenities", "experience")


write.csv(df,file="I:\\Airbnb Group Project\\train1.csv")




## this is the preprocessing--fix missing values and data format--for the test dataset


# 0. Load Libraries
library(readr)
library(tm)
library(SnowballC)
library(dplyr)

#read data 
# has 12208 obs
df <- read_csv("Data/airbnb_test_x_handcleaned.csv")



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


# define function that take in variable number in df and add to a list called selected
add <- function(var_number){
  selected <- c(selected, colnames(df[var_number+1]))
  return(selected)
}

# mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}




# 2. Preprocess Variables
# create empty list to store selected features
selected <- c()


# var2 accommodates---------------
df$accommodates <- impute(df$accommodates, value = 4 )
selected <- add(2) # using custom function to add variable name to list, meaning that we will select this as our feature


# var3 amenities---------------
#creating the var72 amenities_count from the variable amenities
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


# 4 availability_30
## impute 2 NAs
df$availability_30 <- impute(df$availability_30, value = 11 )
selected <-add(4)

# 5 availability_365---------------
#making changes to availability_60 and avail_90 to make them independent of availabi_30 and so on
df$availability_365 <- (df$availability_365-df$availability_90)
df$availability_365 <- impute(df$availability_365, value =168 )
selected <-add(5)

# var6 availability_60
df$availability_60 <- df$availability_60 - df$availability_30
df$availability_60 <- impute(df$availability_60, value = 25)
selected <-add(6)

# var7 availability_90
df$availability_90 <- df$availability_90 - df$availability_60 - df$availability_30
df$availability_90 <- impute(df$availability_90, value = 41)
selected <-add(7)

# var8 bathrooms---------------
##replacing 36 NA data in bathrooms column with the mean(rounded) if corresponding column in trainging data.
# check the mean
df$bathrooms <- impute(df$bathrooms, value = 1.5)
selected <- add(8)

# var9 bed_type---------------
# create new var70 : Real_Bed
df['Real_Bed'] <- ifelse(df$bed_type=='Real Bed',1,0)
selected <- add(9)


# var10 bedrooms---------------
## impute 92 NA with mean, and roundup to no decimals
df$bedrooms <- impute(df$bedrooms, value = 1)
selected <- add(10)



# 11 beds---------------
## impute 83 NA with mean, and roundup to no decimals
df$beds <- impute(df$beds, value = 6)
selected <- add(11)


# 12 cancellation_policy---------------
## map different policy category to integers (1, 2, 5, 6, 8, 10)
df$cancellation_policy[df$cancellation_policy == 'flexible']<-1
df$cancellation_policy[df$cancellation_policy == 'moderate']<-2
df$cancellation_policy[df$cancellation_policy == 'no_refunds']<-5
df$cancellation_policy[df$cancellation_policy == 'strict']<-6
df$cancellation_policy[df$cancellation_policy == 'super_strict_30']<-8
df$cancellation_policy[df$cancellation_policy == 'super_strict_60']<-10
selected <- add(12)



# 15 cleaning_fee (originally 18325 NA)---------------
## impute NA with mean
df$cleaning_fee = as.numeric(substring(as.character(df$cleaning_fee),2))
df$cleaning_fee<- impute(df$cleaning_fee, value = 66.99)
selected <- add(15)


# 17 country_code---------------
## removed 3 rows where country not US
df <- df[which(df$country_code == 'US'),]


# 20 extra_people---------------
## remove dollar sign
df$extra_people <- as.numeric(substring(as.character(df$extra_people), first = 2))
selected <- add(20)


# 21 first_review---------------
## calculate time difference between first_review and now
df$first_review <- as.Date(df$first_review)
df$first_review <- difftime(Sys.Date(), df$first_review)
selected <- add(21)


# 22 guests_included---------------
## very clean but have to think about relationship with extra fee and accommodates
selected <- add(22)

# 23 host_about---------------
## if the hosts didn't write anything about themselves, take 0, otherwise 1
df$host_about <- ifelse(is.na(df$host_about)== TRUE, 0, 1)
selected <- add(23)


# 25 host_has_profile_pic---------------
### impute most common class because almost everyon has profile pic
df$host_has_profile_pic <- impute(df$host_has_profile_pic, value = TRUE)
df$host_has_profile_pic <- ifelse(df$host_has_profile_pic == TRUE, 1, 0)
selected <- add(25)

# 26 host_identity_verified---------------
## impute 142 instances with commono class
df$host_identity_verified <- impute(df$host_identity_verified, value = TRUE)
df$host_identity_verified <- ifelse(df$host_identity_verified == TRUE, 1, 0)
selected <- add(26)

# 27 host_is_superhost---------------
## IMPUTE 142 NA with common class 0
df$host_is_superhost <- as.character(df$host_is_superhost)
df$host_is_superhost <- ifelse(df$host_is_superhost ==TRUE,1,0)
df$host_is_superhost<- impute(df$host_is_superhost, value = 0)
selected <- add(27)


# 28 host_listings_count---------------
### impute na with mean(round)
df$host_listings_count <- impute(df$host_listings_count, roundup = TRUE)
selected <- add(28)


# 32 host_response_rate (15793 NA)---------------
## remove % sign, and impute rounded mean, and divide by 100. 
df$host_response_rate <- as.numeric(gsub("\\%", '', df$host_response_rate)) # remove % sign
df$host_response_rate <- impute(df$host_response_rate, roundup = TRUE)
df$host_response_rate <- df$host_response_rate/100
selected <- add(32)


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
selected <- add(33)

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
selected <- add(38)


# 40 is_business_travel_ready---------------
## convert to 1, 0
## impute 44529 NAs as 0 (not business travel ready)
df$is_business_travel_ready<-ifelse(df$is_business_travel_ready==TRUE,1,0)
df$is_business_travel_ready <- impute(df$is_business_travel_ready, value = 0)
selected <- add(40)


# 41 is_location_exact---------------
## convert to 1, 0
df$is_location_exact<- ifelse(df$is_location_exact== TRUE, 1, 0)
selected <- add(41)


# 47 maximum_nights---------------
## maybe can create new variable: allow long-stay or not
selected <- add(47)

# 54 price---------------
## remove dollar sign and impute 573 NAs with mean
df$price = as.numeric(substring(as.character(df$price),2))
df$price <- impute(df$price)
selected <- add(54)


# 55 property_type---------------
## TODO


# 59 room_type---------------
## TODO onehotencoding


# 60 security_deposit---------------
# remove $, convert numeric values into yes or no(1, 0), impute NA with mode: 1
df$security_deposit <- as.character(df$security_deposit)
df$security_deposit <- as.numeric(substring(as.character(df$security_deposit),2))
df$security_deposit <- ifelse(df$security_deposit>0,1,0)
df$security_deposit <- impute(df$security_deposit, value = 1) 
selected <- add(60)


#-------------the below codes cannot run, yet to be fixed------
# 67 transit---------------
if(FALSE){df$transit <- as.character(df$transit)
matrix <- create_matrix(df$transit, language="english", removeSparseTerms = 0.95, removeStopwords=TRUE, removeNumbers=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)
mat <- as.matrix(matrix)

flexible <- data.frame(mat)
df$flexible <- (flexible$bike+flexible$bus+flexible$buses
                +flexible$metro+flexible$line+flexible$lines+flexible$subway
                +flexible$train+flexible$trains+flexible$transportation)
}





# Export as CSV file
export <- df[selected] # use the selected features
write.csv(export, file="data/test_cleaned.csv", row.names = FALSE) #Write dataframe as CSV







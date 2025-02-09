## this is the preprocessing--fix missing values and data format--for the test dataset


# 0. Load Libraries
library(readr)
library(tm)
library(SnowballC)
library(dplyr)

#read data 
# has 12208 obs
df_test <- read_csv("airbnb_test_x_deleted2obs.csv")





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



# define function that take in variable name in df_test and add to a list called selected_test
add_test <- function(var){
  if ((var %in% selected_test)==FALSE){
    selected_test <- c(selected_test, as.character(var))
  }
  return(selected_test)
}










# 2. Preprocess Variables
# create empty list to store selected features
selected_test <- c()




# var2 accommodates---------------
df_test$accommodates <- impute(df_test$accommodates, value = 4)
selected_test <- add_test('accommodates') # using custom function to add_test variable name to list, meaning that we will select this as our feature


# var3 amenities---------------
#creating the var72 amenities_count from the variable amenities
corpus <- (VectorSource(df_test$amenities))
corpus <- Corpus(corpus)
#summary(corpus)
vectored <-c()
#length(corpus)
#i=0
for (i in c(1:length(df_test$amenities))){
  if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 0){
    vectored <- c(vectored,0) }
  else if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 1){
    vectored <- c(vectored,0)}
  else{
    vectored <- c(vectored,length(unlist(strsplit(corpus[[i]]$content, "[,]"))))
  }
}

df_test$amenities_count <- vectored
selected_test<- add_test('amenities_count')


# 4 availability_30
## impute 2 NAs
df_test$availability_30 <- impute(df_test$availability_30, value = 11)
selected_test <-add_test('availability_30')

# 5 availability_365---------------
#making changes to availability_60 and avail_90 to make them independent of availabi_30 and so on
df_test$availability_365 <- (df_test$availability_365-df_test$availability_90)
df_test$availability_365 <- impute(df_test$availability_365, value =127 )
selected_test <-add_test('availability_365')

# var6 availability_60
df_test$availability_60 <- df_test$availability_60 - df_test$availability_30
df_test$availability_60 <- impute(df_test$availability_60, value = 14)
selected_test <-add_test('availability_60')

# var7 availability_90
df_test$availability_90 <- df_test$availability_90 - df_test$availability_60 - df_test$availability_30
df_test$availability_90 <- impute(df_test$availability_90, value = 16)
selected_test <-add_test('availability_90')

# var8 bathrooms---------------
##replacing 36 NA data in bathrooms column with the mean(rounded) if corresponding column in trainging data.
# check the mean
df_test$bathrooms <- impute(df_test$bathrooms, value = 1.5)
selected_test <- add_test('bathrooms')

# var9 bed_type---------------
# create new var70 : Real_Bed
df_test['Real_Bed'] <- ifelse(df_test$bed_type=='Real Bed',1,ifelse(df_test$bed_type=='Pull-out',1,0))
selected_test <- add_test('Real_Bed')


# var10 bedrooms---------------
## impute 92 NA with mean, and roundup to no decimals
df_test$bedrooms <- impute(df_test$bedrooms, value = 1)
df_test$bedrooms[df_test$bedrooms == 'within an hour'] <- 1
selected_test <- add_test("bedrooms")



# 11 beds---------------
## impute 83 NA with mean, and roundup to no decimals
df_test$beds <- impute(df_test$beds, value = 2)
selected_test <- add_test('beds')


# 12 cancellation_policy---------------
## map different policy category to integers (1, 2, 5, 6, 8, 10)
df_test$cancellation_policy[df_test$cancellation_policy == 'flexible'] <- 1
df_test$cancellation_policy[df_test$cancellation_policy == 'moderate'] <- 2
df_test$cancellation_policy[df_test$cancellation_policy == 'no_refunds'] <- 5
df_test$cancellation_policy[df_test$cancellation_policy == 'strict'] <- 6
df_test$cancellation_policy[df_test$cancellation_policy == 'super_strict_30'] <- 8
df_test$cancellation_policy[df_test$cancellation_policy == 'super_strict_60'] <- 10
df_test$cancellation_policy <- impute(df_test$cancellation_policy, value = 1)
selected_test <- add_test("cancellation_policy")



# 15 cleaning_fee (originally 18325 NA)---------------
## impute NA with mean
df_test$cleaning_fee = as.numeric(substring(as.character(df_test$cleaning_fee),2))
df_test$cleaning_fee<- impute(df_test$cleaning_fee, value = 54.72)
selected_test <- add_test("cleaning_fee")



# 20 extra_people---------------
## remove dollar sign
df_test$extra_people <- as.numeric(substring(as.character(df_test$extra_people), first = 2))
df_test$extra_people <- impute(df_test$extra_people, value = 0)
selected_test <- add_test('extra_people')

# 21 first_review---------------
## calculate time difference between first_review and now
df_test$first_review <- as.Date(df_test$first_review, format='%m/%d/%Y', origin="10/01/1960")
df_test$first_review <- difftime(Sys.Date(), df_test$first_review)
df_test$first_review<- as.integer(df_test$first_review)
df_test$first_review<- impute(df_test$first_review, value = )
selected_test <- add_test('first_review')


# 22 guests_included---------------
df_test$guests_included <- impute(df_test$guests_included, value = 2) # rounded mean 
selected_test <- add_test("guests_included")

# 23 host_about---------------
## if the hosts didn't write anything about themselves, take 0, otherwise 1
df_test$host_about <- ifelse(is.na(df_test$host_about)== TRUE, 0, 1)
selected_test <- add_test('host_about')


# 25 host_has_profile_pic---------------
### impute most common class because almost everyon has profile pic
df_test$host_has_profile_pic <- impute(df_test$host_has_profile_pic, value = TRUE)
df_test$host_has_profile_pic <- ifelse(df_test$host_has_profile_pic == TRUE, 1, 0)
selected_test <- add_test('host_has_profile_pic')

# 26 host_identity_verified---------------
## impute 142 instances with commono class
df_test$host_identity_verified <- impute(df_test$host_identity_verified, value = TRUE)
df_test$host_identity_verified <- ifelse(df_test$host_identity_verified == TRUE, 1, 0)
selected_test <- add_test('host_identity_verified')


# 27 host_is_superhost---------------
## IMPUTE 142 NA with common class 0
df_test$host_is_superhost <- as.character(df_test$host_is_superhost)
df_test$host_is_superhost <- ifelse(df_test$host_is_superhost == TRUE,1,0)
df_test$host_is_superhost<- impute(df_test$host_is_superhost, value = 0)
selected_test <- add_test('host_is_superhost')


# 28 host_listings_count---------------
### impute na with mean(round)
df_test$host_listings_count <- impute(df_test$host_listings_count, value = 10)
selected_test <- add_test('host_listings_count')


# 32 host_response_rate (15793 NA)---------------???
## remove % sign, and impute rounded mean, and divide by 100. 
df_test$host_response_rate <- as.numeric(gsub("\\%", '', df_test$host_response_rate)) # remove % sign
df_test$host_response_rate <- impute(df_test$host_response_rate, value = 96)
df_test$host_response_rate <- df_test$host_response_rate/100
selected_test <- add_test('host_response_rate')


# 33 host_response_time---------------
df_test$host_response_time <- as.character(df_test$host_response_time)
df_test$host_response_time[df_test$host_response_time == 'within an hour'] <- 4
df_test$host_response_time[df_test$host_response_time =='within a few hours'] <- 3
df_test$host_response_time[df_test$host_response_time =='within a day'] <- 2
df_test$host_response_time[df_test$host_response_time == 'a few days or more'] <- 1
df_test$host_response_time <- as.numeric(df_test$host_response_time)
df_test$host_response_time[is.na(df_test$host_response_time)==TRUE] <- 4
selected_test <- add_test('host_response_time')

# 34 host_since---------------
## New var 71 experience
## Transform the "host_since" column to time difference
## remove 142 NA in df_test$experience, this also might affect other columns with 142 NAs
df_test$host_since <- as.Date(df_test$host_since, format='%m/%d/%Y', origin="10/01/1960")
df_test$experience <- difftime(Sys.Date(), df_test$host_since)
df_test$experience <- as.integer(df_test$experience)
selected_test <- add_test('experience')




# 38 instant_bookable---------------
## convert to numeric 1,0 
## impute 3 NAs
df_test$instant_bookable <- ifelse((df_test$instant_bookable == TRUE), 1, 0)
df_test$instant_bookable <- impute(df_test$instant_bookable, value = 0)
selected_test <- add_test('instant_bookable')


# 40 is_business_travel_ready---------------
## convert to 1, 0
## impute 44529 NAs as 0 (not business travel ready)
df_test$is_business_travel_ready<-ifelse(df_test$is_business_travel_ready==TRUE,1,0)
df_test$is_business_travel_ready <- impute(df_test$is_business_travel_ready, value = 0)
selected_test <- add_test('is_business_travel_ready')


# 41 is_location_exact---------------
## convert to 1, 0
df_test$is_location_exact<- ifelse(df_test$is_location_exact== TRUE, 1, 0)
df_test$is_location_exact <- impute(df_test$is_location_exact, value = 1)
selected_test <- add_test('is_location_exact')


# 47 maximum_nights---------------
## create new var73: allow long-stay(more than 7 days) or not
df_test['long_stay'] <- ifelse(df_test$maximum_nights >7, 1, 0)
df_test$long_stay <- impute(df_test$long_stay, value = 1)
selected_test <- add_test('long_stay')


# 48 minimum_nights-----------------
## impute with mode: 1
df_test$minimum_nights <- impute(df_test$minimum_nights, value = 1)
selected_test <- add_test('minimum_nights')



# 54 price---------------
## remove dollar sign and impute 573 NAs with mean
df_test$price = as.numeric(substring(as.character(df_test$price),2))
df_test$price <- impute(df_test$price, value = 145)
selected_test <- add_test('price')


# 55 property_type---------------
## Property type: group into larger groups and onehotencode
apartment <- c("Loft, House", "Apartment", "Condominium", "Timeshare", "Aparthotel", "Serviced apartment")
common_house <- c("Townhouse", "Bed and breakfast", "Bed & Breakfast", "Bungalow", "Villa", "Vacation home", "Chalet", "Resort")
side_house <- c("Guesthouse",  "In-law", "Dorm", "Guest suite", "Cabin", "Cottage",  "Farm stay", "Nature lodge")
hotel <- c("Hotel", "Boutique hotel", "Hostel")
special <- c("Castle", "Camper/RV", "Boat" , "Treehouse", "Tiny house", "Yurt", "Cave", "Casa particular (Cuba)", "Hut", "Tipi", "Earth House","Earth house", "Train", "Barn"  ,"Island" , "Plane", "Lighthouse", "Other", "Camper/RV", "Tent") 

## impute 1 NA with the most common type: Apartment
df_test$property_type <- impute(df_test$property_type, value = 'Apartment')

## onehotencode
df_test['propertyApartment'] <- ifelse((df_test$property_type %in% apartment), 1, 0) #var77
df_test['propertyCommon_house'] <- ifelse((df_test$property_type %in% common_house), 1, 0) #var78
df_test['propertySide_house'] <- ifelse((df_test$property_type %in% side_house), 1, 0) #var79
df_test['propertyHotel'] <- ifelse((df_test$property_type %in% hotel), 1, 0) #var80
df_test['propertySpecial'] <- ifelse((df_test$property_type %in% special), 1, 0) #var81



selected_test <- add_test('propertyApartment')
selected_test <- add_test('propertyCommon_house')
selected_test <- add_test('propertySide_house')
selected_test <- add_test('propertyHotel')
selected_test <- add_test('propertySpecial')


# 56 require_guest_phone_verification------------
df_test$require_guest_phone_verification <- impute(df_test$require_guest_phone_verification, value = FALSE)
df_test$require_guest_phone_verification <- ifelse(df_test$require_guest_phone_verification==TRUE, 1, 0)
selected_test <- add_test('require_guest_phone_verification')

# 57 require_guest_profile_picture----------
df_test$require_guest_profile_picture <- impute(df_test$require_guest_profile_picture, value = FALSE)
df_test$require_guest_profile_picture <- ifelse(df_test$require_guest_profile_picture==TRUE, 1, 0)
selected_test <- add_test('require_guest_profile_picture')

# 58 requires_license---------------
df_test$requires_license <- impute(df_test$requires_license, value = FALSE)
df_test$requires_license <- ifelse(df_test$requires_license==TRUE, 1, 0)
selected_test <- add_test('requires_license')


# 59 room_type---------------
df_test$room_type <- impute(df_test$room_type, value = 'Entire home/apt') # impute one NA with common class
## onehotencode. Created new three columns: var74 roomEntire home/apt, 75 roomPrivate room, 76 roomShared room
library(mltools)
library(data.table)
room_ohe_test <- one_hot(as.data.table(as.factor(df_test$room_type)))
# rename columns
colnames(room_ohe_test) <- c('roomEntire home/apt', 'roomPrivate room', 'roomShared room')
df_test <- cbind(df_test, room_ohe_test)

selected_test <- add_test('roomEntire home/apt')
selected_test <- add_test('roomPrivate room')
selected_test <- add_test('roomShared room')


# 60 security_deposit---------------
# remove $, convert numeric values into yes or no(1, 0), impute NA with mode: 1
df_test$security_deposit <- as.character(df_test$security_deposit)
df_test$security_deposit <- as.numeric(substring(as.character(df_test$security_deposit),2))
df_test$security_deposit <- ifelse(df_test$security_deposit>0,1,0)
df_test$security_deposit <- impute(df_test$security_deposit, value = 1) 

selected_test <- add_test('security_deposit')


# 67 transit---------------
library(maxent)
library(RTextTools)
df_test$transit <- as.character(df_test$transit)
matrix_test <- create_matrix(df_test$transit, language="english", removeSparseTerms = 0.95, removeStopwords=TRUE, removeNumbers=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)
mat_test <- as.matrix(matrix_test)

flexible <- data.frame(mat_test)
df_test$flexible <- (flexible$bike+flexible$bus+flexible$buses
                +flexible$metro+flexible$line+flexible$lines+flexible$subway
                +flexible$train+flexible$trains+flexible$transportation)


selected_test <- add_test('flexible')

#-----------------------
#Normalisation

df_test$accommodates <- scale(df_test$accommodates)
df_test$amenities_count <- scale(df_test$amenities_count)
df_test$availability_30 <- scale(df_test$availability_30)
df_test$availability_365 <- scale(df_test$availability_365)
df_test$availability_60 <- scale(df_test$availability_60)
df_test$availability_90 <- scale(df_test$availability_90)
df_test$bathrooms <- scale(df_test)
df_test$beds <- scale(df_test$beds)
df_test$bedrooms <- scale(df_test$bedrooms)
df_test$cleaning_fee <- scale(df_test$cleaning_fee)
df_test$extra_people <- scale(df_test$extra_people)
df_test$first_review <- scale(df_test$first_review)
df_test$guests_included <- scale(df_test$guests_included)
df_test$host_listings_count <- scale(df_test$host_listings_count)
df_test$maximum_nights <- scale(df_test$maximum_nights)
df_test$price <- scale(df_test$price)
df_test$experience <- scale(df_test$experience)

#Imputing some remaining columns
df_test$availability_30 <- impute(df_test$availability_30)
df_test$availability_60 <- impute(df_test$availability_60)
df_test$availability_90 <- impute(df_test$availability_90)
df_test$experience <- impute(df_test$experience)

# Export as CSV file
df_test<- as.data.frame(df_test)
export <- df_test[selected_test] # use the selected_test features
which(is.na(export), arr.ind=TRUE)
#View(export)

write.csv(export, file="test_cleaned.csv", row.names = FALSE) #Write dataframe as CSV


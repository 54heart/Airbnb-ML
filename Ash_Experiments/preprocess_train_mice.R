#load libraries
# 0. Load Libraries
library(readr)
library(tm)
library(SnowballC)
library(dplyr)
library(mice)

#read data col_types = cols(zipcode = col_character()
train_x <- read_csv("airbnb_train_x.csv")
train_y <- read_csv("airbnb_train_y.csv")
# merge features and label
df <- merge(train_x, train_y, by.x = 'X1', by.y = 'X1')

#Now, first we will feature engineer and then we will remove all NA values at once

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

# define function that take in variable name in df and add to a list called selected
add <- function(var){
  if ((var %in% selected)==FALSE){
    selected <- c(selected, as.character(var))
  }
  return(selected)
}

# calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# 2. Preprocess Variables
# create list to store selected features
selected <- c("high_booking_rate")


# var3 amenities---------------
#creating the var72 amenities_count from the variable amenities
corpus <- (VectorSource(df$amenities))
corpus <- Corpus(corpus)
#summary(corpus)
vectored <-c()

for (i in c(1:length(df$amenities))){
  if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 0){
    vectored <- c(vectored,0) }
  else if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 1){
    vectored <- c(vectored,0)}
  else{
    vectored <- c(vectored,length(unlist(strsplit(corpus[[i]]$content, "[,]"))))
  }
}

df$amenities_count <- vectored
selected<- add('amenities_count')

# 4 availablity_30--------------
selected <-add('availability_30')

# 5 availability_365---------------
#making changes to availability_60 and avail_90 to make them independent of availabi_30 and so on
df$availability_365 <- df$availability_365-df$availability_90
selected <-add('availability_365')

# var6 availability_60
df$availability_60 <- df$availability_60 - df$availability_30
selected <-add('availability_60')

# var7 availability_90
df$availability_90 <- df$availability_90 - df$availability_60 - df$availability_30
selected <-add('availability_90')


# var8 bathrooms---------------
##replacing all character data in bathrooms column with the mean(rounded)
# check the mean
#mean(df$bathrooms, na.rm = TRUE) # mean is 1.28, and we will round to 1.5
#df$bathrooms <- impute(df$bathrooms, value = 1.5) -> MICE instead ahead
selected <- add('bathrooms')

# var9 bed_type---------------
# create new var70 : Real_Bed
df['Real_Bed'] <- ifelse(df$bed_type=='Real Bed',1,ifelse(df$bed_type=='Pull-out',1,0))
selected <- add('Real_Bed')


# var10 bedrooms---------------
## impute 92 NA with mean, and roundup to no decimals
#df$bedrooms <- impute(df$bedrooms, roundup = TRUE) -> Use MICE Package ahead
selected <- add('bedrooms')

# 11 beds---------------
## impute 83 NA with mean, and roundup to no decimals
#df$beds <- impute(df$beds, roundup = TRUE)
selected <- add('beds')  #-> Use MICE Package ahead

# 12 cancellation_policy---------------
## map different policy category to integers (1, 2, 5, 6, 8, 10)
df$cancellation_policy[df$cancellation_policy == 'flexible']<-1
df$cancellation_policy[df$cancellation_policy == 'moderate']<-2
df$cancellation_policy[df$cancellation_policy == 'no_refunds']<-5
df$cancellation_policy[df$cancellation_policy == 'strict']<-6
df$cancellation_policy[df$cancellation_policy == 'super_strict_30']<-8
df$cancellation_policy[df$cancellation_policy == 'super_strict_60']<-10
df$cancellation_policy <- as.integer(df$cancellation_policy)
selected <- add('cancellation_policy')


# 15 cleaning_fee (originally 18325 NA)---------------
## impute NA with zero. (don't charge cleaning fees)
df$cleaning_fee = as.numeric(substring(as.character(df$cleaning_fee),2))
#df$cleaning_fee<- impute(df$cleaning_fee, value = 0) -> MICE Package Ahead
selected <- add('cleaning_fee')


# 17 country_code---------------
## removed 3 rows where country not US
df <- df[which(df$country_code == 'US'),]

# 20 extra_people---------------
## remove dollar sign
df$extra_people <- as.numeric(substring(as.character(df$extra_people), first = 2))
selected <- add('extra_people')



# 21 first_review---------------
## calculate time difference between first_review and now
df$first_review <- as.Date(df$first_review,origin="1960-10-01")
df$first_review <- difftime(Sys.Date(), df$first_review)
df$first_review<-as.integer(df$first_review)
selected <- add('first_review')

# 22 guests_included---------------
## very clean but have to think about relationship with extra fee and accommodates
selected <- add('guests_included')

# 23 host_about---------------
## if the hosts didn't write anything about themselves, take 0, otherwise 1
df$host_about <- ifelse(is.na(df$host_about)== TRUE, 0, 1) # No MICE needed
selected <- add('host_about')

# 25 host_has_profile_pic---------------
### impute most common class because almost everyon has profile pic
#df$host_has_profile_pic <- impute(df$host_has_profile_pic, value = TRUE)-> MICE
df$host_has_profile_pic <- ifelse(df$host_has_profile_pic == TRUE, 1, 0)
selected <- add('host_has_profile_pic')

# 26 host_identity_verified---------------
## impute 142 instances with commono class
#df$host_identity_verified <- impute(df$host_identity_verified, value = TRUE)-> MICE
df$host_identity_verified <- ifelse(df$host_identity_verified == TRUE, 1, 0)
selected <- add('host_identity_verified')


# 27 host_is_superhost---------------
## IMPUTE 142 NA with common class 0
df$host_is_superhost <- as.character(df$host_is_superhost)
df$host_is_superhost <- ifelse(df$host_is_superhost ==TRUE,1,0)
#df$host_is_superhost<- impute(df$host_is_superhost, value = 0) -> MICE ahead
selected <- add('host_is_superhost')


# 28 host_listings_count---------------
### impute na with mean(round)
#df$host_listings_count <- impute(df$host_listings_count, roundup = TRUE) -> Mice ahead
selected <- add('host_listings_count')



# 32 host_response_rate (15793 NA)---------------
## remove % sign, and impute rounded mean, and divide by 100. 
df$host_response_rate <- as.numeric(gsub("\\%", '', df$host_response_rate)) # remove % sign
#df$host_response_rate <- impute(df$host_response_rate, roundup = TRUE) -> MICE
#df$host_response_rate <- df$host_response_rate/100 -> Decide later
selected <- add('host_response_rate')


# 33 host_response_time---------------
df$host_response_time <- as.character(df$host_response_time)
df$host_response_time[df$host_response_time == 'within an hour'] <- 4
df$host_response_time[df$host_response_time =='within a few hours'] <- 3
df$host_response_time[df$host_response_time =='within a day'] <- 2
df$host_response_time[df$host_response_time == 'a few days or more'] <- 1
df$host_response_time <- as.numeric(df$host_response_time)
### impute na with the mode

#md <- Mode(df$host_response_time)
#df$host_response_time[is.na(df$host_response_time)==TRUE] <- md -> MICE Ahead
selected <- add('host_response_time')

# 34 host_since---------------
## New var 71 experience
## Transform the "host_since" column to time difference
## remove 142 NA in df$experience, this also might affect other columns with 142 NAs
df$host_since <- as.Date(df$host_since, origin="1960-10-01")
df$experience <- difftime(Sys.Date(), df$host_since)
df$experience <- as.integer(df$experience)
#df<- remove(df$experience)  -> MICE ahead 
selected <- add('experience')


# 37 house_rules---------------
## no party no pets, no smoking

# 38 instant_bookable---------------
## convert to numeric 1,0 
df$instant_bookable <- ifelse(df$instant_bookable == TRUE, 1, 0)
selected <- add('instant_bookable')


# 40 is_business_travel_ready---------------
## convert to 1, 0
## impute 44529 NAs as 0 (not business travel ready)
df$is_business_travel_ready<-ifelse(df$is_business_travel_ready==TRUE,1,0)
#df$is_business_travel_ready <- impute(df$is_business_travel_ready, value = 0) -> MICE ahead
selected <- add('is_business_travel_ready')


# 41 is_location_exact---------------
## convert to 1, 0
df$is_location_exact<- ifelse(df$is_location_exact== TRUE, 1, 0)
selected <- add('is_location_exact')


# 47 maximum_nights---------------
## create new var73 long_stay: allow long-stay(more than 7 days) or not
df['long_stay'] <- ifelse(df$maximum_nights >7, 1, 0)
selected <- add('long_stay')


# 48 minimum_nights-----------------
selected <- add('minimum_nights')
#Mode(df$minimum_nights) -> MICE ahead

# 54 price---------------
## remove dollar sign and impute 573 NAs with mean
df$price = as.numeric(substring(as.character(df$price),2))
#df$price <- impute(df$price) -> MICE ahead
selected <- add('price')


# 55 property_type---------------
## Property type: group into larger groups and onehotencode
apartment <- c("Loft, House", "Apartment", "Condominium", "Timeshare", "Aparthotel", "Serviced apartment")
common_house <- c("Townhouse", "Bed and breakfast", "Bungalow", "Villa", "Vacation home", "Chalet")
side_house <- c("Guesthouse",  "In-law", "Dorm", "Guest suite", "Cabin", "Cottage",  "Farm stay", "Nature lodge")
hotel <- c("Hotel", "Boutique hotel", "Hostel")
special <- c("Castle", "Camper/RV", "Boat" , "Treehouse", "Tiny house", "Yurt", "Cave", "Casa particular (Cuba)", "Hut", "Tipi", "Earth House","Earth house", "Train", "Barn"  ,"Island" , "Plane", "Lighthouse" ) 

df['propertyApartment'] <- ifelse((df$property_type %in% apartment), 1, 0) #var77
df['propertyCommon_house'] <- ifelse((df$property_type %in% common_house), 1, 0) #var78
df['propertySide_house'] <- ifelse((df$property_type %in% side_house), 1, 0) #var79
df['propertyHotel'] <- ifelse((df$property_type %in% hotel), 1, 0) #var80
df['propertySpecial'] <- ifelse((df$property_type %in% special), 1, 0) #var81


selected <- add('propertyApartment')
selected <- add('propertyCommon_house')
selected <- add('propertySide_house')
selected <- add('propertyHotel')
selected <- add('propertySpecial')





# this code block is an alternative way to onehot encode all of the property types
if(FALSE){
  library(mltools)
  library(data.table)
  newdata <- one_hot(as.data.table(as.factor(df$property_type)))
}



# 56 require_guest_phone_verification------------
df$require_guest_phone_verification <- ifelse(df$require_guest_phone_verification==TRUE, 1, 0)
selected <- add('require_guest_phone_verification')

# 57 require_guest_profile_picture----------
df$require_guest_profile_picture <- ifelse(df$require_guest_profile_picture==TRUE, 1, 0)
selected <- add('require_guest_profile_picture')

# 58 requires_license---------------
df$requires_license <- ifelse(df$requires_license==TRUE, 1, 0)
selected <- add('requires_license')


# 59 room_type---------------
## onehotencode. Created new three columns: var74 roomEntire home/apt, 75 roomPrivate room, 76 roomShared room
room <- as.factor(df$room_type) 
temp <- as.data.frame(model.matrix(~room)) # onehotencode
colnames(temp)[1] <- 'roomEntire home/apt'
df <- cbind(df, temp)

selected <- add('roomEntire home/apt') # ADD THIS VARIABLE LATER
selected <- add('roomPrivate room')
selected <- add('roomShared room')

selected0<-c('roomEntire home/apt','roomPrivate room','roomShared room',
             'roomEntire home/apt')
dfmice0 <- df[,selected0]

# 60 security_deposit---------------
# remove $, convert numeric values into yes or no(1, 0), impute NA with mode: 1
df$security_deposit <- as.character(df$security_deposit)
df$security_deposit <- as.numeric(substring(as.character(df$security_deposit),2))
df$security_deposit <- ifelse(df$security_deposit>0,1,0)
#df$security_deposit <- impute(df$security_deposit, value = 1)  -> MICE ahead
selected <- add('security_deposit')
#-----------------------

# 67 transit---------------
library(maxent)
library(RTextTools)
df$transit <- as.character(df$transit)
matrix <- create_matrix(df$transit, language="english", removeSparseTerms = 0.95, removeStopwords=TRUE, removeNumbers=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)
mat <- as.matrix(matrix)

flexible <- data.frame(mat)
df$flexible <- (flexible$bike+flexible$bus+flexible$buses
                +flexible$metro+flexible$line+flexible$lines+flexible$subway
                +flexible$train+flexible$trains+flexible$transportation)

selected <- add('flexible')
View(df)


selected<-setdiff(selected,"roomEntire home/apt")#selected[!selected = "roomEntire home/apt"]
selected
dfmice <-mice(df[selected] ,m=3,meth='pmm',seed=500) # variables upto cleaning_fee] included
# new dataframe from 'extra_people' upton 'host_has_profile_pic'
selected1 <- c('extra_people','first_review','guests_included','host_about',
               'host_has_profile_pic')
dfmice2 <-mice(df[selected1] ,m=3,meth='pmm',seed=500) # variables upto host_has_profile_pic] included

# new dataframe from 'host_identity_verified' upton 'host_response_rate'
selected2<- c('host_identity_verified','host_is_superhost',
              'host_listings_count','host_response_rate')
dfmice3 <-mice(df[selected2] ,m=3,meth='pmm',seed=500) # variables upto host_response_rate] included

# new dataframe from 'host_response_time' upton 'host_response_rate'
selected3<-c('experience','instant_bookable','is_business_travel_ready',
             'is_location_exact')
dfmice4 <-mice(df[selected3] ,m=3,meth='pmm',seed=500)

selected4 <- c('long_stay','minimum_nights','price','propertyApartment')
dfmice5 <-mice(df[selected4] ,m=3,meth='pmm',seed=500)

selected5<-c('propertyCommon_house','propertySide_house','propertyHotel',
             'propertySpecial')
dfmice6 <-mice(df[selected5] ,m=3,meth='pmm',seed=500)

selected6 <-c('require_guest_phone_verification','require_guest_profile_picture',
              'requires_license')
dfmice7 <-mice(df[selected6] ,m=3,meth='pmm',seed=500)

selected7 <- c('security_deposit',
               'flexible')
dfmice8 <-mice(df[selected7] ,m=3,meth='pmm',seed=500)




df_finalmice <- cbind(dfmice,dfmice2,dfmice3,dfmice4,dfmice5,dfmice6,dfmice7,
                      dfmice8)
which(is.na(df_finalmice))
typeof(df_finalmice)
# variables of property do not exist in this
completedData<- complete(df_finalmice,1)

View(completedData)

df_finalmice$imp
write.csv(completedData, file="micetrain_cleaned.csv")

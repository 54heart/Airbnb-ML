#load libraries
# 0. Load Libraries
library(readr)
library(tm)
library(SnowballC)
library(dplyr)
library(mice)

#read data col_types = cols(zipcode = col_character()
#read data 
# has 12208 obs
df <- read_csv("airbnb_test_x_deleted2obs.csv")



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

addNA <- function(var){
  if ((var %in% selectedNA)==FALSE){
    selectedNA <- c(selectedNA, as.character(var))
  }
  return(selectedNA)
}

# calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 2. Preprocess Variables
# create list to store selected features with NA values
selectedNA <- c()
selected<-c()
View(df)

#No dependent variable in test data
#sum(is.na(df$high_booking_rate)) # There are 0 NA values
#selectedNA <- addNA("high_booking_rate")

# var2 accommodates---------------
### Remove 9 bad recoreds
check(df$accommodates) # 2 NA values -> so we add this column to NA list
selectedNA <- addNA("accommodates")


# var3 amenities---------------
#creating the var72 amenities_count from the variable amenities
corpus <- (VectorSource(df$amenities))
corpus <- Corpus(corpus)
#summary(corpus)
vectored <-c()
#length(corpus)
#i=0

# 1:100000 to c(1:length(df$amenities))
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

check(df$amenities_count) # 0 NA values, so we add this to selected() list

selected<- add('amenities_count')

# 4 availablity_30--------------
check(df$availability_30) # 2 NA values -> so add it to selectedNA() list
selectedNA <-addNA('availability_30')

# 5 availability_365---------------
#making changes to availability_60 and avail_90 to make them independent of availabi_30 and so on
df$availability_365 <- df$availability_365-df$availability_90
check(df$availability_365) # 2 NA values so add it to selected
selectedNA <-addNA('availability_365')

# var6 availability_60
df$availability_60 <- df$availability_60 - df$availability_30
check(df$availability_60) # 2 NA values -> add to selectedNA
selectedNA <-addNA('availability_60')

# var7 availability_90
df$availability_90 <- df$availability_90 - df$availability_60 - df$availability_30
check(df$availability_90) # 2 NA values , so add to selectedNA
selectedNA <-addNA('availability_90')

# var8 bathrooms---------------

check(df$bathrooms) #36 NA values, so we add to selectedNA list
selectedNA <- addNA('bathrooms')

# var9 bed_type---------------
# create new var70 : Real_Bed
df['Real_Bed'] <- ifelse(df$bed_type=='Real Bed',1,ifelse(df$bed_type=='Pull-out',1,0))
check(df$Real_Bed) # 0 NA value, so we add to selectedNA list
selected <- add('Real_Bed')

# var10 bedrooms---------------
## impute 92 NA with mean, and roundup to no decimals
check(df$bedrooms) # 14 NA values, so we add to selectedNA list
selectedNA <- addNA('bedrooms')

# 11 beds---------------
check(df$beds) #7 NA values, so add to selectedNA
selectedNA <- addNA('beds')


# 12 cancellation_policy---------------
## map different policy category to integers (1, 2, 5, 6, 8, 10)
df$cancellation_policy[df$cancellation_policy == 'flexible']<-1
df$cancellation_policy[df$cancellation_policy == 'moderate']<-2
df$cancellation_policy[df$cancellation_policy == 'no_refunds']<-5
df$cancellation_policy[df$cancellation_policy == 'strict']<-6
df$cancellation_policy[df$cancellation_policy == 'super_strict_30']<-8
df$cancellation_policy[df$cancellation_policy == 'super_strict_60']<-10
df$cancellation_policy <- as.integer(df$cancellation_policy)
check(df$cancellation_policy) # 2 NA values, so add to selected list
selectedNA <- addNA('cancellation_policy')

# 15 cleaning_fee (originally 18325 NA)---------------

df$cleaning_fee = as.numeric(substring(as.character(df$cleaning_fee),2))
check(df$cleaning_fee) # 2315 NA values, so add to selectedNA list
selectedNA <- addNA('cleaning_fee')


# 17 country_code---------------
## removed 3 rows where country not US
df <- df[which(df$country_code == 'US'),]
check(df$country_code) # Only US values and no NA values

# 20 extra_people---------------
## remove dollar sign
df$extra_people <- as.numeric(substring(as.character(df$extra_people), first = 2))
check(df$extra_people) # 0 NA values so add to selected list
selected <- add('extra_people')



# 21 first_review---------------
## calculate time difference between first_review and now
df$first_review <- as.Date(df$first_review,origin="1960-10-01")
df$first_review <- difftime(Sys.Date(), df$first_review)
df$first_review<-as.integer(df$first_review)
check(df$first_review) # 7621 NA values, scale the values
df$first_review <- scale(df$first_review)
selectedNA <- addNA('first_review')

# 22 guests_included---------------
## very clean but have to think about relationship with extra fee and accommodates
check(df$guests_included) # 0 NA values so add to selected list
selected <- add('guests_included')

# 23 host_about---------------
## if the hosts didn't write anything about themselves, take 0, otherwise 1
df$host_about <- ifelse(is.na(df$host_about)== TRUE, 0, 1)
check(df$host_about) # 0 NA values, so add to selected
selected <- add('host_about')

# 25 host_has_profile_pic---------------

df$host_has_profile_pic <- ifelse(df$host_has_profile_pic == TRUE, 1, 0)
check(df$host_has_profile_pic) # 21 NA values, so add to selectedNA list
selectedNA <- addNA('host_has_profile_pic')

# 26 host_identity_verified---------------

df$host_identity_verified <- ifelse(df$host_identity_verified == TRUE, 1, 0)
check(df$host_identity_verified) #21 NA values, so add to selectedNA list
selectedNA <- addNA('host_identity_verified')

# 27 host_is_superhost---------------

df$host_is_superhost <- as.character(df$host_is_superhost)
df$host_is_superhost <- ifelse(df$host_is_superhost ==TRUE,1,0)
check(df$host_is_superhost) #21 NA values, so add to selectedNA list
selectedNA <- addNA('host_is_superhost')


# 28 host_listings_count---------------
check(df$host_listings_count)#21 NA values, add to selectedNA
selectedNA <- addNA('host_listings_count')


# 32 host_response_rate (15793 NA)---------------
## remove % sign, and impute rounded mean, and divide by 100. 
df$host_response_rate <- as.numeric(gsub("\\%", '', df$host_response_rate)) # remove % sign
check(df$host_response_rate) #1981 NA values, add to selectedNA
#df$host_response_rate <- df$host_response_rate/100
selectedNA <- addNA('host_response_rate')


# 33 host_response_time---------------
df$host_response_time <- as.character(df$host_response_time)
df$host_response_time[df$host_response_time == 'within an hour'] <- 4
df$host_response_time[df$host_response_time =='within a few hours'] <- 3
df$host_response_time[df$host_response_time =='within a day'] <- 2
df$host_response_time[df$host_response_time == 'a few days or more'] <- 1
df$host_response_time <- as.numeric(df$host_response_time)
check(df$host_response_time) # 1981 na values, add to selectedNA list
selectedNA <- addNA('host_response_time')

# 34 host_since---------------
## New var 71 experience
## Transform the "host_since" column to time difference
df$host_since <- as.Date(df$host_since, origin="1960-10-01")

check(df$host_since) #142 NAs
df$host_since <- as.Date(df$host_since, origin="1960-10-01")
df$experience <- difftime(Sys.Date(), df$host_since)
df$experience <- as.integer(df$experience)
check(df$experience)
df$experience <- scale(df$experience)
selectedNA <- addNA('experience')


# 37 house_rules---------------
## no party no pets, no smoking


# 38 instant_bookable---------------
## convert to numeric 1,0 
df$instant_bookable <- ifelse(df$instant_bookable == TRUE, 1, 0)
check(df$instant_bookable) # 1 NA values, add to selectedNA
selectedNA <- addNA('instant_bookable')

# 40 is_business_travel_ready---------------
## convert to 1, 0

df$is_business_travel_ready<-ifelse(df$is_business_travel_ready==TRUE,1,0)
check(df$is_business_travel_ready) #5564 NA values, add to selectedNA
selectedNA <- addNA('is_business_travel_ready')


# 41 is_location_exact---------------
## convert to 1, 0
df$is_location_exact<- ifelse(df$is_location_exact== TRUE, 1, 0)
check(df$is_location_exact) # 1 NA values so add to selectedNA
selectedNA <- addNA('is_location_exact')

# 47 maximum_nights---------------
## create new var73 long_stay: allow long-stay(more than 7 days) or not
df['long_stay'] <- ifelse(df$maximum_nights >7, 1, 0)
check(df$long_stay) #1 NA values, add to selectedNA
selectedNA <- addNA('long_stay')


# 48 minimum_nights-----------------
check(df$minimum_nights) # 1 NA values, add to selectedNA
selectedNA <- addNA('minimum_nights')


# 54 price---------------
## remove dollar sign and impute 573 NAs with mean
df$price = as.numeric(substring(as.character(df$price),2))
check(df$price) # 69 NA values
df$price <- scale(df$price)
selectedNA <- addNA('price')


# 55 property_type---------------
## Property type: group into larger groups and onehotencode
apartment <- c("Loft, House", "Apartment", "Condominium", "Timeshare", "Aparthotel", "Serviced apartment")
common_house <- c("Townhouse", "Bed and breakfast", "Bungalow", "Villa", "Vacation home", "Chalet")
side_house <- c("Guesthouse",  "In-law", "Dorm", "Guest suite", "Cabin", "Cottage",  "Farm stay", "Nature lodge")
hotel <- c("Hotel", "Boutique hotel", "Hostel")
special <- c("Castle", "Camper/RV", "Boat" , "Treehouse", "Tiny house", "Yurt", "Cave", "Casa particular (Cuba)", "Hut", "Tipi", "Earth House","Earth house", "Train", "Barn"  ,"Island" , "Plane", "Lighthouse" ) 

df['propertyApartment'] <- ifelse((df$property_type %in% apartment), 1, 0) #var77
check(df$propertyApartment) # 0 NA values
selected <- add('propertyApartment')
df['propertyCommon_house'] <- ifelse((df$property_type %in% common_house), 1, 0) #var78
check(df$propertyCommon_house) # 0 NA
selected <- add('propertyCommon_house')
df['propertySide_house'] <- ifelse((df$property_type %in% side_house), 1, 0) #var79
check(df$propertySide_house) # 0 NA
selected<-add('propertySide_house')
df['propertyHotel'] <- ifelse((df$property_type %in% hotel), 1, 0) #var80
check(df$propertyHotel) # 0 NA values
selected <- add('propertyHotel')
df['propertySpecial'] <- ifelse((df$property_type %in% special), 1, 0) #var81
check(df$propertySpecial) # 0 NA
selected <- add('propertySpecial')

# 56 require_guest_phone_verification------------
df$require_guest_phone_verification <- ifelse(df$require_guest_phone_verification==TRUE, 1, 0)
check(df$require_guest_phone_verification) # 1 NA values, add to selectedNA list
selectedNA <- addNA('require_guest_phone_verification')

# 57 require_guest_profile_picture----------
df$require_guest_profile_picture <- ifelse(df$require_guest_profile_picture==TRUE, 1, 0)
check(df$require_guest_profile_picture) # 1 NA values, add to selectedNA list
selectedNA <- addNA('require_guest_profile_picture')

# 58 requires_license---------------
df$requires_license <- ifelse(df$requires_license==TRUE, 1, 0)
check(df$requires_license) # 1 NA values, add to selectedNA
selectedNA <- addNA('requires_license')


# 59 room_type-----------
## onehotencode. Created new three columns: var74 roomEntire home/apt, 75 roomPrivate room, 76 roomShared room

check(df$room_type)

df['Entirehome.apt'] <- ifelse((df$room_type == "Entire home/apt"), 1, 0) #var77
check(df$Entirehome.apt) # 1 NA values
selectedNA <- addNA('Entirehome.apt')
df['Private.room'] <- ifelse((df$room_type == "Private room"), 1, 0) #var78
check(df$Private.room) # 1 NA
selectedNA <- addNA('Private.room')
df['Shared.room'] <- ifelse((df$room_type == "Shared room"), 1, 0) #var79
check(df$Shared.room) # 1 NA
selectedNA <- addNA('Shared.room')

# 60 security_deposit---------------
# remove $, convert numeric values into yes or no(1, 0), impute NA with mode: 1
df$security_deposit <- as.character(df$security_deposit)
df$security_deposit <- as.numeric(substring(as.character(df$security_deposit),2))
df$security_deposit <- ifelse(df$security_deposit>0,1,0)
check(df$security_deposit) #5533 NA values, add to selectedNA
selectedNA <- addNA('security_deposit')


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
check(df$flexible) # 0 NA values, add to selected list
selected <- add('flexible')

#Using mice package-------
df_selected <- df[,selected]
df_selectedNA <- df[,selectedNA]
nrow(df_selected) == nrow(df_selectedNA) # TRUE

#Now we run MICE on df_selectedNA and the cbind the two dataframes

df_selectedNA$experience<-df_selectedNA$experience[,1]
df_selectedNA$price<-df_selectedNA$price[,1]
df_selectedNA$first_review<-df_selectedNA$first_review[,1]
df_selectedNA$host_since<-df_selectedNA$host_since[,1]

glimpse(df_selectedNA)

df_selectednonNA <-mice(df_selectedNA ,m=3,meth='pmm',seed=500)

df_selectednonNA <- complete(df_selectednonNA,1)



View(df_selectednonNA)
nrow(df_selectednonNA) # 12206
nrow(df_selected) # 12206

#Now, we cbind the two dataframes into one

df_final_test <- cbind(df_selected,df_selectednonNA)
View(df_final_test)
colnames(df_final_test)

write.csv(df_final_test, file="test_cleaned(mice).csv", row.names = FALSE) #Write dataframe as CSV

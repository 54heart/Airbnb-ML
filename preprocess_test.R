## this is the preprocessing--fix missing values and data format--for the test dataset

#Changes to Real_Bed:
#df['Real_Bed'] <- ifelse(df$bed_type=='Real Bed',1,ifelse(df$bed_type=='Pull-out',1,0))


# 0. Load Libraries
library(readr)
library(tm)
library(SnowballC)
library(dplyr)

#read data 
# has 12208 obs
df <- read_csv("Data/airbnb_test_x_final.csv")



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
for (i in c(1:length(df)){
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




# Export as CSV file
export <- df[selected] # use the selected features
write.csv(export, file="data/test_cleaned.csv", row.names = FALSE) #Write dataframe as CSV







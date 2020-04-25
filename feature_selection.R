# 0. Load Libraries
library(readr)


#read data col_types = cols(zipcode = col_character()
df <- read_csv("data/train_cleaned.csv")




# Feature Engineering: Selection

#Code to check correlation-------------------------------
check_correlation <-function(y){
  
  return (cor(df$high_booking_rate,y,method = 'pearson'))
}

check_correlation(df$accommodates)
#0.005946965
check_correlation(df$amenities_count)
#0.1713588
check_correlation(df$availability_365)
#0.04885
check_correlation(df$availability_90)
#0.08219
check_correlation(df$availability_60)
#0.05439
check_correlation(df$bathrooms)
#-0.07792
check_correlation(df$Real_Bed)
#0.02039
check_correlation(df$bedrooms)
#-0.08144
check_correlation(df$beds)
#-0.00015
check_correlation(df$cancellation_policy) 
#0.032141
check_correlation(df$cleaning_fee)
#-0.13287
check_correlation(df$extra_people)
#-0.0003823
check_correlation(df$first_review)
#-0.97954
check_correlation(df$host_about)
#0.03497
check_correlation(df$host_has_profile_pic)
#0.00513
check_correlation(df$host_identity_verified)
#0.005135
check_correlation(df$host_is_superhost)
#0.2158221
check_correlation(df$host_listings_count)
#-0.0524
check_correlation(df$host_response_rate)
#0.091267
check_correlation(df$host_response_time)
#0.018632
#check_correlation(df$host_since) # Host since is a date not numeric
check_correlation(df$experience)
#-0.12489
check_correlation(df$instant_bookable)
#0.22696
check_correlation(df$is_business_travel_ready)
#0.1036616
check_correlation(df$is_location_exact)
#-0.027
check_correlation(df$price)
#-0.10873
check_correlation(df$security_deposit)
#-0.07337

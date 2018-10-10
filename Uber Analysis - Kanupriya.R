#----------------------------- Load libraries ----------------------------
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

# ---------------------------- Load the data  ----------------------------
uber_data <- read.csv("Uber Request Data.csv", stringsAsFactors = F)

#  ---------------------------- Clean the data  ----------------------------
# Check structure of data
str(uber_data)
head(uber_data)

#Check if there are any duplicate rows - No duplicate rows found
nrow(uber_data)
nrow(unique(uber_data))

#Verify if in the trips that were completed, if any NA values are there
length(which(uber_data$Status=="Trip Completed" & (is.na(uber_data$Driver.id) | 
                                                     is.na(uber_data$Pickup.point) |
                                                     is.na(uber_data$Request.id) |
                                                     is.na(uber_data$Request.timestamp) |
                                                     is.na(uber_data$Drop.timestamp))))

#Verify that in the cancelled/non-availabilty scenario; if the data required for analysis is present
length(which((uber_data$Status=="Cancelled"| uber_data$Status=="No Cars Available") 
                                & (is.na(uber_data$Pickup.point) |
                                 is.na(uber_data$Request.timestamp))))

#No missing values

#Since the "Request.timestamp" and "Drop.timestamp" columns are not stored consistently; bring them to the same format.

#Mark the current date format in a new column 
#For "13-07-2016 08:33:16" use 2 else use 1
uber_data$date_format <- ifelse(str_detect(uber_data$Request.timestamp, "-"),2,1)

#Correct the date formats so that both have the format "13-07-2016 08:33:16" and store it in New.Request.timestamp and New.Drop.timestamp
uber_data$New.Request.timestamp <- ifelse((uber_data$date_format==1),
                                          paste((str_replace_all(uber_data$Request.timestamp, "/", "-")), "00", sep=":"),
                                          uber_data$Request.timestamp)
uber_data$New.Drop.timestamp <- ifelse((uber_data$date_format==1),
                                       paste((str_replace_all(uber_data$Drop.timestamp, "/", "-")), "00", sep=":"),
                                       uber_data$Drop.timestamp)

#Now the request and drop date+time is stored consistently

#--------------------- Add new columns for better analysis -----------------------

#Get request and drop date and time with as.POSIXct and store it in request_time
uber_data$request_time <- as.POSIXct(uber_data$New.Request.timestamp, format = "%d-%m-%Y %H:%M:%S")
uber_data$drop_time <- as.POSIXct(uber_data$New.Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")

#Get request date details (day of week, day, month, year, hour, minute, second)
uber_data$request_day_of_week <- weekdays(as.Date(uber_data$New.Request.timestamp, format = "%d-%m-%Y %H:%M:%S"))
unique(uber_data$request_day_of_week) #Data is for only 5 days

uber_data$request_day <- format(as.POSIXlt(uber_data$New.Request.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%d")
unique(uber_data$request_day) #Data is for only 5 dates (11-15)

uber_data$request_month <- format(as.POSIXlt(uber_data$New.Request.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%m")
unique(uber_data$request_month) #Data is for only the month of July

uber_data$request_year <- format(as.POSIXlt(uber_data$New.Request.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%Y")
unique(uber_data$request_year) #Data is for only the year 2016 

#The data is from 11th July 2016-15th July 2016

uber_data$request_hour <- format(as.POSIXlt(uber_data$New.Request.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%H")
uber_data$request_minute <- format(as.POSIXlt(uber_data$New.Request.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%M")
uber_data$request_second <- format(as.POSIXlt(uber_data$New.Request.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%S")

#Get details from the drop date also (day, month, year, hour, minute, second)
uber_data$drop_day_of_week <- weekdays(as.Date(uber_data$New.Drop.timestamp, format = "%d-%m-%Y %H:%M:%S"))
uber_data$drop_day <- format(as.POSIXlt(uber_data$New.Drop.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%d")
uber_data$drop_month <- format(as.POSIXlt(uber_data$New.Drop.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%m")
uber_data$drop_year <- format(as.POSIXlt(uber_data$New.Drop.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%Y")
uber_data$drop_hour <- format(as.POSIXlt(uber_data$New.Drop.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%H")
uber_data$drop_minute <- format(as.POSIXlt(uber_data$New.Drop.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%M")
uber_data$drop_second <- format(as.POSIXlt(uber_data$New.Drop.timestamp, format = "%d-%m-%Y %H:%M:%S"), "%S")

#How much time a trip is taking
uber_data$trip_time <- (uber_data$drop_time - uber_data$request_time)

#Create a column which will be true only if Status = "Trip Completed"
uber_data$is_completed <- ifelse(uber_data$Status=="Trip Completed", "Y", "N")

#Create bins - This was done in retrospect; after noticing different patterns of 
#rides in the morning (5-10 am) and evening (5-10 pm) hours.

#If hour = Morning 5am-10am: Morning Rush
#Evening 5pm-10pm: Evening Rush
#All other time: No Rush
uber_data$hour_int <- as.integer(uber_data$request_hour)
uber_data$bin <- ifelse(uber_data$hour_int>=5 & uber_data$hour_int<=10, "Morning Rush",
                        ifelse(uber_data$hour_int>=17 & uber_data$hour_int<=22, "Evening Rush","No Rush"))


#---------------------- Exploratory Data Analysis ------------------------

#------------------------- Univariate analysis ---------------------------

#How much time on average a trip to and from airport takes 
mean(uber_data$trip_time, na.rm = T) #Around 52 minutes
median(uber_data$trip_time, na.rm = T) #Almost the same as the mean

#Mean for various hours
aggregate(trip_time ~ request_hour, uber_data, FUN=mean)
#Nothing significantly high or low here

#For plotting the frequency of Uber requested against a Categorical variables; 
#I am using Bar Charts; as they clearly show the frequencies for various variables 

#Plot the frequency of various Status (Trip Completed, Cancelled, No Cars Available)
ggplot(uber_data, aes(x=uber_data$Status, fill=uber_data$Status)) + geom_bar() + geom_text(stat='count', aes(label=..count..))
#We notice that out of 6745 rides to and from airport; 58% (3914) were not completed (status "Cancelled" or "No Cars Available")
#Trip Completed: 42%; Cancelled: 19%; No Cars Available: 39%

#Plot number of cars requested during various time-slots
ggplot(uber_data, aes(x=uber_data$bin, fill=uber_data$bin)) + geom_bar() + geom_text(stat='count', aes(label=..count..))
#Demand is highest in the evening hours (5-10 pm)

#Plot number of cars requested for Airport-City or City-Airport trips
ggplot(uber_data, aes(x=uber_data$Pickup.point, fill=uber_data$Pickup.point)) + geom_bar() + geom_text(stat='count', aes(label=..count..))
#No major difference

#---------------------- Creating Subsets for segmented analysis  --------------------------

#Create separate subsets for "Cancelled" and "No Cars Available" scenarios.
cancelled <- subset(uber_data, uber_data$Status=="Cancelled",
                    select = c(Pickup.point, Driver.id, Request.timestamp, request_time, request_day_of_week, request_day, request_hour, bin))
no_cars_available <- subset(uber_data, uber_data$Status=="No Cars Available",
                            select = c(Pickup.point, Driver.id, Request.timestamp, request_time, request_day_of_week, request_day, request_hour, bin))

#Create separate subsets for "Airport" and "City" origins
airport_2_city <- subset(uber_data, uber_data$Pickup.point=="Airport",
                         select = c(Status, Driver.id, Request.timestamp, request_time, request_day_of_week, request_day, request_hour, bin, is_completed))
city_2_airport <- subset(uber_data, uber_data$Pickup.point=="City",
                         select = c(Status, Driver.id, Request.timestamp, request_time, request_day_of_week, request_day, request_hour, bin, is_completed))

#Create separate subsets for "Morning Rush", "Evening Rush", "No Rush"
morning_rush <- subset(uber_data, uber_data$bin=="Morning Rush",
                       select = c(Status, Driver.id, Request.timestamp, request_time, request_day_of_week, request_day, request_hour, Pickup.point, is_completed))
evening_rush <- subset(uber_data, uber_data$bin=="Evening Rush",
                       select = c(Status, Driver.id, Request.timestamp, request_time, request_day_of_week, request_day, request_hour, Pickup.point, is_completed))
no_rush <- subset(uber_data, uber_data$bin=="No Rush",
                       select = c(Status, Driver.id, Request.timestamp, request_time, request_day_of_week, request_day, request_hour, Pickup.point, is_completed))

#-------- Analysis Part 1: Identify the most pressing problem for Uber -------------
#-------- Also analyse the most problematic time-slot and ride type ----------------

#For plotting the frequency of Uber requested against 2 Categorical variables; 
#I am using stacked Bar Charts; as they can be used to show the frequencies for 2 categorical variables 

#Check if there is any significant difference between rides requested and completed on various weekdays
ggplot(uber_data, aes(x=uber_data$request_day_of_week, fill=uber_data$Status)) + geom_bar()
#Nothing significant according to days of the week

#Find the spread of rides requested over different hours of the day
ggplot(uber_data, aes(x=uber_data$request_hour, fill=uber_data$Status)) + geom_bar()
#Most trips are being requested in the morning (5-10 am) or evening (5-10 pm). 
#Also noticeable, a high number of "Cancelled" rides in the morning (5-10 am) 
#and a lot of "No Cars Available" in the evening (5-10 pm)

#Replicate the above plot, but with bins instead of hours
ggplot(uber_data, aes(x=uber_data$bin, fill=uber_data$Status)) + geom_bar()
#The number of trips completed remains the same throughout the day; 
#but cancelled rides are much more in the morning rush,
#and "No cars available" much more in the evening rush

#If we make a scaled plot of the same variables; we see that:
# 1. The percentage of completed rides is the best in the "No rush" bin
# 2. The percentage of cancelled rides is the highest in the "Morning rush"
# 3. The percentage of "No cars available" is highest between 12midnight-3am and again 5pm-9pm "Evening rush"
ggplot(uber_data, aes(x=uber_data$bin, fill=uber_data$Status)) + geom_bar(position = "fill")

#---------------- Analysis of Cancelled Rides -----------------
#Let us see the spread of "Cancelled" rides over the hours and what their pickup point was
ggplot(cancelled, aes(x=cancelled$request_hour, fill=cancelled$Pickup.point)) + geom_bar()
ggplot(cancelled, aes(x=cancelled$bin, fill=cancelled$Pickup.point)) + geom_bar()
#A surprisingly high number of cabs are cancelled in the morning hours (4-10 am) and almost all of them originate from the City
#This shows a reluctance in drivers in going to the Airport in the morning (more than 90% of Cancelled rides in the morning are from the city)

#---------------- Analysis of No Cars Available Scenario -----------------
#Let us see the spread of "No cars available" rides over the hours and what their pickup point was
ggplot(no_cars_available, aes(x=no_cars_available$request_hour, fill=no_cars_available$Pickup.point)) + geom_bar()
ggplot(no_cars_available, aes(x=no_cars_available$bin, fill=no_cars_available$Pickup.point)) + geom_bar()
#A surprisingly high number of "No cars available" scenarios in the evening hours (5-10 pm) and most of them originate from the Airport
#Though car availability in general is also not enough.

#Considering all the plots, it shows on average the reluctancy of drivers to go to the airport ("Cancelled") (specially in the morning); 
#at the same time insufficient number of cars at the airport ("No Cars Available") (specially at evening), when the most requests are being made.
#This shows a bad market health, specially in the morning and evening as almost 60% customers are not able to find rides

#-------- Analysis Part 2: Identify the demand-supply gap for Uber -------------
#-------- Also analyse the most problematic time-slot and ride type ----------------

#During various time-slots; how is the demand at city and airport?

#Check demand at City and Airport in the morning rush
ggplot(morning_rush, aes(morning_rush$Pickup.point, fill=morning_rush$Pickup.point)) + geom_bar() + geom_text(stat='count', aes(label=..count..))
#In the morning, demand at city is almost 4x more than at the airport.
#Check the supply by introducing the factor of ride completion 
#Here we are using Stacked Bar Chart with position="fill" so we can observe data in percentage
ggplot(morning_rush, aes(morning_rush$Pickup.point, fill=morning_rush$is_completed)) + geom_bar(position="fill") 
#In the morning, rides requested from the Airport have high completion rate. (Less demand-supply gap)
#While in the city; only 29% requests were successfully completed. 

#Check demand at City and Airport in the evening rush
ggplot(evening_rush, aes(evening_rush$Pickup.point, fill=evening_rush$Pickup.point)) + geom_bar()  + geom_text(stat='count', aes(label=..count..))
#In the evening, demand at the airport is almost 3x more than at city.
#Check the supply by introducing the factor of ride completion 
#Here we are using Stacked Bar Chart with position="fill" so we can observe data in percentage
ggplot(evening_rush, aes(evening_rush$Pickup.point, fill=evening_rush$is_completed)) + geom_bar(position = "fill") 
#In the morning, rides requested from the City have high completion rate. (Less demand-supply gap)
#While at the airport, a meagre 23% requests were successfully completed

#Check demand at City and Airport when there is no rush
ggplot(no_rush, aes(no_rush$Pickup.point, fill=no_rush$Pickup.point)) + geom_bar() + geom_text(stat='count', aes(label=..count..))
#In non-rush hours, demand is slightly higher in the city
#Check the supply by introducing the factor of ride completion 
#Here we are using Stacked Bar Chart with position="fill" so we can observe data in percentage
ggplot(no_rush, aes(no_rush$Pickup.point, fill=no_rush$is_completed)) + geom_bar(position = "fill")
#Even now, supply is not enough for demand (which is almost 2x supply)

#The same demand-supply issue can be observed according to the type of rides "Airport-City" or "City-Airport"
#Here we are using Stacked Bar Chart with position="fill" so we can observe data in percentage

#Plot completed vs not completed for "Airport-City" rides 
ggplot(airport_2_city, aes(x=airport_2_city$bin, fill=airport_2_city$is_completed)) + geom_bar(position = "fill")
#In the evening rush, only 23% could get a ride out of the airport (supply is 23% of demand)
#While in the morning, almost 87% had a successful ride

#Plot completed vs not completed "City-Airport" rides  
ggplot(city_2_airport, aes(x=city_2_airport$bin, fill=city_2_airport$is_completed)) + geom_bar(position = "fill")
#In the morning rush, just 29% passengers could successfully complete their trip to the airport (supply is 29% of demand)
#While in the evening rush, more than 73% completed their trip to the airport

#An overall completion ratio of 42% is still very less.

#Lastly, we are using a scatter-plot with jitter (or jitter-plot) to visually see the relationship
#Between time-slot, pickup-points and ride completion status.

#Plot all rides according to time-slot, pickup-point and status
ggplot(uber_data, aes(x = uber_data$bin, y = uber_data$Pickup.point)) + geom_jitter(width = 0.45, aes(color=uber_data$Status)) 
#Morning hours at the City and Evenings at the Airport are busiest; 
#and also the worst in terms of market health (Completed ratio)

#Overall we can say that the demand-supply gap is worst at the Airport in the evening;
#followed by at the City in the morning.

#--------------------- Export data for plotting in Tableau ----------------------
write.csv(uber_data, file="uberoutput.csv")

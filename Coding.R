############ Importing Essential libraries ###########

library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(mapview)

########### Importing essential dataset #############

listing <- read.csv("new_orleans_airbnb_listings.csv")

########### Checking the Structure and head of the dataset #############

str(listing)
head(listing)

########## Checking for NA values ##############

sum(is.na(listing))

############# Cleaning the data #############

# Droping the NA values

listing <- drop_na(listing)

# Renaming the column

colnames(listing)[27] <- "price_in_dollars"

# Correcting the format for dataset

listing$host_since <- as.Date(listing$host_since, format = "%Y-%m-%d")
listing$first_review <- as.Date(listing$first_review, format = "%Y-%m-%d")
listing$last_review <- as.Date(listing$last_review, format = "%Y-%m-%d")
listing$price_in_dollars <- gsub("[$,]", "", listing$price_in_dollars)
listing$price_in_dollars <- as.double(listing$price_in_dollars)
listing$longitude <- as.double(listing$longitude)
listing$latitude <- as.double(listing$latitude)

# Separating the host_location data

listing <- separate(listing, "host_location", into = c("host_state", "host_city", "host_country"), sep = ",")

# Removing NA values after separating the data

listing <- drop_na(listing)

############### Analyzing the data through the visualization #############

# Checking the summary of the data

summary(listing)

# Visualizing the prices as per the host state

ggplot(listing, aes(x = host_state, y = price_in_dollars, fill = host_state )) + stat_summary(fun = "mean", geom = "col") + theme(axis.text.x = element_text(angle = 90), legend.position="none") 

# Visualizing the prices as per the host city

ggplot(listing, aes(x = host_city, y = price_in_dollars, fill = host_city )) + stat_summary(fun = "mean", geom = "col") + theme(axis.text.x = element_text(angle = 90), legend.position="none")

# Visualization the prices per the host country

ggplot(listing, aes(x = host_country, y = price_in_dollars, fill = host_country )) + stat_summary(fun = "mean", geom = "col") + theme(axis.text.x = element_text(angle = 90), legend.position="none")

# Visualization the prices as per review_ratings 

ggplot(listing, aes(x = review_scores_rating, y = price_in_dollars, color = review_scores_rating )) + stat_summary(fun = "mean", geom = "line")+ geom_smooth() + theme(axis.text.x = element_text(angle = 90), legend.position="none")

# Visualization the prices as per neighborhood 

ggplot(listing, aes(x = host_neighbourhood, y = price_in_dollars, fill = host_neighbourhood )) + stat_summary(fun = "mean", geom = "col") + theme(axis.text.x = element_text(angle = 90), legend.position="none")

# Visualization the prices as per cleansed neighborhood

ggplot(listing, aes(x = neighbourhood_cleansed, y = price_in_dollars, fill = neighbourhood_cleansed )) + stat_summary(fun = "mean", geom = "col") + theme(axis.text.x = element_text(angle = 90), legend.position="none")

# Visualization the prices as per the property  types

ggplot(listing, aes(x = property_type, y = price_in_dollars, fill = property_type )) + stat_summary(fun = "mean", geom = "col") + theme(axis.text.x = element_text(angle = 90), legend.position="none")

# Visualization the prices as per the room types

ggplot(listing, aes(x = room_type, y = price_in_dollars, fill = room_type )) + stat_summary(fun = "mean", geom = "col") + theme(axis.text.x = element_text(angle = 90), legend.position="none")

# Visualization the price as per review score communication

ggplot(listing, aes(x = review_scores_communication, y = price_in_dollars, color = review_scores_communication )) + stat_summary(fun = "mean", geom = "line")+ geom_smooth() + theme(axis.text.x = element_text(angle = 90), legend.position="none")

# Visualization the price as per review score cleanliness

ggplot(listing, aes(x = review_scores_cleanliness, y = price_in_dollars, color = review_scores_cleanliness )) + stat_summary(fun = "mean", geom = "line")+ geom_smooth() + theme(axis.text.x = element_text(angle = 90), legend.position="none")

# Visualization for longitude and latitude for availability for 365 days 

mapview(listing, xcol = "longitude", ycol = "latitude", zcol = "availability_365", crs = 4269, grid = FALSE)


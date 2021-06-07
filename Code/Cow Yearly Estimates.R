# Notes about the data:
# Feb/March 1983 Access was restricted because road was washed out due to  El Nino storms)
# Did not include partial surveys completed during the 2018/2019 government shutdown

library("readxl")
library(dplyr)

setwd("~/Internship Summer 2021")
seals <- read_excel("Eseal_1981-2020.xlsx")
# get rid of weird column
seals <- subset(seals, select = -(...6))
# get rid of deadpup (550 instances)
seals <- subset(seals, Age !="DEADPUP")

# unique age classes: 
unique(seals$Age)
# "BULL-SA4"    "COW"      "IMM"      "PUP"      "SA1-3"     "UNK"      "WNR"      "YRLNG"    "EPUP" 
#                         immature             sub adult   unknown     weaner     yearling   group with pup

# to get the Date into POSIXlt which has nice access to year/month/day use: strptime(seals$Date, format="%Y-%m-%d")
# currently stored as POSIXct which has nice storage in data frames but hard for us to work with
# access month using $mon, year using $year and day using $mday
# remember zero based indexing so Jan is 0 and Dec is 11
# year starts at 1900 so 1981 is actually 81 when you use $year

seals_date <- strptime(seals$Date, format="%Y-%m-%d")

# months included in the data set:
unique(seals_date$mon)
# returns 0 (Jan), 1 (Feb), 2 (March), 10 (Nov), 11 (Dec)

# write a function to get each season defined as November Year1 to March Year2
# for example season 0 is Nov 1980 to March 1981, season 1 is Nov 1981 to March 1982... season 9 is Nov 1989 to March 1990... season 39 is Nov 2019 to March 2020
# works for dates passed in as POSIXlt

# i haven't had trouble with this as a vectorized function in terms of output, but i'm not sure if there might be a problem i don't know about
# i get this warning message when i run it as a vectorized function:
# In if (date$mon > 9) { :
#    the condition has length > 1 and only the first element will be used

get_season <- function(date) {
  if (date$mon > 9) {season <- date$year - 80}
  else season <- date$year - 81 
  return(season)
}

# now that we have the seasons we can add them to the tables
seals$season <- get_season(seals_date)

# subset to grab only cows
cows <- subset(seals, Age == "COW")

# get largest count for each season by location
largest_count_by_location <- function(table) {
  locations <- list("PR Headlands", "Drakes Beach", "South Beach")   #####how to save each table separately???#######
  for (x in locations) {                                             #####i don't actually use this function but i wish i could#####
    tbl <- subset(table, Location == x)
    tbl$month_day <- format(tbl$Date, format="%m-%d")
    tbl2 <- tbl %>% group_by(season) %>% slice(which.max(Count))
    print(tbl2)
  }
}

# had problems with function above so here is the code for grabbing each location and add the month-day date for each season
PRcows <- subset(cows, Location == "PR Headlands")
PRcows$month_day <- format(PRcows$Date, format="%m-%d")

DBcows <- subset(cows, Location == "Drakes Beach")
DBcows$month_day <- format(DBcows$Date, format="%m-%d")

SBcows <- subset(cows, Location == "South Beach")
SBcows$month_day <- format(SBcows$Date, format="%m-%d")

# recreate the multiplier table from Condit et al. (Table A3)
Date <- format(seq(as.Date("1981-01-11"), as.Date("1981-02-08"), by="days"), format="%m-%d")
Multiplier <- c(2.295, 2.107, 1.946, 1.810, 1.693, 1.593, 1.507, 1.434, 1.372, 1.319, 1.274, 1.236, 1.206, 1.181, 1.161, 1.147, 1.138, 1.133, 1.134, 1.139, 1.149, 1.165, 1.186, 1.214, 1.249, 1.292, 1.343, 1.405, 1.479)
Upper <- c(2.678, 2.427, 2.222, 2.056, 1.916, 1.797, 1.696, 1.606, 1.528, 1.463, 1.406, 1.357, 1.316, 1.283, 1.254, 1.230, 1.213, 1.203, 1.200, 1.207, 1.225, 1.253, 1.287, 1.328, 1.378, 1.427, 1.513, 1.597, 1.702)
Lower <- c(1.888, 1.762, 1.650, 1.555, 1.472, 1.401, 1.340, 1.290, 1.246, 1.210, 1.179, 1.154, 1.133, 1.116, 1.104, 1.096, 1.091, 1.089, 1.087, 1.088, 1.093, 1.102, 1.115, 1.134, 1.159, 1.190, 1.228, 1.276, 1.333)
multiplierTable <- data.frame(Date=Date, Lower=Lower, Multiplier=Multiplier, Upper=Upper)

# function to estimate the population using Condit et al.
# not sure how to vectorize so it only works for rows of a table, not a whole table
# returns 0 if the date is out of the range Condit provided
estimate_pop <- function(row, multTbl) {
  if (row$month_day %in% multTbl$Date) {
    m <- multTbl$Multiplier[multTbl$Date == row$month_day] 
    return(m * row$Count)
  } else {
    return(0)
  }
}

# function that estimates the population for a table
# how do i vectorize????? 
estimate_pop_table <- function(table, multTbl) {
  vec <- numeric()
  for (i in 1:nrow(table)) {
    row <- PRcows[i, ]
    est <- estimate_pop(row, multTbl)
    vec <- c(vec, est)
  }
  table$estimate <- vec
  return(table)
}

# add estimates for all dates available and then get the largest estimate for each year
# note: some years will have an estimate of 0 because of the dates surveyed
PRcows <- estimate_pop_table(PRcows, multiplierTable)
DBcows <- estimate_pop_table(DBcows, multiplierTable)
SBcows <- estimate_pop_table(SBcows, multiplierTable)

# PRcows missing season 0 for estimate
PRcows_max_estimate <- PRcows %>% group_by(season) %>% slice(which.max(estimate))
# DBcows starts at season 14 and is missing seasons 14, 38 (probably because of govt shutdowns/partial counts in 2019)
DBcows_max_estimate <- DBcows %>% group_by(season) %>% slice(which.max(estimate))
# SBcows starts at season 14 and is missing seasons 14, 15, 31
SBcows_max_estimate <- SBcows %>% group_by(season) %>% slice(which.max(estimate))

# combine these tables to get yearly estimates for total
# use seasons 0 to 14 from PRcows as total estimate

cow_estimate_total = PRcows_max_estimate$estimate[1:15]
for (x in 16:39) {
  pr <- PRcows_max_estimate$estimate[PRcows_max_estimate$season == x]
  sb <- SBcows_max_estimate$estimate[SBcows_max_estimate$season == x]
  db <- DBcows_max_estimate$estimate[DBcows_max_estimate$season == x]
  cow_estimate_total <- c(cow_estimate_total, pr + sb + db)
}

# I did this first and realized that on the github Ben said to calculate the estimate using the max count for the season so here's that
# missing estimates for seasons 0, 1, and 13
PRcows_max_count <- PRcows %>% group_by(season) %>% slice(which.max(Count))
# missing estimates for seasons 14, 17, 18, 20, 21, 22, 24, 25, 26, 27, 29, 30, 32, 33, 34, 35, 37, 38 (starts at season 14)
DBcows_max_count <- DBcows %>% group_by(season) %>% slice(which.max(Count))
# missing estimates for seasons 14, 15, 16, 17, 19, 21, 22, 24, 25, 26, 27, 28, 29, 30, 31, 33, 35, 36, 37, 38
SBcows_max_count <- SBcows %>% group_by(season) %>% slice(which.max(Count))

# because there are so many missing values for this I didn't calculate season totals for these
# i haven't really dug into the tables i created yet, this is just the code to get there

# need to go back and look at the dates for these max estimates to make sure they seem reasonable for peak dates
# and determine if there's a better strategy for calculating season estimates


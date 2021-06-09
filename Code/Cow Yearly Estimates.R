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

get_season <- function(date) {
  if (date$mon > 6) {season <- date$year - 80}
  else season <- date$year - 81 
  return(season)
}

# function that does get_season on every item in a list
get_season_all <- function(lst) {
  vec <- vector("list", length(lst))
  for (i in 1:length(lst)) {
    vec[[i]] <- get_season(lst[[i]])
  }
  return(vec)
}

# now that we have the seasons we can add them to the tables
seals$season <- get_season_all(seals_date)

# subset to grab only cows
cows <- subset(seals, Age == "COW")

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

# function for lower bound
estimate_pop_lower <- function(row, multTbl) {
  if (row$month_day %in% multTbl$Date) {
    m <- multTbl$Lower[multTbl$Date == row$month_day] 
    return(m * row$Count)
  } else {
    return(0)
  }
}

# function for upper bound
estimate_pop_upper <- function(row, multTbl) {
  if (row$month_day %in% multTbl$Date) {
    m <- multTbl$Upper[multTbl$Date == row$month_day] 
    return(m * row$Count)
  } else {
    return(0)
  }
}

# function that estimates the population for a table
estimate_pop_table <- function(table, multTbl) {
  vec <- numeric()
  lower <- numeric()
  upper <- numeric()
  for (i in 1:nrow(table)) {
    row <- table[i, ]
    est <- estimate_pop(row, multTbl)
    l <- estimate_pop_lower(row, multTbl)
    u <- estimate_pop_upper(row, multTbl)
    vec <- c(vec, est)
    lower <- c(lower, l)
    upper <- c(upper, u)
  }
  table$estimate <- vec
  table$lower <- lower
  table$upper <- upper
  return(table)
}

# add estimates for all dates available 
# note: some years will have an estimate of 0 because of the dates surveyed, work to correct below
PRcows <- estimate_pop_table(PRcows, multiplierTable)
DBcows <- estimate_pop_table(DBcows, multiplierTable)
SBcows <- estimate_pop_table(SBcows, multiplierTable)

# grab the max counts for each season and use the estimate from that date
# missing estimates for seasons 0 (Jan 6), 1 (Jan 6), and 13 (Feb 9)
PRcows_max_count <- PRcows %>% group_by(season) %>% slice(which.max(Count))
# missing estimates for season 17 (Feb 9)
DBcows_max_count <- DBcows %>% group_by(season) %>% slice(which.max(Count))
# missing estimates for seasons 16 (Dec 28) and 17 (Feb 13)
SBcows_max_count <- SBcows %>% group_by(season) %>% slice(which.max(Count))

# visually inspected each season with missing estimates. here are my manual fixes, explanations, and questions

Prcows0 <- subset(PRcows, season == 0)
# three observations on Jan 6, 7, 8 all with a count of 2
# no correction made; left at 0

PRcows1 <- subset(PRcows, season == 1)
# highest count of 2 occurs on Jan 6, 7, 9, and 11, my function above just takes any one of these counts
# and happened to grab one out of range of Condit's dates. Luckily, Jan 11 is in their range, so use that estimate
PRcows_max_count$estimate[PRcows_max_count$season == 1] <- PRcows1$estimate[PRcows1$month_day == "01-11"]
PRcows_max_count$lower[PRcows_max_count$season == 1] <- PRcows1$lower[PRcows1$month_day == "01-11"]
PRcows_max_count$upper[PRcows_max_count$season == 1] <- PRcows1$upper[PRcows1$month_day == "01-11"]
PRcows_max_count$month_day[PRcows_max_count$season == 1] <- "01-11"
PRcows_max_count$Date[PRcows_max_count$season == 1] <- PRcows1$Date[5]

PRcows13 <- subset(PRcows, season == 13)
# only one observation falls within Condit's date range: Jan 14 with count of 141 and estimate of 255.21
# max count occurs on Feb 9 with a count of 154
# should we use the Jan 14 estimate OR use the Feb 9 count * Feb 8 multiplier (because it's only one day off)?
# Feb 9 count * Feb 8 multiplier = 227.766
# no correct made; left at 0

DBcows17 <- subset(DBcows, season == 17)
# max count occurs on Feb 9 with a count of 53
# have estimates for rows 6-13
DBdates17 <- DBcows17$month_day[6:13]
DBest17 <- DBcows17$estimate[6:13]
DBcounts17 <- DBcows17$Count[6:13]
# "01-13" "01-19" "01-21" "01-24" "01-27" "01-30" "01-31" "02-03"
# 3.892    20.580 25.480   23.620   27.312 33.031  28.725  46.132
# 2           15    20       20        24     29     25      38
# same question as above should we use Feb 9 * Feb 8 multiplier:
# 53 * multiplierTable$Multiplier[multiplierTable$Date == "02-08"] = 78.387 
# no correct made; left at 0

SBcows16 <- subset(SBcows, season == 16)
# 4 total observations on 12-28, 01-09, 01-27, 01-31 all have same count (1) 
# Jan 27 has an estimate of 1.138 and Jan 31 has an estimate of 1.149
# these are very similar so I chose to use Jan 27 estimate arbitrarily
SBcows_max_count$estimate[SBcows_max_count$season == 16] <- SBcows16$estimate[SBcows16$month_day == "01-27"]
SBcows_max_count$lower[SBcows_max_count$season == 16] <- SBcows16$lower[SBcows16$month_day == "01-27"]
SBcows_max_count$upper[SBcows_max_count$season == 16] <- SBcows16$upper[SBcows16$month_day == "01-27"]
SBcows_max_count$month_day[SBcows_max_count$season == 16] <- "01-27"
SBcows_max_count$Date[SBcows_max_count$season == 16] <- SBcows16$Date[3]

SBcows17 <- subset(SBcows, season == 17)
# max count occurs on Feb 13 with count of 12
# have esimates for rows 7 to 14
SBdates17 <- SBcows17$month_day[7:14]
SBest17 <- SBcows17$estimate[7:14]
SBcounts17 <- SBcows17$Count[7:14]
# "01-13" "01-19" "01-21" "01-24" "01-27" "01-30" "01-31" "02-03" "02-09" "02-13"
# 1.946   1.372     5.096  3.543   3.414   5.695   3.447   8.498    NA      NA
# 1         1         4      3       3        5      3       7       5      12
# i feel less comfortable proposing to use max count * Feb 8 multiplier but in this case it is
# 12 * multiplierTable$Multiplier[multiplierTable$Date == "02-08"] = 17.748
# # no correct made; left at 0


# calculate totals across locations for cows
# pass in tbales for each location and col as a string 
total_max_count <- function(PR, SB, DB, col) {
  vec <- vector("list", 40)
  for (i in 0:39) {
    a <- PR[[col]][PR$season == i]
    if (identical(a, numeric(0))) { a <- 0}
    b <- SB[[col]][SB$season == i]
    if (identical(b, numeric(0))) { b <- 0}
    c <- DB[[col]][DB$season == i]
    if (identical(c, numeric(0))) { c <- 0}
    vec[[i+1]] <- a + b + c # lists aren't zero indexed here
  }
  return(vec)
}

# total max count for all locations
cow_max_total <- total_max_count(PRcows_max_count, SBcows_max_count, DBcows_max_count, "Count")
# estimations
cow_est_total <- total_max_count(PRcows_max_count, SBcows_max_count, DBcows_max_count, "estimate")
cow_est_lower_total <- total_max_count(PRcows_max_count, SBcows_max_count, DBcows_max_count, "lower")
cow_est_upper_total <- total_max_count(PRcows_max_count, SBcows_max_count, DBcows_max_count, "upper")

# plot total estimates and max counts for each season
plot(1981:2020, cow_est_total, main="Estimated Cow Count from 1981 to 2020\nAll Locations", xlab="Year", ylab='Estimate', pch=15)
lines(1981:2020, cow_est_total)
lines(1981:2020, cow_est_lower_total)
lines(1981:2020, cow_est_upper_total)

# can also plot for each location
# these plots will plot 0 for missing estimates
plot(1981:2020, PRcows_max_count$estimate, main="Estimated Cow Count from 1981 to 2020\nPR Headlands", xlab="Year", ylab='Estimate', pch=15)
lines(1981:2020, PRcows_max_count$estimate)
lines(1981:2020, PRcows_max_count$lower, col="grey")
lines(1981:2020, PRcows_max_count$upper, col="grey")
plot(1995:2020, DBcows_max_count$estimate, main="Estimated Cow Count from 1995 to 2020\nDrakes Beach", xlab="Year", ylab='Estimate', pch=15)
lines(1995:2020, DBcows_max_count$estimate)
lines(1995:2020, DBcows_max_count$lower, col="grey")
lines(1995:2020, DBcows_max_count$upper, col="grey")
plot(1995:2020, SBcows_max_count$estimate, main="Estimated Cow Count from 1995 to 2020\nSouth Beach", xlab="Year", ylab='Estimate', pch=15)
lines(1995:2020, SBcows_max_count$estimate)
lines(1995:2020, SBcows_max_count$lower, col="grey")
lines(1995:2020, SBcows_max_count$upper, col="grey")

# do the max count dates seem reasonable?
PRdays <- unique(PRcows_max_count$month_day)
# range for PR: Jan 6 to Feb 9
DBdays <- unique(DBcows_max_count$month_day)
# range for DB: Jan 23 to Feb 9
SBdays <- unique(SBcows_max_count$month_day)
# range for SB: Jan 17 to Feb 13

library("readxl")
library(dplyr)

setwd("~/Internship Summer 2021")
seals <- read_excel("Eseal_1981-2020.xlsx")
# get rid of weird column
seals <- subset(seals, select = -(...6))
# get rid of deadpup (550 instances)
seals <- subset(seals, Age !="DEADPUP")

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
#get rid of space in col name
colnames(seals)[5] <- "Survey_Type"
cows <- subset(seals, Age == "COW")

# grab cows by location and add month_day
PRcows <- subset(cows, Location == "PR Headlands")
PRcows$month_day <- format(PRcows$Date, format="%m-%d")

DBcows <- subset(cows, Location == "Drakes Beach")
DBcows$month_day <- format(DBcows$Date, format="%m-%d")

SBcows <- subset(seals, Age == "COW" & Location == "South Beach")
SBcows$month_day <- format(SBcows$Date, format="%m-%d")

# from Condit
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

PRcows <- estimate_pop_table(PRcows, multiplierTable)

DBcows <- estimate_pop_table(DBcows, multiplierTable)

SBcows <- estimate_pop_table(SBcows, multiplierTable)


# determine which seasons don't fall into the date range now given by Sarah Codde: Jan 26 to Feb 2
peakDays <- format(seq(as.Date("1981-01-26"), as.Date("1981-02-02"), by="days"), format="%m-%d")

validate_dates <- function(table) {
  valid <- logical()
  for (i in 1:nrow(table)) {
    row <- table[i, ]
    isit <- row$month_day %in% peakDays
    valid <- c(valid, isit)
  }
  table$valid_date <- valid
  return(table)
}

PRcows <- validate_dates(PRcows)

DBcows <- validate_dates(DBcows)

SBcows <- validate_dates(SBcows)

# determine which years/seasons have observations during the peak Days
# returns a list of booleans where each element is a boolean value and named the season it corresponds to
valid_seasons <- function(table) {
  seasons <- unique(table$season) 
  vec <- vector("list", length(seasons))
  names(vec) <- seasons
    for (i in 1:length(seasons)) {
      tb <- subset(table, season == seasons[[i]]) 
      for (j in 1:nrow(tb)) {
        row <- tb[j, ]
        if (row$valid_date == TRUE) {
          vec[[i]] <- TRUE
        } 
      }
      if (is.null(vec[[i]])) {
        vec[[i]] <- FALSE
      }
    }
  return(vec)
}

# takes in a list returned by valid_seasons and a table with all counts for a location
# returns max counts in peakDays if they exist, if they don't take max count
get_max <- function(list, table) {
  df <- data.frame(Date=as.Date(character()), Location=character(), Age=character(), Count=numeric(), Survey_Type=character(), season=numeric(), month_day=character(), estimate=numeric(), lower=numeric(), upper=numeric(), valid_date=logical())
  for (i in 1:length(list)) {
    tb <- subset(table, season == as.numeric(names(list)[i]))
    if (list[[i]]) { #you do have a valid date for that season
      tb <- subset(tb, month_day %in% peakDays)
    } 
    tb <- tb %>% slice(which.max(Count))
    df <- rbind(df, tb)
  }
  return(df)
}

PRvalszn <- valid_seasons(PRcows)
PRcows_max_count <- get_max(PRvalszn, PRcows)
#season 2 only has observations before peakDays,  here uses max count

PRcows0 <- subset(PRcows, season == 0)
# three observations on Jan 6, 7, 8 all with a count of 2, set estimate and CI to 2 as well
PRcows_max_count$estimate[PRcows_max_count$season == 0] <- PRcows0$Count[1]
PRcows_max_count$lower[PRcows_max_count$season == 0] <- PRcows0$Count[1]
PRcows_max_count$upper[PRcows_max_count$season == 0] <- PRcows0$Count[1]

PRcows1 <- subset(PRcows, season == 1)
# highest count of 2 occurs on Jan 6, 7, 9, and 11, set estimate and CI to 2 as well
PRcows_max_count$estimate[PRcows_max_count$season == 1] <- PRcows1$Count[PRcows1$month_day == "01-11"]
PRcows_max_count$lower[PRcows_max_count$season == 1] <- PRcows1$Count[PRcows1$month_day == "01-11"]
PRcows_max_count$upper[PRcows_max_count$season == 1] <- PRcows1$Count[PRcows1$month_day == "01-11"]
PRcows_max_count$month_day[PRcows_max_count$season == 1] <- "01-11"
PRcows_max_count$Date[PRcows_max_count$season == 1] <- PRcows1$Date[5]

PRcows13 <- subset(PRcows, season == 13)
# no counts between 1/14 and 2/9
# max count occurs on Feb 9 with a count of 154
# use the Feb 9 count * Feb 8 multiplier (because it's only one day off)
PRcows_max_count$estimate[PRcows_max_count$season == 13] <- PRcows13$Count[PRcows13$month_day == "02-09"] * multiplierTable$Multiplier[multiplierTable$Date == "02-08"]
PRcows_max_count$upper[PRcows_max_count$season == 13] <- PRcows13$Count[PRcows13$month_day == "02-09"] * multiplierTable$Upper[multiplierTable$Date == "02-08"]
PRcows_max_count$lower[PRcows_max_count$season == 13] <- PRcows13$Count[PRcows13$month_day == "02-09"] * multiplierTable$Lower[multiplierTable$Date == "02-08"]


DBvalszn <- valid_seasons(DBcows)
DBcows_max_count <- get_max(DBvalszn, DBcows)
# guidance from Sarah Codde on season 39: "I'm pretty confident we missed the peak due to bad visibility (fog) 
# and then there was a quick drop off in numbers by 1/31. I think the count on 1/23 is a true peak. I think this 
# should be the one exception (to taking dates between 1/26 and 2/2) and we should keep this one."
# check with Ben though 
# current code goes to 1/31 for count (932)/estimate (1070.868) this switches to 1/23
DB39 <- subset(DBcows, season == 39)
DBcows_max_count$Date[DBcows_max_count$season == 39] <- DB39$Date[DB39$month_day == "01-23"]
DBcows_max_count$month_day[DBcows_max_count$season == 39] <-  "01-23"
DBcows_max_count$valid_date[DBcows_max_count$season == 39] <- FALSE
DBcows_max_count$Count[DBcows_max_count$season == 39] <- DB39$Count[DB39$month_day == "01-23"] #974
DBcows_max_count$estimate[DBcows_max_count$season == 39] <- DB39$estimate[DB39$month_day == "01-23"] #1174.644
DBcows_max_count$lower[DBcows_max_count$season == 39] <- DB39$lower[DB39$month_day == "01-23"]
DBcows_max_count$upper[DBcows_max_count$season == 39] <- DB39$upper[DB39$month_day == "01-23"]
  
  
SBvalszn <- valid_seasons(SBcows)
SBcows_max_count <- get_max(SBvalszn, SBcows)
#SB seasons 14/15 okay as is because few observations

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
lines(1981:2020, cow_est_lower_total, col="grey")
lines(1981:2020, cow_est_upper_total, col="grey")

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



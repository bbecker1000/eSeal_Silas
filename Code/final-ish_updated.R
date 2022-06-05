library("readxl")
library(dplyr)
library(lme4)
library(gamm4)

setwd("~/Internship Summer 2021")
seals <- read_excel("Eseal_1981-2020.xlsx")
# get rid of weird column
seals <- subset(seals, select = -(...6))

###############################
###Exploratory Data Analysis###
###############################

# get rid of deadpup (550 instances)
seals <- subset(seals, Age !="DEADPUP")

seals_date <- strptime(seals$Date, format="%Y-%m-%d")

# months included in the data set:
# 0 (Jan), 1 (Feb), 2 (March), 10 (Nov), 11 (Dec)

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

# function that gets the year corresponding to the season, useful for plotting
get_year <- function(date) {
  if (date$mon > 6) { 
    return(date$year+1901)
  } else {
    return(date$year+1900)
  }
}

# function that does get_year on every item in a list
get_year_all <- function(lst) {
  vec <- numeric()
  for (i in 1:length(lst)) {
    vec <- c(vec, get_year(lst[[i]]))
  }
  return(vec)
}

# now that we have the seasons we can add them to the tables
seals$season <- get_season_all(seals_date)
seals$year <- get_year_all(seals_date)
seals$month_day <- format(seals$Date, format="%m-%d")
# get rid of space in col name
colnames(seals)[5] <- "Survey_Type"

####################
######COW WORK######
####################

cows <- subset(seals, Age == "COW")

# grab cows by location
PRcows <- subset(cows, Location == "PR Headlands")
DBcows <- subset(cows, Location == "Drakes Beach")
SBcows <- subset(seals, Age == "COW" & Location == "South Beach")


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
# returns max counts in peakDays if they exist. if not then just take max count regardless of date
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
#season 2 only has observations before peakDays, here uses max count

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
# without the following code, this defaults to 1/31 for count (932) and estimate (1070.868)
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
  vec <- numeric()
  for (i in 0:39) {
    a <- PR[[col]][PR$season == i]
    if (identical(a, numeric(0))) { a <- 0}
    b <- SB[[col]][SB$season == i]
    if (identical(b, numeric(0))) { b <- 0}
    c <- DB[[col]][DB$season == i]
    if (identical(c, numeric(0))) { c <- 0}
    vec <- c(vec, a + b + c)
  }
  return(vec)
}

# total max count for all locations
cow_max_total <- total_max_count(PRcows_max_count, SBcows_max_count, DBcows_max_count, "Count")
# estimations for all locations
cow_est_total <- total_max_count(PRcows_max_count, SBcows_max_count, DBcows_max_count, "estimate")
cow_est_lower_total <- total_max_count(PRcows_max_count, SBcows_max_count, DBcows_max_count, "lower")
cow_est_upper_total <- total_max_count(PRcows_max_count, SBcows_max_count, DBcows_max_count, "upper")


# plot total estimates and max counts for each season
#plot(1981:2020, cow_est_total, main="Estimated Cow Count from 1981 to 2020\nAll Locations", xlab="Year", ylab='Estimate', pch=15)
#lines(1981:2020, cow_est_total)
#lines(1981:2020, cow_est_lower_total, col="grey")
#lines(1981:2020, cow_est_upper_total, col="grey")

# can also plot for each location
# these plots will plot 0 for missing estimates
#plot(1981:2020, PRcows_max_count$estimate, main="Estimated Cow Count from 1981 to 2020\nPR Headlands", xlab="Year", ylab='Estimate', pch=15)
#lines(1981:2020, PRcows_max_count$estimate)
#lines(1981:2020, PRcows_max_count$lower, col="grey")
#lines(1981:2020, PRcows_max_count$upper, col="grey")

#plot(1995:2020, DBcows_max_count$estimate, main="Estimated Cow Count from 1995 to 2020\nDrakes Beach", xlab="Year", ylab='Estimate', pch=15)
#lines(1995:2020, DBcows_max_count$estimate)
#lines(1995:2020, DBcows_max_count$lower, col="grey")
#lines(1995:2020, DBcows_max_count$upper, col="grey")

#plot(1995:2020, SBcows_max_count$estimate, main="Estimated Cow Count from 1995 to 2020\nSouth Beach", xlab="Year", ylab='Estimate', pch=15)
#lines(1995:2020, SBcows_max_count$estimate)
#lines(1995:2020, SBcows_max_count$lower, col="grey")
#lines(1995:2020, SBcows_max_count$upper, col="grey")

####################
######PUP WORK######
####################

# subset data by age class
# i visually inspected epups and pups_only and didn't see overlap in date and location so change all "EPUPS" to "PUPS"
pups <- subset(seals, Age == "PUP" | Age == "EPUP") 
pups$Age[pups$Age == "EPUP"] <- "PUP"

# just like for cows, we want to have counts from the peak days (Jan 26 to Feb 2)
pups <- validate_dates(pups)

# subset by location and determine which locations have observations during the peak Days to grab the max counts during the peak
PRpups <- subset(pups, Location == "PR Headlands")
PRvalsznp <- valid_seasons(PRpups)
PRpups_max_count <- get_max(PRvalsznp, PRpups)
# seasons 0, 1, 2, 13 do not have observations during the peak days. checked out visually and i'm ok with what was output by get_max


DBpups <- subset(pups, Location == "Drakes Beach")
DBvalsznp <- valid_seasons(DBpups)
DBpups_max_count <- get_max(DBvalsznp, DBpups)
# has a count for each season during the peak days

SBpups <- subset(pups, Location == "South Beach")
SBvalsznp <- valid_seasons(SBpups)
SBpups_max_count <- get_max(SBvalsznp, SBpups)
# seasons 14 and 15 both only have two observations, all of which are outside of peak days. i'm ok with what was output by get_max

# total pups up across locations. pups don't move between beaches so you can total different peak days
pups_max_total <- total_max_count(PRpups_max_count, SBpups_max_count, DBpups_max_count, "Count")

# also want to look at pups and weaned pups together. weaned pups might move between beaches so we need to take a max count from a single day each year

# takes in a table and returns a list indexed by season
# each item in the returned list is a list of all locations counts exist for in that season
which_seasons <- function(table) {
  seasons <- unique(table$season) 
  vec <- vector("list", length(seasons))
  names(vec) <- seasons
  for (i in 1:length(seasons)) {
    tb <- subset(table, season == seasons[[i]])
    locations <- unique(tb$Location)
    vec[[i]] <- locations
  }
  return(vec)
}

# returns a table with dates and counts combined across locations
combine_counts <- function(list, table) {
  df <- data.frame(Date=as.Date(character()), Count=numeric(), season=numeric(), year=numeric(), valid_date=logical(), month_day=character())
  for (i in 1:length(list)) {
    szn <- as.numeric(names(list)[i])
    yr <- szn + 1981
    locations <- list[[i]]
    t <- subset(table, season == szn)
    dates <- unique(t$Date)
    for (j in 1:length(dates)) {
      d <- subset(t, Date == dates[j])
      sum <- 0
      for (k in 1:nrow(d)) {
        sum <- sum + d[k, ]$Count
      }
      r <- data.frame(dates[j], sum, szn, yr, d$valid_date[1], d$month_day[1])
      names(r) <- c("Date", "Count", "season", "year", "valid_date", "month_day")
      df <- rbind(df, r)
    }
  }
  return(df)
}

wnr <- subset(seals, Age == "WNR")
wnr <- validate_dates(wnr)
pw <- rbind(wnr, pups)

pw_locations <- which_seasons(pw)
pw_all <- combine_counts(pw_locations, pw)
pwvalszn <- valid_seasons(pw_all) 
# we have at least 1 observation during the peak days except for seasons 0, 1, and 13 
# as before, seasons 0, 1 have only a handful of observations and 13 doesn't have any observations between 1/14 and 2/9
pw_all_max_counts <- get_max(pwvalszn , pw_all)

# plot Pups and Weaned Pups
#title <- paste ("Maximum Pup and Weaned Pup Counts")
#plot(pw_all_max_counts$year, pw_all_max_counts$Count, main=title, xlab="Year", ylab='Maximum Count', pch=15)
#lines(pw_all_max_counts$year, pw_all_max_counts$Count)


####################
###Tidy(ish) Data###
####################

########COWS########

# puts the data assembled above into a tidy(ish) data format with columns:
# Age ("COW", "PUP", "PUPWNR"); Year (1981:2020); Location ("PR Headlands", "Drakes Beach", "South Beach", "All");
# Count; Count_Type ("max", "estimate", "lower_est", "upper_est")

tidyCows <- data.frame(Age=character(), Year=numeric(), Location=character(), Count=numeric(), Count_Type=character())
season <- 1

for (i in 1981:2020) {
  # grab PR for season iteratively
  PR <- subset(PRcows_max_count, year==i)
  PR <- PR[, c(2, 3, 4, 7, 9, 10, 11)]
  
  max <- data.frame("COW", i, "PR Headlands", PR$Count, "max")
  names(max) <- c("Age", "Year", "Location", "Count", "Count_Type")
  tidyCows <- rbind(tidyCows, max)
  
  est <- data.frame("COW", i, "PR Headlands", PR$estimate, "estimate")
  names(est) <- c("Age", "Year", "Location", "Count", "Count_Type")
  tidyCows <- rbind(tidyCows, est)
  
  low <- data.frame("COW", i, "PR Headlands", PR$lower, "lower estimate")
  names(low) <- c("Age", "Year", "Location", "Count", "Count_Type")
  tidyCows <- rbind(tidyCows, low)
  
  up <- data.frame("COW", i, "PR Headlands", PR$upper, "upper estimate")
  names(up) <- c("Age", "Year", "Location", "Count", "Count_Type")
  tidyCows <- rbind(tidyCows, up)
  
  # grab DB and SB counts when they exist (above season 13)
  if (i >= 1995) {
    DB <- subset(DBcows_max_count, year==i)
    DB <- DB[, c(2, 3, 4, 7, 9, 10, 11)]
    
    max <- data.frame("COW", i, "Drakes Beach", DB$Count, "max")
    names(max) <- c("Age", "Year", "Location", "Count", "Count_Type")
    tidyCows <- rbind(tidyCows, max)
    
    est <- data.frame("COW", i, "Drakes Beach", DB$estimate, "estimate")
    names(est) <-c("Age", "Year", "Location", "Count", "Count_Type")
    tidyCows <- rbind(tidyCows, est)
    
    low <- data.frame("COW", i, "Drakes Beach", DB$lower, "lower estimate")
    names(low) <- c("Age", "Year", "Location", "Count", "Count_Type")
    tidyCows <- rbind(tidyCows, low)
    
    up <- data.frame("COW", i, "Drakes Beach", DB$upper, "upper estimate")
    names(up) <- c("Age", "Year", "Location", "Count", "Count_Type")
    tidyCows <- rbind(tidyCows, up)
    
    SB <- subset(SBcows_max_count, year==i)
    SB <- SB[, c(2, 3, 4, 7, 9, 10, 11)]
    
    max <- data.frame("COW", i, "South Beach", SB$Count, "max")
    names(max) <-c("Age", "Year", "Location", "Count", "Count_Type")
    tidyCows <- rbind(tidyCows, max)
    
    est <- data.frame("COW", i, "South Beach", SB$estimate, "estimate")
    names(est) <- c("Age", "Year", "Location", "Count", "Count_Type")
    tidyCows <- rbind(tidyCows, est)
    
    low <- data.frame("COW", i, "South Beach", SB$lower, "lower estimate")
    names(low) <- c("Age", "Year", "Location", "Count", "Count_Type")
    tidyCows <- rbind(tidyCows, low)
    
    up <- data.frame("COW", i, "South Beach", SB$upper, "upper estimate")
    names(up) <- c("Age", "Year", "Location", "Count", "Count_Type")
    tidyCows <- rbind(tidyCows, up)
  }
  
  
  # get the conts from all locations
  max <- data.frame("COW", i, "All", cow_max_total[season], "max")
  names(max) <- c("Age", "Year", "Location", "Count", "Count_Type")
  tidyCows <- rbind(tidyCows, max)
  
  est <- data.frame("COW", i, "All", cow_est_total[season], "estimate")
  names(est) <- c("Age", "Year", "Location", "Count", "Count_Type")
  tidyCows <- rbind(tidyCows, est)
  
  low <- data.frame("COW", i, "All", cow_est_lower_total[season], "lower estimate")
  names(low) <- c("Age", "Year", "Location", "Count", "Count_Type")
  tidyCows <- rbind(tidyCows, low)
  
  up <- data.frame("COW", i, "All", cow_est_upper_total[season], "upper estimate")
  names(up) <- c("Age", "Year", "Location", "Count", "Count_Type")
  tidyCows <- rbind(tidyCows, up)
  season <- season + 1
}

########PUPS########

tidyPups <- data.frame(Age=character(), Year=numeric(), Location=character(), Count=numeric(), Count_Type=character())
season <- 1
for (i in 1981:2020) {
  PR <- subset(PRpups_max_count, year == i)
  if (nrow(PR) > 0) {
    t <- data.frame("PUP", i, "PR Headlands", PR$Count, "max")
    names(t) <- c("Age", "Year", "Location", "Count", "Count_Type")
    tidyPups <- rbind(tidyPups, t)
  }
  
  DB <- subset(DBpups_max_count, year == i)
  if (nrow(DB) > 0) {
    t <- data.frame("PUP", i, "Drakes Beach", DB$Count, "max")
    names(t) <- c("Age", "Year", "Location", "Count", "Count_Type")
    tidyPups <- rbind(tidyPups, t)
  }
  
  SB <- subset(SBpups_max_count, year == i)
  if (nrow(SB) > 0) {
    t <- data.frame("PUP", i, "South Beach", SB$Count, "max")
    names(t) <- c("Age", "Year", "Location", "Count", "Count_Type")
    tidyPups <- rbind(tidyPups, t)
  }
  
  t <- data.frame("PUP", i, "All", pups_max_total[season], "max")
  names(t) <- c("Age", "Year", "Location", "Count", "Count_Type")
  tidyPups <- rbind(tidyPups, t)
  season <- season + 1
  
  pws <- subset(pw_all_max_counts, year == i)
  t <- data.frame("PUP&WNR", i, "All", pws$Count, "max")
  names(t) <- c("Age", "Year", "Location", "Count", "Count_Type")
  tidyPups <- rbind(tidyPups, t)
}

# WHAT IS GOING ON WITH SEASON 14/YEAR 1995
# I think it's okay but I should come back and look at why the pup count is higher than the pup&weaner count and decide what to do when this happens

tidySeals <- rbind(tidyPups, tidyCows)

######################
###POULATION TRENDS###
######################

###################### 
# Simple linear regression on log(counts) for cows/pups/wnrs at the three sites and as a whole. 
######################

tidyPRcowsest <- subset(tidyCows, Location == "PR Headlands" & Count_Type == "estimate")
tidyPRpup <- subset(tidyPups, Location == "PR Headlands" & Count_Type == "max")

tidyDBcowsest <- subset(tidyCows, Location == "Drakes Beach" & Count_Type == "estimate")
tidyDBpup <- subset(tidyPups, Location == "Drakes Beach" & Count_Type == "max")

tidySBcowsest <- subset(tidyCows, Location == "South Beach" & Count_Type == "estimate")
tidySBpup <- subset(tidyPups, Location == "South Beach" & Count_Type == "max")

tidyALLcowsest <- subset(tidyCows, Location == "All" & Count_Type == "estimate")
tidyALLpup <- subset(tidyPups, Location == "All" & Age == "PUP" & Count_Type == "max")
tidyALLpupwnr <- subset(tidyPups, Location == "All" & Age == "PUP&WNR" & Count_Type == "max")

#PR COW
LM_PR_cow <- lm(log(tidyPRcowsest$Count) ~ tidyPRcowsest$Year)
summary(LM_PR_cow)
plot(tidyPRcowsest$Year, log(tidyPRcowsest$Count), main="Log Estimated Cow Count from 1981 to 2020\nPR Headlands", xlab="Year", ylab='Log Count', pch=16)
abline(LM_PR_cow, col="red")
plot(LM_PR_cow)

#PR PUP
LM_PR_pup <- lm(log(tidyPRpup$Count) ~ tidyPRpup$Year)
summary(LM_PR_pup)
plot(tidyPRpup$Year, log(tidyPRpup$Count), main="Log Maximum Pup Count from 1981 to 2020\nPR Headlands", xlab="Year", ylab='Log Count', pch=16)
abline(LM_PR_pup, col="red")
plot(LM_PR_pup)

#DB COW
LM_DB_cow <- lm(log(tidyDBcowsest$Count) ~ tidyDBcowsest$Year)
summary(LM_DB_cow)
plot(tidyDBcowsest$Year, log(tidyDBcowsest$Count), main="Log Estimated Cow Count from 1995 to 2020\nDrakes Beach", xlab="Year", ylab='Log Count', pch=16)
abline(LM_DB_cow, col="red")
plot(LM_DB_cow)
# honestly this does a pretty ok job

#DB PUP
LM_DB_pup <- lm(log(tidyDBpup$Count) ~ tidyDBpup$Year)
summary(LM_DB_pup)
plot(tidyDBpup$Year, log(tidyDBpup$Count), main="Log Maximum Pup Count from 1995 to 2020\nDrakes Beach", xlab="Year", ylab='Log Count', pch=16)
abline(LM_DB_pup, col="red")
plot(LM_DB_pup)
# honestly this does a pretty ok job

#SB COW
LM_SB_cow <- lm(log(tidySBcowsest$Count) ~ tidySBcowsest$Year)
summary(LM_SB_cow)
plot(tidySBcowsest$Year, log(tidySBcowsest$Count), main="Log Estimated Cow Count from 1995 to 2020\nSouth Beach", xlab="Year", ylab='Log Count', pch=16)
abline(LM_SB_cow, col="red")
plot(LM_SB_cow)
# honestly this does a pretty ok job

#SB PUP
LM_SB_pup <- lm(log(tidySBpup$Count) ~ tidySBpup$Year)
summary(LM_SB_pup)
plot(tidySBpup$Year, log(tidySBpup$Count), main="Log Maximum Pup Count from 1983 to 2020\nSouth Beach", xlab="Year", ylab='Log Count', pch=16)
abline(LM_SB_pup, col="red")
plot(LM_SB_pup)
# honestly this does a pretty ok job, but could do better by dropping the one odd count from 1983

#SB PUP NO 1983
tidySBpupN <- subset(tidySBpup, Year != 1983)
LM_SB_pupN <- lm(log(tidySBpupN$Count) ~ tidySBpupN$Year)
summary(LM_SB_pupN)
plot(tidySBpupN$Year, log(tidySBpupN$Count), main="Log Maximum Pup Count from 1995 to 2020\nSouth Beach", xlab="Year", ylab='Log Count', pch=16)
abline(LM_SB_pupN, col="red")
plot(LM_SB_pupN)
# ok maybe I like the SB linear regressions less than I initially thought...

#ALL LOCATIONS COW
LM_all_cow <- lm(log(tidyALLcowsest$Count) ~ tidyALLcowsest$Year)
summary(LM_all_cow)
plot(tidyALLcowsest$Year, log(tidyALLcowsest$Count), main="Log Estimated Cow Count from 1981 to 2020\nAll Locations", xlab="Year", ylab='Log Count', pch=16)
abline(LM_all_cow, col="red")
plot(LM_all_cow)

#ALL LOCATIONS PUP
LM_all_pup <- lm(log(tidyALLpup$Count) ~ tidyALLpup$Year)
summary(LM_all_pup)
plot(tidyALLpup$Year, log(tidyALLpup$Count), main="Log Maximum Pup Count from 1981 to 2020\nAll Locations", xlab="Year", ylab='Log Count', pch=16)
abline(LM_all_pup, col="red")
plot(LM_all_pup)

#ALL LOCATIONS PUP AND WEANER
LM_all_pupwnr <- lm(log(tidyALLpupwnr$Count) ~ tidyALLpupwnr$Year)
summary(LM_all_pupwnr)
plot(tidyALLpupwnr$Year, log(tidyALLpupwnr$Count), main="Log Maximum Pup and Weaner Count from 1981 to 2020\nAll Locations", xlab="Year", ylab='Log Count', pch=16)
abline(LM_all_pupwnr, col="red")
plot(LM_all_pupwnr)


###################### Generalized Linear Mixed-model with counts poisson distributed
###################### ignore?? double check

# format: m.glmer <- glmer(Count ~ Year + (1|Site), family = poisson, data = DATA) 

#COW
tidycowsNoT <- subset(tidyCows, Location != "All" & Count_Type == "estimate")
tidycowsNoT$rounded <- round(tidycowsNoT$Count) #need integer values
#glmr_cow <- glmer(rounded ~ Year + (1|Location), family = poisson, data = tidycowsNoT)
# above throws Warning message: 
#  In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                 Model is nearly unidentifiable: very large eigenvalue
#               - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
#               - Rescale variables?

#I figure that the same will happen for pups and pups/weaners so I didn't try it


#ALL LOCATIONS COW
glm_all_cow <- glm(round(tidyALLcowsest$Count) ~ tidyALLcowsest$Year, family = poisson) #wants integer values only so round the estimated counts
plot(tidyALLcowsest$Year, log(tidyALLcowsest$Count), main="Log Estimated Cow Counts from 1981 to 2020\nAll Locations", xlab="Year", ylab='log count', pch=16)
abline(glm_all_cow, col="blue")

#ALL LOCATIONS PUP
glm_all_pup <- glm(tidyALLpup$Count ~ tidyALLpup$Year, family = poisson)
plot(tidyALLpup$Year, log(tidyALLpup$Count), main="Log Max Pup Counts from 1981 to 2020\nAll Locations", xlab="Year", ylab='log count', pch=16)
abline(glm_all_pup, col="blue")

#ALL LOCATIONS PUP AND WEANER
glm_all_pupwnr <- glm(tidyALLpupwnr$Count ~ tidyALLpupwnr$Year, family = poisson)
plot(tidyALLpupwnr$Year, log(tidyALLpupwnr$Count), main="Log Max Pup and Weaner Counts from 1981 to 2020\nAll Locations", xlab="Year", ylab='log count', pch=16)
abline(glm_all_pupwnr, col="blue")


###################### Generalized Additive Mixed model with counts poisson distributed
#update 6/5 added family arg -> nonlinear plots..

tidypupNoT <- subset(tidyPups, Location != "All" & Age == "PUP")
tidypupwnrNoT <- subset(tidyPups, Age == "PUP&WNR")

gamm_cow <- gamm4(Count ~ s(Year), random=~(1|Location), data=tidycowsNoT, family=poisson)
plot(gamm_cow$gam)
summary(gamm_cow$gam) 

gamm_pup <- gamm4(Count ~ s(Year), random=~(1|Location), data=tidypupNoT, family=poisson)
plot(gamm_pup$gam)

#removed random effects because it's calculated across all locations not by location (let me know if we need it by location)
gamm_pupwnr <- gamm4(Count ~ s(Year), data=tidypupwnrNoT, family=poisson)
plot(gamm_pupwnr$gam)
#plots of all are similar.. gamm_pupwnr is a little different than the other two

#poisson residuals better

ga_cow <- gam(round(Count) ~ s(Year), family=poisson, data=tidyALLcowsest)
plot(ga_cow)
plot(fitted(ga_cow), resid(ga_cow))

ga_pup <- gam(Count ~s(Year), family=poisson, data=tidyALLpup)
plot(ga_pup)
plot(fitted(ga_pup), resid(ga_pup))

ga_pupwnr <- gam(Count ~s(Year), family=poisson, data=tidyALLpupwnr)
plot(ga_pupwnr)
plot(fitted(ga_pupwnr), resid(ga_pupwnr))



#look into breakpoints -> two piecewise linear models https://cran.r-project.org/web/packages/segmented/segmented.pdf
#https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

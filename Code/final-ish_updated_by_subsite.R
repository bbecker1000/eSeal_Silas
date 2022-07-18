#test
library("readxl")
library(dplyr)
library(lme4)
library(gamm4)
library(ggrepel)
library(gratia)

seals <- read_excel("Data/Eseal_1981-2020_BySubsite.xlsx")
names(seals)[names(seals) == "StartDate"] <- "Date"
names(seals)[names(seals) == "SiteCode"] <- "Site"
names(seals)[names(seals) == "SubSiteName"] <- "SubSite"
names(seals)[names(seals) == "MatureCode"] <- "Age"

###############################
###Exploratory Data Analysis###
###############################

# select age classes
seals <- subset(seals, Age == "PUP" | Age == "WNR" | Age == "COW")

seals_date <- strptime(seals$Date, format="%Y-%m-%d")


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


####################
######COW WORK######
####################
cows <- subset(seals, Age == "COW")
locations <- unique(seals$SubSite)


# from Condit
Date <- format(seq(as.Date("1981-01-11"), as.Date("1981-02-08"), by="days"), format="%m-%d")
Multiplier <- c(2.295, 2.107, 1.946, 1.810, 1.693, 1.593, 1.507, 1.434, 1.372, 1.319, 1.274, 1.236, 1.206, 1.181, 1.161, 1.147, 1.138, 1.133, 1.134, 1.139, 1.149, 1.165, 1.186, 1.214, 1.249, 1.292, 1.343, 1.405, 1.479)
Upper <- c(2.678, 2.427, 2.222, 2.056, 1.916, 1.797, 1.696, 1.606, 1.528, 1.463, 1.406, 1.357, 1.316, 1.283, 1.254, 1.230, 1.213, 1.203, 1.200, 1.207, 1.225, 1.253, 1.287, 1.328, 1.378, 1.427, 1.513, 1.597, 1.702)
Lower <- c(1.888, 1.762, 1.650, 1.555, 1.472, 1.401, 1.340, 1.290, 1.246, 1.210, 1.179, 1.154, 1.133, 1.116, 1.104, 1.096, 1.091, 1.089, 1.087, 1.088, 1.093, 1.102, 1.115, 1.134, 1.159, 1.190, 1.228, 1.276, 1.333)
multiplierTable <- data.frame(Date=Date, Lower=Lower, Multiplier=Multiplier, Upper=Upper)

# function to estimate the population using Condit et al.
# not sure how to vectorize so it only works for rows of a table, not a whole table
# returns the straight count if the date is out of the range Condit provided
estimate_pop <- function(row, multTbl) {
  if (row$month_day %in% multTbl$Date) {
    m <- multTbl$Multiplier[multTbl$Date == row$month_day] 
    return(m * row$Count)
  } else {
    return(row$Count)
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
  #lower <- numeric()
  #upper <- numeric()
  for (i in 1:nrow(table)) {
    row <- table[i, ]
    est <- estimate_pop(row, multTbl)
    #l <- estimate_pop_lower(row, multTbl)
    #u <- estimate_pop_upper(row, multTbl)
    vec <- c(vec, est)
    #lower <- c(lower, l)
    #upper <- c(upper, u)
  }
  table$estimate <- vec
  #table$lower <- lower
  #table$upper <- upper
  return(table)
}

cows <- estimate_pop_table(cows, multiplierTable)


# determine which seasons don't fall into the date range now given by Sarah Codde: Jan 20 to Feb 10
peakDaysPup <- format(seq(as.Date("1981-01-20"), as.Date("1981-02-10"), by="days"), format="%m-%d")
peakDaysWeaned <- format(seq(as.Date("1981-02-15"), as.Date("1981-03-05"), by="days"), format="%m-%d")

validate_dates <- function(table, peak) {
  valid <- logical()
  for (i in 1:nrow(table)) {
    row <- table[i, ]
    isit <- row$month_day %in% peak
    valid <- c(valid, isit)
  }
  table$valid_date <- valid
  return(table)
}

valid_cows = validate_dates(cows, peakDaysPup)
#if you don't want to use the condit estimates don't do the renaming below
names(valid_cows)[names(valid_cows) == "Count"] <- "Ground"
names(valid_cows)[names(valid_cows) == "estimate"] <- "Count"

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
get_max <- function(list, table, peakDays) {
  df <- data.frame(Date=as.Date(character()), Site=character(), Subsite=character(), Age=character(), Count=numeric(), season=numeric(), month_day=character(), valid_date=logical())
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

all_the_above <- function(tabl, subsite, peakDays, pup) {
  subtab <- subset(tabl, SubSite == subsite)
  val1 <- validate_dates(subtab, peakDays)
  
  val2 <- valid_seasons(val1)
  final <- get_max(val2, val1, peakDays)
  if (pup) {
    final$Pup_estimate <- final$Count * 0.97
    final$Pup_rounded <- round(final$Pup_estimate)
  }
  return(final)
}

cow_list <- list()

PR <- all_the_above(valid_cows, "PR Headlands", peakDaysPup, TRUE)
# seasons 0 to 16; invalid for 0, 1

SB <- all_the_above(valid_cows, "South Beach", peakDaysPup, TRUE)
# seasons 14-16; invalid 15

DB1 <- all_the_above(valid_cows, "Drakes Beach 1", peakDaysPup, TRUE)
# seasons 14-39; invalid none

DB2 <- all_the_above(valid_cows, "Drakes Beach 2", peakDaysPup, TRUE)
# seasons 34-39; invalid none

C1 <- all_the_above(valid_cows, "Cove 1", peakDaysPup, TRUE)
# seasons 17-39; invalid none

C2 <- all_the_above(valid_cows, "Cove 2", peakDaysPup, TRUE)
# seasons 17-39; invalid none

C3 <- all_the_above(valid_cows, "Cove 3", peakDaysPup, TRUE)
# seasons 17-39; invalid 36

C4 <- all_the_above(valid_cows, "Cove 4", peakDaysPup, TRUE)
# seasons 17, 19-21, 23-26, 30-32, 34-37; invalid 23, 26, 30, 32, 36, 37

TMC <- all_the_above(valid_cows, "Tip of Main Colony", peakDaysPup, TRUE)
# seasons 17-36; invalid 18, 22, 31, 36

LH <- all_the_above(valid_cows, "Lighthouse", peakDaysPup, TRUE)
# seasons 17-39; invalid none

LBS <- all_the_above(valid_cows, "Life Boat Station", peakDaysPup, TRUE)
# seasons 17, 19, 28-39; invalid 19

DSB <- all_the_above(valid_cows, "Dead Seal Beach", peakDaysPup, TRUE)
# seasons 17-24, 26-39; invalid 17 

LB <- all_the_above(valid_cows, "Loser Beach", peakDaysPup, TRUE)
# seasons 17-18, 21-23, 27, 32-33, 35-36, 38-39; invalid 17, 21, 23, 36, 39

MR <- all_the_above(valid_cows, "Mendoza Ranch", peakDaysPup, TRUE)
# seasons 17, 22, 31; invalid 17

NB <- all_the_above(valid_cows, "Nunnes Beach", peakDaysPup, TRUE)
# seasons 20-25, 28, 33, 35-39; invalid 21, 28, 33, 36

#CRC <- all_the_above(valid_cows, "Chimney Rock Cove", peakDaysPup, TRUE)

GC <- all_the_above(valid_cows, "Gus's Cove", peakDaysPup, TRUE)
# seasons 18-39; invalid 22

#CRCWC3 <- all_the_above(valid_cows, "Chimney Rock Cove - West", peakDaysPup, TRUE)

SLO <- all_the_above(valid_cows, "Sea Lion Overlook", peakDaysPup, TRUE)
# seasons 28, 33; invalid 33

#CRCE <- all_the_above(valid_cows, "Chimney Rock Cove - East", peakDaysPup, TRUE)
# seasons 34-39; invalid none

cow_list <- list(PR, SB, DB1, DB2, C1, C2, C3, C4, TMC, LH, LB, DSB, LB, MR, NB, GC, SLO)

pup_born_final <- PR

for (tbl in cow_list) {
  if (tbl$SubSite[1] == PR$SubSite[1]) {
    
  } else {
    pup_born_final <- rbind(pup_born_final, tbl)
  }
}

pup_born_final <- subset(pup_born_final, select=c(SubSite, season, month_day, valid_date, Pup_rounded)) #, Count, Ground))

pup <- rep(TRUE, times=nrow(pup_born_final))
pup_born_final$pup_born <- pup
names(pup_born_final)[names(pup_born_final) == "Pup_rounded"] <- "Count"


# calculate totals across locations for cows
# pass in tbales for each location and col as a string 
total_max_count <- function(lst, col) {
  vec <- numeric()
  for (i in 0:39) {
    total <- 0
    for (tb in lst) {
      val <- tb[[col]][tb$season == i]
      if (identical(val, numeric(0))) { val <- 0}
      total <- total + val
    }
    vec <- c(vec, total)
  }
  return(vec)
}

# total max count for all locations
cow_max_total <- total_max_count(cow_list, "Count") #change to "Pup_estimate" or "Pup_rounded")


###########################
######WEANED PUP WORK######
###########################

# subset data by age class
weaned <- subset(seals, Age == "PUP" | Age == "WNR") 

summed <- aggregate(weaned$Count, by=list(SubSite=weaned$SubSite, Date=weaned$Date), FUN=sum)
names(summed)[names(summed) == "x"] <- "Count"

weaned_date <- strptime(summed$Date, format="%Y-%m-%d")

summed$season <- get_season_all(weaned_date)
summed$year <- get_year_all(weaned_date)
summed$month_day <- format(summed$Date, format="%m-%d")


# just like for cows, we want to have counts from the peak days (Feb 20 to March 5)
PRw <- all_the_above(summed, "PR Headlands", peakDaysWeaned, FALSE)
# seasons 0-16; invalid 0, 1, 2

SBw <- all_the_above(summed, "South Beach", peakDaysWeaned, FALSE)
# seasons 2, 14-16; invalid 2, 14, 16

DB1w <- all_the_above(summed, "Drakes Beach 1", peakDaysWeaned, FALSE)
# seasons 14-39; invalid none

DB2w <- all_the_above(summed, "Drakes Beach 2", peakDaysWeaned, FALSE)
# seasons 34-39; invalid none

C1w <- all_the_above(summed, "Cove 1", peakDaysWeaned, FALSE)
# seasons 17-39; invalid 36

C2w <- all_the_above(summed, "Cove 2", peakDaysWeaned, FALSE)
# seasons 17-39; invalid none

C3w <- all_the_above(summed, "Cove 3", peakDaysWeaned, FALSE)
# seasons 17-39; invalid 29

C4w <- all_the_above(summed, "Cove 4", peakDaysWeaned, FALSE)
# seasons 17-28, 30-35, 37; invalid 17

TMCw <- all_the_above(summed, "Tip of Main Colony", peakDaysWeaned, FALSE)
# seasons 19-21, 23-24, 26-34, 36; invalid 19, 20, 23, 26, 27, 29, 30, 36

LHw <- all_the_above(summed, "Lighthouse", peakDaysWeaned, FALSE)
# seasons 17-39; invalid none

LBSw <- all_the_above(summed, "Life Boat Station", peakDaysWeaned, FALSE)
# seasons 17, 18, 24-26, 28-31, 33-39; invalid 18

DSBw <- all_the_above(summed, "Dead Seal Beach", peakDaysWeaned, FALSE)
# season 17-22, 24, 26-39; invalid 20

LBw <- all_the_above(summed, "Loser Beach", peakDaysWeaned, FALSE)
# seasons 17, 24, 27-28, 32, 34, 38; invalid 17, 27

MRw <- all_the_above(summed, "Mendoza Ranch", peakDaysWeaned, FALSE)
# seasons 17, 22, 25-26, 31; invalid 22, 25-26, 31

NBw <- all_the_above(summed, "Nunnes Beach", peakDaysWeaned, FALSE)
# seasons 17, 21-25, 28, 31, 34-39; invalid 21, 28

#CRCw <- all_the_above(summed, "Chimney Rock Cove", peakDaysWeaned, FALSE)

GCw <- all_the_above(summed, "Gus's Cove", peakDaysWeaned, FALSE)
# seasons 18-19, 21, 23-39; invalid none

#CRCWC3w <- all_the_above(summed, "Chimney Rock Cove - West", peakDaysWeaned, FALSE)

# the call below will not work correctly because Sea Lion Overlook only has two observations in total, seasons 28 and 33, of cows
#SLOw <- all_the_above(summed, "Sea Lion Overlook", peakDaysWeaned, FALSE)
# seasons 28, 33; invalid 28, 33

#CRCEw <- all_the_above(summed, "Chimney Rock Cove - East", peakDaysWeaned, FALSE)



weaned_list <- list(PRw, SBw, DB1w, DB2w, C1w, C2w, C3w, C4w, TMCw, LHw, LBw, DSBw, LBw, MRw, NBw, GCw)

survived_final <- PRw

for (tbl in weaned_list) {
  if (tbl$SubSite[1] == PRw$SubSite[1]) {
    
  } else {
    survived_final <- rbind(survived_final, tbl)
  }
}

survived_final <- subset(survived_final, select=c(SubSite, season, month_day, valid_date, Count))

survived <- rep(FALSE, times=nrow(survived_final))
survived_final$pup_born <- survived

survived <- total_max_count(weaned_list, "Count")

#think about matching up the two counts, use survived_final and pup_born_final

final <- rbind(pup_born_final, survived_final)




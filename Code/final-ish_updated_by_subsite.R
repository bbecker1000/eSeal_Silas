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
  #vec <- vector("list", length(lst))
  vec <- double()
  for (i in 1:length(lst)) {
    #vec[[i]] <- get_season(lst[[i]])
    vec <- c(vec, get_season(lst[[i]]))
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

if (false) {

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

#LBS <- all_the_above(valid_cows, "Life Boat Station", peakDaysPup, TRUE)
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

#GC <- all_the_above(valid_cows, "Gus's Cove", peakDaysPup, TRUE)
# seasons 18-39; invalid 22

#CRCWC3 <- all_the_above(valid_cows, "Chimney Rock Cove - West", peakDaysPup, TRUE)

#SLO <- all_the_above(valid_cows, "Sea Lion Overlook", peakDaysPup, TRUE)
# seasons 28, 33; invalid 33

#CRCE <- all_the_above(valid_cows, "Chimney Rock Cove - East", peakDaysPup, TRUE)
# seasons 34-39; invalid none

cow_list <- list(PR, SB, DB1, DB2, C1, C2, C3, C4, TMC, LH, DSB, LB, MR, NB)

pup_born_final <- PR

for (tbl in cow_list) {
  if (tbl$SubSite[1] == PR$SubSite[1]) {
    
  } else {
    pup_born_final <- rbind(pup_born_final, tbl)
  }
}


pup_born_final <- subset(pup_born_final, select=c(Site, SubSite, season, month_day, valid_date, Pup_rounded)) #, Count, Ground))

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

summed <- aggregate(weaned$Count, by=list(Site=weaned$Site, SubSite=weaned$SubSite, Date=weaned$Date), FUN=sum)
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

#LBSw <- all_the_above(summed, "Life Boat Station", peakDaysWeaned, FALSE)
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

#GCw <- all_the_above(summed, "Gus's Cove", peakDaysWeaned, FALSE)
# seasons 18-19, 21, 23-39; invalid none

#CRCWC3w <- all_the_above(summed, "Chimney Rock Cove - West", peakDaysWeaned, FALSE)

# the call below will not work correctly because Sea Lion Overlook only has two observations in total, seasons 28 and 33, of cows
#SLOw <- all_the_above(summed, "Sea Lion Overlook", peakDaysWeaned, FALSE)
# seasons 28, 33; invalid 28, 33

#CRCEw <- all_the_above(summed, "Chimney Rock Cove - East", peakDaysWeaned, FALSE)



weaned_list <- list(PRw, SBw, DB1w, DB2w, C1w, C2w, C3w, C4w, TMCw, LHw, DSBw, LBw, MRw, NBw)

survived_final <- PRw

for (tbl in weaned_list) {
  if (tbl$SubSite[1] == PRw$SubSite[1]) {
    
  } else {
    survived_final <- rbind(survived_final, tbl)
  }
}

survived_final <- subset(survived_final, select=c(Site, SubSite, season, month_day, valid_date, Count))

survived <- rep(FALSE, times=nrow(survived_final))
survived_final$pup_born <- survived

survived <- total_max_count(weaned_list, "Count")

#think about matching up the two counts, use survived_final and pup_born_final

final <- rbind(pup_born_final, survived_final)


PRfinal <- subset(final, SubSite == "PR Headlands")
# INTRINSICALLY PROBLEMATIC 
# season 0 has four counts Jan 6 to 8 (3 cow counts and 1 pup count)
# season 1 has 7 counts from Jan 5 to 11 (mostly cow, 2 pup counts)
# season 2 has 7 counts from 1/19 to 1/24 (4 cow counts, 3 pup counts)

# GOOD TO GO
# 3-12, 14-16

# COME BACK TO ME
# season 13 has survived count > born

##### South Beach ####

# INTRINSICALLY PROBLEMATIC 
# season 2 has only 1 count 
# season 14 only has counts on Feb 6 and 14, code above defaults to both counts on the 6th, change below to make pup_born = FALSE row be for 14th
final$month_day[final$season == 14 & final$pup_born == FALSE & final$SubSite == "South Beach"] <- '02-14'
final$Count[final$season == 14 & final$pup_born == FALSE & final$SubSite == "South Beach"] <- 2 # 2 pups + 0 WNR
# season 16 only has counts before the survived peak dates (1 pup + 1 wnr on 2/9 and 1 pup on 2/13), change to 2/13 below
final$month_day[final$season == 16 & final$pup_born == FALSE & final$SubSite == "South Beach"] <- '02-13'
final$Count[final$season == 16 & final$pup_born == FALSE & final$SubSite == "South Beach"] <- 1 # 1 pup

# SLIGHTLY PROBLEMATIC BUT LEAVING AS IS
# season 15 only has counts before the pup born peak dates, defaults to 1/17
SBfinal <- subset(final, SubSite == "South Beach")

DB1final <- subset(final, SubSite == "Drakes Beach 1")
# GOOD TO GO
# 14, 15, 17-25, 27-39

# COME BACK TO ME
# seasons 16, 26 has survived count > born

DB2final <- subset(final, SubSite == "Drakes Beach 2")
# GOOD TO GO
# 34, 36, 37, 38, 39

# COME BACK TO ME
# season 35 has survived count > born

C1final <- subset(final, SubSite == "Cove 1")
# COME BACK TO ME
# season 17 has 148 born 1 survived
# season 18, 31, 32, 37, 38, 39 has survived > born
# season 28 has a big drop 
# season 29 and 36 has small numbers and survived > born
# season 35 has big drop


# GOOD TO GO
# 19-27, 30, 33

# INTRINSICALLY PROBLEMATIC 
# season 36 doesn't have any pup/weaned counts after 1/19 and only 1 cow count during peakDaysPup

C2final <- subset(final, SubSite == "Cove 2")
# COME BACK TO ME
# season 17 goes from 216 to 16
# season 29, 36 has fewer than other seasons

# GOOD TO GO
# 18-18, 30-35, 37-39

C3final <- subset(final, SubSite == "Cove 3")
# GOOD TO GO 
# 17, 19, 20, 22, 31, 35, 39

# COME BACK TO ME
# season 18, 21, 23, 24, 25, 26, 27, 28, 30, 32, 33, 34, 37, 38 has survived > born

# INTRINSICALLY PROBLEMATIC 
# season 29 doesn't have any counts after 2/8
# season 36 only has 1 count (1 weaner on 1/24) during peakDaysPup. 
#currently defaults to cow count-estimate on 1/19 (one day before suggested window by Sarah C.).. also a pup count for that day that's lower


###COVE 4######

#C4final <- subset(final, SubSite == "Cove 4")
# GOOD TO GO
# 35 (but does have one 1 for each count)

# COME BACK TO ME
# season 19, 20, 21, 24, 25, 31, 34 has survived > born

# INTRINSICALLY PROBLEMATIC 
# season 17 only has counts up until 2/11
# season 18 has counts start on 2/23 and no cow counts (only pup and wnr)
# season 22 has counts start on 2/21 and no cow counts (only wnr)
# season 23 has counts start on 2/11 (one day after the end of pup born interval; 2 cows, 1 pup)
# season 26 has no counts during peakDaysPup (and all cow counts < wnr counts)
# season 27 has no cow counts and starts on 2/20
# season 28 has no cow counts; 4 counts on 1/30 (1 pup), 2/18 (1 wnr), 2/24 (3 wnr), 3/13 (4 wnr)
# season 30 has no cow counts during peakDaysPup (1 wnr on 2/7) and counts > @ end
# season 32 has no counts during peakDaysPup and counts > @ end
# season 33 has no cow counts but does have wnr counts during peakDaysPup, use below(??)
c4s33 <- data.frame("PR Headlands", "Cove 4", 33, "02_20", TRUE, 21, TRUE)
names(c4s33) <- c("Site", "SubSite", "season", "month_day", "valid_date", "Count", "pup_born")
final <- rbind(final, c4s33)
# season 36 only has one observation on 12/19
# season 37 has no observations during peakDaysPup (1 cow on 1/16 which is what it's defaulting to)
C4final <- subset(final, SubSite == "Cove 4")


TMCfinal <- subset(final, SubSite == "Tip of Main Colony")
# INTRINSICALLY PROBLEMATIC 
# season 17, 18, 22, 25 has no pup/wnr counts (only cow)
# season 19 has only 1 pup count on 1/15 (before peakDaysPup and peakDaysWeaned) and cow count defaults to 1/22 but has higher counts before peakDaysPup
# season 20 has survived count default to the only existing count of wnr on 3/12 (after peakDaysWeaned).. probably ok to leave
# season 23 has only 1 pup count on 1/6 (before peakDaysPup and peakDaysWeaned)
# season 26 has no counts during peakDaysWeaned, but 31 weaners on 3/15 > pups born count (4)
# season 27 has no counts during peakDaysWeaned, defaults survived to before pups born
# season 29 has no counts after 1/22
# season 30 has no counts during peakDaysWeaned, but 8 on 3/8 > pups born count (3)
# season 36 has 3 observations: 5 cows on 1/5, 1 pup on 1/5, and 1 cow on 3/6

# GOOD TO GO
# 21, 24, 28, 32, 33, 34, 
# season 31 has no counts during peakDaysPup, defaults to 1/19.. ok to leave

LHfinal <- subset(final, SubSite == "Lighthouse")
# GOOD TO GO (all)
# 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39

### LOSER BEACH ###

#COME BACK TO ME
# 17 has survived > born and both dates are outside of peak; born = 2/17 and survived = 3/15
# season 27 has survived outside of peakDaysWeaned bc only 2 observations total (1/28 and 3/12)
# season 28 has 2 observations: 2/05 and 3/04 of wnrs
lbs28 <- data.frame("PR Headlands", "Loser Beach", 28, "02_05", TRUE, 2, TRUE)
names(lbs28) <- c("Site", "SubSite", "season", "month_day", "valid_date", "Count", "pup_born")
final <- rbind(final, lbs28)
# season 38 has survived > born

# GOOD TO GO
# season 32

# INTRINSICALLY PROBLEMATIC 
# season 18, 21, 22, 23, 36 has only 1 observation of 1 cow
# season 24 has only 2 observations of wnrs
# season 33 only has cow counts in decemeber and jan
# season 34 only has 1 observation (of wnr on 2/24)
# season 35 only has 1 observation (of cow on 2/9)
# season 39 only has 1 observation (of cow on 1/7)

LBfinal <- subset(final, SubSite == "Loser Beach")

### DEAD SEAL BEACH ###

# COME BACK TO ME
# season 17 replace cow count for born outside of peakDaysPup with pup count from 2/3 (inside of peakDaysPup)
dsbs17 <- data.frame("PR Headlands", "Dead Seal Beach", 17, "02_03", TRUE, 2, TRUE)
names(dsbs17) <- c("Site", "SubSite", "season", "month_day", "valid_date", "Count", "pup_born")
final <- rbind(final, dsbs17)
# season 17 not sure what to do about survived because wnr count during peakDaysWeaned (3/2) = 1 but on 3/13 there's 2 wnrs
# season 21, 28, 31 has survived > born
# season 35 has 22 born 3 survived; count on 3/18 is for 16 wnrs though... should I replace the default survived count with this?


# GOOD TO GO 
# season 18, 19, 22, 26, 27, 29, 30, 32, 33, 34, 36, 37, 38, 39

# INTRINSICALLY PROBLEMATIC 
# season 24 only has cow counts
# season 20 has no counts between 1/29 and 3/16 (survived currently defaults to before born which is 1 on 1/15 so change to the 3/16 count of 1 wnr below)
final$month_day[final$season == 20 & final$pup_born == FALSE & final$SubSite == "Dead Seal Beach"] <- '03-16'

DSBfinal <- subset(final, SubSite == "Dead Seal Beach")

MRfinal <- subset(final, SubSite == "Mendoza Ranch")
# INTRINSICALLY PROBLEMATIC 
# season 17 has counts start on 2/17
# season 22 has no counts after 2/14
# season 31 only has counts on 1/27
# season 25 only has 1 count of 1 pup on 2/7
# season 26 only has 1 count of 2 wnr on 3/15

NBfinal <- subset(final, SubSite == "Nunnes Beach")
# INTRINSICALLY PROBLEMATIC 
# season 17 only has wnr counts 
# season 20 only has 1 observation (cow, 1/22)
# season 21 only has observations on 1/10 and 1/14
# season 28 doesn't have counts after 2/5
# season 31 only has 1 observation (3 wnr, 2/28)
# season 33 only has 1 observation (1 cow, 1/13)
# season 34 only has 1 observation (1 wnr, 2/17)

# GOOD TO GO
# season 22, 23, 36, 38, 39

# COME BACK TO ME
# season 24, 25, 35, 37 has survived > born

calculate_season_totals_by_site <- function(tbl) {
  born <- numeric()
  survived <- numeric()
  season <- numeric()
  site_list <- c("PR Headlands", "South Beach", "Drakes Beach")
  site <- character()
  for (s in site_list) {
    for (year in 0:39) {
      year_site_born <- subset(tbl, season == year & Site == s & pup_born == TRUE)
      year_site_survived <- subset(tbl, season == year & Site == s & pup_born == FALSE)
      if (nrow(year_site_survived) > 0 & nrow(year_site_born) > 0) {
        born <- c(born, sum(year_site_born$Count))
        survived <- c(survived, sum(year_site_survived$Count))
        season <- c(season, year)
        site <- c(site, s)
      }
    }
  }
  ratio <- survived / born
  df <- data.frame(season, site, born, survived, ratio)
  return(df)
}

site_ratios <- calculate_season_totals_by_site(final)

p0 <- ggplot(site_ratios, aes(season, ratio)) + geom_point() + geom_line()
p0 + facet_grid(rows = vars(site)) + geom_hline(yintercept = 1, color = "red") + geom_hline(yintercept = 0.5, color = "orange")


problematic_site_ratios <- subset(site_ratios, ratio > 1)
View(problematic_site_ratios)
#PR Headlands, season 13 born defaults to 2/9 adjusted cow count of 149 BUT we also have 138 pups and 74 wnrs for a total of 212 that day, could switch to this BUT it would still have ratio > 1
# sarah will review
# PR Headlands, season 25 has born = 523 and survived = 535
View(subset(final, Site == "PR Headlands" & season == 25)) # go back into the born counts and use pup + wnr instead of adjusted cow counts? Coves 3, 4 gain, sarah said don't worry 
# PR Headlands, season 26 has born = 462 and survived = 519
View(subset(final, Site == "PR Headlands" & season == 26)) # go back into the born counts and use pup + wnr instead of adjusted cow counts? Coves 3, 4 and TMC gain, 
#differences in survey methods (final count actually on the ground BUT previous counts are from above).. should we exculde any beach counts?
# South Beach season 17 has born = 11 and survived = 17, don't worry about this one
View(subset(final, Site == "South Beach" & season == 17)) # LH and MR in born and LH + MR + NB in survived
View(subset(seals, SubSite == "Nunnes Beach" & season == 17)) # no NB counts before 2/22
View(subset(seals, SubSite == "Mendoza Ranch" & season == 17)) # no MR counts before 2/17
# South Beach season 25 has born = 38 and survived = 43, similar to season 17 problems
View(subset(final, Site == "South Beach" & season == 25))
View(subset(seals, SubSite == "Nunnes Beach" & season == 25)) #] # during peakDaysPup all cow counts = pup + wnr counts and during peakDaysWeaned we end up with an observation of 12 wnr
View(subset(seals, SubSite == "Mendoza Ranch" & season == 25)) # only has 1 observation of a pup on 2/7 (right now counting towards survived NOT born, should i fix?)
# Drakes Beach season 16 has born = 11 and survived = 12, this is really just the way the numbers shake out
View(subset(final, Site == "Drakes Beach" & season == 16))
# Drakes Beach season 26 has born = 162 and survived = 171, this is really just the way the numbers shake out because there are 171 wnrs observed 
View(subset(final, Site == "Drakes Beach" & season == 26))
View(subset(seals, SubSite == "Drakes Beach 1" & season == 26))

#what to do about the close higher survived than born: exclude or set born = survived?


good_site_ratios <- subset(site_ratios, ratio <= 1)
good_site_ratios

p <- ggplot(good_site_ratios, aes(season, ratio)) + geom_point() + geom_line()
p + facet_grid(rows = vars(site))

}

###################################
### TRY 2 with only 1 peak day ####
###################################

cows_summed <- aggregate(cows$estimate, by=list(Site=cows$Site, Date=cows$Date, season=cows$season, month_day=cows$month_day), FUN=sum)
names(cows_summed)[names(cows_summed) == "x"] <- "Count"
cows_summed$Count <- round(cows_summed$Count * 0.97)

PR_sum <- subset(cows_summed, Site == "PR Headlands")
DB_sum <- subset(cows_summed, Site == "Drakes Beach")
SB_sum <- subset(cows_summed, Site == "South Beach")

v1 <- validate_dates(PR_sum, peakDaysPup)
v2 <- valid_seasons(v1)
###born counts 
born <- get_max(v2, v1, peakDaysPup)


v1 <-validate_dates(DB_sum, peakDaysPup)
v2 <- valid_seasons(v1)
###born counts 
born <- rbind(born, get_max(v2, v1, peakDaysPup))

v1 <- validate_dates(SB_sum, peakDaysPup)
v2 <- valid_seasons(v1)
###born counts 
born <- rbind(born, get_max(v2, v1, peakDaysPup))

survived_summed <- aggregate(weaned$Count, by=list(Site=weaned$Site, Date=weaned$Date, season=weaned$season, month_day=weaned$month_day), FUN=sum)
names(survived_summed)[names(survived_summed) == "x"] <- "Count"

PR_sum2 <- subset(survived_summed, Site == "PR Headlands")
DB_sum2 <- subset(survived_summed, Site == "Drakes Beach")
SB_sum2 <- subset(survived_summed, Site == "South Beach")

v1 <- validate_dates(PR_sum2, peakDaysWeaned)
v2 <- valid_seasons(v1)
###born counts 
survived <- get_max(v2, v1, peakDaysWeaned)


v1 <-validate_dates(DB_sum2, peakDaysWeaned)
v2 <- valid_seasons(v1)
###born counts 
survived <- rbind(survived, get_max(v2, v1, peakDaysWeaned))

v1 <- validate_dates(SB_sum2, peakDaysWeaned)
v2 <- valid_seasons(v1)
###born counts 
survived <- rbind(survived, get_max(v2, v1, peakDaysWeaned))

falsecol <- rep(FALSE, times=nrow(survived))
truecol <- rep(TRUE, times=nrow(born))
born$pup_born <- truecol
survived$pup_born <- falsecol

final2 <- rbind(born, survived)

site_ratios2 <- calculate_season_totals_by_site(final2)

p0 <- ggplot(site_ratios2, aes(season, ratio)) + geom_point() + geom_line()
p0 + facet_grid(rows = vars(site)) + geom_hline(yintercept = 1, color = "red")# + geom_hline(yintercept = 0.5, color = "orange")


problematic_site_ratios2 <- subset(site_ratios2, ratio > 1)
View(problematic_site_ratios2)


good_site_ratios2 <- subset(site_ratios2, ratio <= 1)
good_site_ratios2

p <- ggplot(good_site_ratios2, aes(season, ratio)) + geom_point() + geom_line()
p + facet_grid(rows = vars(site))

born <- born[order(born$season),]

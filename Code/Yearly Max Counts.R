#######################################################
######DONR USE THIS INSTEAD USE OTHER AGE CLASSES######
#######################################################


# Notes about the data:
# (directly from spreadsheet)
# Feb/March 1983 Access was restricted because road was washed out due to  El Nino storms)
# Did not include partial surveys completed during the 2018/2019 government shutdown
# (from Silas)
# In this analysis I ignore the seals labeled as "UNK"/unknown because it doesn't make sense to take the max of this age class

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

# function that returns the season for one date
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

# subset data by age class
cows <- subset(seals, Age == "COW")
bulls <- subset(seals, Age == "BULL-SA4")
imm <- subset(seals, Age == "IMM")
#pups <- subset(seals, Age == "PUP" | Age == "EPUP") 
#pupWnr <- subset(seals, Age == "PUP" | Age == "EPUP" | Age == "WNR")
sa <- subset(seals, Age == "SA1-3")
wnr <- subset(seals, Age == "WNR")
yearling <- subset(seals, Age == "YRLNG")

# i do nothing with unknowns, they occur in seasons 1, 2, and 3 with 55 total age-unidentified seals
# it doesn't make sense to take the max count for each season of the unknowns
unk <- subset(seals, Age == "UNK")

# function that returns a list of tables for each location, can pass in the general seals table or subsets based on age class
by_location <- function(table) {
  locations <- list("PR Headlands", "Drakes Beach", "South Beach")
  tables <- vector("list", 3)
  i <- 1
  for (x in locations) {                                             
    tbl <- subset(table, Location == x)
    tables[[i]] <- tbl
    i <- i + 1
  }
  return(tables)
}

# function that takes a list of tables (like that returned by by_location) and gets the maximum count for each season
max_count <- function(listOfT) {
  vec = vector("list", 3)
  i <- 1
  for (x in listOfT) {
    maxi <- x %>% group_by(season) %>% slice(which.max(Count))
    vec[[i]] <- maxi
    i <- i + 1
  }
  return(vec)
}

# function that takes a list of tables with max seasonal counts (like that returned by max_count) and plots max count over time
# this draws line graphs so for SA DB where you are missing seasons 2-14 it just draws a straight line between the data points it has
plot_counts_line <- function(listOfT, age) {
  for (x in listOfT) {
    title <- paste ("Maximum", age, "Count\n", x$Location[1])
    plot(x$season, x$Count, type='l', main=title, xlab="Season", ylab='Maximum Count')
  }
}

# does the same as plot_counts but with points and not lines
plot_counts_points <- function(listOfT, age) {
  for (x in listOfT) {
    title <- paste ("Maximum", age, "Count\n", x$Location[1])
    plot(x$season, x$Count, main=title, xlab="Season", ylab='Maximum Count', pch=16)
  }
}

# does the same as plot_counts but with points and lines
plot_counts_lp <- function(listOfT, age) {
  for (x in listOfT) {
    title <- paste ("Maximum", age, "Count\n", x$Location[1])
    plot(x$season, x$Count, main=title, xlab="Season", ylab='Maximum Count', pch=15)
    lines(x$season, x$Count)
  }
}

#note: there are some counts for age classes other than cows at DB and SB before season 14

# get each location separated out for cows and find max for each season for each
cows_all <- by_location(cows)
cows_max_all <- max_count(cows_all)
plot_counts_lp(cows_max_all, "Cow")

# do the same for all other age classes
bulls_all <- by_location(bulls)
bulls_max_all <- max_count(bulls_all)
plot_counts_lp(bulls_max_all, "Bull")

imm_all <- by_location(imm)
imm_max_all <- max_count(imm_all)
plot_counts_lp(imm_max_all, "Immature")

#pups_all <- by_location(pups)
#pups_max_all <- max_count(pups_all)
#plot_counts_lp(pups_max_all, "Pup")

sa_all <- by_location(sa)
sa_max_all <- max_count(sa_all)
plot_counts_lp(sa_max_all, "Sub-adult (1-3)")

wnr_all <- by_location(wnr)
wnr_max_all <- max_count(wnr_all)
plot_counts_lp(wnr_max_all, "Weaner")

yrlg_all <- by_location(yearling)
yrlg_max_all <- max_count(yrlg_all)
plot_counts_lp(yrlg_max_all, "Yearling")

# function that takes in a list of tables (like that returned by max_count) and turns a list of seasonal total counts
# each table may not have all seasons
total_max_count <- function(listOfT) {
  vec <- vector("list", 40)
  for (i in 0:39) {
    a <- listOfT[[1]]$Count[listOfT[[1]]$season == i]
    if (identical(a, numeric(0))) { a <- 0}
    b <- listOfT[[2]]$Count[listOfT[[2]]$season == i]
    if (identical(b, numeric(0))) { b <- 0}
    c <- listOfT[[3]]$Count[listOfT[[3]]$season == i]
    if (identical(c, numeric(0))) { c <- 0}
    vec[[i+1]] <- a + b + c # lists aren't zero indexed here
  }
  return(vec)
}

# find seasonal totals for age classes separated out
cows_total <- total_max_count(cows_max_all)
bulls_total <- total_max_count(bulls_max_all)
imm_total <- total_max_count(imm_max_all)
sa_total <- total_max_count(sa_max_all)
wnr_total <- total_max_count(wnr_max_all)
yrlg_total <- total_max_count(yrlg_max_all)

# now for pups
pups_only <- subset(seals, Age == "PUP")
epups <- subset(seals, Age == "EPUP")
# i visually inspected epups and pups_only and didn't see overlap in date and location 
# i season 39 doesn't have any pups (only epups) so i only looked at season 38

pups <- subset(seals, Age == "PUP" | Age == "EPUP") 
pups_all <- by_location(pups)
pups_max_all <- max_count(pups_all)
plot_counts_lp(pups_max_all, "Pup")
pups_total <- total_max_count(pups_max_all)

#plot seasonal totals for age classes separated out
plot(0:39, cows_total, main="Total Cow Counts for All Locations", xlab="Seasons", ylab="Count", pch=15)
lines(0:39, cows_total)

plot(0:39, bulls_total, main="Total Bull Counts for All Locations", xlab="Seasons", ylab="Count", pch=15)
lines(0:39, bulls_total)

plot(0:39, imm_total, main="Total Immature Counts for All Locations", xlab="Seasons", ylab="Count", pch=15)
lines(0:39, imm_total)

plot(0:39, sa_total, main="Total Sub-Adult (1-3) Counts for All Locations", xlab="Seasons", ylab="Count", pch=15)
lines(0:39, sa_total)

plot(0:39, wnr_total, main="Total Weaner Counts for All Locations", xlab="Seasons", ylab="Count", pch=15)
lines(0:39, wnr_total)

plot(0:39, yrlg_total, main="Total Yearling Counts for All Locations", xlab="Seasons", ylab="Count", pch=15)
lines(0:39, yrlg_total)

plot(0:39, pups_total, main="Total Pup Counts for All Locations", xlab="Seasons", ylab="Count", pch=15)
lines(0:39, pups_total)
# find seasonal totals for all seals counted
seasonal_total <- vector("list", 40)
for (i in 1:40) {
  total <- cows_total[[i]] + bulls_total[[i]] + imm_total[[i]] + sa_total[[i]] + wnr_total[[i]] + yrlg_total[[i]] + pups_total[[i]]
  seasonal_total[i] <- total
}

plot(0:39, seasonal_total, main="Total Seasonal Counts for All Age Classes and Locations", xlab="Season", ylab="Count", pch=15)
lines(0:39, seasonal_total)


# I didn't combine pups and weaners yet should I do the max counts for pup and wnr for a season and add together
# or add together the daily counts for pup and wnr and take max daily? (first is going to be a lot easier)
#pupWnr <- subset(seals, Age == "PUP" | Age == "EPUP" | Age == "WNR")








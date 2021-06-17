# library("readxl")
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

# function that gets the year corresponding to the season, useful for plotting
get_year <- function(date) {
  if (date$mon > 6) { 
    return(date$year+1901)
  } else {
    return(date$year+1900)
  }
}

get_year_all <- function(lst) {
  vec <- vector("list", length(lst))
  for (i in 1:length(lst)) {
    vec[[i]] <- get_year(lst[[i]])
  }
  return(vec)
}

# now that we have the seasons and years we can add them to the tables
seals$season <- get_season_all(seals_date)
seals$year <- get_year_all(seals_date)

# subset data by age class
# i visually inspected epups and pups_only and didn't see overlap in date and location so change all "EPUPS" to "PUPS"
pups <- subset(seals, Age == "PUP" | Age == "EPUP") 
pups$Age[pups$Age == "EPUP"] <- "PUP"

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
plot_counts_lp <- function(listOfT, age) {
  for (x in listOfT) {
    title <- paste ("Maximum", age, "Count\n", x$Location[1])
    plot(x$year, x$Count, main=title, xlab="Year", ylab='Maximum Count', pch=15)
    lines(x$year, x$Count)
  }
}

# function that takes in a list of tables (like that returned by max_count) and returns a list of seasonal total counts
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

pups_all <- by_location(pups)
pups_max_all <- max_count(pups_all)
PRpups <- pups_max_all[[1]]
DBpups <- pups_max_all[[2]]
SBpups3 <- pups_max_all[[3]]

plot_counts_lp(pups_max_all, "Pup")
pups_total <- total_max_count(pups_max_all)

plot(1981:2020, pups_total, main="Total Pup Counts for All Locations", xlab="Year", ylab="Count", pch=15)
lines(1981:2020, pups_total)

# do we need to worry about a specific date range here???

# later on combine pup and wnr (accurate through 2014-2015)
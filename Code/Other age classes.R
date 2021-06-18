# other than cows and pups, elephant seals move around so to get an accurate count each season
# need to take one peak day and use the count for that day across all locations

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

# now that we have the seasons and years we can add them to the tables
seals$season <- get_season_all(seals_date)
colnames(seals)[5] <- "Survey_Type"

# subset data by age class
bulls <- subset(seals, Age == "BULL-SA4")
imm <- subset(seals, Age == "IMM")
sa <- subset(seals, Age == "SA1-3")
wnr <- subset(seals, Age == "WNR")
yrlg <- subset(seals, Age == "YRLNG")

# need to determine which seasons have multiple sites and for those seasons combine counts by date

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
  df <- data.frame(Date=as.Date(character()), Count=numeric(), season=numeric(), year=numeric())
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
      r <- data.frame(dates[j], sum, szn, yr)
      names(r) <- c("Date", "Count", "season", "year")
      df <- rbind(df, r)
    }
  }
  return(df)
}

# x is a table, age is an age class, creates a plot with lines and markers for each data point
plot_counts_lp <- function(x, age) {
  title <- paste ("Maximum", age, "Count\n")
  plot(x$year, x$Count, main=title, xlab="Year", ylab='Maximum Count', pch=15)
  lines(x$year, x$Count)
}

bull_locations_by_szn <- which_seasons(bulls)
bulls_all <- combine_counts(bull_locations_by_szn, bulls)
bulls_all_max_count <- bulls_all %>% group_by(season) %>% slice(which.max(Count))
#bulls_all_max_count <- arrange(bulls_all_max_count, season)
plot_counts_lp(bulls_all_max_count, "Bull")

imm_locations_by_szn <- which_seasons(imm)
imm_all <- combine_counts(imm_locations_by_szn, imm)
imm_all_max_count <- imm_all %>% group_by(season) %>% slice(which.max(Count))
plot_counts_lp(imm_all_max_count, "Immature")

sa_locations_by_szn <- which_seasons(sa)
sa_all <- combine_counts(sa_locations_by_szn, sa)
sa_all_max_count <- sa_all %>% group_by(season) %>% slice(which.max(Count))
plot_counts_lp(sa_all_max_count, "Sub-Adult 1-3")

wnr_locations_by_szn <- which_seasons(wnr)
wnr_all <- combine_counts(wnr_locations_by_szn, wnr)
wnr_all_max_count <- wnr_all %>% group_by(season) %>% slice(which.max(Count))
plot_counts_lp(wnr_all_max_count, "Weaner")

yrlg_locations_by_szn <- which_seasons(yrlg)
yrlg_all <- combine_counts(yrlg_locations_by_szn, yrlg)
yrlg_all_max_count <- yrlg_all %>% group_by(season) %>% slice(which.max(Count))
plot_counts_lp(yrlg_all_max_count, "Yearling")

# cow estimates and pup counts can be found in a different file

# now to get a population estimate by combining seal counts on the same day regardless of age class and site 
seals_locations <- which_seasons(seals)
seals_all <- combine_counts(seals_locations, seals)
seals_all_max_count <- seals_all %>% group_by(season) %>% slice(which.max(Count))
plot_counts_lp(seals_all_max_count, "Seal")

# something weird going on in 1995

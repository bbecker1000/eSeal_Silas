#test
library("readxl")
library(dplyr)
library(lme4)
library(gamm4)
library(ggrepel)
library(gratia)
library(tidyverse)

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

# subset data by age class
weaned <- subset(seals, Age == "PUP" | Age == "WNR") 

summed <- aggregate(weaned$Count, by=list(Site=weaned$Site, SubSite=weaned$SubSite, Date=weaned$Date), FUN=sum)
names(summed)[names(summed) == "x"] <- "Count"

weaned_date <- strptime(summed$Date, format="%Y-%m-%d")

summed$season <- get_season_all(weaned_date)
summed$year <- get_year_all(weaned_date)
summed$month_day <- format(summed$Date, format="%m-%d")


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

# Fix some problematic ratios under the guidance of Sarah Codde, copied below:
# PR 1994: I think we might need to remove that year since we're missing surveys from the peak of the pupping.
# (Silas comment) adjusted cow count on 1/14 would be 248 (greater than the 227 survived).currently defaults to 149 on 2/9 which is outside of condit's range
# (Silas comment cont.) if we used the 2/9 count * 2/8 Condit correction factor * 0.97 we would have 221 as the adjusted cow count which is still smaller than survived
# (Silas comment cont.) 154 * 1.479 * 0.97



# SB 1997 - I looked back at the original data for that year and there is a count on 1/13 that shows SB had 2 cows. 
# That count wasn't included in the final data I gave you because that day had a partial survey at Drakes Beach, 
# and I only included data that had full surveys of all sites. So I think we should use born = survived. 

born$Count[born$Site == "South Beach" & born$season == 16] <- 2
born$month_day[born$Site == "South Beach" & born$season == 16] <- "01-13"
born$valid_date[born$Site == "South Beach" & born$season == 16] <- FALSE

# SB 1998 - this one is tricky. This is a big El Nino year and PRH got hit hard, so it's possible that weaned pups from 
# PRH got washed out and they landed back on SB. The other issue is that I looked through all of the original data and
# there was a harem at Mendoza for the first time ever, but the observers didn't notice it until 2/17. So that is probably 
# the main reason why there is a low born count vs a high survived count. I'm not sure how we want to handle this one. 
# We definitely need to keep this year (at least for DB and PRH) since it's such an important year for mortality due to the El Nino. 

# DB 1997 - Looking back at all the counts, there is an early weaned pup so that means the cow of that pup left before the peak. 
# So there really was 12 cows that gave birth that year and all the pups survived. 

born$Count[born$Site == "Drakes Beach" & born$season == 16] <- 12

#remove season 13 PR Headlands
rownames(born) <- 1:nrow(born)    # Assign sequence to row names

born <- born[-c(14),] 
survived <- survived[-c(29),]


#remove season 17 SB
born <- born[-c(25),]
survived <- survived[-c(70),]

final2 <- rbind(born, survived)

site_ratios2 <- calculate_season_totals_by_site(final2)

p0 <- ggplot(site_ratios2, aes(season, ratio)) + geom_point() + geom_line()
p0 + facet_grid(rows = vars(site)) + geom_hline(yintercept = 1, color = "red")# + geom_hline(yintercept = 0.5, color = "orange")


problematic_site_ratios2 <- subset(site_ratios2, ratio > 1)
View(problematic_site_ratios2)


good_site_ratios2 <- subset(site_ratios2, ratio <= 1)
good_site_ratios2$year <- good_site_ratios2$season + 1981
good_site_ratios2

p <- ggplot(good_site_ratios2, aes(year, ratio)) + geom_point() + geom_line()
p + facet_grid(rows = vars(site))


###read in NPGO
npgo <- read.table("Data/npgo.txt", skip=1)
names(npgo) <- c("year", "month", "index")

##read in wave data
wave <- data.frame(year=numeric(), Jmean=numeric(), Fmean=numeric(), Jmax=numeric(), Fmax=numeric())

for (i in 83:98) {
  file = paste("Data/",as.character(i),"wave.txt", sep="")
  t <- read.table(file, TRUE)
  t <- subset(t, WVHT != 99)
  t1 <- subset(t, MM == 1)
  t2 <- subset(t, MM == 2)
  
  #print(unique(t$WVHT))
  #print(" ")
  
  if (length(t1$WVHT) != 0) {
    t1mean <- mean(t1$WVHT)
    t1max <- max(t1$WVHT)
  } else {
    t1mean <- NA
    t1max <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
  } else {
    t2mean <- NA
    t2max <- -NA
  }
  
  wave <- wave %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 1900 + i)
  
}


file = "Data/99wave.txt"
t <- read.table(file, TRUE)
t <- subset(t, WVHT != 99)
t1 <- subset(t, MM == 1)
t2 <- subset(t, MM == 2)

if (length(t1$WVHT) != 0) {
  t1mean <- mean(t1$WVHT)
  t1max <- max(t1$WVHT)
} else {
  t1mean <- NA
  t1max <- NA
}

if (length(t2$WVHT) != 0) {
  t2mean <- mean(t2$WVHT)
  t2max <- max(t2$WVHT)
} else {
  t2mean <- NA
  t2max <- -NA
}

wave <- wave %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 1999)


for (i in 0:4) {
  #07 starts new pattern
  file = paste("Data/0",as.character(i),"wave.txt", sep="")
  t <- read.table(file, TRUE, fill=TRUE)
  t <- subset(t, WVHT != 99)
  t1 <- subset(t, MM == 1)
  t2 <- subset(t, MM == 2)
  
  if (length(t1$WVHT) != 0) {
    t1mean <- mean(t1$WVHT)
    t1max <- max(t1$WVHT)
  } else {
    t1mean <- NA
    t1max <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
  } else {
    t2mean <- NA
    t2max <- -NA
  }
  
  wave <- wave %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 2000 + i)
  

}

#06 has one more column than others
wave3 <- read.table("Data/05wave.txt", TRUE)

for (i in 5:9) {
  file = paste("Data/0",as.character(i),"wave.txt", sep="")
  t <- read.table(file, TRUE, fill=TRUE)
  names(t) <- names(wave3)
  
  t <- subset(t, WVHT != 99)
  t1 <- subset(t, MM == 1)
  t2 <- subset(t, MM == 2)
  
  if (length(t1$WVHT) != 0) {
    t1mean <- mean(t1$WVHT)
    t1max <- max(t1$WVHT)
  } else {
    t1mean <- NA
    t1max <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
  } else {
    t2mean <- NA
    t2max <- -NA
  }
  
  wave <- wave %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 2000 + i)
  

}

for (i in 10:20) {
  file = paste("Data/",as.character(i),"wave.txt", sep="")
  t <- read.table(file, fill=TRUE)
  names(t) <- names(wave3)
  
  t <- subset(t, WVHT != 99)
  t1 <- subset(t, MM == 1)
  t2 <- subset(t, MM == 2)
  
  if (length(t1$WVHT) != 0) {
    t1mean <- mean(t1$WVHT)
    t1max <- max(t1$WVHT)
  } else {
    t1mean <- NA
    t1max <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
  } else {
    t2mean <- NA
    t2max <- -NA
  }
  
  wave <- wave %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 2000 + i)
}

#read in tide data
tide <- read.csv("Data/tideData.csv")
year <- substr(tide$Date, 1, 4)
month <- substr(tide$Date, 6, 7)
tide$year <- as.numeric(year)
tide$month <- as.numeric(month)
tide <- subset(tide, month == 1 | month == 2)

tide <- tide[, c(12, 13, 3, 4)]
names(tide)[names(tide) == "MHHW..ft."] <- "MHHW"

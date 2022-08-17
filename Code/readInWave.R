##read in wave data
wave <- data.frame(year=numeric(), Jmean=numeric(), Fmean=numeric(), Jmax=numeric(), Fmax=numeric(), Jewi=numeric(), Fewi=numeric())

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
    t1ewi <-nrow(subset(t1, WVHT > 4))
  } else {
    t1mean <- NA
    t1max <- NA
    t1ewi <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
    t2ewi <-nrow(subset(t2, WVHT > 4))
  } else {
    t2mean <- NA
    t2max <- -NA
    t2ewi <- NA
  }
  
  wave <- wave %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 1900 + i, Jewi = t1ewi, Fewi = t2ewi)
  
}


file = "Data/99wave.txt"
t <- read.table(file, TRUE)
t <- subset(t, WVHT != 99)
t1 <- subset(t, MM == 1)
t2 <- subset(t, MM == 2)

if (length(t1$WVHT) != 0) {
  t1mean <- mean(t1$WVHT)
  t1max <- max(t1$WVHT)
  t1ewi <- nrow(subset(t1, WVHT > 4))
} else {
  t1mean <- NA
  t1max <- NA
  t1ewi <- NA
}

if (length(t2$WVHT) != 0) {
  t2mean <- mean(t2$WVHT)
  t2max <- max(t2$WVHT)
  t2ewi <- nrow(subset(t2, WVHT > 4))
} else {
  t2mean <- NA
  t2max <- -NA
  t2ewi <- NA
}

wave <- wave %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 1999, Jewi = t1ewi, Fewi = t2ewi)


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
    t1ewi <-nrow(subset(t1, WVHT > 4))
  } else {
    t1mean <- NA
    t1max <- NA
    t1ewi <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
    t2ewi <-nrow(subset(t2, WVHT > 4))
  } else {
    t2mean <- NA
    t2max <- NA
    t2ewi <- NA
  }
  
  wave <- wave %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 2000 + i, Jewi = t1ewi, Fewi = t2ewi)
  
  
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
    t1ewi <-nrow(subset(t1, WVHT > 4))
  } else {
    t1mean <- NA
    t1max <- NA
    t1ewi <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
    t2ewi <-nrow(subset(t2, WVHT > 4))
  } else {
    t2mean <- NA
    t2max <- -NA
    t2ewi <- NA
  }
  
  wave <- wave %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 2000 + i, Jewi = t1ewi, Fewi = t2ewi)
  
  
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
    t1ewi <-nrow(subset(t1, WVHT > 4))
  } else {
    t1mean <- NA
    t1max <- NA
    t1ewi <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
    t2ewi <-nrow(subset(t2, WVHT > 4))
  } else {
    t2mean <- NA
    t2max <- -NA
    t2ewi <- NA
  }
  
  wave <- wave %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 2000 + i, Jewi = t1ewi, Fewi = t2ewi)
}

###
#only south wave data
###
waveSouth <- data.frame(year=numeric(), Jmean=numeric(), Fmean=numeric(), Jmax=numeric(), Fmax=numeric(), Jewi=numeric(), Fewi=numeric())

for (i in 83:98) {
  file = paste("Data/",as.character(i),"wave.txt", sep="")
  t <- read.table(file, TRUE)
  t <- subset(t, WVHT != 99)
  t <- subset(t, WD >= 120)
  t <- subset(t, WD <= 240)
  t1 <- subset(t, MM == 1)
  t2 <- subset(t, MM == 2)
  
  if (length(t1$WVHT) != 0) {
    t1mean <- mean(t1$WVHT)
    t1max <- max(t1$WVHT)
    t1ewi <-nrow(subset(t1, WVHT > 4))
  } else {
    t1mean <- NA
    t1max <- NA
    t1ewi <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
    t2ewi <-nrow(subset(t2, WVHT > 4))
  } else {
    t2mean <- NA
    t2max <- -NA
    t2ewi <- NA
  }
  
  waveSouth <- waveSouth %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 1900 + i, Jewi=t1ewi, Fewi=t2ewi)
  
}


file = "Data/96wave.txt"
t <- read.table(file, TRUE)
t <- subset(t, WVHT != 99)
t <- subset(t, WD >= 120)
t <- subset(t, WD <= 240)
t1 <- subset(t, MM == 1)
t2 <- subset(t, MM == 2)

if (length(t1$WVHT) != 0) {
  t1mean <- mean(t1$WVHT)
  t1max <- max(t1$WVHT)
  t1ewi <-nrow(subset(t1, WVHT > 4))
} else {
  t1mean <- NA
  t1max <- NA
  t1ewi <- NA
}

if (length(t2$WVHT) != 0) {
  t2mean <- mean(t2$WVHT)
  t2max <- max(t2$WVHT)
  t2ewi <-nrow(subset(t2, WVHT > 4))
} else {
  t2mean <- NA
  t2max <- -NA
  t2ewi <- NA
}

waveSouth <- waveSouth %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 1999, Jewi = t1ewi, Fewi = t2ewi)


for (i in 0:4) {
  #07 starts new pattern
  file = paste("Data/0",as.character(i),"wave.txt", sep="")
  t <- read.table(file, TRUE, fill=TRUE)
  t <- subset(t, WVHT != 99)
  t <- subset(t, WD >= 120)
  t <- subset(t, WD <= 240)
  t1 <- subset(t, MM == 1)
  t2 <- subset(t, MM == 2)
  
  if (length(t1$WVHT) != 0) {
    t1mean <- mean(t1$WVHT)
    t1max <- max(t1$WVHT)
    t1ewi <-nrow(subset(t1, WVHT > 4))
  } else {
    t1mean <- NA
    t1max <- NA
    t1ewi <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
    t2ewi <-nrow(subset(t2, WVHT > 4))
  } else {
    t2mean <- NA
    t2max <- -NA
    t2ewi <- NA
  }
  
  waveSouth <- waveSouth %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 2000 + i, Jewi = t1ewi, Fewi = t2ewi)
  
  
}

#06 has one more column than others
wave3 <- read.table("Data/05wave.txt", TRUE)

for (i in 5:9) {
  file = paste("Data/0",as.character(i),"wave.txt", sep="")
  t <- read.table(file, TRUE, fill=TRUE)
  names(t) <- names(wave3)
  
  t <- subset(t, WVHT != 99)
  t <- subset(t, WD >= 120)
  t <- subset(t, WD <= 240)
  t1 <- subset(t, MM == 1)
  t2 <- subset(t, MM == 2)
  
  if (length(t1$WVHT) != 0) {
    t1mean <- mean(t1$WVHT)
    t1max <- max(t1$WVHT)
    t1ewi <-nrow(subset(t1, WVHT > 4))
  } else {
    t1mean <- NA
    t1max <- NA
    t1ewi <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
    t2ewi <-nrow(subset(t2, WVHT > 4))
  } else {
    t2mean <- NA
    t2max <- -NA
    t2ewi <- NA
  }
  
  waveSouth <- waveSouth %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 2000 + i, Jewi = t1ewi, Fewi = t2ewi)
  
  
}

for (i in 10:20) {
  file = paste("Data/",as.character(i),"wave.txt", sep="")
  t <- read.table(file, fill=TRUE)
  names(t) <- names(wave3)
  
  t <- subset(t, WVHT != 99)
  t <- subset(t, WD >= 120)
  t <- subset(t, WD <= 240)
  t1 <- subset(t, MM == 1)
  t2 <- subset(t, MM == 2)
  
  if (length(t1$WVHT) != 0) {
    t1mean <- mean(t1$WVHT)
    t1max <- max(t1$WVHT)
    t1ewi <-nrow(subset(t1, WVHT > 4))
  } else {
    t1mean <- NA
    t1max <- NA
    t1ewi <- NA
  }
  
  if (length(t2$WVHT) != 0) {
    t2mean <- mean(t2$WVHT)
    t2max <- max(t2$WVHT)
    t2ewi <-nrow(subset(t2, WVHT > 4))
  } else {
    t2mean <- NA
    t2max <- NA
    t2ewi <- NA
  }
  
  waveSouth <- waveSouth %>% add_row(Jmean = t1mean, Fmean = t2mean, Jmax = t1max, Fmax = t2max, year = 2000 + i, Jewi =t1ewi, Fewi=t2ewi)
}

write.csv(wave, "Data/waveData.csv", row.names = FALSE)
write.csv(waveSouth, "Data/waveDataSouth.csv", row.names = FALSE)
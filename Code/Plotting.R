## Ben Plotting and analysis with tidydata

library(tidyverse)
library(plyr)

source("Code/final-ish.R")  ## get tidyfiles ready...check directory structure between ben and silas.

head(tidySeals) #this has all the data but missing site level PUP&WNR
unique(tidySeals$Count_Type) # ok, all the count types are here, but change max pup to "estimate" and remove estimate from cows

DATA <- tidySeals
DATA <- DATA %>% filter(Age != "PUP&WNR")              # just keep Pups, will do PUP&WNR when data ready
DATA$Count_Type <- ifelse(DATA$Age == "PUP",   # change pup&wnr countype to estimate
                  "estimate", DATA$Count_Type)
DATA <- DATA %>% filter(Count_Type != "max")       # filter out the cow max since we have estimates


ggplot(DATA, aes(Year, Count, line = Count_Type)) +
  geom_line(color = "blue") +
  geom_point(size = 0.1) +
  facet_grid(Location ~ Age)




## and some time series analysis for cows

library(tscount)



CowDataTot <- tidySeals %>% filter(Age == "COW")
CowDataTot <- CowDataTot %>% filter(Count_Type == "estimate" & Location == "All")
CowDataTot <- as.vector(as.integer(CowDataTot$Count))

plot(CowDataTot)

TIME <- as.vector(c(1:length(CowDataTot)))

# run a poisson and nb model
# need to add ENSO years and to prediction

# Enso from https://www.coaps.fsu.edu/jma
SOI <- c(1983, 1987, 1992, 1998, 
         2003, 2007, 2010, 2015, 2016, 2019)
time <- 1981:2020


SOI.YN <- as.vector(as.integer(ifelse(time %in% SOI, 1, 0))) # these are the enso years
regressors <- cbind(TIME, SOI.YN) #put the two covariates into a file

length(SOI.YN)

cow_fit_poisson <- tsglm(CowDataTot, model = list(past_obs = c(1:2), past_mean = 3), link = "log", distr = "poisson",
                         xreg = regressors)
cow_fit_nb <- tsglm(CowDataTot, model = list(past_obs = c(1,2), past_mean = 3), link = "log", distr = "nbinom",
                         xreg = TIME)
cow_fit_nb.soi <- tsglm(CowDataTot, model = list(past_obs = c(1,2), past_mean = 3), link = "log", distr = "nbinom",
                    xreg = regressors)


summary(cow_fit_nb)
summary(cow_fit_nb.soi) #Higher aic by 15 units with SOI included so dropped
plot(cow_fit_nb)

# go with nb model
pred1.pred <- predict(cow_fit_nb, n.ahead = 10, level = 0.8, global = TRUE,  # 80% CI
           B = 2000, newxreg = c(41:50))$pred  ## add ten years into future

pred1.interval <- predict(cow_fit_nb, n.ahead = 10, level = 0.8, global = TRUE,
                      B = 2000, newxreg = c(41:50))$interval

# plot
# make a DataFrame for plotting

YEAR <- 1981:2020
Estimate <- round(cow_fit_nb[["fitted.values"]])
PlotData <- data.frame(YEAR, Estimate)

Estimate <- pred1.pred
Lower <- pred1.interval[,1]
Upper <- pred1.interval[,2]
PredData <- data.frame(YEAR = c(2021:2030), Estimate, Lower, Upper)
PlotData <- plyr::rbind.fill(PlotData, PredData) # function stacks data frames with differing columns


ggplot(PlotData, aes(YEAR, Estimate, color = ifelse(YEAR>2020, "Predicted", "Estimated"))) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
  scale_x_continuous(limits = c(1983, 2030))+
  scale_y_log10() +
  geom_vline(xintercept = 2010, lty = 2) +
  labs(color=NULL) 

## let's get the lambda values over time for the entire population
PlotData <- PlotData %>% mutate(lambda = Estimate / lag(Estimate , default = first(Estimate)))

ggplot(PlotData, aes(YEAR, lambda)) +
    geom_point(aes(color = ifelse(YEAR>2020, 'Predicted', 'Estimated'))) +
    scale_x_continuous(limits = c(1985, 2030)) +
    ylim(0.5, 1.7) +
    geom_smooth(method = "loess", level = 0.8) +
    #geom_vline(xintercept = 2020.5, lty = 2) +
    labs(color=NULL) 








## Ben Plotting and analysis with tidydata

library(tidyverse)
library(plyr)
library(cowplot)

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
  geom_point(size = 0.2) +
  #scale_y_log10() + 
  theme_gray(base_size = 16) +
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
regressors <- cbind(TIME, SOI.YN) #put the two covariates into a file for model below.

#models comparing Poisson, nb, and nb with ENSO and differing autocorrelation lengths.
cow_fit_nb.3 <- tsglm(CowDataTot, model = list(past_obs = c(1:3)), link = "log", distr = "nbinom",
                      xreg = TIME)
cow_fit_nb.2.3 <- tsglm(CowDataTot, model = list(past_obs = c(1), past_mean = c(1:10)), link = "log", distr = "nbinom",
                    xreg = TIME)

cow_fit_nb.5 <- tsglm(CowDataTot, model = list(past_obs = c(1:5)), link = "log", distr = "nbinom",
                      xreg = TIME)
cow_fit_nb.10 <- tsglm(CowDataTot, model = list(past_obs = c(1:10)), link = "log", distr = "nbinom",
                          xreg = TIME)
cow_fit_nb.15 <- tsglm(CowDataTot, model = list(past_obs = c(1:15)), link = "log", distr = "nbinom",
                       xreg = TIME)
cow_fit_poisson <- tsglm(CowDataTot, model = list(past_obs = c(1:5)), link = "log", distr = "poisson",
                         xreg = TIME)
cow_fit_nb <- tsglm(CowDataTot, model = list(past_obs = c(1,2), past_mean = 3), link = "log", distr = "nbinom",
                         xreg = TIME)
cow_fit_nb.soi <- tsglm(CowDataTot, model = list(past_obs = c(1)), link = "log", distr = "nbinom",
                    xreg = regressors)
cow_fit_nb.small <- tsglm(CowDataTot, model = list(past_obs = c(1), past_mean = 10), link = "log", distr = "nbinom",
                          xreg = TIME)


#look at AICs
AIC(cow_fit_poisson)
AIC(cow_fit_nb.2.3)
AIC(cow_fit_nb.3)
AIC(cow_fit_nb.5)
AIC(cow_fit_nb.10)
AIC(cow_fit_nb.15)
AIC(cow_fit_nb.soi) #Higher aic by 15 units with SOI included so dropped
AIC(cow_fit_nb.small)  #just first order autocorr = lowest AIC
#plot(cow_fit_nb.small)
par(ask=F) # reset graphics

BESTMODEL <- cow_fit_poisson

# go with nb model for cow_fit_nb.soi 10 years into future
pred1.pred <- predict(BESTMODEL, n.ahead = 10, level = 0.8, global = TRUE,  # 80% CI
           B = 2000, newxreg = c(41:50))$pred  ## add ten years into future
pred1.interval <- predict(BESTMODEL, n.ahead = 10, level = 0.8, global = TRUE,
                      B = 2000, newxreg = c(41:50))$interval
# plot
# make a DataFrame for plotting
YEAR <- 1981:2020
Estimate <- round(BESTMODEL[["fitted.values"]])
PlotData <- data.frame(YEAR, Estimate)

Estimate <- pred1.pred
Lower <- pred1.interval[,1]
Upper <- pred1.interval[,2]
PredData <- data.frame(YEAR = c(2021:2030), Estimate, Lower, Upper)
PlotData <- plyr::rbind.fill(PlotData, PredData) # function stacks data frames with differing columns

#plot time series
p.cows <- ggplot(PlotData, aes(YEAR, Estimate, color = ifelse(YEAR>2020, "Predicted", "Estimated"))) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
  scale_x_continuous(limits = c(1983, 2030)) +
  #scale_y_log10() +
  #geom_vline(xintercept = 2010, lty = 2) +
  labs(color=NULL) +
  ylab("Estimated Cows") +
  xlab("Year") +
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.2, 0.8))
p.cows

## let's get the lambda values over time for the entire population
PlotData <- PlotData %>% mutate(lambda = Estimate / lag(Estimate , default = first(Estimate)))


## get mean lambda 2006-2020
PlotData %>% filter(YEAR > 2005 & YEAR < 2021) %>%
  summarize(mean_lambda = mean(lambda))

PlotData %>% filter(YEAR > 2005 & YEAR < 2021) %>%
  summarize(sd_lambda = sd(lambda))

label1 = "Lambda[2006-2020] = 1.06 ± 0.07"

# plot lambda
p.lambda <- ggplot(PlotData, aes(YEAR, lambda)) +
    geom_point(aes(color = ifelse(YEAR>2020, 'Predicted', 'Estimated'))) +
    scale_x_continuous(limits = c(1985, 2030)) +
    ylim(0.5, 1.7) +
    geom_smooth(method = "loess", level = 0.8) +
    geom_hline(yintercept = 1, lty = 2) +
    labs(color=NULL) +
  ylab(expression(lambda)) +
  xlab("Year") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none") +
  annotate(geom = "text", x = 2020, y = 1.5, label = label1) 
p.lambda


cowplot::plot_grid(p.cows, p.lambda, ncol = 1)


#### another time series method....state space
library(dlm)
logCow <- log(CowDataTot)
dlmCow <- dlmModPoly(logCow)
cowFilt <- dlmFilter(CowDataTot, mod = dlmCow)

gasFore <- dlmForecast(cowFilt, nAhead = 10)
sqrtR <- sapply(gasFore$R, function(x) sqrt(x[1,1]))
pl <- gasFore$a[,1] + qnorm(0.05, sd = sqrtR)
pu <- gasFore$a[,1] + qnorm(0.95, sd = sqrtR)
x <- ts.union(window(lGas, start = c(1982, 1)),
                + window(gasSmooth$s[,1], start = c(1982, 1)),
                + gasFore$a[,1], pl, pu)
plot(x, plot.type = "single", type = 'o', pch = c(1, 0, 20, 3, 3),
       + col = c("darkgrey", "darkgrey", "brown", "yellow", "yellow"),
       + ylab = "Log gas consumption")
legend("bottomright", legend = c("Observed",
                                   + "Smoothed (deseasonalized)",
                                   + "Forecasted level", "90% probability limit"),
         + bty = 'n', pch = c(1, 0, 20, 3, 3), lty = 1,
         + col = c("darkgrey", "darkgrey", "brown", "yellow", "yellow"))




##----------------------------------
## another method based on forecast package
#install.packages(c("scoringRules", "forecast"))
library(forecast)
library(scoringRules)

plot(CowDataTot, las = 1)
Cow_train <- CowDataTot[1:40]
Cow_test <- CowDataTot[31:40]  # try to have just 6 for the test

acf(CowDataTot)

train_aa_mod <- auto.arima(Cow_train)
train_aa_mod

train_ar1_mod <- arima(Cow_train, order = c(2, 0, 0))


aa_forecast <- forecast(train_aa_mod, 10, level = c(50, 90))
plot(aa_forecast, xlim = c(1,50), ylim = c(0, 4000), main = "Auto ARIMA", las = 1)
lines(31:40, Cow_test)




train_aa_mod$call$xreg <- forecast:::getxreg(train_aa_mod)

test_xreg <- `colnames<-`(as.matrix(31:40), "drift")

test_aa_forecast <- predict(object = train_aa_mod, n.ahead = 10,
                            newxreg = test_xreg)
test_aa_forecast <- data.frame(time = 31:40,
                               pred = test_aa_forecast$pred,
                               se = test_aa_forecast$se)
aa_forecast_paths <- matrix(NA, nrow = 10, ncol = 1000)
for(i in 1:1000){
  aa_forecast_paths[,i] <- simulate(train_aa_mod, xreg = test_xreg,
                                    bootstrap = TRUE, future = TRUE)
}

Cow_test_NA <- is.na(Cow_test)
Cow_test_no_NAs <- Cow_test[!Cow_test_NA]
aa_forecast_paths_no_NAs <- aa_forecast_paths[!Cow_test_NA, ]

aa_crps <- -crps_sample(Cow_test_no_NAs, aa_forecast_paths_no_NAs)
aa_logs <- -logs_sample(Cow_test_no_NAs, aa_forecast_paths_no_NAs)

mean(aa_crps)
mean(aa_logs)

aa_PIT <- pnorm(Cow_test_no_NAs, mean = test_aa_forecast$pred,
                sd = test_aa_forecast$se)

hist(aa_PIT, breaks = seq(0, 1, 0.1), las = 1, main = "Auto ARIMA", xlab = "PIT")








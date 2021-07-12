
##---what does the future hold?
## let's do 10 years.
## use lambda since 2006
## need to make sure PlotData available from "final-ish.R


lambda_mean <- PlotData %>% filter(YEAR > 2005 & YEAR < 2021) %>%
  summarize(mean_lambda = mean(lambda))

hist(PlotData$lambda[16:40]) # should we use a normal-symmetrical distribution of sd for projections?

lambda_sd <- PlotData %>% filter(YEAR > 2005 & YEAR < 2021) %>%
  summarize(sd_lambda = sd(lambda))


## loop
set.seed(2)
N0 = 1597  #initial population size
times = 11  #number of years into the future
N = vector(length = times)  #empty vector to store pop. sizes
N[1] = N0  #initial population size should be the first N

# lambdas--we only need 19 numbers because growth only
# happens between 2 years.
LAMBDA = rnorm(times - 1, mean = 1.064, sd = 0.068)

# start loop: Take previous year's N and multiply by lambda
for (t in 2:times) {
  N[t] = N[t - 1] * LAMBDA[t - 1]
}
plot(1:times, N, type = "b", las = 1)


## ok multiple loops

# multiple simulations
set.seed(2)
sims = 100
outmat = sapply(1:sims, function(x) {
  times = 11
  N0 = 1597
  N = vector(length = times)
  N[1] = N0
  LAMBDA = rnorm(times - 1, 1.064, 0.068)  # can try larger sd for more storm years
  for (t in 2:times) {
    N[t] = N[t - 1] * LAMBDA[t - 1]
  }
  N
})

## try to plot with ggplot
outmat_tib <- as_tibble(outmat)

## gather the data
out_gathered <- gather(outmat_tib, Simulation, SimCount)

## add year field
out_gathered$Year <- rep(1:NROW(outmat_tib), times = sims)

## give RealYear
out_gathered$RealYear <- out_gathered$Year + 2019 

## plot it

pSim <- ggplot(out_gathered, aes(RealYear, SimCount, group = Simulation)) + 
  geom_smooth(se = FALSE, size = 0.1, level = 0.95, alpha=0.5, color = 'gray') +
  #geom_hline(aes(yintercept = 1597)) + 
  labs(x = "Year", y = "Female elephant seals",
       title = "Projected Female Elephant Seal Population Size at Point Reyes National Seashore",
       subtitle = paste("100 Simulations:", "\u03BB = 0.06 \u00B1 0.07")) +
  ylim(0, 10000) + 
  theme_gray(base_size = 18) +
  scale_y_continuous(limits = c(0, 6000), breaks=c(seq(0, 6000, by = 1000))) +
  scale_x_continuous(limits = c(2020, 2030), breaks=c(seq(2020, 2030, by = 2)))

pSim_plot <- pSim + geom_smooth(aes(group = 1), se = FALSE, color = 'red')
pSim_plot

## make histogram of final values
max_counts <- filter(out_gathered, Year == 20)
hist(max_counts$SimCount)

max_hist <-ggplot(max_counts, aes(SimCount)) +
  labs(x = "Female elephant seals",
       title = "Projected Female Elephant Seal Population Size at Point Reyes National Seashore",
       subtitle = paste("500 Simulations:", "\u03BB = 0.06 \u00B1 0.07")) + 
  geom_histogram() 
max_hist








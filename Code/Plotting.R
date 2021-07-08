## Ben Plotting and analysis with tidydata

library(tidyverse)

source("Code/final-ish.R")  ## get tidyfiles ready...check directory structure between ben and silas.

head(tidySeals) #this has all the data but missing site level PUP&WNR
unique(tidySeals$Count_Type) # ok, all the count types are here, but change max pup to "estimate" and remove estimate from cows

DATA <- tidySeals
DATA <- DATA %>% filter(Age != "PUP&WNR")              # just keep Pups, will do PUP&WNR when data ready
DATA$Count_Type <- ifelse(DATA$Age == "PUP",   # change pup&wnr countype to estimate
                  "estimate", DATA$Count_Type)
DATA <- DATA %>% filter(Count_Type != "max")       # filder out the cow max since we have estimates


ggplot(DATA, aes(Year, Count, line = Count_Type)) +
  geom_line(color = "blue") +
  geom_point(size = 0.1) +
  facet_grid(Location ~ Age)










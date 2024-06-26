library(dplyr)
library(tidyr)
library(ggplot2)

all_dat <- readRDS("data/all_dat.rds")

grep("zoo", unique(all_dat$name), value = TRUE)


zoop_dat <-
# install.packages("readxl")
library(dplyr)
library(tidyr)
library(ggplot2)

ecosim_dat <- readxl::read_xlsx("data/time series for initial threshold analysis (linked to ecol charac that are highly impacted in our BBN).xlsx",
                         sheet = 1, skip = 1) %>% 
  rename(year = 1) %>% 
  pivot_longer(cols = !year) %>% 
  mutate(series = "ewe")


sm_pelagic_dat <- readxl::read_xlsx("data/time series for initial threshold analysis (linked to ecol charac that are highly impacted in our BBN).xlsx",
                                sheet = 2, skip = 1) %>%
  rename(year = 1) %>% 
  pivot_longer(cols = !year) %>% 
  mutate(year = as.numeric(gsub("nov", "", tolower(year))),
         series = "dffe")

seabirds_dat <- readxl::read_xlsx("data/time series for initial threshold analysis (linked to ecol charac that are highly impacted in our BBN).xlsx",
                                    sheet = 3, skip = 1) %>% 
  rename(year = 1) %>% 
  pivot_longer(cols = !year) %>% 
  mutate(series = "dffe")


indiseas_dat <- readxl::read_xlsx("data/time series for initial threshold analysis (linked to ecol charac that are highly impacted in our BBN).xlsx",
                                  sheet = 4, skip = 2) %>% 
  rename(year = 1,
         TL_survey = 11,
         `inverse_fishing_pressure` = 8,
         flagship_com = 9,
         flagship_nc = 10,
         TL_survey = 11) %>% 
  pivot_longer(cols = !year) %>% 
  mutate(series = "indiseas")

biomass_dat <- readxl::read_xlsx("data/time series for initial threshold analysis (linked to ecol charac that are highly impacted in our BBN).xlsx",
                                  sheet = 5, range = "A10:AX48") %>% 
  rename(year = 1) %>% 
  group_by(year) %>% 
  mutate(seabirds = sum(`African Penguin`, `Cape Gannet`, `Cape Cormorant`,`Other seabirds`),
         sm_pelagics = sum(`Anchovy recruits`, `Anchovy adults`,	`Juvenile sardine`,	`Adult sardine`,	Redeye,	`Other small pelagics`),
         lg_pelagics = sum(`Chub mackerel`, Lanternfish,	Lightfish,	Snoek,	`Tuna&Swordfish`, `Large Sparids`,	`Medium Sparids`,	Sciaenids,	Yellowtail,	`Other linefish`)) %>% 
  ungroup() %>% 
  pivot_longer(cols = !year) %>% 
  mutate(series = "ewe") %>% 
  filter(!name %in% c("Cape Gannet", "African Penguin", "Redeye", "Sciaenids"))

all_dat <- bind_rows(sm_pelagic_dat,
                     seabirds_dat,
                     indiseas_dat,
                     ecosim_dat,
                     biomass_dat, 
                     upwelling_dat) %>% 
  arrange(series, name, year)
upwelling_dat <- readxl::read_xlsx("data/time series for initial threshold analysis (linked to ecol charac that are highly impacted in our BBN).xlsx",
                                   sheet = 5, range = "B53:C89") %>% 
  rename(year = 1,
         upwelling = 2)

saveRDS(object = all_dat, file = "data/all_dat.rds")


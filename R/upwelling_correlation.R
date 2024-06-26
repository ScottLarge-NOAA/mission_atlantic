# remotes::install_github("cran/FitAR")
# devtools::install_github("pedrognicolau/ARbiascorrect-v1")
# remotes::install_github("saskiaotto/INDperform")
library(INDperform)
library(ARbiascorrect)
library(dplyr)
library(tidyr)
library(ggplot2)

all_dat <- readRDS("data/all_dat.rds")

ind_list <- c("Phytoplankon 1", "Phytoplakton 2", "Mesozooplankton", "Anchovy recruits", "Juvenile Hmack",
              "Snoek", "Yellowtail", "Sardine", "Redeye", "Sciaenids", "Large M. paradoxus", "PF Chondrichthyans",
              "BF Chondrichthyans", "Marine Mammals (seals&cetaceans)", "African Penguin", "Cape Gannet", "WC rock lobster")


tgam_dat <- all_dat %>% 
  filter(name %in% ind_list) %>% 
  mutate(name = case_when(name == "Phytoplankon 1" ~ "Phytoplankton 1 (small)",
                          name == "Phytoplakton 2" ~ "Phytoplankton 2 (large)",
                          name == "Juvenile Hmack" ~ "Horse Mackerel juveniles",
                          name == "Sardine" ~ "Sardine SSB",
                          name == "Redeye" ~ "Redeye round herring SSB",
                          name == "PF Chondrichthyans" ~ "PF (pelagic-feeding) chondrichthyans", 
                          name == "BF Chondrichthyans" ~ "BF (benthic-feeding) chondrichthyans", 
                          name == "Marine Mammals (seals&cetaceans)" ~ "Marine Mammals (seals & cetaceans)",
                          TRUE ~ name)) %>% 
  group_by(name) %>% 
  mutate(zscore = scale(value)[,1]) %>% 
  arrange(year, series, name) %>% 
  distinct(.keep_all = TRUE)


ggplot(tgam_dat, aes(x = year, y = zscore, group = name, color = name)) + 
  facet_wrap(~ name) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  NULL


ggplot(tgam_dat, aes(x = upwelling, y = value, group = name)) + 
  facet_wrap(~ name, scales = "free_y") +
  geom_point(aes(color = tgam_dat$year), show.legend = FALSE) +
  geom_path(aes(color = tgam_dat$year), show.legend = FALSE) +
  geom_text(aes(color = tgam_dat$year), label = tgam_dat$year, show.legend = FALSE) +
  viridis::scale_color_viridis(breaks = seq(1975, 2015, by = 10)) +
  theme_minimal() +
  NULL


# Using the Baltic Sea demo data in this package
ind_tbl <- ind_ex[ ,-1] # excluding the year
time <- ind_ex$Year
# Using the default settings
trend_tbl <- model_trend(ind_tbl, time)
# Change the training and test data assignment
model_trend(ind_tbl, time, train = .5, random = TRUE)
# To keep the name when testing only one indicator, coerce vector to data frame
model_trend(data.frame(MS = ind_tbl$MS), time, train = .5, random = TRUE)
pt <- plot_trend(trend_tbl)
pt[[2]]

# Using the Baltic Sea demo data in this package
sb_tbl <- tgam_dat %>% 
  select(year, name, zscore) %>% 
  pivot_wider(names_from = name, values_from = zscore)

# excluding the year
sb_time <- sb_tbl$year
sb_tbl <- sb_tbl %>% 
  select(-year)

# Using the default settings
sb_trend_tbl <- model_trend(sb_tbl, sb_time)
# Change the training and test data assignment
model_trend(ind_tbl, time, train = .5, random = TRUE)
# To keep the name when testing only one indicator, coerce vector to data frame
model_trend(data.frame(MS = ind_tbl$MS), time, train = .5, random = TRUE)
sb_pt <- plot_trend(sb_trend_tbl)
sb_pt$Phytoplankton_1_small


ggplot() +
  geom_path(data = tgam_dat %>% filter(name == "Anchovy recruits"), aes(x = year, y = zscore)) +
  geom_path(data = all_dat %>% filter(name == "Anchovy recruits"), aes(x = year, y = scale(upwelling)), color = "red")


upwelling <- all_dat %>% 
  filter(name == "Anchovy recruits",
         !is.na(upwelling)) %>% 
  select(year, upwelling) %>% 
  mutate(upwelling = scale(upwelling)[,1])

upwelling_red <- expand.grid(year = seq(from = 1979, length.out = 36),
                             lambda = seq(from = 0.00, to = .99, by = 0.01)) %>% 
  left_join(upwelling) %>% 
  group_by(lambda) %>%
  mutate(red = (lambda*scale(lag(upwelling))[,1] + upwelling))


ggplot(upwelling_red, aes(x = year, y = red, by = lambda)) +
  geom_path(aes(color = lambda))

upwelling_corr <- upwelling_red %>% 
  pivot_wider(id_cols = year, names_from = lambda, values_from = red) %>% 
  left_join(anch_dat) %>% 
  filter(year != 1979) %>% 
  summarize(across(!c(year, value), ~ cor(.x, value, method = "spearman"))) %>% 
  pivot_longer(cols = everything(), names_to = "lambda", values_to = "value")

ggplot(upwelling_corr, aes(x = lambda, y = value)) +
  geom_point()



ggplot() +
  geom_path(data = upwelling, aes(x = year, y = upwelling))+
  geom_path(data = upwelling, aes(x = year, y = red_upwelling), color = "red")


install.packages("colorednoise")
library(colorednoise)

upwelling <- data.frame(phi = seq(from = 0.00, to = .99, by = 0.01)) %>% 
  # mutate(red = purrr::map(phi, ~colorednoise::colored_noise(timesteps = 38, mean = 0, sd = 1, phi = .x))) %>% 
  unnest(cols = red) %>% 
  mutate(year = rep(seq(from = 1978, length.out = 38), 100))

anch_dat <- tgam_dat %>%
  filter(name == "Anchovy recruits") %>% 
  ungroup() %>% 
  select(year, value)

corr_tt <- tt %>% 
  pivot_wider(id_cols = year, names_from = lambda, values_from = red) %>% 
  left_join(anch_dat) %>% 
  summarize(across(!c(year, value), ~ cor(.x, value))) %>% 
  pivot_longer(cols = everything(), names_to = "phi", values_to = "value")


  summarize(across)
  mutate(data = purrr::map2(.x = value, .y = red, ~cor(x = .x, y = .y, method = "pearson")))



group_by(Species) %>% nest() %>% 
  mutate(data = map(data, compose(stretch, correlate))) %>% 
  unnest()

cor(x = corr_tt$value, y = corr_tt$red, method = "pearson")

ggplot(tt, aes(x = time, y = red, group = lambda)) +
  geom_path(aes(color = lambda))




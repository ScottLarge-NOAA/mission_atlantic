# install.packages("readxl")
# install.packages("googlesheets4")
library(dplyr)
library(tidyr)
library(ggplot2)
library(rerddap)
library(googlesheets4)

### EwE data -----
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

upwelling_dat <- readxl::read_xlsx("data/time series for initial threshold analysis (linked to ecol charac that are highly impacted in our BBN).xlsx",
                                   sheet = 5, range = "B53:C89") %>% 
  rename(year = 1,
         upwelling = 2)

all_dat <- bind_rows(sm_pelagic_dat,
                     seabirds_dat,
                     indiseas_dat,
                     ecosim_dat,
                     biomass_dat) %>% 
  arrange(series, name, year) %>% 
  left_join(upwelling_dat)

saveRDS(object = all_dat, file = "data/all_dat.rds")

## Upwelling Data ------
### Tarron Lamont
upwelling_dat <- readxl::read_xlsx("data/upwelling_index_monthly_Lamont.xlsx",
                                   sheet = 1, range = "A2:C440") %>% 
  rename(year = 1,
         southern_benguela = 2,
         agulhas_bank = 3) %>% 
  mutate(total = rowSums(pick(southern_benguela, agulhas_bank))) %>% 
  pivot_longer(!year, names_to = "area", values_to = "value") %>% 
  arrange(area, year)

saveRDS(upwelling_dat, file = "data/upwelling_index.rds")

## DFFE data ---------
pelagic_r_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1ePUM5Ms3bzv97jNfR9wV5cEpdcs2FLmH-u_cPNt3Xys/edit#gid=1722486359",
                            sheet = "dffe_recruit") %>% 
  pivot_longer(!year, names_to = "var", values_to = "value") %>% 
  separate(col = var, c("species", "indicator", "unit")) %>% 
  mutate(area = "total",
         species = case_when(species == "anch" ~ "Anchovy",
                             species == "sard" ~ "Sardine",
                             species == "rherring" ~ "Roundeye Herring",
                             TRUE ~ NA_character_))
saveRDS(pelagic_r_dat, "data/pelagic_recruitment.rds")

pelagic_b_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1ePUM5Ms3bzv97jNfR9wV5cEpdcs2FLmH-u_cPNt3Xys/edit#gid=1722486359",
                            sheet = "dffe_biomass") %>% 
  pivot_longer(!year, names_to = "var", values_to = "value") %>% 
  separate(col = var, c("species", "indicator", "area")) %>% 
  mutate(species = case_when(species == "anch" ~ "Anchovy",
                             species == "sard" ~ "Sardine",
                             species == "rherring" ~ "Roundeye Herring",
                             TRUE ~ NA_character_))
saveRDS(pelagic_b_dat, "data/pelagic_biomass.rds")


ggplot(pelagic_b_dat %>% filter(indicator == "biomass"), aes(x = year, y = value, group = area, color = area)) +
  geom_path() +
  facet_wrap(~species, nrow = 3, scales = "free_y") +
  theme_minimal()


ggplot(pelagic_r_dat, aes(x = year, y = value)) +
  geom_path() +
  facet_grid(unit~species, scales = "free") +
  theme_minimal()


  ## Pelagic data ----------
### from Table 4 FISHERIES/2024/MAR/SWG-PEL/12


## Benguela Nino -----------
# Monthly mean SSTs are from the upgraded version of Optimum Interpolation SSTs with improved bias corrections
# (OIv2.1; Huang et al., 2021). OIv2.1 data are available from September 1981 onward and on a 1° by 1° resolution. 
# Benguela Niño index are defined as the averaged SST anomalies (SSTAs) in (10°–20°S, 7°–11°E).


#  rerddap::info(datasetid = "ncdcOisst21Agg", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df, latitude = c(-10, -20), longitude = c(7, 11)){
  OISST_dat <- rerddap::griddap(datasetx = "ncdcOisst21Agg",
                                url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                                time = c(time_df$start, time_df$end), 
                                zlev = c(0, 0),
                                latitude = latitude,
                                longitude = longitude,
                                fields = "anom")$data %>% 
    dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) %>% 
    dplyr::rename(t = time, anom = anom, lon = longitude, lat = latitude) %>% 
    dplyr::select(lon, lat, t, anom) %>% 
    stats::na.omit()
}


# Date download range by start and end dates per year
dl_years <- data.frame(date_index = 1:6,
                       start = c("1982-01-01", "1990-01-01", 
                                 "1998-01-01", "2006-01-01", 
                                 "2014-01-01", "2020-01-01"),
                       end = c("1989-12-31", "1997-12-31", 
                               "2005-12-31", "2013-12-31", 
                               "2019-12-31", "2023-12-31"))

OISST_data <- dl_years %>% 
  dplyr::group_by(date_index) %>% 
  dplyr::group_modify(~OISST_sub_dl(.x)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(lon, lat, t, anom)

# saveRDS(OISST_data, file = "data/oisst_anom.rds")

OISST_data <- readRDS("data/oisst_anom.rds")
OISST_month <- OISST_data %>%
  mutate(t_month = format(OISST_data$t, "%Y-%m")) %>%
  group_by(t_month, lon, lat) %>%
  summarize(anom_mon = mean(anom, na.rm = TRUE),
            anom_cv = sd(anom, na.rm = TRUE)/anom_mon)

benguela_nino <- OISST_month %>%
  ungroup() %>% 
  select(t_month, anom_mon) %>%
  group_by(t_month) %>% 
  summarize(bni = mean(anom_mon, na.rm = TRUE)) %>% 
  mutate(date = lubridate::ymd(paste(t_month,"01", sep="-")), 
         col = factor(ifelse(bni > 0, 2, 1)),
         bni_curve = slider::slide_dbl(bni, ~ mean(.x), .before = 1, .after = 1))


saveRDS(benguela_nino, file = "data/bni.rds")

# ggplot(benguela_nino) +
#   geom_bar(aes(x = date, y = bni, fill = col), stat = "identity", show.legend = FALSE)+
#   geom_hline(yintercept = 0, linewidth = .75) +
#   geom_line(aes(x = date, y = bni_curve), linewidth = .5) +
#   # facet_wrap(~Station.Name)+
#   theme(axis.text.x = element_text(angle = 90)) +
#   theme_minimal() +
#   scale_x_date(date_breaks = "5 years",
#                date_labels = "%Y") +
#   scale_fill_manual(values = c("blue", "red")) +
#   labs(title = "Benguela Niño Index (BNi)",
#        x = "",
#        y = "")


## Catch data ------
sa_catches <- read_sheet("https://docs.google.com/spreadsheets/d/1ePUM5Ms3bzv97jNfR9wV5cEpdcs2FLmH-u_cPNt3Xys/edit#gid=1722486359",
                 sheet = "dffe_sa_catches") %>% 
  pivot_longer(cols = !year:table, names_to = "month", values_to = "catch") %>% 
  group_by(year, species) %>% 
  summarize(value = sum(catch, na.rm = TRUE)*1000,
            unit = "tonnes")


rherring_catches <- read_sheet("https://docs.google.com/spreadsheets/d/1ePUM5Ms3bzv97jNfR9wV5cEpdcs2FLmH-u_cPNt3Xys/edit#gid=1722486359",
                         sheet = "dffe_rherring_catches") %>% 
  group_by(year) %>% 
  summarize(value = sum(value, na.rm = TRUE),
            species = "rherring",
            unit = "tonnes")

catches <- bind_rows(sa_catches,
                     rherring_catches) %>% 
  mutate(species = case_when(species == "anchovy" ~ "Anchovy",
                             species == "sardine" ~ "Sardine",
                             species == "rherring" ~ "Roundeye Herring",
                             TRUE ~ NA_character_))

saveRDS(catches, "data/pelagic_catch.rds")



## Monthly catches

sa_catches_m <- read_sheet("https://docs.google.com/spreadsheets/d/1ePUM5Ms3bzv97jNfR9wV5cEpdcs2FLmH-u_cPNt3Xys/edit#gid=1722486359",
                         sheet = "dffe_sa_catches") %>% 
  mutate(table = purrr::map_chr(table, as.character))

saveRDS(sa_catches_m, "data/pelagic_catch_monthly.rds")


# ggplot(catches, aes(x = year, y = value/1000, group = species, fill = species, color = species)) +
#   geom_path() +
#   # geom_bar(stat = "identity") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   labs(x = "", y = "tonnes (thousands)",
#        title = "Small pelagic catches") + 
#   NULL

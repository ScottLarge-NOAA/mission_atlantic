# install.packages("readxl")
# install.packages("googlesheets4")
library(dplyr)
library(tidyr)
library(ggplot2)
library(rerddap)
library(googlesheets4)
library(terra)
library(tidyterra)
library(sf)


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
  # mutate(total = rowSums(pick(southern_benguela, agulhas_bank))) %>% 
  pivot_longer(!year, names_to = "area", values_to = "value") %>% 
  arrange(area, year)

# saveRDS(upwelling_dat, file = "data/upwelling_index.rds")

upwelling_month <- upwelling_dat %>% 
  group_by(area) %>%
  mutate(upwelling_date = lag(year, n = 6), ##  years run from July (previous year) to June (current year). eg: 1979 = 1 July 1979 to 30 June 1980
         upwelling_year = lubridate::year(upwelling_date),
         months = lubridate::month(year),
         area = case_when(area == "agulhas_bank" ~ "AB",
                          area == "southern_benguela" ~ "SB",
                          TRUE ~ NA_character_)) %>% 
  filter(!is.na(upwelling_date)) 

# upwelling_dat <- upwelling_month %>% ## cumulative coastal upwelling from December to February
#   group_by(area, upwelling_year) %>%
#   summarize(value = sum(value, na.rm = TRUE))

upwelling_summer_dat <- upwelling_month %>% 
  filter(!is.na(upwelling_date), 
         months %in% c(12, 1, 2),
         area != "total") %>% ## cumulative coastal upwelling from December to february
  group_by(area, upwelling_year) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(variable = paste0("upwelling_", area)) %>% 
  select(year = upwelling_year, variable, value)

mean_upwelling_ab <- read.csv("https://repository.ocean.gov.za/index.php/s/ipLnwLGk3Ekrnwz/download?path=&files=AB_shelf_seasonal_mean_total_cumulative_upwelling_timeseries.csv") %>% 
  select(-starts_with("X"))
mean_upwelling_sb <- read.csv("https://repository.ocean.gov.za/index.php/s/RZegn2aXMbjsryH/download?path=&files=SB_shelf_seasonal_mean_total_cumulative_upwelling_timeseries.csv") %>% 
  select(-starts_with("X"))

mean_upwelling <- mean_upwelling_ab %>% 
  left_join(mean_upwelling_sb) %>% 
  pivot_longer(-year, names_to = "variable", values_to = "value") %>% 
  separate_wider_delim(variable, delim = "_", names = c("area", "type", "season", "var", "unit")) %>% 
  filter(type == "shelf",
         season == "summer") %>% 
  mutate(variable = paste0(var, "_", area)) %>% 
  select(year, variable, value) %>% 
  pivot_wider(names_from = "variable", values_from = "value")

ggplot() + 
  geom_point(data = upwelling_summer_dat, aes(x = year, y = value/1000, color = variable), shape = 2) +
  geom_point(data = mean_upwelling, aes(x = year, y = value/1000, color = variable), shape = 19) +
  theme_minimal() +
  theme(legend.position =  "bottom")

upwelling_all <- bind_rows(upwelling_summer_dat, 
                             mean_upwelling %>% filter(year > 2014))

# ggplot() + 
#   geom_path(data = upwelling_all, aes(x = year, y = value/1000, color = variable)) +
#   theme_minimal() +
#   theme(legend.position =  "bottom")
# 
saveRDS(upwelling_all, file = "data/upwelling_index.rds")

  

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
OISST_sub_dl <- function(time_df, latitude = c(-20, -10), longitude = c(7, 11)){
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

## Agulhas Bank SST indexes
# info("ncdcOisst21Agg")
# This function downloads and prepares data based on user provided start and end dates
OISST_agulhas_dl <- function(time_df, latitude = c(-32, -38), longitude = c(16, 24)){
  OISST_dat <- rerddap::griddap(datasetx = "ncdcOisst21Agg",
                                url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                                time = c(time_df$start, time_df$end), 
                                zlev = c(0, 0),
                                latitude = latitude,
                                longitude = longitude,
                                fields = "sst")$data %>% 
    dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) %>% 
    dplyr::rename(t = time, sst = sst, lon = longitude, lat = latitude) %>% 
    dplyr::select(lon, lat, t, sst) %>% 
    stats::na.omit()
}


# Date download range by start and end dates per year
dl_agulhas_years <- data.frame(start = paste0(seq(1982, 2024, by = 10), "-01-01T00:00:00Z"),
                       end = paste0(c(seq(1991, 2024, by = 10), 2023), "-12-31T00:00:00Z")) %>% 
  mutate(date_index = row_number())

agulhas_data <- dl_agulhas_years %>% 
  dplyr::group_by(date_index) %>% 
  dplyr::group_modify(~OISST_agulhas_dl(.x)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(lon, lat, t, sst)

# saveRDS(agulhas_data, file = "data/agulhas_sst.rds")
agulhas_data <- readRDS("data/agulhas_sst.rds")

agulhas_zone <- agulhas_data %>% 
  mutate(zone = case_when(between(lon, 19, 20) & between(lat, -35, -34) ~ "WABC",
                          between(lon, 20, 22) & between(lat, -35, -34) ~ "CABC",
                          between(lon, 22, 23) & between(lat, -35, -34) ~ "EABC",
                          between(lon, 20, 22) & between(lat, -36, -35) ~ "CABO",
                          TRUE ~ NA_character_),
         t_month = format(agulhas_data$t, "%Y-%m"),
         month = lubridate::month(t),
         year = lubridate::year(t)) %>% 
  na.omit(zone) %>% 
  filter(month %in% c(10:12)) %>% 
  group_by(year, zone) %>%
  summarize(sst_mon = mean(sst, na.rm = TRUE),
            sst_cv = sd(sst, na.rm = TRUE)/sst_mon)


agulhas_gradient <- agulhas_zone %>% 
  select(-sst_cv) %>% 
  filter(zone %in% c("CABO", "CABC")) %>% 
  pivot_wider(names_from = zone, values_from = sst_mon) %>%
  mutate(sst_gradient = CABO-CABC,
         date = lubridate::ymd(paste(year,"01-01", sep="-")))

ggplot(agulhas_gradient, aes(x = date, y = sst_gradient))+
  geom_path() + 
  geom_point() +
  theme_minimal() +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y") +
  labs(title = "Central Agulhas Bank Offshore - Central Agulhas Bank Coastal",
       x = "",
       y = "")
saveRDS(agulhas_gradient, file = "data/agulhas_gradient.rds")
# 
# 
# agulhas_year <- agulhas_zone %>% 
#   ungroup() %>% 
#   mutate(year = lubridate::year(paste(t_month,"01", sep="-"))) %>% #, 
#          # year = format(agulhas_zone$date, "%Y")) %>%
#   group_by(zone, year) %>%
#   summarize(sst = mean(sst_mon, na.rm = TRUE)) %>% 
#   mutate(date = lubridate::ymd(paste(year,"01-01", sep="-")))
# 
# ggplot(agulhas_year, aes(x = date, y = sst, group = zone, color = zone))+
#   geom_path() + 
#   facet_wrap(~zone)+
#   theme_minimal() +
#   scale_x_date(date_breaks = "5 years",
#                date_labels = "%Y") +
#   labs(title = "SST of Agulhas sub-domains",
#        x = "",
#        y = "")



## NCEP/NCAR Pressure
# info('esrlNcepRe')
# This function downloads and prepares data based on user provided start and end dates
ncep_sub_dl <- function(time_df, latitude = c(-35, -35), longitude = c(22.5, 22.5)){
  ncep_dat <- rerddap::griddap(datasetx = "esrlNcepRe",
                               url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                               time = c(time_df$start, time_df$end), 
                               # zlev = c(0, 0),
                               latitude = latitude,
                               longitude = longitude,
                               fields = "pres")$data %>% 
    dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) %>% 
    dplyr::rename(t = time, pres = pres, lon = longitude, lat = latitude) %>% 
    dplyr::select(lon, lat, t, pres) %>% 
    stats::na.omit()
}


# Date download range by start and end dates per year
dl_years <- data.frame(start = paste0(seq(1948, 2024, by = 10), "-01-01T00:00:00Z"),
                       end = paste0(c(seq(1957, 2024, by = 10), 2023), "-12-31T00:00:00Z")) %>% 
  mutate(date_index = row_number())


ncep_dat <- dl_years %>%
  # head(1) %>%
  dplyr::group_by(date_index) %>% 
  dplyr::group_modify(~ncep_sub_dl(.x)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(lon, lat, t, pres)

saveRDS(ncep_dat, file = "data/pressure.rds")

# ncep_dat <- readRDS("data/pressure.rds")
ncep_month <- ncep_dat %>%
  mutate(t_month = format(ncep_dat$t, "%Y-%m")) %>%
  group_by(t_month, lon, lat) %>%
  summarize(pres_mon = mean(pres, na.rm = TRUE),
            pres_cv = sd(pres, na.rm = TRUE)/pres_mon)

pressure <- ncep_dat %>%
  mutate(year = format(ncep_dat$t, "%Y")) %>%
  group_by(year) %>%
  summarize(pressure = mean(pres, na.rm = TRUE),
            pres_cv = sd(pres, na.rm = TRUE)/pressure) %>% 
  mutate(pressure = pressure/100,
            date = lubridate::ymd(paste(year,"01-01", sep="-")))

# ggplot(pressure,aes(x = date, y = pressure), show.legend = FALSE) +
#   geom_path()+
#   geom_point()+
#   theme_minimal() +
#   scale_x_date(date_breaks = "5 years",
#                date_labels = "%Y") +
#   labs(title = "Atmospheric pressure (hPa)",
#        subtitle = "NCEP/NCAR Reanalysis",
#        x = "",
#        y = "")
saveRDS(pressure, file = "data/pressure.rds")

## CAP ---

# 40°S, 20°E and 35°S, 20°E

# info('esrlNcepRe')
# This function downloads and prepares data based on user provided start and end dates
ncep_cap_dl <- function(time_df, latitude = c(-40, -35), longitude = c(20, 20)){
  ncep_dat <- rerddap::griddap(datasetx = "esrlNcepRe",
                               url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                               time = c(time_df$start, time_df$end), 
                               # zlev = c(0, 0),
                               latitude = latitude,
                               longitude = longitude,
                               fields = "pres")$data %>% 
    dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) %>% 
    dplyr::rename(t = time, pres = pres, lon = longitude, lat = latitude) %>% 
    dplyr::select(lon, lat, t, pres) %>% 
    stats::na.omit()
}


# Date download range by start and end dates per year
dl_years <- data.frame(start = paste0(seq(1948, 2024, by = 10), "-01-01T00:00:00Z"),
                       end = paste0(c(seq(1957, 2024, by = 10), 2023), "-12-31T00:00:00Z")) %>% 
  mutate(date_index = row_number())


cap_dat <- dl_years %>%
  # head(1) %>%
  dplyr::group_by(date_index) %>% 
  dplyr::group_modify(~ncep_cap_dl(.x)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(lon, lat, t, pres)


saveRDS(cap_dat, file = "data/cap_dat.rds")

cap_month <- cap_dat %>%
  mutate(year = lubridate::year(t),
         month = lubridate::month(t),
         t_month = format(cap_dat$t, "%Y-%m"), 
         zone = case_when(lat == -40 ~ "forty-south",
                          lat == -35 ~ "thirty-five-south",
                          TRUE ~ NA_character_)) %>% 
  filter(!lat == -37.5) %>% 
  group_by(t_month, zone) %>% 
  summarize(pressure = mean(pres, na.rm = TRUE)/100) %>% 
  pivot_wider(names_from = zone, values_from = pressure) %>% 
  mutate(cap = `forty-south` - `thirty-five-south`) #%>% 
  # select(year, cap)

ref_val <- cap_year %>% 
  filter(between(year, 1975, 1995)) %>% 
  ungroup() %>% 
  summarize(mean = mean(cap, na.rm = TRUE),
            sd = sd(cap, na.rm = TRUE))

cap_anomaly <- cap_month %>% 
  mutate(anomaly = (cap - ref_val$mean)/ref_val$sd) %>% 
  mutate(date = lubridate::ymd(paste(t_month,"01", sep="-")), 
         month = lubridate::month(date),
         year = lubridate::year(date)) %>% 
  filter(month >= 9) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarize(cap_anomaly = mean(anomaly, na.rm = TRUE),
            col = factor(ifelse(cap_anomaly > 0, 2, 1))) %>% 
  mutate(date = lubridate::ymd(paste(year,"01-01", sep="-")))

# The CAP anomaly was calculated as the difference between the CAP indices each month
# and the 1975–1995 monthly average. This was then standardized by dividing the anomaly 
# by the 1975–1995 standard deviation.

saveRDS(cap_anomaly, file = "data/cap_anomaly.rds")

ggplot(cap_anomaly) +
  geom_bar(aes(x = date, y = cap_anomaly, fill = col), stat = "identity", show.legend = FALSE)+
  geom_hline(yintercept = 0, linewidth = .75) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_minimal() +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Standardized Cape Agulhas Pressure (CAP) anomaly",
       x = "",
       y = "")
# 
# miller_cap <- read_sheet("https://docs.google.com/spreadsheets/d/1nRAMYP-gvdpAwTR0f2iek2pS81Vopy5btuk6OgDYQAA/edit?gid=0#gid=0") %>%
#   select(year, CAP)
# 
# tt <- cap_anomaly %>%
#   left_join(miller_cap) %>%
#   mutate(CAP = as.numeric(CAP))
# 
# str(tt)
# ggplot(tt, aes(x = year)) +
#   geom_path(aes(y = CAP), color = "black") +
#   geom_path(aes(y = cap_anomaly), color = "red") +
#   geom_point(aes(y = CAP), color = "black") +
#   geom_point(aes(y = cap_anomaly), color = "red") +
#   scale_y_continuous(expand = expansion()) +
#   theme_minimal()
# 
# ggplot(tt, aes(x = CAP, y = cap_anomaly)) +
#   geom_point()

### Thermal spawning area ---
# The area of 16–19°C water was computed from the five Agulhas Bank survey 
# strata used during MCM spawner biomass surveys based on remotely sensed
# satellite SST data obtained during September and October each year.
agulhas_data <- readRDS("data/agulhas_sst.rds")

agulhas_tsa <- agulhas_data %>%
  mutate(t_month = format(t, "%Y-%m"),
         month = lubridate::month(t),
         year = lubridate::year(t)) %>% 
  filter(month %in% c(9,10)) %>% 
  group_by(lon, lat, year) %>%
  summarize(sst_mean = mean(sst, na.rm = TRUE)) %>% 
  as.data.frame()

sa_countries <- rnaturalearth::ne_countries(scale = 10,
                                            continent = "Africa",
                                            returnclass = "sf")

## https://geonode.goosocean.org/layers/geonode:agulhas_bank_zooplankton_monitoring/metadata_detail
spawner_sf <- read_sf(here::here("data/shapefiles/agulhas_bank_zooplankton_monitoring.shp"))
sa_crs <- st_crs(spawner_sf)
spawner_area <- st_area(spawner_sf)

# agulhas_month <- agulhas_tsa %>% 
#   filter()

agulhas_r <- reshape(agulhas_tsa, timevar="year", idvar = c("lon", "lat"), direction="wide")
agulhas_rast <- terra::rast(agulhas_r, type="xyz")


terra::crs(agulhas_rast) <- st_crs(sa_crs)$wkt
tsa_mask <- terra::mask(agulhas_rast, spawner_sf, touches = FALSE)
tsa_threshold <- terra::clamp(tsa_mask, lower = 16, upper = 19, values = FALSE)
# 
# ggplot() + 
#   tidyterra::geom_spatraster(data = tsa_threshold[[5]]) +
#   geom_sf(data = sa_countries) +
#   geom_sf(data = spawner_sf, color = "red", fill = "transparent") +
#   scale_fill_continuous(na.value = "transparent") +
#   coord_sf(ylim = c(-38, -34), xlim = c(16, 24), crs = sa_crs) + 
#   theme_bw() + 
#   NULL

# ggplot() + 
#   tidyterra::geom_spatraster_contour_filled(data = tsa_mask, aes(fill = after_stat(level)),
#                                      na.rm = TRUE) +
#   geom_sf(data = sa_countries) +
#   geom_sf(data = spawner_sf, color = "black", fill = "transparent") +
#   coord_sf(ylim = c(-38, -34), xlim = c(16, 24), crs = sa_crs) + 
#   theme_bw() + 
#   NULL

tsa <- agulhas_tsa %>% 
  select(year) %>%
  distinct() %>%
  mutate(area = terra::expanse(tsa_threshold, unit = "m")$area,
         spawner_area = spawner_area,
         proportion = as.numeric(area/ spawner_area),
         proportion = ifelse(proportion >= 1, 1, proportion))

saveRDS(tsa, file = "data/tsa.rds")

# tsa_plot <- ggplot(tsa, aes(x = year, y = proportion)) +
#   geom_path() +
#   geom_point() +
#   theme_minimal() +
#     labs(x = "", y = "",
#          title = "Thermal Spawning Area index",
#          subtitle = "proportion of Agulhas Bank SST within 16–19°C during September and October") +
#   NULL



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




## Calendar table
cal_tab <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/12rYNpri4UQ8UUCIN9HGW7xrI48_gzEauaK4UWPiJdMU/edit?gid=0#gid=0")

saveRDS(cal_tab, "data/calendar_table.rds")



# ggplot(catches, aes(x = year, y = value/1000, group = species, fill = species, color = species)) +
#   geom_path() +
#   # geom_bar(stat = "identity") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   labs(x = "", y = "tonnes (thousands)",
#        title = "Small pelagic catches") + 
#   NULL


## Agulhas bank (Huggett et al 2023)
agulhas_dat <- readxl::read_xlsx("data/Huggett_et_al_Timeseries_AB.xlsx") %>% 
  filter(area == "ALL") %>%
  select(year, sst_AB = tempsurf, chlaship_AB = fluosurf,
         copepod_AB = total_mgC_m2, 
         calanus_AB = Cal_mgC_m2, SmCal_mgC_m2, Oithona_mgC_m2, Oncaea_mgC_m2) %>% 
  mutate(smcopepod_AB = rowSums(select(., contains("mgC_m2"))))

# tempsurf - Mean sea temperature (°C) near the surface (5 m depth)
# fluosurf - Mean chlorophyll a (mg m-3) near the surface (5 m depth)
# fluo30m - Mean chlorophyll a (mg m-3) at a depth of 30 m
# total_mgC_m2 - Mean integrated total copepod biomass (mg C m-2) in the upper 200 m of the water column
# Cal_mgC_m2 - Mean integrated biomass (mg C m-2) of Calanus agulhensis (all stages) in the upper 200 m of the water column
# SmCal_mgC_m2 - Mean integrated biomass (mg C m-2) of small calanoid copepods (mainly Paracalanidae and Clausocalanidae) in the upper 200 m of the water column
# Oithona_mgC_m2 - Mean integrated biomass (mg C m-2) of Oithonidae in the upper 200 m of the water column
# Oncaea_mgC_m2 - Mean integrated biomass (mg C m-2) of Oncaeidae in the upper 200 m of the water column

mean_chla_ab <- read.csv("https://repository.ocean.gov.za/index.php/s/fBF6LYP6mTtY7YP/download?path=&files=AB_shelf_openocean_seasonal_mean_reconstructed_chla_timeseries.csv")
mean_chla_sb <- read.csv("https://repository.ocean.gov.za/index.php/s/ZsFSnxDbPompNJN/download?path=&files=SB_shelf_openocean_seasonal_mean_reconstructed_chla_timeseries.csv")

mean_chla <- mean_chla_ab %>% 
  left_join(mean_chla_sb) %>% 
  pivot_longer(-year, names_to = "variable", values_to = "value") %>% 
  separate_wider_delim(variable, delim = "_", names = c("area", "type", "season", "var")) %>% 
  filter(type == "shelf",
         season == "summer") %>% 
  mutate(variable = paste0(var, "_", area)) %>% 
  select(year, variable, value) %>% 
  pivot_wider(names_from = "variable", values_from = "value")

# ggplot(mean_chla, aes(x = year, y = value, color = season)) +
#   geom_path() +
#   facet_wrap(type~area, scales = "free_y") +
#   theme_minimal() +
#   labs(color = "", y = "", x = "") +
#   theme(legend.position = "bottom")

micro_sb <- read.csv("https://repository.ocean.gov.za/index.php/s/bnsJp3Rf3ryEoDf/download?path=&files=SB_shelf_openocean_seasonal_mean_reconstructed_microphytoplankton_proportion_timeseries.csv")
nano_sb <- read.csv("https://repository.ocean.gov.za/index.php/s/e9DtYPtgsAeXHcN/download?path=&files=SB_shelf_openocean_seasonal_mean_reconstructed_nanophytoplankton_proportion_timeseries.csv")
pico_sb <- read.csv("https://repository.ocean.gov.za/index.php/s/2dEn9XN4KZrYXKF/download?path=&files=SB_shelf_openocean_seasonal_mean_reconstructed_picophytoplankton_proportion_timeseries.csv")

micro_ab <- read.csv("https://repository.ocean.gov.za/index.php/s/pc9YTctjmswKy8s/download?path=&files=AB_shelf_openocean_seasonal_mean_reconstructed_microphytoplankton_proportion_timeseries.csv")
nano_ab <- read.csv("https://repository.ocean.gov.za/index.php/s/exfdZrFSjy9frGE/download?path=&files=AB_shelf_openocean_seasonal_mean_reconstructed_nanophytoplankton_proportion_timeseries.csv")
pico_ab <- read.csv("https://repository.ocean.gov.za/index.php/s/X7RM2L9ifKioQwg/download?path=&files=AB_shelf_openocean_seasonal_mean_reconstructed_picophytoplankton_proportion_timeseries.csv")

pp_size_dat <- micro_sb %>% 
  left_join(nano_sb) %>% 
  left_join(pico_sb) %>% 
  left_join(micro_ab) %>% 
  left_join(nano_ab) %>% 
  left_join(pico_ab) %>% 
  pivot_longer(-year, names_to = "variable", values_to = "value") %>% 
  separate_wider_delim(variable, delim = "_", names = c("area", "type", "season", "var", "unit")) %>% 
  filter(type == "shelf",
         season == "summer") %>% 
  mutate(var = gsub("phytoplankton", "", var),
         variable = paste0(var, "_", area)) %>% 
  select(year, variable, value) %>% 
  pivot_wider(names_from = "variable", values_from = "value")

# ggplot(pp_size_dat, aes(x = year, y = value, color = var)) +
#   geom_path() +
#   facet_grid(area~season, scales = "free_y") +
#   theme_minimal() +
#   labs(color = "", y = "", x = "") +
#   theme(legend.position = "bottom")
# 
# 
# sb_dat_pp <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1s2FCTq4-hFfVZ83U7k_RTLJxmAwIhOroCwhs7oD9gQE/edit?gid=1899541669#gid=1899541669",
#                                     sheet = 2) %>% 
#   mutate(species = gsub("phytoplankton", "", species),
#          species = gsub("all", "chla", species),
#          species = case_when(area == "southern benguela" ~ paste0(species, "_SB"),
#                              area == "agulhas bank" ~ paste0(species, "_AB"),
#                              # grep("all", species, value = TRUE) ~ ,
#                              TRUE ~ species)) %>% 
#   select(year, species, value) %>%
#   pivot_wider(names_from = species, values_from = value)

sb_dat_zp <- bind_rows(googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1s2FCTq4-hFfVZ83U7k_RTLJxmAwIhOroCwhs7oD9gQE/edit?gid=1899541669#gid=1899541669",
                                                 sheet = 3),
                       googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1s2FCTq4-hFfVZ83U7k_RTLJxmAwIhOroCwhs7oD9gQE/edit?gid=1899541669#gid=1899541669",
                                                 sheet = 1)) %>% 
  mutate(species = case_when(grepl("small", species) ~ "smallzp",
                             grepl("medium|large", species) ~ "mdlgzp",
                             grepl("all", species) ~ "copepodabund", 
                             TRUE ~ species),
         species = case_when(area == "southern benguela" ~ paste0(species, "_SB"),
                             TRUE ~ species)) %>% 
  filter(season %in% c("summer", "autumn")) %>% 
  group_by(year, species) %>% 
  summarize(value = sum(value, na.rm = TRUE)) %>% 
  select(year, species, value) %>% 
  pivot_wider(names_from = species, values_from = value)

ltl_dat <- sb_dat_zp %>% 
  full_join(agulhas_dat) %>% 
  full_join(mean_chla) %>% 
  full_join(pp_size_dat)

saveRDS(ltl_dat, file = "data/ltl_dat.rds")



sb_sf <- read_sf(here::here("data/shapefiles/provinces.shp")) %>% 
  st_crop(xmin = 8, ymin = -38, xmax = 20, ymax = -29)
sa_crs <- st_crs(sb_sf)

ab_sf <- read_sf(here::here("data/shapefiles/agulhas_bank_zooplankton_monitoring.shp")) %>% 
  st_transform(sa_crs)

xlims <- c(10, 22)
ylims <- c(-38, -28)
res <- 1
bath_filename <- sprintf("marmap_coord_%s;%s;%s;%s_res_%s.csv",
                         xlims[1], ylims[1], xlims[2], ylims[2], res)

sa_bath <- marmap::getNOAA.bathy(lon1 = xlims[1], lon2 = xlims[2],
                                 lat1 = ylims[1], lat2 = ylims[2],
                                 resolution = res,
                                 keep = TRUE) 

bath_sp <- marmap::as.xyz(sa_bath) %>% 
  mutate(V3 = ifelse(V3 > 0 | V3 < -1000, NA, V3)) %>% 
  sf::st_as_sf(coords = c("V1","V2"), crs = sa_crs) %>% 
  sf::st_intersection(sb_sf) %>% 
  sf::st_transform(sa_crs)

## Agulhas Bank SST indexes
# info("ncdcOisst21Agg")
# This function downloads and prepares data based on user provided start and end dates
OISST_dl <- function(time_df, latitude = c(-28, -38), longitude = c(10, 24)){
  OISST_dat <- rerddap::griddap(datasetx = "ncdcOisst21Agg",
                                url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                                time = c(time_df$start, time_df$end), 
                                zlev = c(0, 0),
                                latitude = latitude,
                                longitude = longitude,
                                fields = "sst")$data %>% 
    dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) %>% 
    dplyr::rename(t = time, sst = sst, lon = longitude, lat = latitude) %>% 
    dplyr::select(lon, lat, t, sst) %>% 
    stats::na.omit()
}

# Date download range by start and end dates per year
dl_sst_years <- data.frame(date_index = 1:6,
                           start = c("1982-01-01", "1990-01-01", 
                                     "1998-01-01", "2006-01-01", 
                                     "2014-01-01", "2020-01-01"),
                           end = c("1989-12-31", "1997-12-31", 
                                   "2005-12-31", "2013-12-31", 
                                   "2019-12-31", "2023-12-31"))

OISST_data <- dl_sst_years %>% 
  dplyr::group_by(date_index) %>% 
  dplyr::group_modify(~OISST_dl(.x)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(lon, lat, t, sst)

# saveRDS(OISST_data, file = "data/oisst_sa.rds")
# 

sa_sst <- OISST_data %>%
  mutate(t_month = format(t, "%Y-%m"),
         month = lubridate::month(t),
         year = lubridate::year(t),
         lag_year = ifelse(month == 12, year +1, year)) %>%
  filter(month %in% c(12, 1, 2)) %>% 
  group_by(lag_year, lon, lat) %>% 
  summarize(sst_mean = mean(sst, na.rm = TRUE)) %>% 
  rename(year = lag_year) %>% 
  as.data.frame()

ab_r <- reshape(sa_sst, timevar = "year", idvar = c("lon", "lat"), direction = "wide")
ab_rast <- terra::rast(ab_r, type = "xyz")

terra::crs(ab_rast) <- st_crs(sa_crs)$wkt
ab_mask <- terra::mask(x = ab_rast, mask = ab_sf, touches = TRUE)

ab_means <- lapply(ab_mask, function(x) global(x, 'mean', na.rm = TRUE))

ab_sst <- data.frame(year = 1982:2024,
                     sst = do.call(rbind.data.frame, ab_means))

sb_r <- reshape(sa_sst, timevar = "year", idvar = c("lon", "lat"), direction = "wide")
sb_rast <- terra::rast(sb_r, type = "xyz")

terra::crs(sb_rast) <- st_crs(sa_crs)$wkt
sb_mask <- terra::mask(x = sb_rast, mask = bath_sp, touches = TRUE)

sb_means <- lapply(sb_mask, function(x) global(x, 'mean', na.rm = TRUE))
sb_sst <- data.frame(year = 1982:2024,
                     sst = do.call(rbind.data.frame, sb_means))

summer_sst <- bind_rows(ab_sst %>%  mutate(variable = "sst_AB"),
                        sb_sst %>%  mutate(variable = "sst_SB"))

# ggplot(summer_sst, aes(x = year, y = mean, color = variable)) +
#   geom_path() +
#   theme_minimal() +
#   theme(legend.position = "bottom")
saveRDS(summer_sst, file = "data/summer_sst.rds")
# 
# ggplot() + 
#   geom_sf(data = country) +
#   geom_tile(data = bat_sp, aes(x = V1, y = V2, fill = V3)) +
#   # geom_sf(data = pp_sf) +
#   # geom_sf(data = ab_sf) +
#   # geom_contour(data = bat_sp,
#   #              aes(x = V1, y = V2, z = V3),
#   #              binwidth = 100, color = "grey85", size = 0.1) +
#   # geom_contour(data = bat_sp,
#   #              aes(x = V1, y = V2, z = V3),
#   #              breaks = -1000, color = "grey85", size = 0.5) +
#   geom_sf(data = country) +
#   coord_sf(xlim = xlims, 
#            ylim = ylims) +
#   labs(x = "Longitude",  y = "Latitude", fill = "Depth (m)") +
#   theme_minimal()


# install.packages("readxl")
# install.packages("googlesheets4")
library(dplyr)
library(tidyr)
library(ggplot2)
library(rerddap)
library(googlesheets4)
library(terra)
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
info("ncdcOisst21Agg")
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

saveRDS(agulhas_data, file = "data/agulhas_sst.rds")
agulhas_data <- readRDS("data/agulhas_sst.rds")

agulhas_zone <- agulhas_data %>% 
  mutate(zone = case_when(between(lon, 19, 20) & between(lat, -35, -34) ~ "WABC",
                          between(lon, 20, 22) & between(lat, -35, -34) ~ "CABC",
                          between(lon, 22, 23) & between(lat, -35, -34) ~ "EABC",
                          between(lon, 20, 22) & between(lat, -36, -35) ~ "CABO",
                          TRUE ~ NA_character_),
         t_month = format(agulhas_data$t, "%Y-%m")) %>% 
  na.omit(zone) %>% 
  group_by(t_month, zone) %>%
  summarize(sst_mon = mean(sst, na.rm = TRUE),
            sst_cv = sd(sst, na.rm = TRUE)/sst_mon)

agulhas_year <- agulhas_zone %>% 
  ungroup() %>% 
  mutate(year = lubridate::year(paste(t_month,"01", sep="-"))) %>% #, 
         # year = format(agulhas_zone$date, "%Y")) %>%
  group_by(zone, year) %>%
  summarize(sst = mean(sst_mon, na.rm = TRUE)) %>% 
  mutate(date = lubridate::ymd(paste(year,"01-01", sep="-")))

agulhas_gradient <- agulhas_zone %>% 
  select(-sst_cv) %>% 
  filter(zone %in% c("CABO", "CABC")) %>% 
  pivot_wider(names_from = zone, values_from = sst_mon) %>% 
  mutate(sst_gradient = CABO-CABC,
         date = lubridate::ymd(paste(t_month,"01", sep="-")))

ggplot(agulhas_year, aes(x = date, y = sst, group = zone, color = zone))+
  geom_path() + 
  facet_wrap(~zone)+
  theme_minimal() +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y") +
  labs(title = "SST of Agulhas sub-domains",
       x = "",
       y = "")

ggplot(agulhas_gradient %>% filter(t_month < "2005-12"), aes(x = date, y = sst_gradient))+
  geom_path() + 
  geom_point() +
  theme_minimal() +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y") +
  labs(title = "Central Agulhas Bank Offshore - Central Agulhas Bank Coastal",
       x = "",
       y = "")

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

## Thermal spawning area
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


library(tidyterra)
ggplot() + 
  tidyterra::geom_spatraster(data = tsa_threshold[[5]]) +
  geom_sf(data = sa_countries) +
  geom_sf(data = spawner_sf, color = "red", fill = "transparent") +
  scale_fill_continuous(na.value = "transparent") +
  coord_sf(ylim = c(-38, -34), xlim = c(16, 24), crs = sa_crs) + 
  theme_bw() + 
  NULL

ggplot() + 
  tidyterra::geom_spatraster_contour_filled(data = tsa_threshold, aes(fill = after_stat(level)),
                                     na.rm = TRUE) +
  geom_sf(data = sa_countries) +
  geom_sf(data = spawner_sf, color = "black", fill = "transparent") +
  coord_sf(ylim = c(-38, -34), xlim = c(16, 24), crs = sa_crs) + 
  theme_bw() + 
  NULL


# terra::contour(agulhas_rast)
# terra::expanse(tsa_threshold, unit = "m")$area

tsa <- agulhas_tsa %>% 
  select(year) %>%
  distinct() %>%
  mutate(area = terra::expanse(tsa_threshold, unit = "m")$area,
         spawner_area = spawner_area,
         proportion = as.numeric(area/ spawner_area))

ggplot(tsa, aes(x = year, y = proportion)) +
  geom_path() +
  geom_point() +
  theme_minimal() +
  NULL


## https://geonode.goosocean.org/layers/geonode:agulhas_bank_zooplankton_monitoring/metadata_detail


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
cal_tab <- read_sheet("https://docs.google.com/spreadsheets/d/12rYNpri4UQ8UUCIN9HGW7xrI48_gzEauaK4UWPiJdMU/edit?gid=0#gid=0")

saveRDS(cal_tab, "data/calendar_table.rds")



# ggplot(catches, aes(x = year, y = value/1000, group = species, fill = species, color = species)) +
#   geom_path() +
#   # geom_bar(stat = "identity") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   labs(x = "", y = "tonnes (thousands)",
#        title = "Small pelagic catches") + 
#   NULL

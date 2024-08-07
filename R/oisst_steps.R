library(dplyr)
library(ggplot2)
library(tidyr)
library(rerddap)

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
    dplyr::rename(t = time, temp = sst, lon = longitude, lat = latitude) %>% 
    dplyr::select(lon, lat, t, temp) %>% 
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

test1 <- OISST_sub_dl(dl_years[1,])

base::system.time(
  OISST_data <- dl_years %>% 
    dplyr::group_by(date_index) %>% 
    dplyr::group_modify(~OISST_sub_dl(.x)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(lon, lat, t, temp)
) # 518 seconds, ~100 seconds per batch

library(ggplot2)
sst_plot <- OISST_month %>%
# OISST_data %>% 
  dplyr::filter(t_month == "2023-12")
  

ggplot() + 
  ggplot2::geom_sf(data = sa_countries, color = "grey60", size = 0.25) +
  # ggplot2::ggplot(data = sst_plot, aes(x = lon, y = lat)) +
  ggplot2::geom_tile(data = sst_plot, aes(x = lon, y = lat, fill = temp_cv)) +
  # ggplot2::borders() + # Activate this line to see the global map
  ggplot2::scale_fill_viridis_c() +
  ggplot2::coord_sf(crs = crs, xlim = c(0, 20), ylim = c(-5, -50)) +
  # ggplot2::coord_quickmap(expand = F) +
  ggplot2::labs(x = NULL, y = NULL, fill = "SST (°C)") +
  ggplot2::theme(legend.position = "bottom")


base::saveRDS(OISST_data, file = "data/oisst.Rds")

## 2) North America layer
sa_countries <- rnaturalearth::ne_countries(scale = 10,
                                            continent = "Africa",
                                            returnclass = "sf") %>%
  sf::st_transform(crs = crs)
head(OISST_data)
OISST_month <- OISST_data %>%
  mutate(t_month = format(OISST_data$t, "%Y-%m")) %>%
  group_by(t_month, lon, lat) %>%
  summarize(temp_mean = mean(temp, na.rm = TRUE),
            temp_cv = sd(temp, na.rm = TRUE)/temp_mean)

long_term_mean <- OISST_month %>%
  filter(t_month >= "1990-01",
         t_month <= "2020-12") %>%
  ungroup() %>%
  summarize(ltm = mean(temp_mean, na.rm = TRUE)) %>%
  pull(ltm)

benguela_nino <- OISST_month %>%
  select(t_month, temp_mean) %>%
  summarize(mean = mean(temp_mean, na.rm = TRUE),
            bni = mean - long_term_mean)

ggplot(benguela_nino, aes(x = t_month, y = bni)) +
  geom_line()

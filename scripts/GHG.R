library(here)
library(raster)
library(ggplot2)
library(stars)
library(tidyr)
library(viridis)
library(RColorBrewer)
source('scripts/common.R')
# world 
GHG <- raster('data/GHG/v6.0_CO2_org_short-cycle_C_2015_TOTALS.0.1x0.1.nc')
GHG[GHG==0] <- NA
plot(GHG)

library(tidync)
library(tidyverse)
library(viridis)
#Load netcdf file, extract data into tibble dataframe and filter out zero values:
nc_143 <- tidync::tidync('data/GHG/v6.0_CO2_org_short-cycle_C_2015_TOTALS.0.1x0.1.nc') %>%
  tidync::hyper_tibble() %>%
  dplyr::filter(emi_co2 > 0)
#plot the emi_co2 values:
ggplot(data = nc_143, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = emi_co2)) +
  scale_fill_viridis(option = 'A', trans = "log")+
  coord_fixed()


library(ncdf4)
GHG <- nc_open('data/GHG/v6.0_CO2_excl_short-cycle_org_C_2015_TOTALS.0.1x0.1.nc')
lon <- ncvar_get(GHG, "lon")
lat <- ncvar_get(GHG, "lat")
#dates <- as.Date("1900-01-01") + ncvar_get(bom, "time")
emi_co2 <- ncvar_get(GHG, "emi_co2")
dimnames(emi_co2) <- list(lon, lat)

m <- emi_co2 %>%
  reshape2::melt(varnames = c("lon", "lat")) %>%
  subset(!is.na(value)) %>% 
  subset(value>0)
head(m)
summary(m$value)
GHG <- ggplot(m, aes(x = lon, y = lat, fill = value)) + 
  geom_tile() + 
  scale_fill_gradientn(colors = brewer.pal(9, "Blues"), trans = "log") + 
  labs(title = "total_emi_co2 (kg/year)") + 
  coord_fixed()
ggsave(plot=GHG, filename = 'GHG.png', width = 6,height = 4)





ggplot()+
  geom_stars(data = GHG)+ #downsample = c(5,5,1)
  scale_fill_distiller(palette = 'RdYlGn', 'GHG', na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

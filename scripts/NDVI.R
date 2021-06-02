library(here)
library(raster)
library(stringr)
library(ggplot2)
library(stars)
library(viridis)
source('scripts/common.R')

NDVI <- raster('input_data/NDVI/ndvi2015.tif')

beginCluster()

P3_NDVI <- NDVI %>% 
  projectRaster(Mask_5km) %>% 
  resample(Mask_5km, method = 'ngb') %>% 
  mask(Mask_5km,
       filename = here('output_raster/Indicator_5km', sprintf("P3_NDVI%s.tif", 2015)),
       overwrite = TRUE)

endCluster()

# vis
P3_NDVI2015 <- read_stars('output_raster/Indicator_5km/P3_NDVI2015.tif')

fig_NDVI <- ggplot() +
  geom_stars(data = P3_NDVI2015) + 
  geom_sf(data = china_aea, color='grey80', fill='transparent', size=0.1) +
  scale_fill_distiller(palette = 'BrBG', direction = 1, 'NDVI',na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = fig_NDVI, filename = 'figures/fig_NDVI.png', w=8, h=6)



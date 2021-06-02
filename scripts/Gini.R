library(here)
library(raster)
library(ggplot2)
library(stars)
library(viridis)
library(RColorBrewer)
source('scripts/common.R')
# read nc ----
Gini <- raster('input_data/Gini/gini_2010.tif')
plot(Gini)

# project & resample ----
beginCluster()

P1_Gini2010 <- Gini %>% 
  crop(china_84) %>% # CHina extent
  projectRaster(Mask_5km) %>% 
  resample(Mask_5km, method='ngb') # method='ngb'

endCluster()

P1_Gini2010[is.na(P1_Gini2010[])] <- 0 
P1_Gini2010 <- P1_Gini2010 %>% 
  mask(Mask_5km,
       filename = here('output_raster/Indicator_5km', sprintf("P1_Gini%s.tif", 2010)),
       overwrite = TRUE)

plot(P1_Gini2010)

# Vis ----
P1_Gini2010 <- read_stars('output_raster/Indicator_5km/P1_Gini2010.tif')

fig_Gini <- ggplot() +
  geom_stars(data = P1_Gini2010) + #downsample = c(5,5,1)
  geom_sf(data = china_aea, color='grey20', fill='transparent', size=0.1) +
  scale_fill_distiller(palette = 'RdBu', 'Gini',na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = fig_Gini, filename = 'figures/fig_Gini.png', w=8, h=6)



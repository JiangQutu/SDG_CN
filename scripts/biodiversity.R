library(here)
library(raster)
library(ggplot2)
library(stars)
library(viridis)
source('scripts/common.R')

# Species Richness - the raw IUCN ranges (version 2017-3) for amphibians, birds and mammals
# https://www.iucnredlist.org/resources/other-spatial-downloads#species_selection_and_processing
SR_IUCN <- raster('input_data/IUCN/IUCN2017/Richness_version_2017_3/Richness_all.tif')

beginCluster()

SR_CN_aea <- SR_IUCN %>% 
  crop(china_84) %>% 
  projectRaster(Mask_5km) %>% 
  resample(Mask_5km, method='ngb') %>% 
  mask(Mask_5km, 
       filename=here('output_raster','Indicator_5km','P3_SR.tif'),
       overwrite = TRUE)

endCluster()

# vis
P3_SR <- read_stars('output_raster/Indicator_5km/P3_SR.tif')

fig_SR <- ggplot()+
  geom_stars(data = P3_SR)+
  geom_sf(data = china_aea, fill='transparent', color='grey80', size=0.1)+
  #Species Richness
  scale_fill_viridis(option = 'C','SR',na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

ggsave(plot = fig_SR, filename = 'figures/fig_SR.png', w=8, h=6)





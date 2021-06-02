library(here)
library(raster)
library(ggplot2)
library(stars)
library(viridis)
source('scripts/common.R')
# world 
Access <- raster('input_data/Access/2015_accessibility_to_cities_v1.0.tif')

# crop China
beginCluster()

Ac_CN_aea <- Access %>% 
  crop(china_84) %>% 
  projectRaster(Mask_5km) %>% 
  resample(Mask_5km) %>% # method='ngb'
  mask(Mask_5km, 
       filename=here('output_raster','Indicator_5km','P2_Access2015.tif'),
       overwrite = TRUE)

endCluster()

# vis
P2_Access <- read_stars('output_raster/Indicator_5km/P2_Access2015.tif')
P2_Access <- read_stars('output_raster/Indicator_5km/P2_Access2015.tif') %>% 
  as.data.frame() %>% na.omit() %>% rename('Access'='P2_Access2015.tif') %>% 
  mutate(Access_log = log_trans(Access))

fig_Access <- ggplot()+
  geom_tile(data = P2_Access, aes(x,y,fill=Access_log))+ #downsample = c(5,5,1)
  geom_sf(data = china_aea, fill='transparent', color='grey80', size=0.1)+
  # 2015_accessibility_to_cities 
  scale_fill_viridis(option = 'B', 'Accessibility', direction = -1, na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = fig_Access, filename = 'figures/fig_Access.png', w=8, h=6)


# by stars
fig_Access <- ggplot()+
  geom_stars(data = P2_Access)+ #downsample = c(5,5,1)
  geom_sf(data = china_aea, fill='transparent', color='grey80', size=0.1)+
  # 2015_accessibility_to_cities 
  scale_fill_viridis(option = 'C', 'Accessibility', na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

ggsave(plot = fig_Access, filename = 'figures/fig_Access.png', w=8, h=6)



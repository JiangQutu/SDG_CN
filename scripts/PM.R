library(here)
library(raster)
library(stringr)
library(ggplot2)
library(stars)
library(viridis)
library(ncdf4)
source('scripts/common.R')
# read nc ----
ls_nc <- list.files(path = 'data/PM', pattern = '.nc')
PM_r <- raster(here('data/PM',ls_nc[4]))
plot(PM_r)

# project & resample ----
beginCluster()

P1_PM2015 <- PM_r %>% 
  projectRaster(Mask_5km) %>% 
  resample(Mask_5km, method = 'ngb') %>% 
  mask(Mask_5km,
       filename = here('output_raster/Indicator_5km', sprintf("P1_PM%s.tif", 2015)),
       overwrite = TRUE)

endCluster()

# Vis ----
#plot(PM2015)
P1_PM2015 <- read_stars('output_raster/Indicator_5km/P1_PM2015.tif')

fig_PM <- ggplot() +
  geom_stars(data = P1_PM2015) + #downsample = c(5,5,1)
  geom_sf(data = china_aea, color='grey80', fill='transparent', size=0.1) +
  # scale_fill_gradientn(colours=rev(c("#D73027","#FC8D59","#FEE08B","#D9EF8B","#91CF60","#1A9850")),
  #                      values = c(0,0.1,0.3,0.6,0.8,1),'PM2.5',
  #                      breaks = c(0,20,40,60,80,100),
  #                      label = c(0,20,40,60,80,100), na.value='transparent')+
  scale_fill_distiller(palette = 'RdYlGn', 'PM2.5',na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = fig_PM, filename = 'figures/fig_PM.png', w=8, h=6)









PM_stars <- read_stars(here('data/PM',ls_nc[4]))
#PM_aea <- PM_stars %>% st_set_crs(4326) %>% st_transform_proj(crs = aea)
# ggplot() +
#   geom_stars(data = PM_stars, downsample = c(5,5,1)) + #downsample = c(10,10,1)
#   theme_void() +
#   coord_fixed() +
#   scale_fill_viridis('PM2.5', na.value='transparent') +
#   scale_x_discrete(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0))

PM_df <- PM_stars %>% as.data.frame() %>% na.omit() %>% 
  setNames(c('x','y','PM25')) %>% 
  mutate(PM25 = as.numeric(gsub('[ug/m3]','', PM25))) 
head(PM_df)

beginCluster()

PM_1km <- rasterFromXYZ(PM_df, crs=CRS('+init=epsg:4326')) %>% 
  projectRaster(china_mask_aea_1km) %>% 
  resample(china_mask_aea_1km, method = 'ngb') %>% 
  mask(china_mask_aea_1km,
       filename = here('output_raster/PM_1km', sprintf("PM%s_aea_1km.tif", 2015)),
       overwrite = TRUE)

endCluster()

# vis
PM_stars <- read_stars('output_raster/PM_1km/P1_PM2015.tif')
PM_df <- read_stars('output_raster/PM_1km/P1_PM2015.tif') %>% 
  as.data.frame() %>% na.omit %>% setNames(c('x','y','PM2.5'))
head(PM_df)

PM_df$PM2.5[PM_df$PM2.5 > 100] <- 100
summary(PM_df$PM2.5)

fig_PM <- ggplot() +
  geom_tile(data = PM_df, aes(x,y, fill=PM2.5)) + 
  geom_sf(data = china_aea, color='grey80', fill='transparent', size=0.1) +
  theme_void() +
  scale_fill_distiller(palette = 'RdYlGn', 'PM2.5',na.value='transparent')+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = fig_PM, filename = 'figures/fig_PM.png', w=8, h=6)


# by stars
fig_PM <- ggplot() +
  geom_stars(data = PM_aea, downsample = c(5,5,1)) + 
  geom_sf(data = china_aea, color='grey80', fill='transparent', size=0.1) +
  # scale_fill_gradientn(colours=rev(c("#D73027","#FC8D59","#FEE08B","#D9EF8B","#91CF60","#1A9850")),
  #                      values = c(0,0.1,0.3,0.6,0.8,1),'PM2.5',
  #                      breaks = c(0,20,40,60,80,100),
  #                      label = c(0,20,40,60,80,100), na.value='transparent')+
  scale_fill_distiller(palette = 'RdYlGn', 'PM2.5',na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = fig_PM, filename = 'figures/fig_PM.png', w=8, h=6)








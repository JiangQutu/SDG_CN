library(here)
library(raster)
library(dplyr)
library(stars)
library(ggplot2)
source('scripts/common.R')
fl_UGS <- list.files(path = 'data/Urban/UGS_2015', pattern = '.tif$')
fl_UISA <- list.files(path = 'data/Urban/UISA_2015', pattern = '.tif$')

UGS_Beijing <- read_stars(here('data/Urban/UGS_2015',fl_UGS[2])) %>% 
  as.data.frame() %>% na.omit()
head(UGS_Beijing)



UGS_Beijing_r <- raster(here('data/Urban/UGS_2015',fl_UGS[2]))
library(gdalUtils)
system.time(test <- gdalwarp(UGS_Beijing_r, 
                           Mask_5km, 
                           r='average', 
                           #multi=TRUE, 
                           #tr=res(UGS_Beijing_r)*10, 
                           output_Raster=TRUE))
plot(test)

a3 <- gdalwarp(f, f2 <- tempfile(fileext='.tif'), r='mode', 
               multi=TRUE, tr=res(a)*10, output_Raster=TRUE)



Beijing_aea <- china_84 %>% st_transform(UGS_Beijing_r@crs) %>% filter(NAME=='北京市')

ggplot() +
  geom_tile(data = UGS_Beijing, aes(x, y, fill=UGS_2015_Beijing.tif)) + 
  scale_fill_distiller(palette = 'BrBG', direction = 1, 'UGS',na.value='transparent') +
  geom_sf(data = Beijing_aea, color='grey50', fill='transparent', size=0.2) +
  ggtheme_map() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))


ggplot() +
  geom_stars(data = UGS_Beijing, downsample = c(20,20,1)) + 
  scale_fill_distiller(palette = 'BrBG', direction = 1, 'FVC',na.value='transparent') +
  geom_sf(data = Beijing_aea, color='grey80', fill='transparent', size=0.1) +
  ggtheme_map() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))






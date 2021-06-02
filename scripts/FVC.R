library(here)
library(raster)
library(stringr)
library(ggplot2)
library(stars)
library(viridis)
source('scripts/common.R')
# 一带一路及其毗邻区域1 km分辨率年最大植被覆盖度数据集（2015）
FVC <- raster('input_data/FVC/MuSyQ-MFVC-1km-2015/MuSyQ-MFVC-1km-2015.tif')

beginCluster()

P3_FVC <- FVC %>% 
  projectRaster(Mask_5km) %>% 
  resample(Mask_5km, method = 'ngb') %>% 
  mask(Mask_5km,
       filename = here('output_raster/Indicator_5km', sprintf("P3_FVC%s.tif", 2015)),
       overwrite = TRUE)

endCluster()

# vis
P3_FVC2015 <- read_stars('output_raster/Indicator_5km/P3_FVC2015.tif')

fig_FVC <- ggplot() +
  geom_stars(data = P3_FVC2015) + 
  geom_sf(data = china_aea, color='grey80', fill='transparent', size=0.1) +
  scale_fill_distiller(palette = 'BrBG', direction = 1, 'FVC',na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = fig_FVC, filename = 'figures/fig_FVC.png', w=8, h=6)






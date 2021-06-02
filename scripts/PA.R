library(here)
library(raster)
library(ggplot2)
library(stars)
library(sf)
library(viridis)
source('scripts/common.R')
# PA boundary Source: 
# Shrestha, N., Xu, X., Meng, J., & Wang, Z. (2021). Vulnerabilities of protected lands in the face of climate and human footprint changes. Nature communications, 12(1), 1-9.
PA <- read_sf('input_data/PA/CHN_PA_new.shp') %>% st_transform(crs = aea)
head(PA)

PA_rst <- fasterize::fasterize(PA, Mask_5km, field = "Area") %>% 
  reclassify(cbind(NA, NA, 0), right=FALSE) %>% 
  mask(Mask_5km) %>% 
  writeRaster(filename = here('output_raster','Indicator_5km','P3_PA.tif'), overwrite=TRUE)
# plot(PA_rst)

P3_PA <- read_stars('output_raster/Indicator_5km/P3_PA.tif') %>% 
  as.data.frame() %>% na.omit() %>% setNames(c('x','y','PA')) %>% 
  mutate(PA_log = log_trans(PA))
head(P3_PA)

fig_PA <- ggplot()+
  geom_tile(data = P3_PA, aes(x,y, fill=PA_log)) + 
  #geom_stars(data = P3_PA)+
  geom_sf(data = china_aea, fill='transparent', color='grey20', size=0.2)+
  scale_fill_viridis(na.value='transparent')+
  ggtheme_map()+
  #theme(legend.position = 'none')+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

ggsave(plot = fig_PA, filename = 'figures/fig_PA.png', w=8, h=6)




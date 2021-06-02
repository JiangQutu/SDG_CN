library(here)
source('scripts/common.R')
# GCO2, GNOX, GSO2 10km*10km shp-----
GCO2 <- read_sf('input_data/GHG/CHRED/GCO2.shp') %>% select(OBJECTID, CO2=sum)
GNOX <- read_sf('input_data/GHG/CHRED/GNOX.shp') %>% select(OBJECTID, NOX=sum)
GSO2 <- read_sf('input_data/GHG/CHRED/GSO2.shp') %>% select(OBJECTID, SO2=SUM)

GCO2$NOX <- GNOX$NOX; GCO2$SO2 <- GSO2$SO2
GHG <- GCO2 %>% mutate(GHG = CO2+NOX+SO2)
head(GHG)

# Rasterize ----
GHG_r <- fasterize(GHG, Mask_5km, field = "GHG") %>% 
  mask(Mask_5km) %>% 
  writeRaster(filename = here('output_raster','Indicator_5km','P3_GHG.tif'), overwrite=TRUE)

# Vis -----
options(scipen=999)
GHG_df <- GHG_r %>% raster::as.data.frame(xy=T, na.rm=T)
GHG_df$emissions <- cut(GHG_df$layer,
                     breaks = c(-Inf,100,1000,10000,25000,50000,100000,500000,1000000,Inf),
                     labels = c(0,100,1000,10000,25000,50000,100000,500000,'>1000000')
                     )

cols_rg <- rev(colorRampPalette(brewer.pal(11, 'RdYlGn')[1:10])(255))

fig_GHG <- ggplot()+
  #geom_tile(data = GHG_df, aes(x,y, fill=emissions))+
  geom_tile(data = GHG_df, aes(x,y, fill=log10(layer+1)))+
  #scale_fill_brewer(palette = 'RdYlGn', direction = -1)+
  scale_fill_gradientn(colors = cols_rg, na.value = 'white')+
  #scale_fill_distiller(palette = 'RdYlGn', 'Emission/ton (log)',direction = -1)+
  geom_sf(data = China_aea, fill='transparent', color='grey20', size=0.2)+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

ggsave(plot = fig_GHG, filename = 'figures/fig_GHG.png', w=8, h=6)


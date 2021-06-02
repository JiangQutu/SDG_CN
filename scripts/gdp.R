library(here)
library(raster)
library(stringr)
library(ggplot2)
source('scripts/common.R')
rastlist <- list.files(path = 'data/gdp', pattern = '.tif$', full.names = T) # 万元/平方千米

# gdp 1km resample to mask and then aggregate to 10km ----
beginCluster()
for(rast in rastlist){  
  # rast = rastlist[1] gdp1995.tif
  # rast = rastlist[2] gdp2000.tif
  yr <- as.numeric(as.character(str_sub(basename(rast), 4, 7)))
  message('Processing raster gdp', yr)
  
  raster(rast) %>% # here('data','gdp', rast)
    raster::resample(china_mask_aea_1km, method = 'bilinear') %>% # method = 'ngb'
    raster::aggregate(fact=10, fun=sum, na.rm = T,
                      filename = here('output_raster/gdp_10km', sprintf("gdp%s_aea_10km.tif", yr)),
                      overwrite = TRUE)
}
endCluster()

# vis gdp_10km 2000-2015 ----
gdplist <- list.files(path = 'output_raster/gdp_10km', pattern = '.tif$', full.names = T)
gdp <- stack(gdplist[2:5]) %>% 
  rasterToPoints() %>% as.data.frame() %>% 
  setNames(c('x', 'y', 'GDP_2000','GDP_2005','GDP_2010','GDP_2015')) %>%
  tidyr::gather(year, gdp, 3:6) %>% 
  mutate(gdp_log = log_trans(gdp))
head(gdp); summary(gdp)

max_gdp <- max(gdp$gdp, na.rm = TRUE)
labels_gdp <- c(0, 100, 20000, max_gdp)
breaks_gdp <- log_trans(labels_gdp)

fig_gdp <- ggplot() +
  ggtheme_map(base_size = 9) +
  geom_tile(data = gdp, aes(x, y, fill = gdp_log)) +
  scale_fill_viridis_c(breaks = breaks_gdp, labels = labels_gdp, na.value = 'grey80') +
  geom_sf(data = china_aea, fill = 'transparent', color = 'grey40', size = .10) +
  facet_wrap(~year) +
  coord_sf(datum = NA) + ### ditch graticules
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = 'bold', size = 12),
    legend.background = element_blank(),
    legend.key.width = unit(.25, 'cm'),
    legend.title = element_blank())

ggsave(plot = fig_gdp, filename = 'figures/fig_gdp.png', w=8, h=6)









# gdp 1km resample ----
rastlist <- list.files(path = 'data/gdp', pattern = '.tif$', full.names = T) # 万元/平方千米

beginCluster()
for(rast in rastlist){  
  # rast = rastlist[1] gdp1995.tif
  # rast = rastlist[2] gdp2000.tif
  yr <- as.numeric(as.character(str_sub(basename(rast), 4, 7)))
  message('Processing raster gdp', yr)
  
  raster(rast) %>% # here('data','gdp', rast)
    raster::resample(china_mask_aea_1km, method = 'bilinear',
                     filename = here('output_raster/gdp_1km', sprintf("gdp%s_aea_1km.tif", yr)),
                     overwrite = TRUE) # method = 'ngb'
}
endCluster()

# vis gdp_1km 2000-2015 ----
gdplist <- list.files(path = 'output_raster/gdp_1km', pattern = '.tif$', full.names = T)

beginCluster()
gdp <- stack(gdplist[2:5]) %>% 
  rasterToPoints() %>% as.data.frame() %>% 
  setNames(c('x', 'y', 'GDP_2000','GDP_2005','GDP_2010','GDP_2015')) %>%
  tidyr::gather(year, gdp, 3:6) %>% 
  mutate(gdp_log = log_trans(gdp))
head(gdp)

max_gdp <- max(gdp$gdp, na.rm = TRUE)
labels_gdp <- c(0, 100, 20000, max_gdp)
breaks_gdp <- log_trans(labels_gdp)

fig_gdp <- ggplot() +
  ggtheme_map(base_size = 9) +
  geom_tile(data = gdp, aes(x, y, fill = gdp_log)) +
  scale_fill_viridis_c(breaks = breaks_gdp, labels = labels_gdp, na.value = 'grey80') +
  geom_sf(data = china_aea, fill = 'transparent', color = 'grey40', size = .10) +
  facet_wrap(~year) +
  coord_sf(datum = NA) + ### ditch graticules
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = 'bold', size = 12),
    legend.background = element_blank(),
    legend.key.width = unit(.25, 'cm'),
    legend.title = element_blank())
ggsave(plot = fig_gdp, filename = 'figures/fig_gdp_1km.png', w=8, h=6)

beginCluster()






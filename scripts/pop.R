library(here)
library(raster)
library(stringr)
library(ggplot2)
source('scripts/common.R')
rastlist <- list.files(path = 'data/pop', pattern = '.tif$', full.names = T) # 人/平方公里

# pop 1km resample to mask and then aggregate to 10km ----
beginCluster()
for(rast in rastlist){  
  # rast = rastlist[1] pop1995.tif
  # rast = rastlist[2] pop2000.tif
  yr <- as.numeric(as.character(str_sub(basename(rast), 5, 8)))
  message('Processing raster pop', yr)
  
  raster(rast) %>% # here('data','pop', rast)
    raster::resample(china_mask_aea_1km, method = 'ngb') %>% # method = 'bilinear'
    raster::aggregate(fact=10, fun=sum, na.rm = T,
                      filename = here('output_raster/pop_10km', sprintf("pop%s_aea_10km.tif", yr)),
                      overwrite = TRUE)
}
endCluster()

# vis pop_10km 2000-2015 ----
poplist <- list.files(path = 'output_raster/pop_10km', pattern = '.tif$', full.names = T)
pop <- stack(poplist[2:5]) %>% 
  rasterToPoints() %>% as.data.frame() %>% 
  setNames(c('x', 'y', 'pop_2000','pop_2005','pop_2010','pop_2015')) %>%
  tidyr::gather(year, pop, 3:6) %>% 
  mutate(pop_log = log_trans(pop))
head(pop); summary(pop)

max_pop <- max(pop$pop, na.rm = TRUE)
labels_pop <- c(0, 100, 20000, max_pop)
breaks_pop <- log_trans(labels_pop)

fig_pop <- ggplot() +
  ggtheme_map(base_size = 9) +
  geom_tile(data = pop, aes(x, y, fill = pop_log)) +
  scale_fill_viridis_c(breaks = breaks_pop, labels = labels_pop, na.value = 'grey80') +
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

ggsave(plot = fig_pop, filename = 'figures/fig_pop.png', w=8, h=6)










# pop 1km resample ----
rastlist <- list.files(path = 'data/pop', pattern = '.tif$', full.names = T) # 人/平方公里

beginCluster()
for(rast in rastlist){  
  # rast = rastlist[1] pop1995.tif
  # rast = rastlist[2] pop2000.tif
  yr <- as.numeric(as.character(str_sub(basename(rast), 5, 8)))
  message('Processing raster pop', yr)
  
  raster(rast) %>% # here('data','pop', rast)
    raster::resample(china_mask_aea_1km, method = 'ngb', # method = 'bilinear'
                     filename = here('output_raster/pop_1km', sprintf("pop%s_aea_1km.tif", yr)),
                     overwrite = TRUE) 
}
endCluster()

# vis pop_1km 2000-2015 ----
poplist <- list.files(path = 'output_raster/pop_1km', pattern = '.tif$', full.names = T)

beginCluster()
pop <- stack(poplist[2:5]) %>% 
  rasterToPoints() %>% as.data.frame() %>% 
  setNames(c('x', 'y', 'pop_2000','pop_2005','pop_2010','pop_2015')) %>%
  tidyr::gather(year, pop, 3:6) %>% 
  mutate(pop_log = log_trans(pop))
head(pop)

max_pop <- max(pop$pop, na.rm = TRUE)
labels_pop <- c(0, 100, 20000, max_pop)
breaks_pop <- log_trans(labels_pop)

fig_pop <- ggplot() +
  ggtheme_map(base_size = 9) +
  geom_tile(data = pop, aes(x, y, fill = pop_log)) +
  scale_fill_viridis_c(breaks = breaks_pop, labels = labels_pop, na.value = 'grey80') +
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
ggsave(plot = fig_pop, filename = 'figures/fig_pop_1km.png', w=8, h=6)

beginCluster()




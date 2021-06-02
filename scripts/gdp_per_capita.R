library(here)
library(raster)
library(stringr)
library(ggplot2)
source('scripts/common.R')
# load gdp & pop
#gdplist <- list.files(path = 'output_raster/gdp_1km', pattern = '.tif$', full.names = T)
#poplist <- list.files(path = 'output_raster/pop_1km', pattern = '.tif$', full.names = T)
gdplist <- list.files(path = 'data/gdp', pattern = '.tif$', full.names = T)
poplist <- list.files(path = 'data/pop', pattern = '.tif$', full.names = T)

# year 2015
gdp <- raster(gdplist[5])
pop <- raster(poplist[5])

gdp_per_capita <- overlay(gdp, pop, fun = function(x,y) {(x/y)},
                          filename = here('output_raster/Economy_gdp_per_capita_1km', sprintf("gdp_per_capita%s_1km.tif", 2015)),
                          overwrite = TRUE)
plot(gdp_per_capita)

# calculate gdp per capita
beginCluster()
for (i in 1:5) {
  gdp <- raster(gdplist[i])
  pop <- raster(poplist[i])
  yr <- as.numeric(as.character(str_sub(basename(gdplist[i]), 4, 7)))
  message('Processing gdp/pop ', yr)
  gdp_per_capita <- overlay(gdp, pop, fun = function(x,y) {(x/y)},
                            filename = here('output_raster/Economy_gdp_per_capita_1km', sprintf("gdp_per_capita%s_1km.tif", yr)),
                            overwrite = TRUE)
}
endCluster()

# vis gdp per capita ----
ls <- list.files(path = 'output_raster/Economy_gdp_per_capita_1km', pattern = '.tif$', full.names = T)

ppp2015 <- raster(ls[5]) %>% 
  rasterToPoints() %>% as.data.frame() %>% 
  setNames(c('x', 'y', 'gdp_per_capita')) %>% 
  #tidyr::gather(year, pop, 3:6) %>% 
  mutate(ppp_log = log_trans(gdp_per_capita))

max_ppp <- max(ppp2015$gdp_per_capita, na.rm = TRUE)
labels_ppp <- c(0, 100, max_ppp)
breaks_ppp <- log_trans(labels_ppp)

fig_ppp <- ggplot() +
  ggtheme_map(base_size = 9) +
  geom_tile(data = ppp2015, aes(x, y, fill = ppp_log)) +
  scale_fill_viridis_c(breaks = breaks_ppp, labels = labels_ppp, na.value = 'grey80') +
  geom_sf(data = china_aea, fill = 'transparent', color = 'grey40', size = .10) +
  #facet_wrap(~year) +
  coord_sf(datum = NA) + ### ditch graticules
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = 'bold', size = 12),
    legend.background = element_blank(),
    legend.key.width = unit(.25, 'cm'),
    legend.title = element_blank())
ggsave(plot = fig_ppp, filename = 'figures/fig_ppp_1km.png', w=8, h=6)








library(here)
library(raster)
library(stringr)
library(viridis)
library(ggplot2)
source('scripts/common.R')
# Data from 'Chen, Q., Ye, T., Zhao, N., Ding, M., Ouyang, Z., Jia, P., ... & Yang, X. (2020). 
# Mapping China’s regional economic activity by integrating points-of-interest and remote sensing data with random forest. Environment and Planning B: Urban Analytics and City Science, 2399808320951580.'
# 基于兴趣点和多源遥感数据的中国大陆各产业GDP空间分布数据集（2010年）
# 2010年中国大陆农业、林业、牧业、渔业、第二产业、第三产业以及GDP总量-1km栅格数据集
# unit: million CNY/km2 百万元/平方千米

beginCluster()

GDP_tertiary <- raster('data/gdp/GDP2010/GDP_tertiary sector.tif') 
GDP_total <- raster('data/gdp/GDP2010/GDP_total.tif')
GDP_ter_tot <- overlay(GDP_tertiary, GDP_total, fun = function(x,y) {(x/y)})

endCluster()


beginCluster()

GDP_tertiary <- raster('data/gdp/GDP2010/GDP_tertiary sector.tif') 
GDP_total <- raster('data/gdp/GDP2010/GDP_total.tif')
GDP_ter_tot <- overlay(GDP_tertiary, GDP_total, fun = function(x,y) {(x/y)}) %>% 
  resample(china_mask_aea_1km,
           filename = here('output_raster/gdp_1km', sprintf("GDP%s_ter_tot.tif", 2010)),
           overwrite = TRUE)

endCluster()

# vis 
GDP_ter_tot_df <- GDP_ter_tot %>% 
  rasterToPoints() %>% as.data.frame() %>% 
  setNames(c('x', 'y', 'GDP_ter_tot')) %>%
  mutate(GDP_ter_tot2 = case_when(GDP_ter_tot > 0.8 ~ 'D:0.8~1',
                                  GDP_ter_tot >= 0.6 & GDP_ter_tot < 0.8 ~ 'C:0.6~0.8',
                                  GDP_ter_tot >= 0.2 & GDP_ter_tot < 0.6 ~ 'B:0.2~0.6',
                                  GDP_ter_tot >= 0 & GDP_ter_tot < 0.2 ~ 'A:0~0.2'))
  
head(GDP_ter_tot_df)

fig_gdp <- ggplot() +
  ggtheme_map(base_size = 9) +
  geom_tile(data = GDP_ter_tot_df, aes(x, y, fill = GDP_ter_tot2)) +
  scale_fill_viridis_d()+
  #scale_fill_viridis_c(limits = c(0,1), na.value = 'grey80') +
  geom_sf(data = china_aea, fill = 'transparent', color = 'grey40', size = .10) +
  coord_sf(datum = NA) + ### ditch graticules
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = 'bold', size = 12),
    legend.background = element_blank(),
    legend.key.width = unit(.25, 'cm'),
    legend.title = element_blank())
ggsave(plot = fig_gdp, filename = 'figures/fig_GDP_ter_tot2.png', w=8, h=6)




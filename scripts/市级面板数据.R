library(here)
library(raster)
library(ggplot2)
library(sf)
library(dplyr)
library(stringr)
library(viridis)
library(readxl)
source('scripts/common.R')
# Map_city, Krassovsky椭球为基准，投影方式为Albers投影 ----
City_Map <- st_read('input_data/map_raw/2019行政区划/市.shp') %>% 
  st_transform(aea) %>% 
  mutate(City_name = substring(市, 1, 2)) # 用市前两字匹配

#City_Map_df <- as.data.frame(City_Map) %>% select(1:5) # 查看速度快

# 城市统计年鉴地级市面板数据 ----
options(scipen = 200)
City_Stat <- 
  #readRDS('data/Stat_data/中国城市统计年鉴地级市面板数据.rds') %>% 
  readxl::read_xlsx('input_data/Stat_data/中国城市统计年鉴地级市面板数据.xlsx') %>% 
  filter(年份==2015) %>% 
  select(City="城市", City_code=as.character("行政区划代码"), 
           TGDP='第三产业占GDP的比重_全市_百分比',
           #GDP="地区生产总值_当年价格_全市_万元",
           PPP="人均地区生产总值_全市_元", 
           #Beds="医院、卫生院床位数_全市_张",
           #Invest="环境污染治理投资额_全市_万元" ,
           Edu="每万人在校大学生数_全市_人"
           ) %>% 
  mutate(City_name = substring(City, 1, 2)) %>% 
  mutate(Province_code = as.numeric(paste0(substring(City_code, 1, 2), '0000'))) %>% 
  arrange(Province_code)

# 省PPP, Edu均值用以填充 ----
Province <- City_Stat %>% group_by(Province_code) %>% 
  summarise(PPP_Province = mean(PPP, na.rm=T),
            Edu_Province = mean(Edu, na.rm=T),
            TGDP_Province = mean(TGDP, na.rm=T))

# Join ----
# 人均地区生产总值_全市_元; 每万人在校大学生数_全市_人
City_level <- City_Map %>%
  left_join(City_Stat, by = 'City_name') %>% 
  left_join(Province, by = c('省代码'='Province_code'))

# Gap fill ----
City_fill <- City_level %>% 
  mutate(PPP_fill = ifelse(is.na(PPP), PPP_Province/10000, PPP/10000), # 万元
         Edu_fill = ifelse(is.na(Edu), as.integer(Edu_Province), as.integer(Edu)),
         TGDP_fill = ifelse(is.na(TGDP), TGDP_Province, TGDP)) # 人
head(City_fill)


# PPP:
City_level_PPP <- ggplot()+
  geom_sf(data = City_level, aes(fill = PPP), size=0.05)+
  scale_fill_distiller(palette = 'Greens', direction = 1)+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = City_level_PPP, filename = 'figures/City_PPP_miss.png', w=8, h=6)



City_PPP <- ggplot()+
  geom_sf(data = City_fill, aes(fill=PPP_fill), size=0.05)+
  scale_fill_distiller(palette = 'Greens', direction = 1)+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = City_PPP, filename = 'figures/City_PPP2015.png', w=8, h=6)


# Edu: 
City_level_Edu <- ggplot()+
  geom_sf(data = City_level, aes(fill = Edu), size=0.05)+
  scale_fill_distiller(palette = 'Blues', direction = 1)+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = City_level_Edu, filename = 'figures/City_Edu_miss.png', w=8, h=6)

City_Edu <- ggplot()+
  geom_sf(data = City_fill, aes(fill = Edu_fill), size=0.05)+
  scale_fill_distiller(palette = 'Blues', direction = 1)+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = City_Edu, filename = 'figures/City_Edu2015.png', w=8, h=6)

# TGDP: 
City_level_TGDP <- ggplot()+
  geom_sf(data = City_level, aes(fill = TGDP), size=0.05)+
  scale_fill_distiller(palette = 'Reds', direction = 1)+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = City_level, filename = 'figures/City_TGDP_miss.png', w=8, h=6)

City_TGDP <- ggplot()+
  geom_sf(data = City_fill, aes(fill = TGDP_fill), size=0.05)+
  scale_fill_distiller(palette = 'Reds', direction = 1)+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = City_TGDP, filename = 'figures/City_TGDP2015.png', w=8, h=6)

# Rasterize ----
City_PPP_rst <- fasterize(City_fill, Mask_5km, field = "PPP_fill") %>% 
  resample(Mask_5km, method = 'ngb') %>% 
  mask(Mask_5km, 
       filename = here('output_raster/Indicator_5km', sprintf("P2_PPP%s.tif", 2015)),
       overwrite = TRUE)

P2_PPP <- read_stars('output_raster/Indicator_5km/P2_PPP2015.tif')

fig_PPP <- ggplot() +
  geom_stars(data = P2_PPP) + #downsample = c(5,5,1)
  geom_sf(data = china_aea, color='grey80', fill='transparent', size=0.1) +
  scale_fill_distiller(palette = 'Greens', 'PPP', direction = 1, na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = fig_PPP, filename = 'figures/fig_PPP.png', w=8, h=6)


City_Edu_rst <- fasterize(City_fill, Mask_5km, field = "Edu_fill") %>% 
  resample(Mask_5km, method = 'ngb') %>% 
  mask(Mask_5km, 
       filename = here('output_raster/Indicator_5km', sprintf("P1_Edu%s.tif", 2015)),
       overwrite = TRUE)

P1_Edu <- read_stars('output_raster/Indicator_5km/P1_Edu2015.tif')

fig_Edu <- ggplot() +
  geom_stars(data = P1_Edu) + #downsample = c(5,5,1)
  geom_sf(data = china_aea, color='grey80', fill='transparent', size=0.1) +
  scale_fill_distiller(palette = 'Blues', 'Edu', direction = 1, na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = fig_Edu, filename = 'figures/fig_Edu.png', w=8, h=6)

# TGDP:
City_TGDP_rst <- fasterize(City_fill, Mask_5km, field = "TGDP_fill") %>% 
  resample(Mask_5km, method = 'ngb') %>% 
  mask(Mask_5km, 
       filename = here('output_raster/Indicator_5km', sprintf("P2_TGDP%s.tif", 2015)),
       overwrite = TRUE)

P2_TGDP <- read_stars('output_raster/Indicator_5km/P2_TGDP2015.tif')

fig_TGDP <- ggplot() +
  geom_stars(data = P2_TGDP) + #downsample = c(5,5,1)
  geom_sf(data = china_aea, color='grey80', fill='transparent', size=0.1) +
  scale_fill_distiller(palette = 'Reds', 'TGDP', direction = 1, na.value='transparent')+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
ggsave(plot = fig_TGDP, filename = 'figures/fig_TGDP.png', w=8, h=6)




"行政区划代码"
"城市"
"年份"
"人均地区生产总值_全市_元"
"地区生产总值增长率_全市_百分比"
"第三产业占GDP的比重_全市_百分比"
"人均肉类产量_全市_千克"
"人均蔬菜产量_全市_千克"
"人均水果产量_全市_千克"
"人均牛羊奶产量_全市_千克"

"年末实有耕地面积_全市_千公顷"
"年末总人口_全市_万人"

"教育事业费支出_全市_万元"
"科学技术支出_全市_万元"
"环境污染治理投资额_全市_万元" 
"一般工业固体废物综合利用率_全市_百分比"
"污水处理厂集中处理率_全市_百分比"
"生活垃圾无害化处理率_全市_百分比"
"人均绿地面积_市辖区_平方米每人"

"职工平均工资_全市_元"
"每万人在校大学生数_全市_人"
"医院、卫生院床位数_全市_张"
"居民人均生活用电量_市辖区_千瓦时"
"人均家庭生活用水量_市辖区_吨"











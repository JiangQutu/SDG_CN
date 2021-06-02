library(here)
library(raster)
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
# China county ----
# Krassovsky椭球为基准，投影方式为Albers投影
aea <- sp::CRS('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +datum=WGS84 +units=m +no_defs')
# Load China county boundary-国家地理信息中心; options = "ENCODING=GBK" !!!
# County_Map <- st_read('data/map_raw/县级行政界线/BOUNT_poly.shp', options = "ENCODING=GBK") %>% 
#   st_set_crs(4326) %>% st_transform(aea)
County_Map <- st_read('data/map_raw/2019行政区划/县.shp') %>% st_transform(aea) %>% 
  mutate(County_NAME = substr(NAME, 1, 2))
#head(County_Map)

head(County_Map)

# China county Statistics Year Book ----
# 国泰君安数据库 ---
#County_Stat <- readxl::read_excel("data/County/Rstata_2000_2019CountyStat.xlsx")
#saveRDS(County_Stat, 'data/County/Rstata_2000_2019CountyStat.Rda')
County_Stat <- readRDS('data/Stat_data/Rstata_2000_2019CountyStat.Rda')
# 县域统计年鉴 ---
# County_Stat <- readxl::read_excel("data/County/Rstata_1997_2018CountyStat.xlsx")
# saveRDS(County_Stat, 'data/County/Rstata_1997_2018CountyStat.Rda')
# County_Stat <- readRDS('data/Stat_data/Rstata_1997_2018CountyStat.Rda')
head(County_Stat)
#names(County_Stat)

# Select specific variables -----
# names(County_Stat)
# name_ind <- c("年份","县代码","县","市","省","区县政府经度","区县政府纬度","行政区域面积_平方公里","耕地面积_平方公里",
#               "地区生产总值_万元","第一产业生产总值_万元","第二产业生产总值_万元","第三产业生产总值_万元","人均生产总值_万元",
#               "年末常住人口数_万人","年末常住人口数_男性_万人","年末常住人口数_女性_万人","年末常住人口数_城镇_万人","年末常住人口数_乡村_万人",
#               "城市居民最低生活保障人数_人","农村居民最低生活保障人数_人",
#               "财政一般预算收入_万元","财政一般预算支出_万元","财政总收入_万元","科学事业费支出_万元",
#               "城镇居民人均可支配收入_元","农村居民人均可支配收入_元",
#               "农村居民人均纯收入_元","就业人员平均工资_元",
#               "互联网宽带接入用户数_万户","渔业产值_万元","农用化肥施用量_吨",
#               "卫生机构数_个","卫生机构人员数_人","卫生技术人员数_人","卫生机构实有床位数_张"
#               )
# name_ind <- c("年份","县代码","县","市","省",
#               "城镇居民人均可支配收入_元",
#               "农村居民人均可支配收入_元")
# Select year
County_Stat2015 <- County_Stat %>% 
  dplyr::select("年份","县代码","县","市","省",
                PPP="人均生产总值_万元"
                ) %>% 
  dplyr::filter(年份 %in% c('2015')) %>% 
  mutate(County_NAME = substr(县, 1, 2))

head(County_Stat2015)
head(County_Map)
# 按县名字匹配 ----
County_PPP <- County_Map %>% left_join(County_Stat2015, by = 'County_NAME')
County_PPP <- County_Map %>% left_join(County_Stat2015)

# 按县代码匹配
#County_2015 <- County_Map %>% left_join(County_Stat2015, by = c('PAC'='县代码'))

# 人均生产总值PPP -----  
fig_County_PPP <- ggplot()+
  geom_sf(data = County_PPP, aes(fill = PPP), color='grey60',size=0.05)+
  #scale_fill_brewer(palette = 'Greens',direction = 1, na.value='grey50')+
  scale_fill_distiller(palette = 'Greens',direction = 1, na.value='grey50')+
  theme_void()
ggsave(plot = fig_County_PPP, filename = 'figures/County_PPP2015.png', w=8, h=6)


# Gap fill ----
head(City_fill)
head(County_PPP)

City_fill_df <- City_fill %>% as.data.frame() %>% select(省, 市, PPP_fill)


County_fill <- County_PPP %>% 
  left_join(City_fill_df) %>% 
  mutate(PPP_final = ifelse(is.na(PPP), PPP_fill, PPP)) %>% # 万元 
  arrange(desc(PPP_final))
head(County_fill)

fig_County_PPP_fill <- ggplot()+
  geom_sf(data = County_fill, aes(fill = PPP_final), color='grey60',size=0.05)+
  #scale_fill_brewer(palette = 'Greens',direction = 1, na.value='grey50')+
  scale_fill_distiller(palette = 'Greens',direction = 1, na.value='grey50')+
  theme_void()
ggsave(plot = fig_County_PPP_fill, filename = 'figures/County_PPP_fill2015.png', w=8, h=6)


# Rasterize -----
source('scripts/common.R')
County_PPP_rst <- fasterize(County_fill, Mask_5km, field = "PPP_final")
plot(County_PPP_rst)

PPP <- County_PPP_rst %>% 
  resample(Mask_5km) %>% mask(Mask_5km)
plot(PPP)

writeRaster(PPP, 'output_raster/Indicator_5km/PPP.tif', overwrite=T)




# 县区市名称模糊匹配 -----
County_Map_Name <- County_Map %>% select(NAME99) %>% 
  as.data.frame() %>% select(NAME99) %>% na.omit() %>% 
  dplyr::filter(NAME99 != '台湾省')

County_Stat_Name <- County_Stat2015 %>% select('name'="县") %>% 
  dplyr::filter(name != 'NA')

diffs <- setdiff(County_Map_Name$NAME99, County_Stat_Name$name) # 466 unmatch 2015

library(Rwordseg)
County_Map_Name$split <- segmentCN(County_Map_Name$NAME99)

#creat for circulation
for (i in 1:dim(County_Map_Name)[1]) {
  message('processing', i)
  words <- unlist(County_Map_Name$split[i])
  len <- length(words)
  index <- 1
  County_Stat_Name$score <- 0
  while (index <= len) {
    for(j in 1:dim(County_Stat_Name)[1])
      if(grepl(words[index],County_Stat_Name$name[j])==T) {
        County_Stat_Name$score[j] <- County_Stat_Name$score[j]+1
      }
    index <- index+1
  }
  County_Map_Name[i,3] <- County_Stat_Name[which(County_Stat_Name$score==max(County_Stat_Name$score)),1][1]
  County_Map_Name[i,4] <- County_Stat_Name[which(County_Stat_Name$score==max(County_Stat_Name$score)),2][1]
}
names(County_Map_Name) <- c("dpp_name","split","bpp_name","score")
County_Map_Name[which(County_Map_Name$score==0),"bpp_name"] <- NA






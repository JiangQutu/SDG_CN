library(dplyr)
library(sf)
library(raster)
library(fasterize)
library(ggplot2)
library(RColorBrewer)

log_trans <- function(x) log10(x+1)

crs_84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# china_aea ----
# Albers (Asia North Albers Equal Area Conic) Projection
#aea <- '+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs'
#aea_proj4 <- CRS('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs')
csr_aea <- CRS('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +datum=WGS84 +units=m +no_defs')
# 国家基础地理信息系统数据 https://guangchuangyu.github.io/chinamap/
# load('data/china.rda')

# china_84 <- china %>% 
#   st_as_sf() %>%
#   st_set_crs(4326) %>% 
#   na.omit() %>%
#   filter(AREA > 0.01) %>% # remove small islands for better vis
#   group_by(NAME) %>%
#   summarize() %>%
#   st_cast("MULTIPOLYGON")
# 
# ggplot()+geom_sf(data = china_84, size=0.1)
# saveRDS(china_84, 'data/china_84.Rda')
China_84 <- readRDS('input_data/china_84.Rda')

# china_aea <- china %>% st_as_sf() %>%
#   st_set_crs(4326) %>% st_transform(crs = aea) %>%
#   na.omit() %>%
#   filter(AREA > 0.01) %>% # remove small islands for better vis
#   group_by(NAME) %>%
#   summarize() %>%
#   st_cast("MULTIPOLYGON")
# ggplot()+geom_sf(data = china_aea, size=0.1)
# saveRDS(china_aea, 'data/china_aea.Rda')
# st_write(China_aea, 'input_data/map_raw/map_aea/China_aea.shp',layer_options = "ENCODING=UTF-8",delete_layer = TRUE)
China_aea <- readRDS('input_data/china_aea.Rda')


# China Land aea mask 10km ----
# china_aea_10km <- raster(as(china_aea, "Spatial"), res=10000)
# china_mask_aea_10km <- fasterize::fasterize(china_aea, china_aea_10km)
# writeRaster(china_mask_aea_10km, 'data/china_mask_aea_10km.tif', overwrite = TRUE)
# Mask_10km <- raster('input_data/china_mask_aea_10km.tif')

# China Land aea mask 5km ----
# china_aea_5km <- raster(as(china_aea, "Spatial"), res=5000)
# china_mask_aea_5km <- fasterize::fasterize(china_aea, china_aea_5km)
# writeRaster(china_mask_aea_5km, 'data/china_mask_aea_5km.tif', overwrite = TRUE)
Mask_5km <- raster('input_data/china_mask_aea_5km.tif')


# China Land aea mask 1km ----
# china_aea_1km <- raster(as(china_aea, "Spatial"), res=1000)
# china_mask_aea_1km <- fasterize::fasterize(china_aea, china_aea_1km)
# writeRaster(china_mask_aea_1km, 'data/china_mask_aea_1km.tif', overwrite = TRUE)
# Mask_1km <- raster('input_data/china_mask_aea_1km.tif')


# # Crop & Projection & Mask ----
# moll_china <- st_transform(china_aea, crs = HF@crs)
# #bb <- st_bbox(moll_china)
# #ext <- c(xmin = bb[1], xmax = bb[3], ymin = bb[2], ymax = bb[4]) %>% extent()

### generic theme for maps
ggtheme_map <- function(base_size = 12) {
  theme(text             = element_text(family = 'Helvetica', color = 'gray30', size = base_size),
        plot.title       = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
        panel.background = element_blank(),
        legend.position  = 'right',
        panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        axis.text        = element_blank(),
        axis.title       = element_blank(),
        legend.key       = element_rect(colour = NA, fill = NA),
        axis.line        = element_blank()) # element_line(colour = "grey30", size = .5)) +
}




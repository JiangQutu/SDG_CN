library(here)
library(raster)
library(ggplot2)
library(stars)
library(viridis)
source('scripts/common.R')
fls <- list.files(path = 'output_raster/indicator_5km', pattern = '.tif$')

P1 <- stack(here('output_raster/indicator_5km', fls[1:3]))
plot(P1)
P2 <- stack(here('output_raster/indicator_5km', fls[4:6]))
plot(P2)
P3 <- stack(here('output_raster/indicator_5km', fls[7:9]))
plot(P3)

# Log trans ----
# P1_Edu log trans
for(i in 1) P1[[i]] <- log10(P1[[i]] + 1)

# P2 all log trans
for(i in 1:3) P2[[i]] <- log10(P2[[i]] + 1)
plot(P2)

# P3_PA log trans
for(i in 2) P3[[i]] <- log10(P3[[i]] + 1)
plot(P3[[2]])

Positive_Norm <- function(x) {
  min_value <- minValue(x)
  max_value <- maxValue(x)
  x <- (x - min_value) *100 / (max_value - min_value) + 0
  x
}
Negative_Norm <- function(x) {
  min_value <- minValue(x)
  max_value <- maxValue(x)
  x <- (max_value - x) *100 / (max_value - min_value) + 0
  x
}


# Standarization -----
for(i in 1) P1[[i]] <- Positive_Norm(P1[[i]])
for(i in 2:3) P1[[i]] <- Negative_Norm(P1[[i]])
plot(P1)

for(i in 2:3) P2[[i]] <- Positive_Norm(P2[[i]])
for(i in 1) P2[[i]] <- Negative_Norm(P2[[i]])
plot(P2)

for(i in 1:3) P3[[i]] <- Positive_Norm(P3[[i]])
plot(P3)

# Mean Score for each pillar ----
P1_score <- P1 %>% mean() %>% setNames('P1') #%>%
  #raster::as.data.frame(xy=T, na.rm=T)
plot(P1_score)

P2_score <- P2 %>% mean() %>% setNames('P2') #%>%
#raster::as.data.frame(xy=T, na.rm=T)
plot(P2_score)

P3_score <- P3 %>% mean() %>% setNames('P3') #%>%
#raster::as.data.frame(xy=T, na.rm=T)
plot(P3_score)

Pillars <- stack(P1_score, P2_score, P3_score) %>% 
  as.data.frame(xy=T, na.rm=T) %>% 
  tidyr::gather(type, score, 3:5)
head(Pillars)

fig_pillars <- ggplot()+
  geom_tile(data=Pillars, aes(x,y, fill=score))+
  scale_fill_distiller(palette = 'RdYlGn',direction=1)+
  geom_sf(data = china_aea, fill='transparent', color='grey50', size=0.1)+
  facet_wrap(~type)
ggsave(plot = fig_pillars, filename = 'figures/fig_pillars.png', w=15, h=6)



# Sustainability Socre ----
Sustainability <- mean(P1_score, P2_score, P3_score) %>% 
  writeRaster(filename=here('output_raster','Indicator_5km','Sustainability.tif'),
              overwrite = TRUE)

Sus_df <- Sustainability %>% raster::as.data.frame(xy=T, na.rm=T)

fig_Sus <- ggplot()+
  geom_tile(data = Sus_df, aes(x,y, fill=Sustainability))+
  scale_fill_distiller(palette = 'RdYlGn', direction = 1)+
  #scale_fill_brewer(palette = 'RdYlGn', direction = 1)+
  geom_sf(data = china_aea, fill='transparent', color='grey50', size=0.1)+
  ggtheme_map()+
  theme(legend.key.width = unit(0.25,'cm'),
        legend.key.height = unit(1,'cm'),
        legend.title = element_text(size = 10))+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

ggsave(plot = fig_Sus, filename = 'figures/fig_Sus2.png', w=8, h=6)

# Vis ----
library(classInt)
classIntervals(Sus_df$Sustainability, n = 11, style = 'fisher')

Sus_df$Sustainability <- cut(Sus_df$Sustainability,
                             breaks = c(0, 28, 38, 42, 47, 49, 51, 53, 55, 58, 62, 100),
                             labels = c('<28','28-38','38-42','42-47','47-49','49-51',
                                        '51-53','53-55','55-58','58-62','>62'))

fig_Sus <- ggplot()+
  geom_tile(data = Sus_df, aes(x,y, fill=Sustainability))+
  #scale_fill_distiller(palette = 'RdYlGn', direction = 1)+
  scale_fill_brewer(palette = 'RdYlGn', direction = 1)+
  geom_sf(data = china_aea, fill='transparent', color='grey80', size=0.1)+
  ggtheme_map()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

ggsave(plot = fig_Sus, filename = 'figures/fig_Sus.png', w=8, h=6)

# Compare with SDG ----
library(exactextractr)
Zonel_mean_Sus <- exact_extract(Sustainability, china_aea, append_cols = T, c('mean'))

China_Sus <- china_aea %>% left_join(Zonel_mean_Sus)

classIntervals(China_Sus$mean, n = 11, style = 'fisher')
China_Sus$Sustainability <- cut(China_Sus$mean,
                                breaks = c(0, 40, 42, 45, 48, 51, 53, 55, 58, 60, 100),
                                labels = c('<40','40-42','42-45','45-48','48-51','51-53',
                                           '53-55','55-58','58-60','>60'))

p_Sus <- ggplot()+
  geom_sf(data = China_Sus, aes(fill = Sustainability), size=0.3)+
  scale_fill_brewer(palette = 'RdYlGn', 'Sustainability', direction = 1)
  #scale_fill_distiller(palette = 'RdYlGn', 'Sustainability', direction = 1)
p_Sus
ggsave(plot = p_Sus, filename = 'figures/China_sus.png', w=8, h=6)


SDG <- read_excel("input_data/SDG.xlsx", sheet = 'Subnational') %>% 
  filter(Year == 2015) %>% select(Provinces, Name_CN, SDGIndex)

Sus_SDG <- China_Sus %>% 
  left_join(SDG, by = c('NAME'='Name_CN')) %>% 
  st_drop_geometry()
head(Sus_SDG)

library(ggpmisc)
library(ggrepel)
formula <- y ~ x
p_Sus_SDG <- ggplot(Sus_SDG, aes(mean, SDGIndex, label = Provinces))+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_text_repel(show.legend = F, size=3.5) +
  stat_poly_eq(formula = formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  xlab('Sustainability of this study')+
  ylab('SDG index, 2015')+
  #scale_x_continuous(limits = c(40, 66))+
  #scale_y_continuous(limits = c(40, 66))+
  coord_fixed()
ggsave(plot = p_Sus_SDG, filename = 'figures/Sus_SDG.png', w=8, h=6)

test <- Sus_SDG %>% na.omit()
cor(test$mean, test$SDGIndex)


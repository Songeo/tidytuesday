

library(tidyverse)
library(maps)
library(tmap)
library(sp)
theme_set(theme_bw())

# Datos ----
df_coffee_raw <- readxl::read_xlsx("data/week6_coffee_chains.xlsx")
df_coffee_raw %>% 
  data.frame %>% head
df_coffee_raw %>%  summary()
filter(df_coffee_raw, is.na(Longitude))

df_coffee_raw %>% 
  group_by(Brand) %>% 
  summarise(n_ctry = n_distinct(Country),
            n_obs = n())
df_coffee_raw$`Ownership Type` %>% table


# World map ----
map_wd <- map_data("world") 
map_wd %>% head
gg <- ggplot() + 
  geom_polygon(data = map_wd, 
               aes(x=long, y = lat, group = group), 
               fill = "mistyrose") + 
  geom_point(data = df_coffee_raw, 
             aes(x = Longitude, y = Latitude, 
                 color = Brand), 
             alpha = .3) + 
  coord_fixed(1.3) + 
  facet_wrap(~Brand)
ggsave(plot = gg, filename = "graphs/180507_worldmap.png", 
       width = 12, height = 8)

# Mexico ----
# Just mapping Mexico (my home)
map_mex <- map_wd %>% 
  filter(region == "Mexico")

df_coffee_raw_mx <- df_coffee_raw %>% 
  filter(Country %in% c("MX"))

df_coffee_raw_mx$`City` %>% unique()
df_coffee_raw_mx$`State/Province` %>% unique()

# mapping with ggplot
gg <- ggplot() + 
  geom_polygon(data = map_mex, 
               aes(x=long, y = lat, group = group), 
               fill = "gray80") + 
  geom_point(data = df_coffee_raw_mx, 
             aes(x = Longitude, y = Latitude, 
                 color = `State/Province` ), 
             alpha = .3) + 
  theme(legend.position = "none") + 
  coord_fixed(1.3) + 
  facet_wrap(~Brand)
ggsave(gg, filename = "graphs/180507_ggplot_mex.png", 
       width = 10, height = 7)

df_coffee_raw_mx %>% 
  group_by(`State/Province`) %>% 
  tally() 


# Shape files Mexico por estado ----
# Mapping with tmap, very similar to ggplot

# Shape files from mexico by state
dir("src/mex_edos_shapes")
shp_edo_rgdal <-  rgdal::readOGR("src/mex_edos_shapes/Mex_Edos.shp") %>% 
  sp::merge(read_csv("src/180507_mex_states.csv"), 
            by = "NOM_ENT") %>% 
  sp::merge(df_coffee_raw_mx %>% 
              group_by(`State/Province`) %>% 
              summarise(`num tiendas` = n()) )

# DF Polygone
class(shp_edo_rgdal)
shp_edo_rgdal@data %>% head()
shp_edo_rgdal@data %>% summary()

# Map polygons and bubles
tm_shape(shp_edo_rgdal) + 
  tm_fill(col = "num tiendas", 
          palette = "RdYlBu", 
          title = "Number stores in MEX",
          style="fixed",
          breaks=c(-Inf, 0, 1, 10, 
                   50, 100, 250, Inf),
          contrast=.7) + 
  tm_bubbles(size = "num tiendas",
             col = "red",
             title.size="Number of stores", 
             alpha = .3,
             contrast = 1) + 
  tm_borders() + 
  tm_text("State/Province", col = "gray40") +
  tm_style_gray() + 
  tm_format_World()
save_tmap(filename = "graphs/180507_tmap_mex.png", 
          width = 10, height = 7)




# Script adicional
{
# # function to obtain US county shape
# # https://github.com/mtennekes/tmap/blob/master/demo/USChoropleth/US_choropleth.R
# get_US_county_2010_shape <- function() {
#   dir <- tempdir()
#   download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", 
#                 destfile = file.path(dir, "gz_2010_us_050_00_20m.zip"))
#   unzip(file.path(dir, "gz_2010_us_050_00_20m.zip"), exdir = dir)
#   US <- read_shape(file.path(dir, "gz_2010_us_050_00_20m.shp"))
#   levels(US@data$NAME) <- iconv(levels(US@data$NAME), from = "latin1", to = "utf8")
#   US
# }
# # obtain US county shape
# US <- get_US_county_2010_shape()
# US@data %>% head
# data("World")
# World
# World@data %>% head
}



library(tidyverse)
library(maps)
library(tmap)
library(sp)
# library(tmaptools)

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

ggplot() + 
  geom_polygon(data = map_wd, 
               aes(x=long, y = lat, group = group), 
               fill = "white") + 
  geom_point(data = df_coffee_raw, 
             aes(x = Longitude, y = Latitude, 
                 color = Brand), 
             alpha = .3) + 
  coord_fixed(1.3) + 
  facet_wrap(~Brand)


# Mexico ----
map_mex <- map_wd %>% 
  filter(region == "Mexico")

df_coffee_raw_mx <- df_coffee_raw %>% 
  filter(Country %in% c("MX"))

df_coffee_raw_mx$`City` %>% unique()
df_coffee_raw_mx$`State/Province` %>% unique()

ggplot() + 
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

df_coffee_raw_mx %>% 
  group_by(`State/Province`) %>% 
  tally() 


# Shape files Mexico por estado ----
dir("src/mex_edos_shapes")
shp_edo_rgdal <-  rgdal::readOGR("src/mex_edos_shapes/Mex_Edos.shp") %>% 
  sp::merge(read_csv("src/180507_mex_states.csv"), 
            by = "NOM_ENT") %>% 
  sp::merge(df_coffee_raw_mx %>% 
              group_by(`State/Province`) %>% 
              summarise(`num tiendas` = n()) )

class(shp_edo_rgdal)
shp_edo_rgdal@data %>% head()
shp_edo_rgdal@data %>% summary()


tm_shape(shp_edo_rgdal) + 
  tm_bubbles(size = "num tiendas") + 
  tm_borders() 




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

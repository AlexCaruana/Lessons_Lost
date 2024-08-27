## Lessons Lost - ERDF & LIFE Data Analysis
# Figure 1 - EU ERDF & LIFE Heatmaps

install.packages('devtools')
install_github("thomasp85/patchwork")
install.packages("tidyr")
install.packages('tidyverse', dependencies = TRUE)
install.packages("vctrs")
install.package(leaflet)
install.packages('sf', dependencies = TRUE)
install.packages('scales', dependencies = TRUE)
install.packages('cowplot', dependencies = TRUE)
install.packages('ggthemes', dependencies = TRUE)
install.packages("ggarrange")


install.packages('eurostat', dependencies = TRUE)
install.packages("readxl")

library(readxl)
library(eurostat)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)
library(patchwork)
library(devtools)
library(vctrs)
library(tidyverse)
library(leaflet)
library(scales)
library(cowplot)
library(ggthemes)


getwd()
setwd('C:/Users/Alex/Desktop/Masters Thesis/Final Sorted Datasets - Github')
ERDF_map_data <- read_xlsx("ERDF & LIFE Extracted Datasets 1.0.xlsx", sheet = "1.2 ERDF Heatmap Dataset")
LIFE_map_data <- read_xlsx("ERDF & LIFE Extracted Datasets 1.0.xlsx", sheet = "1.3 LIFE Heatmap Dataset")

SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)

# Stretched map due to EU islands (e.g., Azores)
        SHP_0 %>% 
                ggplot() +
                geom_sf()
        
        EU28 <- eu_countries %>% 
                select(geo = code, name)
        
        SHP_28 <- SHP_0 %>% 
                select(geo = NUTS_ID, geometry) %>% 
                inner_join(EU28, by = "geo") %>% 
                arrange(geo) %>% 
                st_as_sf()

# Resized map to focus on Europe
        SHP_28 %>% 
                ggplot() +
                geom_sf() +
                scale_x_continuous(limits = c(-10, 35)) +
                scale_y_continuous(limits = c(35, 65))
        
        # "#1B4B36" original green colour  
# ERDF Analysis
        erdf_data_shp <- ERDF_map_data %>% 
                select(geo, values) %>% 
                inner_join(SHP_28, by = "geo") %>% 
                st_as_sf()
     
        ERDF_Heatmap_Funding_Quantity <- erdf_data_shp %>% 
                ggplot(aes(fill = values)) +
                geom_sf() + # show.legend=FALSE
                scale_fill_continuous(low="white", high="#295E11", 
                                      guide="colorbar",na.value="grey", breaks = seq(0, 240, 30), limits = c(0, 240)) +   
                scale_x_continuous(limits = c(-10, 35)) +
                scale_y_continuous(limits = c(35, 65)) +
                theme_bw()+
                theme(axis.line = element_line(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.ticks.x=element_blank())+
                guides(fill = guide_colorbar(barwidth = 1, barheight = 15, title="Funding (€ Millions)", draw.ulim = TRUE, draw.llim = TRUE))+
                theme(legend.spacing.y = unit(0.5, "cm"))
        
        erdf_data_shp_totalproj <- ERDF_map_data %>% 
                select(geo, Proj_amount) %>% 
                inner_join(SHP_28, by = "geo") %>% 
                st_as_sf()
        
        ERDF_Heatmap_Total_Projects <- erdf_data_shp_totalproj %>% 
                ggplot(aes(fill = Proj_amount)) +
                geom_sf() + # show.legend=FALSE
                scale_fill_continuous(low="white", high="#295E11", 
                                      guide="colorbar",na.value="grey", breaks = seq(0, 500, 50), limits = c(0, 500)) +   
                scale_x_continuous(limits = c(-10, 35)) +
                scale_y_continuous(limits = c(35, 65)) +
                theme_bw()+
                theme(axis.line = element_line(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.ticks.x=element_blank())+
                guides(fill = guide_colorbar(barwidth = 1, barheight = 15, title="Total Projects", draw.ulim = TRUE, draw.llim = TRUE)) +
                theme(legend.spacing.y = unit(0.5, "cm"))
        
        # "#831212" original red colour
        # Blue-Yellow: #025196 - #FDB338
        # Blue-Red: #2F67B1 - #BF2C23
        # Green-Purple: #295E11 - #58094F"
        
# LIFE Analysis
        life_data_shp <- LIFE_map_data %>% 
                select(geo, values) %>% 
                inner_join(SHP_28, by = "geo") %>% 
                st_as_sf()
        
        LIFE_Heatmap_Funding_Quantity <- life_data_shp %>% 
                ggplot(aes(fill = values)) +
                geom_sf() +
                scale_fill_continuous(low="white", high="#58094F", 
                                      guide="colorbar",na.value="grey", breaks = seq(0, 240, 30), limits = c(0, 240)) +  
                scale_x_continuous(limits = c(-10, 35)) +
                scale_y_continuous(limits = c(35, 65)) +
                theme_bw()+
                theme(axis.line = element_line(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.ticks.x=element_blank()) +
                guides(fill = guide_colorbar(barwidth = 1, barheight = 15, title="Funding (€ Millions)", draw.ulim = TRUE, draw.llim = TRUE)) +
                theme(legend.spacing.y = unit(0.5, "cm"))
        
        life_data_shp_totalproj <- LIFE_map_data %>% 
                select(geo, Proj_amount) %>% 
                inner_join(SHP_28, by = "geo") %>% 
                st_as_sf()
        
        LIFE_Heatmap_Total_Projects <- life_data_shp_totalproj %>% 
                ggplot(aes(fill = Proj_amount)) +
                geom_sf() +
                scale_fill_continuous(low="white", high="#58094F", 
                                      guide="colorbar",na.value="grey", breaks = seq(0, 90, 10), limits = c(0, 80)) +  
                scale_x_continuous(limits = c(-10, 35)) +
                scale_y_continuous(limits = c(35, 65)) +
                theme_bw()+
                theme(axis.line = element_line(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.ticks.x=element_blank()) +
                guides(fill = guide_colorbar(barwidth = 1, barheight = 15, title="Total Projects", draw.ulim = TRUE, draw.llim = TRUE)) +
                theme(legend.spacing.y = unit(0.5, "cm"))

# Merged
# Exported as 1000px x 800px
Figure_1 <- LIFE_Heatmap_Total_Projects + ERDF_Heatmap_Total_Projects + LIFE_Heatmap_Funding_Quantity + ERDF_Heatmap_Funding_Quantity + plot_annotation(tag_levels = 'A')




              
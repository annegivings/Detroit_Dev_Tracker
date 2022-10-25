install.packages('rvest')
install.packages('ggmap')
install.packages('sf')
library(rvest)
library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)
library(leaflet)
library(rgdal)
library(janitor)
library(devtools)
library(formattable)
library(tidyr)
library(reshape2)
library(data.table)
library(DT)
library(expss)
library(rsconnect)
library(ggmap)
library(raster)
library(sp)
library(sf)

setwd("C:/Users/annek/Desktop/R files/OZ_app")


#Scrape development tracker site
tracker <- read_html("https://developmenttracker.detourdetroiter.com/projects")
tracker

tracker %>%
  html_nodes(".font-normal") %>%
  html_text()

dev <- data.frame(
  address = tracker %>% html_nodes(".font-normal") %>% html_text()
)

dev$full_address <- paste(dev$address, " Detroit, MI")
locations <- mutate_geocode(dev, full_address)

clean_up <- subset(locations, is.na(locations$lon))
dev$full_address[86]<- paste("3107 16th St Detroit, MI")
locations <- mutate_geocode(dev, full_address)
save(locations, file="dev_locations.Rda")

# load map from OZ report
detroit <- readOGR(".", "City_of_Detroit_Boundary")
tracts <- readOGR(".", "Clean_DetroitTracts")
tracts$cat<- "OZ"
tracts$cat[tracts$Cat_map==0] <-"Eligible"
tracts$cat[tracts$Cat_map==4] <-"Contiguous"
tracts$cat[is.na(tracts$Cat_map)] <- "Not eligible"
pal <- colorFactor(
  palette = c('#f05821', '#28546b', '#808080','#b9d645'),
  domain = tracts$cat
)

#Open docs
df_table <- readRDS(file="master_table.Rda")
load(file="dev_locations.Rda")


leaflet()%>% 
    addPolygons(data=detroit, layerId=~label, 
                weight = 1, smoothFactor=.5, 
                opacity=1, 
                color="#444444"
    ) %>%
    addPolygons(data=tracts,layerId=~GEOID,
                weight=1, smoothFactor=0.5, opacity=1,
                fillOpacity=0.5,
                color = ~pal(cat),
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE)
    ) %>%
    addLegend(position="bottomright", 
              pal=pal, values=tracts$cat,
              title="OZ status", opacity=1
    ) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(data=locations, lng = ~lon, lat = ~lat,
                     radius = 2, fillOpacity = 1)


tracts_sf <- tracts %>% st_as_sf()
locations_sf <- locations %>% st_as_sf(coords = c("lon", "lat"),
                                       crs=4269)

st_crs(locations_sf)
st_crs(tracts_sf)

sf_summary <- tracts_sf %>% 
  mutate(counts = lengths(st_intersects(., locations_sf)))

final_table <- sf_summary %>%
  group_by(cat) %>%
  summarize(sum(counts))

# 86% of new development is happening in OZs

tracts_sf %>% count(cat)
sf_summary <- sf_summary %>%
  mutate(dev = case_when(
    counts == 0 ~ 0,
    counts > 0 ~ 1,
  ))
sf_summary %>% count(cat, dev)

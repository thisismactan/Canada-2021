library(tidyverse)
library(reshape2)
library(randomForest)
library(lubridate)
library(Hmisc)
library(scales)
library(mvnfast)
library(readr)
library(rvest)
library(xml2)
library(MCMCpack)

# Mapping
library(sp)
library(sf)
library(rmapshaper)
library(rgdal)
library(leaflet)

party_order <- c("NDP", "Green", "Bloc", "Liberal", "Conservative", "People's")

party_colors <- c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Green" = "green4", 
                  "People's" = "midnightblue", "Bloc" = "#8ECEF9")

party_fullnames <- c("Liberal" = "Liberal Party", "Conservative" = "Conservative Party", "NDP" = "New Democratic Party", 
                     "Green" = "Green Party", "People's" = "People's Party", "Bloc" = "Bloc Québécois") %>%
  enc2utf8()

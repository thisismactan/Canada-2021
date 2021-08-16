library(tidyverse)
library(reshape2)
library(randomForest)
library(lubridate)
library(Hmisc)
library(scales)
library(mvnfast)
library(readr)

# Mapping
library(sp)
library(sf)
library(rmapshaper)
library(rgdal)
library(leaflet)

party_order <- c("NDP", "Green", "Bloc", "Liberal", "Conservative", "People's")

party_colors <- c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Bloc" = "#8ECEF9", "Green" = "green4", 
                  "People's" = "midnightblue")

party_fullnames <- c("Liberal" = "Liberal Party", "Conservative" = "Conservative Party", "NDP" = "New Democratic Party", 
                     "Bloc" = "Bloc Québécois", "Green" = "Green Party", "People's" = "People's Party")
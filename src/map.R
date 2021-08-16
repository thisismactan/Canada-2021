source("src/lib.R")

# Create tables of summary stats for the map ####
## Riding level
district_summary_stats_probs <- district_summary_stats %>%
  left_join(district_probs %>%
              melt(measure.vars = c("Liberal", "Conservative", "NDP", "Green", "People's", "Bloc"), variable.name = "party", value.name = "prob"), 
            by = c("district_code", "party")) %>%
  mutate(prob = ifelse(is.na(prob), 0, prob)) %>%
  dplyr::select(province, district_code, district_name = district, party, prob, pct_05, mean, pct_95) %>%
  arrange(district_code, desc(prob), desc(mean)) %>%
  group_by(district_code) %>%
  mutate(rank = 1:n()) %>%
  ungroup()

district_summary_stats_1 <- district_summary_stats_probs %>%
  filter(rank == 1) %>%
  dplyr::select(province, district_code, district_name, party_1 = party, prob_1 = prob, pct_05_1 = pct_05, mean_1 = mean, pct_95_1 = pct_95)

district_summary_stats_2 <- district_summary_stats_probs %>%
  filter(rank == 2) %>%
  dplyr::select(district_code, district_name, party_2 = party, prob_2 = prob, pct_05_2 = pct_05, mean_2 = mean, pct_95_2 = pct_95)

district_summary_stats_3 <- district_summary_stats_probs %>%
  filter(rank == 3) %>%
  dplyr::select(district_code, district_name, party_3 = party, prob_3 = prob, pct_05_3 = pct_05, mean_3 = mean, pct_95_3 = pct_95)

district_summary_stats_4 <- district_summary_stats_probs %>%
  filter(rank == 4) %>%
  dplyr::select(district_code, district_name, party_4 = party, prob_4 = prob, pct_05_4 = pct_05, mean_4 = mean, pct_95_4 = pct_95)

district_summary_stats_5 <- district_summary_stats_probs %>%
  filter(rank == 5) %>%
  dplyr::select(district_code, district_name, party_5 = party, prob_5 = prob, pct_05_5 = pct_05, mean_5 = mean, pct_95_5 = pct_95)

district_summary_stats_6 <- district_summary_stats_probs %>%
  filter(rank == 6) %>%
  dplyr::select(district_code, district_name, party_6 = party, prob_6 = prob, pct_05_6 = pct_05, mean_6 = mean, pct_95_6 = pct_95)

district_summary_stats_wide <- district_summary_stats_1 %>%
  left_join(district_summary_stats_2, by = c("district_code", "district_name")) %>%
  left_join(district_summary_stats_3, by = c("district_code", "district_name")) %>%
  left_join(district_summary_stats_4, by = c("district_code", "district_name")) %>%
  left_join(district_summary_stats_5, by = c("district_code", "district_name")) %>%
  left_join(district_summary_stats_6, by = c("district_code", "district_name"))

## Province level
province_summary_stats_1 <- province_summary_stats %>%
  arrange(province, desc(vote_pct_50)) %>%
  group_by(province) %>%
  mutate(rank = 1:n()) %>%
  filter(rank == 1) %>%
  dplyr::select(province, party_1 = party, vote_pct_05_1 = vote_pct_05, vote_pct_50_1 = vote_pct_50, vote_pct_95_1 = vote_pct_95,
                seats_pct_05_1 = seats_pct_05, seats_pct_50_1 = seats_pct_50, seats_pct_95_1 = seats_pct_95)

province_summary_stats_2 <- province_summary_stats %>%
  arrange(province, desc(vote_pct_50)) %>%
  group_by(province) %>%
  mutate(rank = 1:n()) %>%
  filter(rank == 2) %>%
  dplyr::select(province, party_2 = party, vote_pct_05_2 = vote_pct_05, vote_pct_50_2 = vote_pct_50, vote_pct_95_2 = vote_pct_95,
                seats_pct_05_2 = seats_pct_05, seats_pct_50_2 = seats_pct_50, seats_pct_95_2 = seats_pct_95)

province_summary_stats_3 <- province_summary_stats %>%
  arrange(province, desc(vote_pct_50)) %>%
  group_by(province) %>%
  mutate(rank = 1:n()) %>%
  filter(rank == 3) %>%
  dplyr::select(province, party_3 = party, vote_pct_05_3 = vote_pct_05, vote_pct_50_3 = vote_pct_50, vote_pct_95_3 = vote_pct_95,
                seats_pct_05_3 = seats_pct_05, seats_pct_50_3 = seats_pct_50, seats_pct_95_3 = seats_pct_95)

province_summary_stats_4 <- province_summary_stats %>%
  arrange(province, desc(vote_pct_50)) %>%
  group_by(province) %>%
  mutate(rank = 1:n()) %>%
  filter(rank == 4) %>%
  dplyr::select(province, party_4 = party, vote_pct_05_4 = vote_pct_05, vote_pct_50_4 = vote_pct_50, vote_pct_95_4 = vote_pct_95,
                seats_pct_05_4 = seats_pct_05, seats_pct_50_4 = seats_pct_50, seats_pct_95_4 = seats_pct_95)

province_summary_stats_5 <- province_summary_stats %>%
  arrange(province, desc(vote_pct_50)) %>%
  group_by(province) %>%
  mutate(rank = 1:n()) %>%
  filter(rank == 5) %>%
  dplyr::select(province, party_5 = party, vote_pct_05_5 = vote_pct_05, vote_pct_50_5 = vote_pct_50, vote_pct_95_5 = vote_pct_95,
                seats_pct_05_5 = seats_pct_05, seats_pct_50_5 = seats_pct_50, seats_pct_95_5 = seats_pct_95)

province_summary_stats_6 <- province_summary_stats %>%
  arrange(province, desc(vote_pct_50)) %>%
  group_by(province) %>%
  mutate(rank = 1:n()) %>%
  filter(rank == 6) %>%
  dplyr::select(province, party_6 = party, vote_pct_05_6 = vote_pct_05, vote_pct_50_6 = vote_pct_50, vote_pct_95_6 = vote_pct_95,
                seats_pct_05_6 = seats_pct_05, seats_pct_50_6 = seats_pct_50, seats_pct_95_6 = seats_pct_95)

province_summary_stats_wide <- province_summary_stats_1 %>%
  left_join(province_summary_stats_2, by = c("province")) %>%
  left_join(province_summary_stats_3, by = c("province")) %>%
  left_join(province_summary_stats_4, by = c("province")) %>%
  left_join(province_summary_stats_5, by = c("province")) %>%
  left_join(province_summary_stats_6, by = c("province"))

# The map itself ####
## Create Lambert conformal conic CRS for leaflet (see http://spatialreference.org/ref/esri/canada-lambert-conformal-conic/ )
crs_proj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
crs_lcc <- leafletCRS(code = "ESRI:102002", proj4def = crs_proj)

district_shp <- readOGR("data/shapes/FED_CA_2_2_ENG.shp") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  st_as_sf() %>%
  mutate(district_code = as.numeric(FED_NUM)) %>%
  ms_simplify()

province_shp <- readOGR("data/shapes/provinces.shp") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  st_as_sf() %>%
  ms_simplify()

# Swap out shapes from provincial file for the territories since they look nicer
district_shp$geometry[district_shp$PROVCODE == "YT"] <- province_shp$geometry[province_shp$PRENAME == "Yukon"]
district_shp$geometry[district_shp$PROVCODE == "NT"] <- province_shp$geometry[province_shp$PRENAME == "Northwest Territories"]
district_shp$geometry[district_shp$PROVCODE == "NU"] <- province_shp$geometry[province_shp$PRENAME == "Nunavut"]

# Define mapping from party to color
party_to_color <- function(party) {
  case_when(party == "Liberal" ~ "red",
            party == "Conservative" ~ "blue",
            party == "NDP" ~ "#EE7600",
            party == "Green" ~ "#008B00",
            party == "Bloc" ~ "#8B008B",
            party == "People's" ~ "midnightblue")
}

district_map_data <- district_shp %>%
  left_join(district_summary_stats_wide, by = "district_code") %>%
  mutate(color_1 = party_to_color(party_1),
         color_2 = party_to_color(party_2),
         color_3 = party_to_color(party_3),
         color_4 = party_to_color(party_4),
         color_5 = party_to_color(party_5),
         color_6 = party_to_color(party_6),
         mouseover_label = district_name,
         alpha = (prob_1 - 0.4) / 0.6) %>%
  
  # Create the infobox
  mutate(popup_label = case_when(
    # When there is a sixth party (i.e. Quebec)
    !is.na(party_6) ~ paste0("<H4><b><u>", district_name, "</u><br><i>", province, "</i></b></H4>
                              <b><i>Projected vote (90% CI)</i></b><br>
                              <font color = ", color_1, "><b>", party_1, "</b></font>: <b><font color =", color_1, ">", 
                             percent(mean_1, accuracy = 0.1), "</font></b> (", percent(pct_05_1, accuracy = 0.1), " – ", 
                             percent(pct_95_1, accuracy = 0.1), ")<br>
                              <font color = ", color_2, "><b>", party_2, "</b></font>: <b><font color =", color_2, ">", 
                             percent(mean_2, accuracy = 0.1), "</font></b> (", percent(pct_05_2, accuracy = 0.1), " – ", 
                             percent(pct_95_2, accuracy = 0.1), ")<br>
                              <font color = ", color_3, "><b>", party_3, "</b></font>: <b><font color =", color_3, ">", 
                             percent(mean_3, accuracy = 0.1), "</font></b> (", percent(pct_05_3, accuracy = 0.1), " – ", 
                             percent(pct_95_3, accuracy = 0.1), ")<br>
                              <font color = ", color_4, "><b>", party_4, "</b></font>: <b><font color =", color_4, ">", 
                             percent(mean_4, accuracy = 0.1), "</font></b> (", percent(pct_05_4, accuracy = 0.1), " – ", 
                             percent(pct_95_4, accuracy = 0.1), ")<br>
                              <font color = ", color_5, "><b>", party_5, "</b></font>: <b><font color =", color_5, ">", 
                             percent(mean_5, accuracy = 0.1), "</font></b> (", percent(pct_05_5, accuracy = 0.1), " – ", 
                             percent(pct_95_5, accuracy = 0.1), ")<br>
                              <font color = ", color_6, "><b>", party_6, "</b></font>: <b><font color =", color_6, ">", 
                             percent(mean_6, accuracy = 0.1), "</font></b> (", percent(pct_05_6, accuracy = 0.1), " – ", 
                             percent(pct_95_6, accuracy = 0.1), ")<br>
                              <br>
                              <b><i>Win probability</i><b><br>
                              <font color = ", color_1, "><b>", party_1, "</b></font>", ": <font color = ", color_1, "><b>", 
                             percent(prob_1, accuracy = 1), "</b></font><br>
                              <font color = ", color_2, "><b>", party_2, "</b></font>", ": <font color = ", color_2, "><b>", 
                             percent(prob_2, accuracy = 1), "</b></font><br>
                              <font color = ", color_3, "><b>", party_3, "</b></font>", ": <font color = ", color_3, "><b>", 
                             percent(prob_3, accuracy = 1), "</b></font><br>
                              <font color = ", color_4, "><b>", party_4, "</b></font>", ": <font color = ", color_4, "><b>", 
                             percent(prob_4, accuracy = 1), "</b></font><br>
                              <font color = ", color_5, "><b>", party_5, "</b></font>", ": <font color = ", color_5, "><b>", 
                             percent(prob_5, accuracy = 1), "</b></font><br>
                              <font color = ", color_6, "><b>", party_6, "</b></font>", ": <font color = ", color_6, "><b>", 
                             percent(prob_6, accuracy = 1), "</b></font>"),
    is.na(party_6) ~ paste0("<H4><b><u>", district_name, "</u><br><i>", province, "</i></b></H4>
                              <b><i>Projected vote (90% CI)</i></b><br>
                              <font color = ", color_1, "><b>", party_1, "</b></font>: <b><font color =", color_1, ">", 
                            percent(mean_1, accuracy = 0.1), "</font></b> (", percent(pct_05_1, accuracy = 0.1), " – ", 
                            percent(pct_95_1, accuracy = 0.1), ")<br>
                              <font color = ", color_2, "><b>", party_2, "</b></font>: <b><font color =", color_2, ">", 
                            percent(mean_2, accuracy = 0.1), "</font></b> (", percent(pct_05_2, accuracy = 0.1), " – ", 
                            percent(pct_95_2, accuracy = 0.1), ")<br>
                              <font color = ", color_3, "><b>", party_3, "</b></font>: <b><font color =", color_3, ">", 
                            percent(mean_3, accuracy = 0.1), "</font></b> (", percent(pct_05_3, accuracy = 0.1), " – ", 
                            percent(pct_95_3, accuracy = 0.1), ")<br>
                              <font color = ", color_4, "><b>", party_4, "</b></font>: <b><font color =", color_4, ">", 
                            percent(mean_4, accuracy = 0.1), "</font></b> (", percent(pct_05_4, accuracy = 0.1), " – ", 
                            percent(pct_95_4, accuracy = 0.1), ")<br>
                              <font color = ", color_5, "><b>", party_5, "</b></font>: <b><font color =", color_5, ">", 
                            percent(mean_5, accuracy = 0.1), "</font></b> (", percent(pct_05_5, accuracy = 0.1), " – ", 
                            percent(pct_95_5, accuracy = 0.1), ")<br>
                              <br>
                              <b><i>Win probability</i><b><br>
                              <font color = ", color_1, "><b>", party_1, "</b></font>", ": <font color = ", color_1, "><b>", 
                            percent(prob_1, accuracy = 1), "</b></font><br>
                              <font color = ", color_2, "><b>", party_2, "</b></font>", ": <font color = ", color_2, "><b>", 
                            percent(prob_2, accuracy = 1), "</b></font><br>
                              <font color = ", color_3, "><b>", party_3, "</b></font>", ": <font color = ", color_3, "><b>", 
                            percent(prob_3, accuracy = 1), "</b></font><br>
                              <font color = ", color_4, "><b>", party_4, "</b></font>", ": <font color = ", color_4, "><b>", 
                            percent(prob_4, accuracy = 1), "</b></font><br>
                              <font color = ", color_5, "><b>", party_5, "</b></font>", ": <font color = ", color_5, "><b>", 
                            percent(prob_5, accuracy = 1), "</b></font>")))

write_rds(district_map_data, "shiny-app/data/district_map_data.rds")

leaflet(district_map_data) %>%
  addTiles() %>%
  addPolygons(color = "#666666", weight = 1, opacity = 1, fill = TRUE, fillColor = ~color_1, fillOpacity = ~(prob_1 - 0.4) / 0.6, 
              label = ~district_name, popup = ~popup_label,
              highlightOptions = highlightOptions(color = "black", weight = 4, bringToFront = TRUE, opacity = 1))

# Province map ####
province_map_data <- province_shp %>%
  left_join(province_summary_stats_wide, by = c("PRENAME" = "province")) %>%
  left_join(district_probs %>% group_by(province) %>% summarise(n_seats = n()), by = c("PRENAME" = "province")) %>%
  mutate(color_1 = party_to_color(party_1),
         color_2 = party_to_color(party_2),
         color_3 = party_to_color(party_3),
         color_4 = party_to_color(party_4),
         color_5 = party_to_color(party_5),
         color_6 = party_to_color(party_6),
         alpha = sqrt((vote_pct_50_1 - 0.3) / 0.3),
         seat_word_total = ifelse(n_seats == 1, "riding", "ridings"),
         seat_word_1 = ifelse(seats_pct_50_1 == 1, "seat", "seats"),
         seat_word_2 = ifelse(seats_pct_50_2 == 1, "seat", "seats"),
         seat_word_3 = ifelse(seats_pct_50_3 == 1, "seat", "seats"),
         seat_word_4 = ifelse(seats_pct_50_4 == 1, "seat", "seats"),
         seat_word_5 = ifelse(seats_pct_50_5 == 1, "seat", "seats"),
         seat_word_6 = ifelse(seats_pct_50_6 == 1, "seat", "seats"),
         mouseover_label = PRENAME) %>%
  
  # Infobox
  # Create the infobox
  mutate(popup_label = case_when(
    # When there is a sixth party (i.e. Quebec)
    !is.na(party_6) ~ paste0("<H4><b><u>", PRENAME, " (", n_seats, " ", seat_word_total, ")", "</u></b></H4>
                              <b><i>Projected vote (90% CI)</i></b><br>
                              <font color = ", color_1, "><b>", party_1, "</b></font>: <b><font color =", color_1, ">", 
                             percent(vote_pct_50_1, accuracy = 0.1), "</font></b> (", percent(vote_pct_05_1, accuracy = 0.1), " – ", 
                             percent(vote_pct_95_1, accuracy = 0.1), ")<br>
                              <font color = ", color_2, "><b>", party_2, "</b></font>: <b><font color =", color_2, ">", 
                             percent(vote_pct_50_2, accuracy = 0.1), "</font></b> (", percent(vote_pct_05_2, accuracy = 0.1), " – ", 
                             percent(vote_pct_95_2, accuracy = 0.1), ")<br>
                              <font color = ", color_3, "><b>", party_3, "</b></font>: <b><font color =", color_3, ">", 
                             percent(vote_pct_50_3, accuracy = 0.1), "</font></b> (", percent(vote_pct_05_3, accuracy = 0.1), " – ", 
                             percent(vote_pct_95_3, accuracy = 0.1), ")<br>
                              <font color = ", color_4, "><b>", party_4, "</b></font>: <b><font color =", color_4, ">", 
                             percent(vote_pct_50_4, accuracy = 0.1), "</font></b> (", percent(vote_pct_05_4, accuracy = 0.1), " – ", 
                             percent(vote_pct_95_4, accuracy = 0.1), ")<br>
                              <font color = ", color_5, "><b>", party_5, "</b></font>: <b><font color =", color_5, ">", 
                             percent(vote_pct_50_5, accuracy = 0.1), "</font></b> (", percent(vote_pct_05_5, accuracy = 0.1), " – ", 
                             percent(vote_pct_95_5, accuracy = 0.1), ")<br>
                              <font color = ", color_6, "><b>", party_6, "</b></font>: <b><font color =", color_6, ">", 
                             percent(vote_pct_50_6, accuracy = 0.1), "</font></b> (", percent(vote_pct_05_6, accuracy = 0.1), " – ", 
                             percent(vote_pct_95_6, accuracy = 0.1), ")<br>
                              <br>
                              <b><i>Projected seats (90% CI)</i></b><br>
                              <font color = ", color_1, "><b>", party_1, "</b></font>: <b><font color =", color_1, ">", seats_pct_50_1, 
                             "</font></b> ", seat_word_1, " (", seats_pct_05_1, " – ", seats_pct_95_1, ")<br>
                              <font color = ", color_2, "><b>", party_2, "</b></font>: <b><font color =", color_2, ">", seats_pct_50_2, 
                             "</font></b> ", seat_word_2, " (", seats_pct_05_2, " – ", seats_pct_95_2, ")<br>
                              <font color = ", color_3, "><b>", party_3, "</b></font>: <b><font color =", color_3, ">", seats_pct_50_3, 
                             "</font></b> ", seat_word_3, " (", seats_pct_05_3, " – ", seats_pct_95_3, ")<br>
                              <font color = ", color_4, "><b>", party_4, "</b></font>: <b><font color =", color_4, ">", seats_pct_50_4, 
                             "</font></b> ", seat_word_4, " (", seats_pct_05_4, " – ", seats_pct_95_4, ")<br>
                              <font color = ", color_5, "><b>", party_5, "</b></font>: <b><font color =", color_5, ">", seats_pct_50_5, 
                             "</font></b> ", seat_word_5, " (", seats_pct_05_5, " – ", seats_pct_95_5, ")<br>
                              <font color = ", color_6, "><b>", party_6, "</b></font>: <b><font color =", color_6, ">", seats_pct_50_6, 
                             "</font></b> ", seat_word_6, " (", seats_pct_05_6, " – ", seats_pct_95_6, ")"),
    is.na(party_6) ~ paste0("<H4><b><u>", PRENAME, " (", n_seats, " ", seat_word_total, ")", "</u></b></H4>
                              <b><i>Projected vote (90% CI)</i></b><br>
                              <font color = ", color_1, "><b>", party_1, "</b></font>: <b><font color =", color_1, ">", 
                            percent(vote_pct_50_1, accuracy = 0.1), "</font></b> (", percent(vote_pct_05_1, accuracy = 0.1), " – ", 
                            percent(vote_pct_95_1, accuracy = 0.1), ")<br>
                              <font color = ", color_2, "><b>", party_2, "</b></font>: <b><font color =", color_2, ">", 
                            percent(vote_pct_50_2, accuracy = 0.1), "</font></b> (", percent(vote_pct_05_2, accuracy = 0.1), " – ", 
                            percent(vote_pct_95_2, accuracy = 0.1), ")<br>
                              <font color = ", color_3, "><b>", party_3, "</b></font>: <b><font color =", color_3, ">", 
                            percent(vote_pct_50_3, accuracy = 0.1), "</font></b> (", percent(vote_pct_05_3, accuracy = 0.1), " – ", 
                            percent(vote_pct_95_3, accuracy = 0.1), ")<br>
                              <font color = ", color_4, "><b>", party_4, "</b></font>: <b><font color =", color_4, ">", 
                            percent(vote_pct_50_4, accuracy = 0.1), "</font></b> (", percent(vote_pct_05_4, accuracy = 0.1), " – ", 
                            percent(vote_pct_95_4, accuracy = 0.1), ")<br>
                              <font color = ", color_5, "><b>", party_5, "</b></font>: <b><font color =", color_5, ">", 
                            percent(vote_pct_50_5, accuracy = 0.1), "</font></b> (", percent(vote_pct_05_5, accuracy = 0.1), " – ", 
                            percent(vote_pct_95_5, accuracy = 0.1), ")<br>
                              <br>
                              <b><i>Projected seats (90% CI)</i></b><br>
                              <font color = ", color_1, "><b>", party_1, "</b></font>: <b><font color =", color_1, ">", seats_pct_50_1, 
                            "</font></b> ", seat_word_1, " (", seats_pct_05_1, " – ", seats_pct_95_1, ")<br>
                              <font color = ", color_2, "><b>", party_2, "</b></font>: <b><font color =", color_2, ">", seats_pct_50_2, 
                            "</font></b> ", seat_word_2, " (", seats_pct_05_2, " – ", seats_pct_95_2, ")<br>
                              <font color = ", color_3, "><b>", party_3, "</b></font>: <b><font color =", color_3, ">", seats_pct_50_3, 
                            "</font></b> ", seat_word_3, " (", seats_pct_05_3, " – ", seats_pct_95_3, ")<br>
                              <font color = ", color_4, "><b>", party_4, "</b></font>: <b><font color =", color_4, ">", seats_pct_50_4, 
                            "</font></b> ", seat_word_4, " (", seats_pct_05_4, " – ", seats_pct_95_4, ")<br>
                              <font color = ", color_5, "><b>", party_5, "</b></font>: <b><font color =", color_5, ">", seats_pct_50_5, 
                            "</font></b> ", seat_word_5, " (", seats_pct_05_5, " – ", seats_pct_95_5, ")")))

write_rds(province_map_data, "shiny-app/data/province_map_data.rds")

leaflet(province_map_data) %>%
  addTiles() %>%
  addPolygons(color = "#666666", weight = 1, opacity = 1, fill = TRUE, fillColor = ~color_1, fillOpacity = ~alpha, label = ~PRENAME,
              popup = ~popup_label, highlightOptions = highlightOptions(color = "black", weight = 4, bringToFront = TRUE, opacity = 1))

write_rds(paste0(as.character(Sys.time()), " EDT"), "shiny-app/data/update_time.rds")
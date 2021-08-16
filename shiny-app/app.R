library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(leafem)
library(readr)
library(sf)
library(sp)
library(tidyverse)
library(lubridate)
library(scales)
library(extrafont)
library(ggiraph)
library(Hmisc)
library(mapview)

# Party colors / orders
party_order <- c("Liberal", "Conservative", "NDP", "Green", "People's", "Bloc")

party_colors <- c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Bloc" = "#8ECEF9", "Green" = "green4", 
                  "People's" = "midnightblue")

party_fullnames <- c("Liberal" = "Liberal Party", "Conservative" = "Conservative Party", "NDP" = "New Democratic Party", 
                     "Bloc" = "Bloc Québécois", "Green" = "Green Party", "People's" = "People's Party")

result_colors <- c("Conservative minority" = "#8888FF", "Liberal majority" = "#990000", "Liberal minority" = "#FF8888", "Tie" = "#888888")

result_linetypes <- c("Conservative minority" = "d", "Liberal majority" = "a", "Liberal minority" = "d", "Tie" = "a")

# Shapefiles
district_shp <- read_rds("data/district_map_data.rds")
province_shp <- read_rds("data/province_map_data.rds")

shp_list <- list(Province = province_shp, Riding = district_shp)

# Simulation results and timelines
district_sims_sample <- read_csv("data/district_sims_1-1000.csv")
province_sims <- read_csv("data/province_sims.csv") %>%
    mutate(party = ordered(party, levels = party_order))
summary_stats_timeline <- read_csv("data/summary_stats_timeline.csv") %>%
    mutate(party = ordered(party, levels = party_order))
overall_result_timeline <- read_csv("data/overall_result_timeline.csv")

# Polls
polls <- read_csv("data/polls.csv") %>%
    filter(!is.na(age)) %>%
    mutate(party = ordered(party, levels = party_order),
           tooltip = paste0(pollster, "\n", median_date, "\nNational n = ", comma(n, accuracy = 1)))

poll_average_timeline <- read_csv("data/poll_averages_over_time.csv") %>%
    mutate(party = ordered(party, levels = party_order),
           tooltip = paste0(date, "\n", party, ": ", percent(avg, accuracy = 0.1), " (90% CI: ",
                            percent(lower, accuracy = 0.1), " - ", percent(upper, accuracy = 0.1), ")"))
    
# Vectors/lists that might need to be called later
region_names <- c("Atlantic (NL/PE/NS/NB)" = "Atlantic", "Quebec" = "Quebec", "Ontario" = "Ontario", "Prairie (MB/SK)" = "Prairie", 
                  "Alberta" = "Alberta", "British Columbia" = "British Columbia")
province_names <- province_shp$mouseover_label
riding_names <- district_shp$mouseover_label

riding_key <- district_shp %>%
    as.data.frame() %>%
    dplyr::select(district_code, district_name, province) %>%
    as_tibble() %>%
    arrange(province, district_name)

# Coordinates for zooming
district_coords <- read_csv("data/district_coords.csv")
province_coords <- read_csv("data/province_coords.csv")

# CSS stuff
tooltip_css <- "background-color:gray;color:white;font-style:italic;padding:10px;border-radius:5px;"

# Forecast update time
update_time <- read_rds("data/update_time.rds")

# The app
ui <- fluidPage(
    theme = shinytheme("flatly"),
    tags$head(
        ## Some light CSS for table formatting
        tags$style(HTML("
                    table {
                      border-spacing: 2px;
                    }
                    
                    th, td {
                      padding: 3px;
                    }
                    "))),
    
    # Actual interface
    navbarPage("The Election StatSheet Canada 2021 forecast",
        # The map page
        tabPanel(
            "Map",
            sidebarLayout(
                mainPanel = mainPanel(
                    leafletOutput("map", width = 1200, height = 800)
                ),
                sidebarPanel = sidebarPanel(
                    # Map geography level
                    HTML("<h2>Map options</h2>"),
                    HTML("<h3>Geography</h3>"),
                    radioButtons(inputId = "map_geography", label = NULL, choices = list("Riding", "Province")),
                    br(),
                    
                    # Zoom to a riding
                    HTML("<h3>Go to...</h3>"),
                    inputPanel(selectInput(inputId = "province_select", label = "Province", choices = c("Choose a province", province_names),
                                           selected = "Choose a province"),
                               tags$head(tags$style(HTML(".selectize-input {width: 400px;}"))),
                               conditionalPanel(condition = "input.province_select !== 'Choose a province' & input.map_geography == 'Riding'",
                                                uiOutput("riding_menu")),
                               tags$head(tags$style(HTML(".selectize-input {width: 400px;}"))),
                               
                               # Separate buttons for province and constituency level
                               conditionalPanel(condition = "input.map_geography == 'Province' & input.province_select !== 'Choose a province'",
                                                actionButton("go_province", "Go!")),
                               conditionalPanel(condition = "input.map_geography == 'Riding' & input.riding_select !== 'Choose a riding'",
                                                actionButton("go_riding", "Go!"))
                    )
                ),
                position = "right"
                
            )
        ),
        
        tabPanel(
            "Forecast",
            sidebarLayout(
                ## Main panel: display graphs
                mainPanel = mainPanel(ggiraphOutput("forecast_graph", width = 1200, height = 800),
                                      downloadLink("download_sims", label = "Click here to download first 1,000 riding-level simulations")),
                
                ## Sidebar panel: choose between projected vote and projected seats, current and over time, possibly filter by province
                sidebarPanel = sidebarPanel(tags$h3("Graph settings"),
                                            radioButtons("graph_type", label = "Graph", choices = c("Current", "Over time")),
                                            radioButtons("forecast_outcome", label = "Forecast outcome", 
                                                         choices = c("Seats", "Vote share", "Overall outcome")),
                                            pickerInput("province_filter", label = "Select province", choices = c("National", province_names), 
                                                        selected = "National", multiple = FALSE),
                                            conditionalPanel(condition = "input.graph_type == 'Over time' | input.forecast_outcome == 'Overall outcome",
                                                             sliderInput("date_range_forecast", "Date range", min = as.Date("2021-08-15"), 
                                                                         max = as.Date("2021-09-20"), value = as.Date(c("2021-08-15", "2021-09-20"))
                                                             )
                                            )
                ),
                position = "right")
        ),
        
        tabPanel(
            "Polling",
            sidebarLayout(
                ## Main panel: display graph
                mainPanel = mainPanel(ggiraphOutput("poll_graph", width = "1200", height = "800"),
                                      downloadLink("download_polls", label = "Click here to download all polls")),
                
                ## Sidebar panel: choose graph
                sidebarPanel = sidebarPanel(radioButtons("poll_graph_type", label = "Display", 
                                                         choices = c("Current polling average", "Polling averages over time")),
                                            pickerInput("poll_region_filter", label = "Select region", 
                                                        choices = c("National" = "National", region_names), 
                                                        selected = "National", multiple = FALSE),
                                            conditionalPanel(condition = "input.poll_graph_type == 'Polling averages over time'",
                                                             sliderInput("date_range_polls", "Date range", min = as.Date("2020-01-01"),
                                                                         max = as.Date("2021-09-20"), value = as.Date(c("2020-01-01", "2021-09-20"))
                                                             )
                                            )
                ),
                position = "right"))),
    tags$footer(HTML(paste("<br><p><i>Last updated", update_time, "</i>"))))

server <- function(input, output) {
    # THE MAP ####
    ## Allow user to choose geography (province or riding)
    map_geography <- reactive({
        shp_list[[input$map_geography]]
    })
    
    ## Base map
    output$map <- renderLeaflet({
        leaflet(district_shp) %>%
            addTiles() %>%
            addPolygons(color = "#666666", weight = 1, opacity = 1, fill = TRUE, fillColor = ~color_1, fillOpacity = ~alpha, 
                        label = ~mouseover_label, popup = ~popup_label,
                        highlightOptions = highlightOptions(color = "black", weight = 4, bringToFront = TRUE, opacity = 1)) %>%
            setView(lng = -96.416, lat = 60.283, zoom = 4)
    })
    
    ## Add appropriate geography polygons based on user choice
    observe({
        leafletProxy("map", data = map_geography()) %>%
            clearShapes() %>%
            addPolygons(color = "#666666", weight = 1, opacity = 1, fill = TRUE, fillColor = ~color_1, fillOpacity = ~alpha, 
                        label = ~mouseover_label, popup = ~popup_label,
                        highlightOptions = highlightOptions(color = "black", weight = 4, bringToFront = TRUE, opacity = 1))
    })
    
    ## The go-to section of the sidebar
    output$riding_menu <- renderUI({
        selectInput("riding_select", label = "Riding", 
                    choices = c("Choose a riding", riding_key %>% filter(province == input$province_select) %>% pull(district_name)))
    })
    
    ## The zoom function
    ### For provinces
    province_center <- reactive({
        province_coords %>% filter(province == input$province_select)
    })
    
    observeEvent(
        input$go_province,
        handlerExpr = {
            leafletProxy("map", data = map_geography()) %>%
                flyTo(lng = province_center()$lng, lat = province_center()$lat, zoom = province_center()$zoom)
        }
    )
    
    ### For ridings
    district_center <- reactive({
        district_coords %>% filter(district_name == input$riding_select)
    })
    
    observeEvent(
        input$go_riding,
        handlerExpr = {
            leafletProxy("map", data = map_geography()) %>%
                flyTo(lng = district_center()$lng, lat = district_center()$lat, zoom = district_center()$zoom)
        }
        
    )
    
    # THE FORECAST ####
    ## Simulation subset to provinces
    province_sim_subset <- reactive(
        province_sims %>%
            filter(province %in% input$province_filter,
                   party %in% c("Conservative", "Liberal", "NDP", "Bloc"))
    )
    
    ## Seat part
    province_seat_sim_subset <- reactive(
        province_sims %>%
            filter(province %in% input$province_filter,
                   party %in% c("Conservative", "Liberal", "NDP", "Bloc")) %>%
            mutate(binwidth = pmax(floor(log(max(seats))), 1)) %>%
            mutate(round_seats = binwidth * floor(seats / binwidth)) %>%
            group_by(party, round_seats, binwidth) %>%
            summarise(prob = n() / 10000) %>%
            ungroup() %>%
            mutate(description = case_when(
                binwidth == 1 & round_seats == 1 ~ paste0(round_seats, " seat\nProbability: ", percent(prob, accuracy = 0.1)),
                binwidth == 1 & round_seats != 1 ~ paste0(round_seats, " seats\nProbability: ", percent(prob, accuracy = 0.1)),
                binwidth > 1 ~ paste0(round_seats, "-", round_seats + (binwidth - 1), " seats\nProbability: ", percent(prob, accuracy = 0.1))))
    )
    
    ## Vote part
    province_vote_sim_subset <- reactive(
        province_sims %>%
            filter(province %in% input$province_filter,
                   party %in% c("Conservative", "Liberal", "NDP", "Bloc")) %>%
            mutate(round_pct = floor(100 * pct) / 100) %>%
            group_by(party, round_pct) %>%
            summarise(prob = n() / 10000) %>%
            ungroup() %>%
            mutate(description = paste0(100 * round_pct, "-", 100 * round_pct + 1, "% of vote\nProbability: ", percent(prob, accuracy = 0.1)))
    )
    
    ## Same for seat timeline
    province_seat_timeline_subset <- reactive(
        summary_stats_timeline %>%
            filter(geography %in% input$province_filter,
                   party %in% c("Conservative", "Liberal", "NDP", "Bloc"),
                   outcome == "Seats")
    )
    
    ## And vote timeline
    province_vote_timeline_subset <- reactive(
        summary_stats_timeline %>%
            filter(geography %in% input$province_filter,
                   party %in% c("Conservative", "Liberal", "NDP", "Bloc"),
                   outcome == "Vote share")
    )
    
    province_subset <- reactive(
        input$province_filter
    )
    
    output$forecast_graph <- renderggiraph({
        # First: input$graph_type == "Current" and input$forecast_outcome == "Seats"
        if(input$graph_type == "Current" & input$forecast_outcome == "Seats") {
            girafe(ggobj = province_seat_sim_subset() %>%
                       ggplot(aes(x = round_seats, y = prob, fill = party)) +
                       geom_vline_interactive(data = province_sim_subset() %>% group_by(party) %>% summarise(avg_seats = median(seats)) %>% ungroup(),
                                              aes(xintercept = avg_seats, col = party, 
                                                  tooltip = paste0("Median prediction: ", avg_seats, " seats")), 
                                              size = 0, show.legend = FALSE) +
                       facet_wrap(~party, ncol = 2, labeller = labeller(party = party_fullnames), scales = "free_x") +
                       geom_col_interactive(aes(tooltip = description), alpha = 0.5, show.legend = FALSE) +
                       scale_x_continuous(breaks = case_when(max(province_seat_sim_subset()$round_seats) < 10 ~ as.numeric(0:10),
                                                             max(province_seat_sim_subset()$round_seats) %in% 10:19 ~ 2 * (0:10),
                                                             max(province_seat_sim_subset()$round_seats) %in% 20:49 ~ 5 * (0:10),
                                                             max(province_seat_sim_subset()$round_seats) %in% 50:99 ~ 10 * (0:10),
                                                             max(province_seat_sim_subset()$round_seats) %in% 100:300 ~ 20 * (0:10)),
                                          limits = c(-mean(province_seat_sim_subset()$binwidth) / 2, 
                                                     max(province_seat_sim_subset()$round_seats) + 5)) +
                       scale_y_continuous(labels = percent_format(accuracy = 1)) +
                       scale_fill_manual(name = "Party", values = party_colors, labels = party_fullnames) +
                       scale_colour_manual(name = "Party", values = party_colors, labels = party_fullnames) +
                       theme(text = element_text(family = "Lato"), strip.text = element_text(size = 8), axis.text = element_text(size = 7)) +
                       labs(title = "Current projected seats", x = "Seats", y = "Probability",
                            subtitle = province_subset()), width_svg = 7)
        }
        # Second: input$graph_type == "Current" and input$forecast_outcome == "Vote share"
        else if(input$graph_type == "Current" & input$forecast_outcome == "Vote share") {
            girafe(ggobj = province_vote_sim_subset() %>%
                       ggplot(aes(x = round_pct, y = prob, fill = party)) +
                       geom_vline_interactive(data = province_sim_subset() %>% group_by(party) %>% summarise(avg_pct = mean(pct)) %>% ungroup(),
                                              aes(xintercept = avg_pct, col = party, 
                                                  tooltip = paste0("Mean prediction: ", percent(avg_pct, accuracy = 0.1))), 
                                              size = 0, show.legend = FALSE) +
                       facet_wrap(~party, ncol = 2, labeller = labeller(party = party_fullnames), scales = "free_x") +
                       geom_col_interactive(aes(tooltip = description), alpha = 0.5, show.legend = FALSE) +
                       scale_x_continuous(breaks = 0.1 * (0:10), limits = c(-0.005, max(province_vote_sim_subset()$round_pct) + 0.01), 
                                          labels = percent_format(accuracy = 1)) +
                       scale_y_continuous(labels = percent_format(accuracy = 1)) +
                       scale_fill_manual(name = "Party", values = party_colors, labels = party_fullnames) +
                       scale_colour_manual(name = "Party", values = party_colors, labels = party_fullnames) +
                       theme(text = element_text(family = "Lato"), strip.text = element_text(size = 8), axis.text = element_text(size = 7)) +
                       labs(title = "Current projected vote share", x = "Vote share", y = "Probability",
                            subtitle = province_subset()), width_svg = 7)
        }
        # Third: input$graph_type == "Over time" and input$forecast_outcome == "Seats"
        else if(input$graph_type == "Over time" & input$forecast_outcome == "Seats") {
            girafe(ggobj = province_seat_timeline_subset() %>%
                       ggplot(aes(x = date, y = pct_50)) +
                       geom_vline(xintercept = as.Date("2021-09-20"), size = 0, col = "black") +
                       facet_wrap(~party, ncol = 2, labeller = labeller(party = party_fullnames), scales = "free_x") +
                       geom_ribbon(aes(ymin = pct_05, ymax = pct_95, fill = party), alpha = 0.2, show.legend = FALSE) +
                       geom_point_interactive(aes(col = party, tooltip = paste0(date, "\n", pct_50, " seats (90% CI: ", pct_05, "-", pct_95, ")")), 
                                              size = 1, alpha = 0.01, show.legend = FALSE) +
                       geom_line(aes(col = party), show.legend = FALSE) +
                       scale_x_date(date_breaks = case_when(diff(input$date_range_forecast) <= 7 ~ "days",
                                                            diff(input$date_range_forecast) > 7 & diff(input$date_range_forecast) <= 30 ~ "weeks",
                                                            diff(input$date_range_forecast) > 30 & diff(input$date_range_forecast) <= 60 ~ "2 weeks",
                                                            diff(input$date_range_forecast) > 60 ~ "months"), 
                                    limits = input$date_range_forecast, date_labels = "%e %b %Y") +
                       scale_y_continuous(breaks = case_when(max(province_seat_timeline_subset()$pct_95) < 10 ~ as.numeric(0:10),
                                                             max(province_seat_timeline_subset()$pct_95) %in% 10:19 ~ 2 * (0:10),
                                                             max(province_seat_timeline_subset()$pct_95) %in% 20:49 ~ 5 * (0:10),
                                                             max(province_seat_timeline_subset()$pct_95) %in% 50:99 ~ 10 * (0:10),
                                                             max(province_seat_timeline_subset()$pct_95) %in% 100:199 ~ 20 * (0:10),
                                                             max(province_seat_timeline_subset()$pct_95) >= 200 ~ 50 * (0:10)), 
                                          limits = c(-1, NA)) +
                       scale_colour_manual(name = "Party", values = party_colors, labels = party_fullnames) +
                       scale_fill_manual(name = "Party", values = party_colors, labels = party_fullnames) +
                       theme(text = element_text(family = "Lato"), strip.text = element_text(size = 8), axis.text = element_text(size = 6),
                             axis.text.x = element_text(angle = 90, vjust = 0.5)) +
                       labs(title = "Forecasted seats over time", x = "Date", y = "Seats",
                            subtitle = province_subset(), caption = "Error bars indicate 90% confidence intervals"), width_svg = 7)
        }
        # Fourth: input$graph_type == "Over time" and input$forecast_outcome == "Vote share"
        else if(input$graph_type == "Over time" & input$forecast_outcome == "Vote share") {
            girafe(ggobj = province_vote_timeline_subset() %>%
                       ggplot(aes(x = date, y = pct_50)) +
                       geom_vline(xintercept = as.Date("2021-09-20"), size = 0, col = "black") +
                       facet_wrap(~party, labeller = labeller(party = party_fullnames), ncol = 2) +
                       geom_ribbon(aes(ymin = pct_05, ymax = pct_95, fill = party), alpha = 0.2, show.legend = FALSE) +
                       geom_line(aes(col = party), show.legend = FALSE) +
                       geom_point_interactive(aes(col = party, tooltip = paste0(date, "\n", percent(pct_50, accuracy = 0.1), 
                                                                                " vote (90% CI: ", percent(pct_05, accuracy = 0.1), "-", 
                                                                                percent(pct_95, accuracy = 0.1), ")")), 
                                              size = 1, alpha = 0.01, show.legend = FALSE) +
                       scale_x_date(date_breaks = case_when(diff(input$date_range_forecast) <= 7 ~ "days",
                                                            diff(input$date_range_forecast) > 7 & diff(input$date_range_forecast) <= 30 ~ "weeks",
                                                            diff(input$date_range_forecast) > 30 & diff(input$date_range_forecast) <= 60 ~ "2 weeks",
                                                            diff(input$date_range_forecast) > 60 ~ "months"), 
                                    limits = input$date_range_forecast, date_labels = "%e %b %Y") +
                       scale_y_continuous(breaks = (0:10) / 10, labels = percent_format(accuracy = 1), limits = c(0, NA)) +
                       scale_colour_manual(name = "Party", values = party_colors, labels = party_fullnames) +
                       scale_fill_manual(name = "Party", values = party_colors, labels = party_fullnames) +
                       theme(text = element_text(family = "Lato"), strip.text = element_text(size = 8), axis.text = element_text(size = 6),
                             axis.text.x = element_text(angle = 90, vjust = 0.5)) +
                       labs(title = "Forecasted vote share over time", x = "Date", y = "Vote share",
                            subtitle = province_subset(), caption = "Error bars indicate 90% confidence intervals"), width_svg = 7)
        }
        # Fifth: outcome probability timeline
        else if(input$forecast_outcome == "Overall outcome") {
            girafe(ggobj = overall_result_timeline %>%
                       ggplot(aes(x = date, y = prob)) +
                       geom_vline(xintercept = as.Date("2021-09-20"), size = 0, col = "black") +
                       geom_line(aes(col = result)) +
                       geom_point_interactive(aes(col = result, tooltip = paste0(date, "\n", percent(prob, accuracy = 1))),
                                              size = 1, alpha = 0.01, show.legend = FALSE) +
                       geom_text(data = overall_result_timeline %>% filter(date == max(date)), 
                                 aes(x = date + diff(input$date_range_forecast) / 36, y = prob, label = percent(prob, accuracy = 1), col = result), 
                                 size = 2, family = "Lato", show.legend = FALSE) +
                       scale_x_date(date_breaks = case_when(diff(input$date_range_forecast) <= 7 ~ "days",
                                                            diff(input$date_range_forecast) > 7 & diff(input$date_range_forecast) <= 30 ~ "weeks",
                                                            diff(input$date_range_forecast) > 30 & diff(input$date_range_forecast) <= 60 ~ "2 weeks",
                                                            diff(input$date_range_forecast) > 60 ~ "months"), 
                                    limits = input$date_range_forecast, date_labels = "%e %b %Y") +
                       scale_y_continuous(breaks = (0:5) / 5, labels = percent_format(accuracy = 1), limits = c(0, 1)) +
                       scale_colour_manual(name = "Outcome", values = result_colors) +
                       scale_linetype_manual(name = "Outcome", values = result_linetypes) +
                       theme(text = element_text(family = "Lato"), strip.text = element_text(size = 8), axis.text = element_text(size = 6),
                             axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom") +
                       guides(col = guide_legend(nrow = 4)) +
                       labs(title = "Overall outcome probabilities over time", x = "Date", y = "Probability"))
        }
    })
    
    # THE POLLS ####
    ## Subset polls/timelines by region
    polls_region_filter <- reactive(
        polls %>%
            filter(region == input$poll_region_filter)
    )
    
    poll_average_timeline_region_filter <- reactive(
        poll_average_timeline %>%
            filter(region == input$poll_region_filter)
    )
    
    region_filter_name <- reactive(
        input$poll_region_filter
    )
    
    ## Graphs
    output$poll_graph <- renderggiraph({
        if(input$poll_graph_type == "Current polling average") {
            girafe(ggobj = polls_region_filter() %>%
                       mutate(weight = (age <= 45) * loess_weight / exp((age + 1)^0.5)) %>%
                       group_by(party) %>%
                       summarise(avg = wtd.mean(pct, weight),
                                 sd = sqrt(n() * wtd.var(pct, weight) / (n() - 1.5)),
                                 eff_n = sum(weight)^2 / sum(weight^2)) %>%
                       ungroup() %>%
                       ggplot(aes(x = party, y = avg, fill = party)) +
                       geom_col_interactive(aes(tooltip = paste0(party, ": ", percent(avg, accuracy = 0.1), "±", 
                                                                 number(164.5 * sd / sqrt(eff_n), accuracy = 0.1), " pp"))) +
                       geom_errorbar(aes(ymin = avg - 1.645 * sd / sqrt(eff_n), ymax = avg + 1.645 * sd / sqrt(eff_n)), col = "#888888") +
                       geom_text(aes(y = avg + 0.008, label = percent(avg, accuracy = 0.1)), size = 3, family = "Lato", fontface = "bold") +
                       scale_x_discrete(labels = party_order) + 
                       scale_y_continuous(labels = percent_format(accuracy = 1)) +
                       scale_fill_manual(name = "Party", values = party_colors, labels = party_fullnames) +
                       theme(text = element_text(family = "Lato"), axis.text = element_text(size = 6), axis.ticks.x = element_blank()) +
                       labs(title = "2021 Canadian federal election polling average", x = "", y = "Vote share",
                            caption = "Error bars indicate 90% confidence intervals", subtitle = region_filter_name()), width_svg = 7)
        } else if(input$poll_graph_type == "Polling averages over time") {
            girafe(ggobj = poll_average_timeline_region_filter() %>%
                       ggplot(aes(x = date, y = avg)) +
                       geom_vline(xintercept = as.Date("2021-09-20")) +
                       geom_point_interactive(data = polls_region_filter(),
                                              aes(x = median_date, y = pct, col = party, tooltip = tooltip), size = 0, alpha = 0.5) +
                       geom_ribbon(aes(ymin = lower, ymax = upper, fill = party), alpha = 0.2) +
                       geom_line(aes(col = party)) +
                       geom_point_interactive(aes(tooltip = tooltip), size = 1, alpha = 0.01) +
                       geom_text(data = poll_average_timeline_region_filter() %>% filter(date == max(date)), 
                                 aes(x = date + diff(input$date_range_polls) / 45, y = avg, label = percent(avg, accuracy = 0.1), col = party), 
                                 size = 2, family = "Lato", show.legend = FALSE) +
                       scale_x_date(date_breaks = case_when(diff(input$date_range_polls) <= 7 ~ "days",
                                                            diff(input$date_range_polls) > 7 & diff(input$date_range_polls) <= 30 ~ "weeks",
                                                            diff(input$date_range_polls) > 30 & diff(input$date_range_polls) <= 60 ~ "2 weeks",
                                                            diff(input$date_range_polls) > 60 & diff(input$date_range_polls) <= 360 ~ "months",
                                                            diff(input$date_range_polls) > 360 ~ "2 months"), 
                                    limits = input$date_range_polls, date_labels = "%e %b %Y") +
                       scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.001, NA)) +
                       scale_colour_manual(name = "Party", values = party_colors) +
                       scale_fill_manual(name = "Party", values = party_colors) +
                       theme(text = element_text(family = "Lato"), axis.text = element_text(size = 6), axis.ticks.x = element_blank(),
                             axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom") +
                       labs(title = "2021 Canadian federal election polling", x = "Date", y = "Vote share",
                            caption = "Error bars indicate 90% confidence intervals", subtitle = region_filter_name()), width_svg = 7)
        }
    })
    
    # DOWNLOADABLE ####
    output$download_sims <- downloadHandler("riding_sims_1-1000.csv", function(file) write_csv(district_sims_sample, file))
    output$download_polls <- downloadHandler("polls.csv", 
                                             function(file) write_csv(polls %>% dplyr::select(region, pollster, median_date, n, party, pct), file))
}

shinyApp(ui = ui, server = server)

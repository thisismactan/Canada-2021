source("src/lib.R")

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(ggiraph)
library(extrafont)

party_order <- c("NDP", "Green", "Bloc", "Liberal", "Conservative", "People's")

party_colors <- c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Green" = "green4", 
                  "People's" = "midnightblue", "Bloc" = "#8ECEF9")

party_fullnames <- c("Liberal" = "Liberal Party", "Conservative" = "Conservative Party", "NDP" = "New Democratic Party", 
                     "Green" = "Green Party", "People's" = "People's Party", "Bloc" = "Bloc Québécois") %>%
  enc2utf8()

tooltip_css <- "background-color:gray;color:white;font-style:italic;padding:10px;border-radius:5px;"

showtext_auto()

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
  
  navbarPage(
    "Viz for me",
    tabPanel(
      "Current polling average",
      sidebarLayout(
        mainPanel = mainPanel(
          girafeOutput("polling_average", width = "1200", height = "800")
        ),
        sidebarPanel = sidebarPanel(
          radioButtons("poll_average_type", label = "Geography", choices = c("National", "By region"))
        ),
        position = "right"
      )
    ),
    tabPanel(
      "Poll timeline",
      sidebarLayout(
        mainPanel = mainPanel(
          girafeOutput("polling_average_timeline", width = "1600", height = "800")
        ),
        sidebarPanel = sidebarPanel(
          pickerInput("poll_average_timeline_region", label = "Select region", 
                      choices = c("National", "Atlantic", "Quebec", "Ontario", "Prairie", "Alberta", "British Columbia"), 
                      selected = "National", multiple = FALSE),
          width = 2
        ),
        position = "left"
      )
    ),
    tabPanel(
      "Seat distributions",
      sidebarLayout(
        mainPanel = mainPanel(
          girafeOutput("seat_forecast", width = "1200", height = "800")
        ),
        sidebarPanel = sidebarPanel(
          pickerInput("seat_forecast_province", label = "Select province", 
                      choices = c("National", "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick",
                                  "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Yukon", 
                                  "Northwest Territories", "Nunavut"), 
                      selected = "National", multiple = FALSE),
          width = 2
        ),
        position = "right"
      )
    )
  )
)

server <- function(input, output) {
  output$polling_average <- renderGirafe({
    if(input$poll_average_type == "National") {
      girafe(ggobj = natl_polls %>%
               mutate(weight = (age <= 45) * loess_weight / exp((age + 1)^0.5)) %>%
               group_by(party = ordered(party, levels = c("Liberal", "Conservative", "NDP", "Green", "People's", "Bloc"))) %>%
               summarise(avg = wtd.mean(pct, weight),
                         sd = sqrt(n() * wtd.var(pct, weight) / (n() - 1.5)),
                         eff_n = sum(weight)^2 / sum(weight^2)) %>%
               ungroup() %>%
               ggplot(aes(x = party, y = avg, fill = party)) +
               geom_col() +
               geom_errorbar(aes(ymin = avg - 1.645 * sd / sqrt(eff_n), ymax = avg + 1.645 * sd / sqrt(eff_n)), col = "#888888") +
               geom_text(aes(y = avg + 0.008, label = percent(avg, accuracy = 0.1)), size = 3, family = "Lato") +
               scale_x_discrete(labels = c("Liberal", "Conservative", "NDP", "Green", "People's", "Bloc")) + 
               scale_y_continuous(labels = percent_format(accuracy = 1)) +
               scale_fill_manual(name = "Party", values = party_colors, labels = party_fullnames) +
               theme(text = element_text(family = "Lato"), axis.text = element_text(size = 6), axis.ticks.x = element_blank()) +
               labs(title = "2021 Canadian federal election polling average", x = "", y = "Vote share",
                    caption = "Error bars indicate 90% confidence intervals", subtitle = "National"), width_svg = 7)
    } else if(input$poll_average_type == "By region") {
      girafe(ggobj = regional_polls %>%
               na.omit() %>%
               mutate(weight = (age <= 45) * loess_weight / exp((age + 1)^0.5)) %>%
               group_by(party = ordered(party, levels = c("Liberal", "Conservative", "NDP", "Green", "People's", "Bloc")), 
                        region = ordered(region, levels = c("Atlantic", "Quebec", "Ontario", "Prairie", "Alberta", "British Columbia"))) %>%
               summarise(avg = wtd.mean(pct, weight),
                         sd = sqrt(n() * wtd.var(pct, weight) / (n() - 1.5)),
                         eff_n = sum(weight)^2 / sum(weight^2)) %>%
               ungroup() %>%
               ggplot(aes(x = party, y = avg, fill = party)) +
               facet_wrap(~region, nrow = 2) +
               geom_col() +
               geom_errorbar(aes(ymin = avg - 1.645 * sd / sqrt(eff_n), ymax = avg + 1.645 * sd / sqrt(eff_n)), col = "#888888") +
               geom_text(aes(y = avg + 0.016, label = percent(avg, accuracy = 0.1)), size = 2, family = "Lato") +
               scale_y_continuous(labels = percent_format(accuracy = 1)) +
               scale_fill_manual(name = "Party", values = party_colors, labels = party_fullnames) +
               theme(text = element_text(family = "Lato"), axis.text.x = element_blank(), 
                     axis.text = element_text(size = 6), axis.ticks.x = element_blank()) +
               labs(title = "2021 Canadian federal election polling average", x = "", y = "Vote share",
                    caption = "Error bars indicate 90% confidence intervals", subtitle = "By region"), width_svg = 7)
    }
  })
  
  polling_average_region <- reactive(
    input$poll_average_timeline_region
  )
  
  polling_average_timeline_df <- reactive(
    bind_rows(
      natl_poll_averages_smoothed %>%
        mutate(region = "National"),
      regional_poll_averages_smoothed
    ) %>%
      na.omit() %>%
      filter(region == input$poll_average_timeline_region)
  )
  
  polls_region_filter <- reactive(
    bind_rows(
      natl_polls %>%
        mutate(region = "National"),
      regional_polls
    ) %>%
      dplyr::select(-natl_pct, -provincial_lean) %>%
      na.omit() %>%
      filter(region == input$poll_average_timeline_region)
  )
  
  output$polling_average_timeline <- renderGirafe({
    girafe(ggobj = polling_average_timeline_df() %>%
             mutate(party = ordered(party, levels = c("Liberal", "Conservative", "NDP", "Green", "People's", "Bloc"))) %>%
             ggplot(aes(x = date, y = avg)) +
             geom_vline(xintercept = as.Date("2021-09-20")) +
             geom_point(data = polls_region_filter(),
                        aes(x = median_date, y = pct, col = party), size = 0, alpha = 0.5) +
             geom_ribbon(aes(ymin = lower, ymax = upper, fill = party), alpha = 0.2) +
             geom_line(aes(col = party)) +
             geom_text(data = polling_average_timeline_df() %>% filter(date == max(date)), 
                       aes(x = date + 14, y = avg, label = percent(avg, accuracy = 0.1), col = party), 
                       size = 2, family = "Lato", show.legend = FALSE) +
             scale_x_date(date_breaks = "2 months", limits = as.Date(c("2020-01-01", "2021-10-01")), date_labels = "%e %b %Y") +
             scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.001, NA)) +
             scale_colour_manual(name = "Party", values = party_colors) +
             scale_fill_manual(name = "Party", values = party_colors) +
             theme(text = element_text(family = "Lato"), axis.text = element_text(size = 6), axis.ticks.x = element_blank(),
                   axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom") +
             labs(title = "2021 Canadian federal election polling", x = "Date", y = "Vote share",
                  caption = "Error bands indicate 90% confidence intervals", subtitle = polling_average_region()), width_svg = 7)
  })
  
  national_seat_sims <- reactive(
    seat_sims %>%
      filter(party %in% c("Conservative", "Liberal", "NDP", "Bloc")) %>%
      mutate(party = ordered(party, levels = c("Liberal", "Conservative", "NDP", "Bloc")))
  )
  
  province_sim_subset <- reactive(
    province_sims %>%
      filter(province == input$seat_forecast_province,
             party %in% c("Conservative", "Liberal", "NDP", "Bloc")) %>%
      mutate(party = ordered(party, levels = c("Liberal", "Conservative", "NDP", "Bloc")))
  )
  
  seat_forecast_province <- reactive(
    input$seat_forecast_province
  )
  
  output$seat_forecast <- renderGirafe({
    if(input$seat_forecast_province == "National") {
      girafe(ggobj = national_seat_sims() %>%
               group_by(party, seats) %>%
               summarise(prob = n() / 1e4) %>%
               ggplot(aes(x = seats, y = prob, fill = party)) +
               facet_wrap(~party, nrow = 2, labeller = labeller(party = party_fullnames), scales = "free_x") +
               geom_vline(data = national_seat_sims() %>%
                            group_by(party) %>% 
                            summarise(avg_seats = median(seats)) %>% 
                            ungroup(),
                          aes(xintercept = avg_seats, col = party), show.legend = FALSE) +
               geom_col(alpha = 0.5, show.legend = FALSE) +
               scale_x_continuous(breaks = 20 * (0:15),
                                  limits = c(-0.5, max(seat_sims$seats + 1))) +
               scale_y_continuous(breaks = (0:50) / 50, labels = percent_format(accuracy = 1)) +
               scale_fill_manual(name = "Party", values = party_colors, labels = party_fullnames) +
               scale_colour_manual(name = "Party", values = party_colors, labels = party_fullnames) +
               theme(text = element_text(family = "Lato"), strip.text = element_text(size = 8), axis.text = element_text(size = 7)) +
               labs(title = "Current projected seats", x = "Seats", y = "Probability",
                    subtitle = seat_forecast_province()), 
             width_svg = 7)
    } else if(input$seat_forecast_province != "National") {
      girafe(ggobj = province_sim_subset() %>%
               group_by(party, seats) %>%
               summarise(prob = n() / 1e4) %>%
               ggplot(aes(x = seats, y = prob, fill = party)) +
               facet_wrap(~party, nrow = 2, labeller = labeller(party = party_fullnames), scales = "free_x") +
               geom_vline(data = province_sim_subset() %>%
                            group_by(party) %>% 
                            summarise(avg_seats = median(seats)) %>% 
                            ungroup(),
                          aes(xintercept = avg_seats, col = party), show.legend = FALSE) +
               geom_col(alpha = 0.5, show.legend = FALSE) +
               scale_x_continuous(breaks = case_when(max(province_sim_subset()$seats) < 10 ~ as.numeric(0:10),
                                                     max(province_sim_subset()$seats) %in% 10:19 ~ 2 * (0:10),
                                                     max(province_sim_subset()$seats) %in% 20:49 ~ 5 * (0:10),
                                                     max(province_sim_subset()$seats) %in% 50:99 ~ 10 * (0:10),
                                                     max(province_sim_subset()$seats) %in% 100:300 ~ 20 * (0:10)),
                                  limits = c(-1, max(province_sim_subset()$seats + 1))) +
               scale_y_continuous(labels = percent_format(accuracy = 1)) +
               scale_fill_manual(name = "Party", values = party_colors, labels = party_fullnames) +
               scale_colour_manual(name = "Party", values = party_colors, labels = party_fullnames) +
               theme(text = element_text(family = "Lato"), strip.text = element_text(size = 8), axis.text = element_text(size = 7)) +
               labs(title = "Current projected seats", x = "Seats", y = "Probability",
                    subtitle = seat_forecast_province()), 
             width_svg = 7)
    }
  })
}

shinyApp(ui = ui, server = server)

source("src/lib.R")
source("src/polling_error.R")
source("src/shape_2021_data.R")

# National polls
natl_polls <- read_csv("data/polls_2021_natl.csv") %>%
  dplyr::select(-lead) %>%
  mutate(tracking_weight = case_when(grepl("1/4", n) ~ 1/4,
                                     grepl("1/3", n) ~ 1/3,
                                     !grepl("/", n) ~ 1),
         tracking_weight = ifelse(pollster == "Forum Research", tracking_weight * 1/4, tracking_weight),
         n = gsub(",| ", "", n),
         n = gsub("\\(1/.\\)", "", n) %>% as.numeric()) %>%
  melt(measure.vars = c("Liberal", "Conservative", "NDP", "Green", "Bloc", "People's"), variable.name = "party", value.name = "pct") %>%
  mutate(pct = pct / 100,
         loess_weight = (n^0.25) * sqrt(tracking_weight),
         age = as.numeric(today() - median_date)) %>%
  dplyr::select(pollster, median_date, age, n, loess_weight, party, pct) %>%
  as_tibble()

## Current polling average
natl_polling_average <- natl_polls %>%
  mutate(weight = (age <= 60) * loess_weight / exp((age + 1)^0.4)) %>%
  filter(weight > 0) %>%
  group_by(party) %>%
  summarise(avg = wtd.mean(pct, weight),
            sd = sqrt(wtd.var(pct, weight) * n() / (n() - 1.5)),
            eff_n = sum(weight)^2 / sum(weight^2))

natl_polling_average %>%
  mutate(party = ordered(party, levels = party_order)) %>%
  ggplot(aes(x = party, y = avg, fill = party)) +
  geom_col() +
  geom_errorbar(aes(ymin = avg - 1.645 * sd / sqrt(eff_n), ymax = avg + 1.645 * sd / sqrt(eff_n))) + 
  geom_text(aes(y = avg + 0.01, label = percent(avg, accuracy = 0.1)), size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = (0:10) / 10) +
  scale_fill_manual(name = "Party", values = party_colors) +
  theme(axis.ticks.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "2021 Canadian federal election polling average", y = "Support", caption = "Error bars indicate 90% confidence intervals",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

## Average and polls over time
average_dates <- seq(from = as.Date("2020-01-01") - 4, to = today(), by = 1)
n_days <- length(average_dates)
poll_average_list <- vector("list", n_days)

for(i in 1:n_days) {
  poll_average_list[[i]] <- suppressMessages(natl_polls %>%
    filter(median_date <= average_dates[i]) %>%
    mutate(age = as.numeric(average_dates[i] - median_date),
           weight = 10 * (age <= 60) * loess_weight / exp((age + 1)^0.4)) %>%
    filter(weight > 0) %>%
    group_by(party, date = average_dates[i]) %>%
    summarise(avg = wtd.mean(pct, weight),
              sd = sqrt(wtd.var(pct, weight) * n() / (n() - 1.5)),
              eff_n = sum(weight)^2 / sum(weight^2)))
}

natl_poll_averages <- bind_rows(poll_average_list) %>%
  mutate(lower = avg - 1.645 * sd / sqrt(eff_n),
         upper = avg + 1.645 * sd / sqrt(eff_n)) %>%
  arrange(party, date)

natl_poll_averages_smoothed <- natl_poll_averages %>%
  group_by(party) %>%
  mutate_at(vars(c("avg", "sd", "eff_n", "lower", "upper")),
            function(x) {
              (x + lag(x) + lag(x, 2) + lag(x, 3) + lag(x, 4)) / 5
            })

natl_polls %>%
  mutate(party = ordered(party, levels = party_order)) %>%
  ggplot() +
  geom_point(aes(x = median_date, y = pct, col = party), size = 1, alpha = 0.5) +
  geom_ribbon(data = natl_poll_averages_smoothed %>% mutate(party = ordered(party, levels = party_order)), 
              aes(x = date, ymin = lower, ymax = upper, fill = party), alpha = 0.2) +
  geom_line(data = natl_poll_averages_smoothed %>% mutate(party = ordered(party, levels = party_order)), 
            aes(x = date, y = avg, col = party), size = 1) +
  geom_text(data = natl_polling_average %>% mutate(party = ordered(party, levels = party_order)), 
            aes(x = today() + 8, y = avg, col = party, label = percent(avg, accuracy = 0.1)), size = 3,
            show.legend = FALSE) +
  scale_x_date(breaks = "months", date_labels = "%b %Y", limits = as.Date(c("2020-01-01", NA))) +
  scale_y_continuous(breaks = (0:10) / 10, labels = percent_format(accuracy = 1)) +
  scale_colour_manual(name = "Party", values = party_colors) +
  scale_fill_manual(name = "Party", values = party_colors) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5), axis.title.x = element_blank()) +
  labs(title = "2021 Canadian federal election polling", y = "Support", caption = "Error bands indicate 90% confidence intervals")

# National poll covariance
natl_polls_wide <- natl_polls %>%
  distinct() %>%
  spread(party, pct) %>%
  mutate(weight = (age <= 30) * loess_weight / exp((age + 1)^0.5)) %>%
  filter(weight > 0)

natl_poll_cov_major <- cov.wt(natl_polls_wide %>% dplyr::select(Liberal, Conservative, NDP, Green, Bloc),
                              wt = natl_polls_wide %>% pull(weight))$cov
natl_poll_cov <- cov.wt(natl_polls_wide %>% dplyr::select(Liberal, Conservative, NDP, Green, Bloc, `People's`) %>% na.omit(),
                        wt = natl_polls_wide %>% na.omit() %>% pull(weight))$cov
natl_poll_cov[1:5, 1:5] <- natl_poll_cov_major
diag(natl_poll_cov) <- diag(natl_poll_cov) + natl_poll_mse$mse

while(any(eigen(natl_poll_cov)$values <= 0)) {
  diag(natl_poll_cov) <- diag(natl_poll_cov) + 1e-5
}

# Regional leans relative to nation
regional_polls <- bind_rows(
  read_csv("data/polls_2021_atl.csv"), 
  read_csv("data/polls_2021_qc.csv"), 
  read_csv("data/polls_2021_on.csv"), 
  read_csv("data/polls_2021_pr.csv"),
  read_csv("data/polls_2021_ab.csv"),
  read_csv("data/polls_2021_bc.csv")
  ) %>%
  dplyr::select(-lead) %>%
  mutate(tracking_weight = case_when(grepl("1/4", pollster) ~ 1/4,
                                     grepl("1/3", pollster) ~ 1/3,
                                     !grepl("/", pollster) ~ 1),
         tracking_weight = ifelse(pollster == "Forum Research", tracking_weight * 1/4, tracking_weight),
         pollster = gsub(" \\(1/.\\)", "", pollster),
         `People's` = ifelse(is.na(`People's`), 0, `People's`)) %>%
  filter(Liberal > 0) %>% # there's a row for Angus Reid that seems to not have real numbers
  melt(measure.vars = c("Liberal", "Conservative", "NDP", "Green", "Bloc", "People's"), variable.name = "party", value.name = "pct") %>%
  mutate(pct = pct / 100,
         region = case_when(region == "ATL" ~ "Atlantic",
                            region == "QC" ~ "Quebec",
                            region == "ON" ~ "Ontario",
                            region == "MB/SK" ~ "Prairie",
                            region == "AB" ~ "Alberta",
                            region == "BC" ~ "British Columbia")) %>%
  left_join(natl_polls %>% rename(natl_pct = pct), by = c("pollster", "median_date", "party")) %>%
  filter(!is.na(pct)) %>%
  mutate(provincial_lean = pct - natl_pct) %>%
  as_tibble()

## Current polling average
regional_polling_average <- regional_polls %>%
  mutate(weight = (age <= 45) * loess_weight / exp((age + 1)^0.5)) %>%
  filter(weight > 0) %>%
  group_by(party, region) %>%
  summarise(avg = wtd.mean(pct, weight),
            var = wtd.var(pct, weight) * n() / (n() - 1.5),
            sd = sqrt(wtd.var(pct, weight) * n() / (n() - 1.5)),
            eff_n = sum(weight)^2 / sum(weight^2))

regional_polling_average %>%
  mutate(party = ordered(party, levels = party_order)) %>%
  ggplot(aes(x = party, y = avg, fill = party)) +
  facet_wrap(~region, nrow = 2) +
  geom_col() +
  geom_errorbar(aes(ymin = avg - 1.645 * sd / sqrt(eff_n), ymax = avg + 1.645 * sd / sqrt(eff_n))) + 
  geom_text(aes(y = avg + 0.02, label = percent(avg, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = (0:10) / 10) +
  scale_fill_manual(name = "Party", values = party_colors) +
  theme(axis.ticks.x = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()) +
  labs(title = "2021 Canadian federal election polling average by region", y = "Support", caption = "Error bars indicate 90% confidence intervals",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

## Party covariance within region
within_region_covariance <- vector("list", 6)
polling_regions <- unique(regional_polls$region)

for(i in 1:length(polling_regions)) {
  region_poll_subset <- regional_polls %>%
    filter(region == polling_regions[i]) %>%
    dplyr::select(-natl_pct, -provincial_lean) %>%
    spread(party, pct) %>%
    mutate(weight = (age <= 60) * loess_weight / exp((1 + age)^0.4)) %>%
    filter(weight > 0)
  
  if(polling_regions[i] == "Quebec") {
    region_cov <- cov.wt(region_poll_subset %>% na.omit() %>% dplyr::select(Liberal, Conservative, NDP, Green, Bloc, `People's`),
                         wt = region_poll_subset %>% na.omit() %>% pull(weight))$cov
    
    region_cov_major <- cov.wt(region_poll_subset %>% dplyr::select(Liberal, Conservative, NDP, Green, Bloc),
                               wt = region_poll_subset %>% pull(weight))$cov
    
    region_cov[1:5, 1:5] <- region_cov_major
  } else {
    region_cov <- cov.wt(region_poll_subset %>% na.omit() %>% dplyr::select(Liberal, Conservative, NDP, Green, `People's`),
                         wt = region_poll_subset %>% na.omit() %>% pull(weight))$cov
    
    region_cov_major <- cov.wt(region_poll_subset %>% dplyr::select(Liberal, Conservative, NDP, Green),
                               wt = region_poll_subset %>% pull(weight))$cov
    
    region_cov[1:4, 1:4] <- region_cov_major
  }
  within_region_covariance[[i]] <- region_cov
  
  # Add in variance from regional polling error
  region_polling_error_mse <- regional_poll_mse %>%
    filter(region == polling_regions[i]) %>%
    mutate(party = ordered(party, levels = c("Liberal", "Conservative", "NDP", "Green", "Bloc", "People's"))) %>%
    arrange(party) %>%
    pull(mse)
  
  diag(within_region_covariance[[i]]) <- diag(within_region_covariance[[i]]) + region_polling_error_mse
  
  # Force to positive definiteness
  while(any(eigen(within_region_covariance[[i]])$values <= 0)) {
    diag(within_region_covariance[[i]]) <- diag(within_region_covariance[[i]]) + 1e-5
  }
  
}
  
## Average and polls over time
average_dates <- seq(from = as.Date("2020-01-01") - 4, to = today(), by = 1)
n_days <- length(average_dates)
poll_average_list <- vector("list", n_days)

for(i in 1:n_days) {
  poll_average_list[[i]] <- suppressMessages(regional_polls %>%
    filter(median_date <= average_dates[i]) %>%
    mutate(age = as.numeric(average_dates[i] - median_date),
           weight = 10 * (age <= 45) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0) %>%
    group_by(region, party, date = average_dates[i]) %>%
    summarise(avg = wtd.mean(pct, weight),
              sd = sqrt(wtd.var(pct, weight) * n() / (n() - 1.5)),
              eff_n = sum(weight)^2 / sum(weight^2)))
}

regional_poll_averages <- bind_rows(poll_average_list) %>%
  mutate(lower = avg - 1.645 * sd / sqrt(eff_n),
         upper = avg + 1.645 * sd / sqrt(eff_n)) %>%
  arrange(party, date)

regional_poll_averages_smoothed <- regional_poll_averages %>%
  group_by(region, party) %>%
  mutate_at(vars(c("avg", "sd", "eff_n", "lower", "upper")),
            function(x) {
              (x + lag(x) + lag(x, 2) + lag(x, 3) + lag(x, 4)) / 5
            })

showtext_auto()
regional_polls %>%
  mutate(party = ordered(party, levels = party_order)) %>%
  ggplot() +
  facet_wrap(~region, nrow = 2) +
  geom_point(aes(x = median_date, y = pct, col = party), size = 1, alpha = 0.5) +
  geom_ribbon(data = regional_poll_averages_smoothed %>% mutate(party = ordered(party, levels = party_order)), 
              aes(x = date, ymin = lower, ymax = upper, fill = party), alpha = 0.2) +
  geom_line(data = regional_poll_averages_smoothed %>% mutate(party = ordered(party, levels = party_order)), 
            aes(x = date, y = avg, col = party), size = 1) +
  geom_text(data = regional_polling_average %>% mutate(party = ordered(party, levels = party_order)), 
            aes(x = today() + 4, y = avg, col = party, label = percent(avg, accuracy = 0.1)), size = 3,
            show.legend = FALSE) +
  scale_x_date(breaks = "months", date_labels = "%b %Y", limits = as.Date(c("2021-07-01", NA))) +
  scale_y_continuous(breaks = (0:10) / 10, labels = percent_format(accuracy = 1)) +
  scale_colour_manual(name = "Party", values = party_colors) +
  scale_fill_manual(name = "Party", values = party_colors) +
  theme(legend.position = "bottom", text = element_text(family = "Lato"), axis.text.x = element_text(angle = 90, vjust = 0.5), 
        axis.title.x = element_blank()) +
  labs(title = "2021 Canadian federal election polling by region", y = "Support", caption = "Error bands indicate 90% confidence intervals")

# Riding leans relative to region
source("src/scrape_district_polls.R")

district_poll_leans <- district_polls %>%
  left_join(regional_poll_averages %>% dplyr::select(region, party, date, avg), by = c("region", "party", "median_date" = "date")) %>%
  mutate(regional_avg = ifelse(is.na(avg), 0, avg),
         pct = ifelse(is.na(pct), 0, pct),
         lean = pct - regional_avg,
         age = as.numeric(today() - median_date),
         weight = 10 * (ifelse(mode == "IVR", 1, 3) * n^0.25 / exp((age + 1)^(1/3))))

district_poll_averages <- district_poll_leans %>%
  group_by(region, district_code, district, party) %>%
  summarise(avg_lean = wtd.mean(lean, weight),
            sd = sqrt(wtd.var(lean, weight) * n() / (n() - 1.5)),
            eff_n = sum(weight)^2 / sum(weight^2),
            min_n = min(n)) %>%
  mutate(se = case_when(sd == 0 | is.na(sd) ~ sqrt((avg_lean + 1/3) * (1 - (avg_lean + 1/3)) / min_n),
                        sd > 0 ~ sd / sqrt(eff_n))) %>%
  left_join(regional_polling_average %>% dplyr::select(party, region, region_avg = avg, region_var = var, region_eff_n = eff_n), 
            by = c("party", "region")) %>%
  mutate(region_avg = ifelse(is.na(region_avg), 0, region_avg),
         region_var = ifelse(is.na(region_var), 0, region_var),
         region_eff_n = ifelse(is.na(region_eff_n), 1, region_var),
         district_avg = avg_lean + region_avg,
         district_var = se^2 + region_var^2 / region_eff_n) %>%
  ungroup()

# Write the polls and averages to the Shiny folder
## Polls
bind_rows(
  natl_polls %>%
    mutate(region = "National"),
  regional_polls %>% 
    dplyr::select(pollster, median_date, age, n, loess_weight, party, pct, region)
) %>%
  write_csv("shiny-app/data/polls.csv")

## Polling averages
bind_rows(
  natl_poll_averages_smoothed %>%
    na.omit() %>%
    mutate(region = "National"),
  regional_poll_averages_smoothed %>%
    na.omit() %>%
    arrange(region, party, date)
) %>%
  write_csv("shiny-app/data/poll_averages_over_time.csv")

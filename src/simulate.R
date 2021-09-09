source("src/poll_averages.R")
source("src/shape_2021_data.R")

regions <- c("Atlantic", "Quebec", "Ontario", "Prairie", "Alberta", "British Columbia", "The frigid northlands")

# Quickly fit models
district_results <- read_csv("data/district_results.csv")
votes_per_district <- district_results %>%
  filter(year == 2019) %>%
  group_by(district_code) %>%
  summarise(votes = sum(votes))
regional_natl_results <- read_csv("data/regional_natl_results.csv")

district_model <- lm(pct ~ I(province_abbr == "QC") : (pct_lag + region_change + I(incumbent_running == party)) +
                       (pct_lag + region_change + I(incumbent_running == party)),
                     data = district_results %>% 
                       filter(party %in% c("Bloc", "Conservative", "Green", "Liberal", "NDP"),
                              !incumbent_running %in% c("Independent", "People's")))

region_model <- lm(region_pct ~ region_lag + natl_change, data = regional_natl_results %>% filter(!party %in% c("Bloc", "People's")))
region_model_bloc <- lm(region_pct ~ 0 + natl_pct, data = regional_natl_results %>% filter(party == "Bloc"))

# National popular vote simulations
set.seed(2021)
n_sims <- 1e4

natl_vote_sims <- rmvn(n_sims, mu = natl_polling_average$avg, sigma = natl_poll_cov) %>%
  as.data.frame() %>%
  dplyr::select(Liberal = V1, Conservative = V2, NDP = V3, Green = V4, Bloc = V5, `People's` = V6) %>%
  mutate(id = 1:n_sims) %>%
  melt(id.vars = "id", variable.name = "party", value.name = "natl_pct") %>%
  as_tibble()

# Initial forecast at the region level
region_preds <- regional_natl_results %>%
  filter(year == 2019) %>%
  dplyr::select(region, party, region_lag = region_pct, natl_lag = natl_pct) %>%
  right_join(natl_vote_sims, by = "party") %>%
  mutate(natl_change = natl_pct - natl_lag) %>%
  mutate(pred = case_when(party != "Bloc" ~ predict(region_model, newdata = .),
                          party == "Bloc" ~ predict(region_model, newdata = .)))

## Add in simulated error (different by party)
regional_model_mse <- read_csv("data/regional_model_mse.csv")

region_error_sims <- expand.grid(party = c("Liberal", "Conservative", "NDP", "Green", "People's"),
                                 region = regions,
                                 id = 1:n_sims) %>%
  arrange(party, region, id) %>%
  mutate(error = 0) %>%
  filter(!(party == "Bloc" & region != "Quebec")) %>%
  as_tibble()

for(p in unique(region_error_sims$party)) {
  region_mse <- regional_model_mse %>% filter(party == p) %>% pull(mse)
  for(r in unique(region_error_sims$region)) {
    region_error_sims[region_error_sims$party == p & region_error_sims$region == r, "error"] <- rnorm(n_sims, 0, sqrt(region_mse))
  }
}

region_error_sims <- region_error_sims %>%
  bind_rows(tibble(region = "Quebec", party = "Bloc", id = 1:n_sims, error = rnorm(n_sims, 0, summary(region_model_bloc)$sigma)))

# Regional polling simulations
regional_polling_sim_list <- vector("list", 6)

for(i in 1:length(polling_regions)) {
  region_polling_stats <- regional_polling_average %>%
    filter(region == polling_regions[i])
  
  regional_polling_sim_list[[i]] <- rmvn(n_sims, mu = region_polling_stats$avg, sigma = within_region_covariance[[i]]) %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(id = 1:n_sims,
           region = polling_regions[i])
  
  names(regional_polling_sim_list[[i]]) <- c(as.character(region_polling_stats$party), "id", "region")
}

regional_polling_sims <- bind_rows(regional_polling_sim_list) %>%
  melt(id.vars = c("id", "region"), variable.name = "party", value.name = "poll_sim") %>%
  as_tibble() %>%
  group_by(region, party) %>%
  mutate(poll_var = var(poll_sim)) %>%
  ungroup()

# Compute weighted average for region
region_sims <- region_preds %>%
  left_join(region_error_sims, by = c("party", "region", "id")) %>%
  mutate(pred_sim = pred + error) %>%
  group_by(region, party) %>%
  mutate(pred_var = var(pred_sim)) %>%
  left_join(regional_polling_sims, by = c("id", "region", "party")) %>%
  mutate(poll_weight = 1 / poll_var,
         pred_weight = 1 / pred_var) %>%
  mutate(poll_weight = ifelse(is.na(poll_weight), 0, poll_weight),
         pred_weight = ifelse(is.na(pred_weight), 0, pred_weight),
         poll_sim = ifelse(poll_weight == 0, 0, poll_sim),
         pred_sim = ifelse(pred_weight == 0, 0, pred_sim),
         region_pct = (poll_weight * poll_sim + pred_weight * pred_sim) / (poll_weight + pred_weight)) %>%
  dplyr::select(id, party, region, region_pct) %>%
  ungroup() %>%
  arrange(party, region, id)

# Simulating at riding level
error_params <- read_csv("data/district_model_mse.csv")

## Initial predictions
district_sims <- data_2021 %>%
  left_join(region_sims, by = c("region", "party")) %>%
  mutate(region_change = region_pct - region_lag) %>%
  mutate(pred_pct = predict(district_model, newdata = .))

## Add on errors
district_error_covariances <- read_rds("data/district_error_covariances.rds")

district_errors <- district_sims %>%
  filter(party != "Independent") %>%
  dplyr::select(id, region, district_code, party) %>%
  arrange(district_code, party, id) %>%
  mutate(error = 0) %>%
  spread(party, error) %>%
  dplyr::select(id, region, district_code, Liberal, Conservative, NDP, Green, Bloc, `People's`)

error_sim_list <- vector("list", length(regions))

for(i in 1:length(regions)) {
  n_district_sims <- district_errors %>%
    filter(region == regions[i]) %>%
    nrow()
  
  if(regions[i] != "Quebec") {
    error_sim_list[[i]] <- rmvn(n_district_sims, mu = rep(0, 5), sigma = district_error_covariances[[i]]) %>%
      as.data.frame() %>%
      dplyr::select(Liberal = V1, Conservative = V2, NDP = V3, Green = V4, `People's` = V5) %>%
      mutate(id = district_errors %>% filter(region == regions[i]) %>% pull(id),
             district_code = district_errors %>% filter(region == regions[i]) %>% pull(district_code)) %>%
      as_tibble()
  } else {
    error_sim_list[[i]] <- rmvn(n_district_sims, mu = rep(0, 6), sigma = district_error_covariances[[i]]) %>%
      as.data.frame() %>%
      dplyr::select(Liberal = V1, Conservative = V2, NDP = V3, Green = V4, Bloc = V5, `People's` = V6) %>%
      mutate(id = district_errors %>% filter(region == regions[i]) %>% pull(id),
             district_code = district_errors %>% filter(region == regions[i]) %>% pull(district_code)) %>%
      as_tibble()
  }
}

district_errors <- bind_rows(error_sim_list) %>%
  melt(id.vars = c("id", "district_code"), variable.name = "party", value.name = "pred_error") %>%
  filter(!is.na(pred_error)) %>%
  as_tibble()

## Simulate from riding-level polling
district_poll_averages_simp <- district_poll_averages %>%
  dplyr::select(district_code, district, party, district_avg, district_var) %>%
  filter(party %in% c("Liberal", "Conservative", "NDP", "Green", "People's", "Bloc"))

district_polling_sims <- do.call("rbind", replicate(n_sims, district_poll_averages_simp, simplify = FALSE)) %>%
  arrange(district_code, party) %>%
  mutate(id = rep(1:n_sims, n() / n_sims),
         poll_sim = rnorm(n(), district_avg, sqrt(district_var))) %>%
  dplyr::select(id, district_code, district, party, poll_sim) 

district_undecided_sims <- district_polling_sims %>%
  group_by(id, district_code, district) %>%
  summarise(undecided = 1 - sum(poll_sim)) %>%
  arrange(district_code, district, id)

undecided_dirichlet_params <- district_poll_averages_simp %>%
  arrange(district_code, party) %>%
  group_by(district_code) %>%
  mutate(alpha = 10 * district_avg / sum(district_avg)) %>%
  dplyr::select(-district_avg, -district_var) %>%
  mutate(alpha = pmax(0, alpha)) %>%
  spread(party, alpha, fill = 0) %>%
  ungroup()
  
undecided_dirichlet_params

district_codes_with_polling <- unique(district_undecided_sims$district_code)
undecided_allocation_list <- vector("list", length(district_codes_with_polling))

for(i in 1:length(district_codes_with_polling)) {
  district_dirichlet_params <- undecided_dirichlet_params %>%
    filter(district_code == district_codes_with_polling[i]) %>%
    dplyr::select(-district_code, -district) %>%
    as.matrix() %>%
    as.vector()
  
  # Simulate undecided fractions
  undecided_allocation_list[[i]] <- rdirichlet(n_sims, district_dirichlet_params) %>%
    as.data.frame() %>%
    mutate(district_code = district_codes_with_polling[i],
           id = 1:n_sims) %>%
    as_tibble()
}

district_undecided_frac <- bind_rows(undecided_allocation_list) %>%
  dplyr::select(id, district_code, Liberal = V1, Conservative = V2, NDP = V3, Green = V4, `People's` = V5, Bloc = V6) %>%
  melt(id.vars = c("id", "district_code"), variable.name = "party", value.name = "undecided_frac") %>%
  as_tibble()

district_undecided_allocation <- district_undecided_frac %>%
  left_join(district_undecided_sims, by = c("id", "district_code")) %>%
  mutate(undecided_pct = undecided * undecided_frac) %>%
  dplyr::select(id, district_code, party, undecided_pct)

## Add undecided onto sims
district_polling_sims <- district_polling_sims %>%
  left_join(district_undecided_allocation, by = c("id", "district_code", "party")) %>%
  mutate(poll_sim = poll_sim + undecided_pct) %>%
  group_by(district_code) %>%
  mutate(poll_weight = 1 / var(poll_sim)) %>%
  ungroup()

## Weighted average
district_sims <- district_sims %>%
  left_join(district_errors, by = c("district_code", "id", "party")) %>%
  mutate(pred_sim = pred_pct + pred_error) %>%
  group_by(district_code, party) %>%
  mutate(pred_weight = 1 / var(pred_sim)) %>%
  ungroup() %>%
  left_join(district_polling_sims, by = c("id", "district_code", "district", "party")) %>%
  mutate(poll_weight = ifelse(is.na(poll_weight), 0, poll_weight),
         poll_sim = ifelse(is.na(poll_sim), 0, poll_sim),
         pct = (pred_weight * pred_sim + poll_weight * poll_sim) / (pred_weight + poll_weight)) %>%
  mutate(pct = pmax(pct, 0)) %>%
  dplyr::select(id, region, province, district_code, district, party, candidate, incumbent_running, pct) %>%
  filter(!is.na(id))

district_sims %>%
  filter(id <= 1000) %>%
  dplyr::select(id, district_code, district, party, pct) %>%
  mutate(pct = round(pct, 4)) %>%
  write_csv("shiny-app/data/district_sims_1-1000.csv")

district_winners <- district_sims %>%
  group_by(id, district_code) %>%
  arrange(desc(pct)) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  dplyr::select(id, district_code, winner = party)

# Implied national and provincial popular vote
## National
natl_vote_implied <- district_sims %>%
  left_join(votes_per_district, by = "district_code") %>%
  mutate(pred_sim_votes = pct * votes) %>%
  group_by(id, party) %>%
  summarise(total_votes = sum(votes),
            party_votes = sum(pred_sim_votes)) %>%
  group_by(id) %>%
  # Recalculate denominator
  mutate(total_votes = sum(party_votes) / 0.995,
         pct = party_votes / total_votes)

## Provincial
provincial_vote_implied <- district_sims %>%
  left_join(votes_per_district, by = "district_code") %>%
  mutate(pred_sim_votes = pct * votes) %>%
  group_by(id, province, party) %>%
  summarise(total_votes = sum(votes),
            party_votes = sum(pred_sim_votes)) %>%
  group_by(id, province) %>%
  # Recalculate denominator
  mutate(total_votes = sum(party_votes) / 0.995,
         pct = party_votes / total_votes)

# Summary stats
district_summary_stats <- district_sims %>%
  filter(party != "Independent") %>%
  group_by(district_code, party) %>%
  summarise(pct_05 = quantile(pct, 0.05),
            mean = mean(pct),
            pct_95 = quantile(pct, 0.95))

district_probs <- district_sims %>%
  group_by(district_code, id) %>%
  filter(pct == max(pct)) %>%
  group_by(province, district, district_code, party) %>%
  summarise(prob = n() / n_sims) %>%
  spread(party, prob) %>%
  arrange(district_code) %>%
  dplyr::select(province, district_code, district, Liberal, Conservative, NDP, Green, Bloc, `People's`)

district_probs %>% print(n = Inf)

seat_sims <- district_sims %>%
  group_by(id, district_code) %>%
  filter(pct == max(pct)) %>%
  group_by(id, party) %>%
  summarise(seats = n()) %>%
  spread(party, seats, fill = 0) %>%
  melt(id.vars = "id", variable.name = "party", value.name = "seats") %>%
  as_tibble() %>%
  mutate(party = as.character(party))

sim_results <- seat_sims %>%
  group_by(id) %>%
  filter(seats == max(seats)) %>%
  mutate(tied_parties = n()) %>%
  ungroup() %>%
  mutate(winner = case_when(tied_parties == 1 ~ party,
                            tied_parties == 2 ~ "Tie"),
         win_type = case_when(seats > 338/2 & winner != "Tie" ~ "Majority",
                              seats <= 338/2 & winner != "Tie" ~ "Minority",
                              winner == "Tie" ~ ""),
         result = paste(winner, tolower(win_type)) %>% trimws()) %>%
  distinct(id, result)

result_probs <- sim_results %>%
  group_by(result) %>%
  summarise(prob = n() / n_sims) %>%
  mutate(date = today()) %>%
  dplyr::select(date, result, prob)

## At provincial level
province_key <- tibble(province_code = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 60, 61, 62),
                       province = c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick", "Quebec",
                                    "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Yukon", "Northwest Territories",
                                    "Nunavut"),
                       province_abbr = c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU"),
                       region = c("Atlantic", "Atlantic", "Atlantic", "Atlantic", "Quebec", "Ontario", "Prairie", "Prairie", "Alberta",
                                  "British Columbia", "The frigid northlands", "The frigid northlands", "The frigid northlands"))

province_sims <- provincial_vote_implied %>%
  left_join(district_winners %>%
              mutate(province_code = floor(district_code / 1000)) %>%
              left_join(province_key %>% dplyr::select(province_code, province), by = "province_code") %>%
              group_by(id, province, party = winner) %>%
              summarise(seats = n()),
            by = c("id", "province", "party")) %>%
  mutate(seats = ifelse(is.na(seats), 0, seats))

province_sims %>%
  bind_rows(natl_vote_implied %>% mutate(province = "National") %>% left_join(seat_sims, by = c("id", "party"))) %>%
  write_csv("shiny-app/data/province_sims.csv")

province_summary_stats <- province_sims %>%
  group_by(province, party) %>%
  summarise(vote_pct_05 = quantile(pct, 0.05),
            vote_pct_50 = median(pct),
            vote_pct_95 = quantile(pct, 0.95),
            seats_pct_05 = round(quantile(seats, 0.05)),
            seats_pct_50 = round(median(seats)),
            seats_pct_95 = round(quantile(seats, 0.95)))

province_summary_stats %>%
  print(n = Inf)

## Summary stats format for the timeline
summary_stats_by_geo <- bind_rows(
  # National seats
  seat_sims %>%
    filter(party %in% party_order) %>%
    group_by(party) %>%
    summarise(pct_05 = quantile(seats, 0.05),
              pct_50 = quantile(seats, 0.5),
              pct_95 = quantile(seats, 0.95)) %>%
    mutate(date = today(),
           geography = "National",
           outcome = "Seats") %>%
    dplyr::select(geography, date, party, outcome, pct_05, pct_50, pct_95),
  # National vote
  natl_vote_implied %>%
    filter(party %in% party_order) %>%
    group_by(party) %>%
    summarise(pct_05 = quantile(pct, 0.05),
              pct_50 = quantile(pct, 0.5),
              pct_95 = quantile(pct, 0.95)) %>%
    mutate(date = today(),
           geography = "National",
           outcome = "Vote share") %>%
    dplyr::select(geography, date, party, outcome, pct_05, pct_50, pct_95),
  # Province summary stats
  province_sims %>%
    dplyr::select(-total_votes, -party_votes) %>%
    rename(`Vote share` = pct, Seats = seats) %>%
    melt(measure.vars = c("Vote share", "Seats"), variable.name = "outcome", value.name = "value") %>%
    group_by(party, geography = province, outcome) %>%
    summarise(pct_05 = quantile(value, 0.05),
              pct_50 = quantile(value, 0.5),
              pct_95 = quantile(value, 0.95)) %>%
    mutate(date = today())
) %>%
  arrange(geography, as.character(outcome), date, party)

## Create (or add to existing) summary stats timeline
if(!("summary_stats_timeline.csv" %in% list.files("shiny-app/data"))) {
  write_csv(summary_stats_by_geo, "shiny-app/data/summary_stats_timeline.csv")
}

summary_stats_timeline <- read_csv("shiny-app/data/summary_stats_timeline.csv") %>%
  bind_rows(summary_stats_by_geo) %>%
  distinct(geography, date, party, outcome, .keep_all = TRUE)

write_csv(summary_stats_timeline, "shiny-app/data/summary_stats_timeline.csv")

if(!("overall_result_timeline.csv" %in% list.files("shiny-app/data"))) {
  write_csv(result_probs, "shiny-app/data/overall_result_timeline.csv")
}

overall_result_timeline <- read_csv("shiny-app/data/overall_result_timeline.csv") %>%
  bind_rows(result_probs) %>%
  distinct(date, result, .keep_all = TRUE) %>%
  spread(result, prob, fill = 0) %>%
  melt(id.vars = "date", variable.name = "result", value.name = "prob") %>%
  as_tibble()

write_csv(overall_result_timeline, "shiny-app/data/overall_result_timeline.csv")

# Cleanup
rm(district_errors)
rm(region_error_sims)
gc()

source("src/lib.R")

district_results <- read_csv("data/district_results.csv")

# Train/test split
## Training: 2006, 2008, and 2011 elections
train <- district_results %>%
  filter(year %in% 2006:2011 | (year == 2015 & province_abbr %in% c("PE", "YT", "NT", "NU")),
         party %in% c("Liberal", "Conservative", "NDP", "Bloc", "Green"),
         incumbent_running %in% c("Liberal", "Conservative", "NDP", "Bloc", "Green", "Open")) %>%
  mutate(quebec = province_abbr == "QC")

## Test: 2019 election
test <- district_results %>%
  filter(year == 2019,
         party %in% c("Liberal", "Conservative", "NDP", "Bloc", "Green"),
         incumbent_running %in% c("Liberal", "Conservative", "NDP", "Bloc", "Green", "Open")) %>%
  mutate(quebec = province_abbr == "QC")

# Modeling change in share of vote
formula_1 <- formula(pct_change ~ natl_change)
lm_1 <- lm(formula_1, data = train)

formula_2 <- formula(pct_change ~ region_change)
lm_2 <- lm(formula_2, data = train) # better than national

formula_3 <- formula(pct_change ~ region_change + natl_change)
lm_3 <- lm(formula_3, data = train) # no better than regional alone

formula_4 <- formula(pct_change ~ region_change + incumbent_running : party)
lm_4 <- lm(formula_4, data = train)

formula_5 <- formula(pct_change ~ region_change + I(incumbent_running == party) : party)
lm_5 <- lm(formula_5, data = train)

formula_6 <- formula(pct_change ~ party : (region_change + incumbent_running))
lm_6 <- lm(formula_6, data = train)

formula_7 <- formula(pct_change ~ natl_change * I(incumbent_running == party))
lm_7 <- lm(formula_7, data = train)

formula_8 <- formula(pct_change ~ I(province_abbr == "QC") : natl_change * I(incumbent_running == party) + 
                       natl_change * I(incumbent_running == party))
lm_8 <- lm(formula_8, data = train)

formula_9 <- formula(pct_change ~ I(province_abbr == "QC") : region_change * I(incumbent_running == party) + 
                       region_change * I(incumbent_running == party))
lm_9 <- lm(formula_9, data = train)

formula_10 <- formula(pct_change ~ I(province_abbr == "QC") : (region_change * I(incumbent_running == party) + 
                        region_change * I(incumbent_last == party)) + region_change * I(incumbent_running == party) +
                        region_change * I(incumbent_last == party))
lm_10 <- lm(formula_10, data = train)

formula_11 <- formula(pct_change ~ I(province_abbr == "QC") : (region_change + I(incumbent_running == party) : party) +
                        (region_change + I(incumbent_running == party) : party)) 
lm_11 <- lm(formula_11, data = train)

formula_12 <- formula(pct_change ~ I(province_abbr == "QC") : (region_change + incumbent_running : party) +
                        (region_change + incumbent_running : party)) 
lm_12 <- lm(formula_12, data = train)

model_list <- list(lm_1 = lm_1, lm_2 = lm_2, lm_3 = lm_3, lm_4 = lm_4, lm_5 = lm_5, lm_6 = lm_6, lm_7 = lm_7, lm_8 = lm_8, lm_9 = lm_9,
                   lm_10 = lm_10, lm_11 = lm_11, lm_12 = lm_12)

# Model evaluation
change_model_predictions <- test
for(i in 1:length(model_list)) {
  change_model_predictions[, names(model_list)[i]] <- predict(model_list[[i]], newdata = change_model_predictions) + 
    change_model_predictions$pct_lag
}

change_model_predictions %>%
  melt(measure.vars = names(model_list), variable.name = "model", value.name = "pred") %>%
  mutate(error = pred - pct) %>%
  group_by(model) %>%
  summarise(bias = mean(error),
            mad = mean(abs(error)),
            pct_05 = quantile(error, 0.05),
            pct_95 = quantile(error, 0.95))

change_model_predictions %>%
  melt(measure.vars = names(model_list), variable.name = "model", value.name = "pred") %>%
  mutate(error = pred - pct) %>%
  group_by(model, party) %>%
  summarise(mad = mean(abs(error))) %>%
  spread(party, mad)

change_model_predictions %>%
  melt(measure.vars = names(model_list), variable.name = "model", value.name = "pred") %>%
  mutate(error = pred - pct) %>%
  group_by(model, region) %>%
  summarise(mad = mean(abs(error))) %>%
  spread(party, mad)

# Random forest for comparison
set.seed(2021)
hyperparams <- expand.grid(mtry = 2:5, nodesize = 3:9)
rf_formula <- formula(pct_change ~ quebec + incumbent_running + incumbent_last + party + region_change + natl_change)
rf_list <- vector("list", nrow(hyperparams))
rf_model_predictions <- test
for(i in 1:nrow(hyperparams)) {
  rf_list[[i]] <- randomForest(rf_formula, data = train, mtry = hyperparams$mtry[i], nodesize = hyperparams$nodesize[i])
  rf_model_predictions[, paste0("rf_", i)] <- predict(rf_list[[i]], newdata = rf_model_predictions) + rf_model_predictions$pct_lag
}

rf_model_predictions %>%
  melt(measure.vars = paste0("rf_", 1:length(rf_list)), variable.name = "model", value.name = "pred") %>%
  mutate(error = pred - pct) %>%
  group_by(model) %>%
  summarise(bias = mean(error),
            mad = mean(abs(error)),
            pct_05 = quantile(error, 0.05),
            pct_95 = quantile(error, 0.95)) %>%
  bind_cols(hyperparams) %>%
  dplyr::select(model, mtry, nodesize, everything()) %>%
  print(n = Inf)

rf_model_predictions %>%
  melt(measure.vars = paste0("rf_", 1:length(rf_list)), variable.name = "model", value.name = "pred") %>%
  mutate(error = pred - pct) %>%
  group_by(model, party) %>%
  summarise(mad = mean(abs(error))) %>%
  spread(party, mad) %>%
  print(n = Inf)

# Modeling actual share of vote
formula_1 <- formula(pct ~ pct_lag + natl_change)
lm_1 <- lm(formula_1, data = train)

formula_2 <- formula(pct ~ pct_lag + pct_lag + region_change)
lm_2 <- lm(formula_2, data = train) # better than national

formula_3 <- formula(pct ~ pct_lag + pct_lag + region_change + natl_change)
lm_3 <- lm(formula_3, data = train) # no better than regional alone

formula_4 <- formula(pct ~ pct_lag + region_change + incumbent_running : party)
lm_4 <- lm(formula_4, data = train)

formula_5 <- formula(pct ~ I(province_abbr == "QC") : (pct_lag + region_change + I(incumbent_running == party) : party) +
                       (pct_lag + region_change + I(incumbent_running == party) : party)) 
lm_5 <- lm(formula_5, data = train)

formula_6 <- formula(pct ~ pct_lag + party : (region_change + incumbent_running))
lm_6 <- lm(formula_6, data = train)

formula_7 <- formula(pct ~ pct_lag + natl_change * I(incumbent_running == party))
lm_7 <- lm(formula_7, data = train)

formula_8 <- formula(pct ~ pct_lag + I(province_abbr == "QC") : natl_change * I(incumbent_running == party) + 
                       natl_change * I(incumbent_running == party))
lm_8 <- lm(formula_8, data = train)

formula_9 <- formula(pct ~ pct_lag + I(province_abbr == "QC") : region_change * I(incumbent_running == party) + 
                       region_change * I(incumbent_running == party))
lm_9 <- lm(formula_9, data = train)

formula_10 <- formula(pct ~ pct_lag + I(province_abbr == "QC") : 
                        (region_change * I(incumbent_running == party) + region_change * I(incumbent_last == party)) + 
                        region_change * I(incumbent_running == party) +
                        region_change * I(incumbent_last == party))
lm_10 <- lm(formula_10, data = train)

formula_11 <- formula(pct ~ I(province_abbr == "QC") : (pct_lag + region_change + I(incumbent_running == party)) +
                       (pct_lag + region_change + I(incumbent_running == party))) 
lm_11 <- lm(formula_11, data = train)

formula_12 <- formula(pct ~ I(province_abbr == "QC") : (pct_lag + region_change + incumbent_running : party) +
                        (pct_lag + region_change + incumbent_running : party)) 
lm_12 <- lm(formula_12, data = train)

model_list <- list(lm_1 = lm_1, lm_2 = lm_2, lm_3 = lm_3, lm_4 = lm_4, lm_5 = lm_5, lm_6 = lm_6, lm_7 = lm_7, lm_8 = lm_8, lm_9 = lm_9,
                   lm_10 = lm_10, lm_11 = lm_11, lm_12 = lm_12)

pct_model_predictions <- test
for(i in 1:length(model_list)) {
  pct_model_predictions[, names(model_list)[i]] <- predict(model_list[[i]], newdata = pct_model_predictions)
}

pct_model_predictions %>%
  melt(measure.vars = names(model_list), variable.name = "model", value.name = "pred") %>%
  mutate(error = pred - pct) %>%
  group_by(model) %>%
  summarise(bias = mean(error),
            mad = mean(abs(error)),
            pct_05 = quantile(error, 0.05),
            pct_95 = quantile(error, 0.95))

pct_model_predictions %>%
  melt(measure.vars = names(model_list), variable.name = "model", value.name = "pred") %>%
  mutate(error = pred - pct) %>%
  group_by(model, party) %>%
  summarise(mad = mean(abs(error))) %>%
  spread(party, mad)

pct_model_predictions %>%
  melt(measure.vars = names(model_list), variable.name = "model", value.name = "pred") %>%
  mutate(error = pred - pct) %>%
  group_by(model, region) %>%
  summarise(mad = mean(abs(error))) %>%
  spread(region, mad)

# Selected model and error variance for that one (pct model lm_11) by party and region
error_params <- pct_model_predictions %>%
  mutate(pred = lm_11,
         error = pred - pct) %>%
  group_by(party, region) %>%
  summarise(avg = mean(error),
            var = var(error),
            mse = mean(error^2))

error_params %>%
  bind_rows(error_params %>%
              filter(party == "Green") %>%
              mutate(party = "People's")) %>%
  write_csv("data/district_model_mse.csv")

# Party error covariance
district_errors_wide <- pct_model_predictions %>%
  mutate(pred = lm_11, 
         error = pred - pct) %>%
  dplyr::select(district_code, region, party, error) %>%
  spread(party, error)

regions <- unique(district_errors_wide$region)
district_error_covariances <- vector("list", length(regions))

for(i in 1:length(regions)) {
  if(regions[i] != "Quebec") {
    district_error_covariances[[i]] <- district_errors_wide %>%
      filter(region == regions[i]) %>%
      mutate(`People's` = 0) %>%
      dplyr::select(Liberal, Conservative, NDP, Green, `People's`) %>% 
      cov()
  } else {
    district_error_covariances[[i]] <- district_errors_wide %>%
      filter(region == regions[i]) %>%
      mutate(`People's` = 0) %>%
      dplyr::select(Liberal, Conservative, NDP, Green, Bloc, `People's`) %>% 
      cov()
  }
  
  while(any(eigen(district_error_covariances[[i]])$values <= 0)) {
    diag(district_error_covariances[[i]]) <- diag(district_error_covariances[[i]]) + 1e-5
  }
}

write_rds(district_error_covariances, "data/district_error_covariances.rds")

# Regional-level modeling
regional_natl_results <- regional_results %>%
  left_join(natl_results, by = c("year", "party")) %>%
  arrange(region, party, year) %>%
  group_by(region, party) %>%
  mutate(region_lag = lag(region_pct),
         natl_lag = lag(natl_pct)) %>%
  ungroup() %>%
  mutate(region_change = region_pct - region_lag,
         natl_change = natl_pct - natl_lag) %>%
  left_join(district_results %>%
              group_by(year, party, region) %>%
              mutate(n_races = n()) %>%
              filter(incumbent == "Y") %>%
              group_by(year, party, region, n_races) %>%
              summarise(n_incumbents = n()) %>%
              mutate(frac_incumbents = n_incumbents / n_races),
            by = c("year", "party", "region")) %>%
  mutate(frac_incumbents = ifelse(is.na(frac_incumbents), 0, frac_incumbents)) %>%
  group_by(region, party) %>%
  mutate(frac_incumbent_lag = lag(frac_incumbents),
         frac_incumbent_change = frac_incumbents - frac_incumbent_lag) %>%
  ungroup()

write_csv(regional_natl_results, "data/regional_natl_results.csv")

regional_train <- regional_natl_results %>%
  filter(year < 2019, !party %in% c("Bloc", "People's"))

region_lm_1 <- lm(region_change ~ natl_change, data = regional_train)
region_lm_2 <- lm(region_change ~ natl_lag + natl_pct, data = regional_train)
region_lm_3 <- lm(region_pct ~ region_lag + natl_change, data = regional_train)
region_lm_4 <- lm(region_pct ~ region_lag + natl_lag + natl_pct, data = regional_train)
region_lm_5 <- lm(region_pct ~ region_lag + natl_lag + natl_pct + frac_incumbent_change, data = regional_train)
region_lm_6 <- lm(region_pct ~ region_lag + natl_change + frac_incumbent_change, data = regional_train)

regional_test <- regional_natl_results %>%
  filter(year == 2019, party != "People's") %>%
  mutate(lm_1 = predict(region_lm_1, newdata = .) + region_lag,
         lm_2 = predict(region_lm_2, newdata = .) + region_lag,
         lm_3 = predict(region_lm_3, newdata = .),
         lm_4 = predict(region_lm_4, newdata = .),
         lm_5 = predict(region_lm_5, newdata = .),
         lm_6 = predict(region_lm_6, newdata = .))

regional_test %>%
  melt(measure.vars = c("lm_1", "lm_2", "lm_3", "lm_4", "lm_5", "lm_6"), variable.name = "model", value.name = "pred") %>%
  mutate(error = pred - region_pct) %>%
  group_by(model) %>%
  summarise(bias = mean(error),
            mad = mean(abs(error)),
            pct_05 = quantile(error, 0.05),
            pct_95 = quantile(error, 0.95))

regional_error_params <- regional_test %>%
  filter(!party %in% c("Bloc", "Independent")) %>%
  mutate(error = lm_3 - region_pct) %>%
  group_by(party) %>%
  summarise(mse = mean(error^2))

regional_error_params %>%
  bind_rows(regional_error_params %>%
              filter(party == "Green") %>%
              mutate(party = "People's")) %>%
  write_csv("data/regional_model_mse.csv")

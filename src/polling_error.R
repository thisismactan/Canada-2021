source("src/lib.R")

# National polling
natl_polls_2019 <- read_csv("data/polls_2019.csv") 

natl_poll_mse <- natl_polls_2019 %>%
  mutate(age = as.numeric(as.Date("2019-10-21") - date),
         weight = (age <= 30) * loess_weight / exp((age + 1)^0.4)) %>%
  filter(weight > 0, !is.na(pct)) %>%
  group_by(party) %>%
  summarise(sse = sum(error^2),
            eff_n = sum(weight)^2 / sum(weight^2)) %>%
  mutate(mse = sse / eff_n)

# Regional polling
regional_polls_2019 <- read_csv("data/regional_polls_2019.csv")
regional_natl_results <- read_csv("data/regional_natl_results.csv") 

regional_poll_mse <- regional_polls_2019 %>%
  mutate(age = as.numeric(as.Date("2019-10-21") - median_date),
         weight = (age <= 30) * (n^0.25) / (exp((age + 1)^0.4) * ifelse(!LV, 3, 1) * ifelse(mode == "IVR", 3, 1))) %>%
  filter(weight > 0, !is.na(pct)) %>%
  arrange(age, region, party) %>%
  group_by(party) %>%
  left_join(regional_natl_results %>%
              filter(year == 2019) %>%
              dplyr::select(region, party, region_pct),
            by = c("region", "party")) %>%
  mutate(error = pct - region_pct) %>%
  group_by(party, region) %>%
  summarise(sse = sum(error^2),
            eff_n = sum(weight)^2 / sum(weight^2)) %>%
  mutate(mse = sse / eff_n)
  

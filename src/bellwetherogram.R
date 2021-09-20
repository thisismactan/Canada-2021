source("src/lib.R")

condition_lpc_win <- district_winners %>%
  filter(winner == "Liberal") %>%
  left_join(sim_results, by = "id") %>%
  group_by(district_code) %>%
  summarise(lpc_riding_prob = n() / n_sims,
            lpc_national_prob = mean(grepl("Liberal", result))) %>%
  mutate(lpc_national_base_prob = result_probs %>%
           filter(grepl("Liberal", result)) %>%
           pull(prob) %>%
           sum(),
         lpc_bayes_factor = (lpc_national_prob / (1 - lpc_national_prob)) / (lpc_national_base_prob / (1 - lpc_national_base_prob)))

condition_cpc_win <- district_winners %>%
  filter(winner == "Conservative") %>%
  left_join(sim_results, by = "id") %>%
  group_by(district_code) %>%
  summarise(cpc_riding_prob = n() / n_sims,
            cpc_national_prob = mean(grepl("Conservative", result))) %>%
  mutate(cpc_national_base_prob = result_probs %>%
           filter(grepl("Conservative", result)) %>%
           pull(prob) %>%
           sum(),
         cpc_bayes_factor = (cpc_national_prob / (1 - cpc_national_prob)) / (cpc_national_base_prob / (1 - cpc_national_base_prob)))

bellwetherogram <- condition_lpc_win %>%
  left_join(condition_cpc_win, by = "district_code") %>%
  left_join(data_2021 %>% dplyr::select(district_code, district) %>% distinct(), by = "district_code") %>%
  dplyr::select(district_code, district, everything())

showtext_auto()
bellwetherogram %>%
  filter(lpc_bayes_factor < Inf,
         cpc_bayes_factor < Inf,
         lpc_riding_prob > 0.1,
         cpc_riding_prob > 0.1) %>%
  mutate(bpi = lpc_riding_prob * lpc_bayes_factor + cpc_riding_prob * cpc_bayes_factor) %>%
  ggplot(aes(x = 100 * (lpc_national_prob - lpc_national_base_prob), y = 100 * (cpc_national_prob - cpc_national_base_prob))) +
  geom_text(aes(col = log(lpc_riding_prob / cpc_riding_prob), label = district), size = 3, show.legend = FALSE) +
  scale_colour_gradient2(name = "Relative log-odds", low = "blue", mid = "#FF00FF", high = "red") +
  theme(text = element_text(family = "Lato")) +
  labs(title = "Canada 2021 bellwether-o-gram", x = "Increase to chance of Liberal plurality, pp",
       y = "Increase to chance of Conservative plurality, pp") +
  lims(x = c(-5, 50), y = c(-5, 50))

source("src/lib.R")

data_2021 <- read_csv("data/2021_data.csv") %>%
  mutate(incumbent_running = ifelse(incumbent == "Y", party, NA)) %>%
  group_by(district_code) %>%
  mutate(incumbent_running = max(incumbent_running, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(incumbent_running = ifelse(is.na(incumbent_running), "Open", incumbent_running))



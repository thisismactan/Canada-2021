source("src/lib.R")

# Handle 2004 separately (it's got a different format) ####
filepaths <- list.files("data/raw/2004", full.names = TRUE)

results_list <- vector("list", length(filepaths))
for(i in 1:length(filepaths)) {
  results_list[[i]] <- suppressMessages(suppressWarnings(read_csv(filepaths[i], locale = locale(encoding = "LATIN1"))) %>%
    dplyr::select(-`Rejected Ballots`, -`Total Vote`, -`Electors`) %>%
    melt(id.vars = c("District", "Poll Number", "Poll Name"), variable.name = "candidate", value.name = "votes") %>%
    mutate(votes = as.numeric(votes)) %>%
    group_by(district = District, candidate) %>%
    summarise(votes = sum(votes, na.rm = TRUE)))
}

# candidates_2004 <- read_csv("data/candidates_2004.csv") %>%
#   mutate(candidate = case_when(!is.na(candidate_middle) ~ paste(candidate_first, candidate_middle, candidate_last),
#                                is.na(candidate_middle) ~ paste(candidate_first, candidate_last))) %>%
#   dplyr::select(district_code, district = district_name_english, party, candidate, candidate_last, incumbent)

candidates_2004 <- read_csv("data/candidates_2004_literal.csv")

results_2004 <- results_list %>%
  bind_rows() %>%
  left_join(candidates_2004, by = c("district", "candidate")) %>%
  group_by(district) %>%
  mutate(district = gsub("/.*", "", district), 
         candidate = as.character(candidate) %>% enc2utf8(),
         pct = votes / sum(votes)) %>%
  ungroup() %>%
  arrange(district_code, party) %>%
  mutate(year = 2004) %>%
  dplyr::select(year, district_code, district, party, candidate, incumbent, votes, pct)

# 2006-2019 ####
election_years <- c(2006, 2008, 2011, 2015, 2019)
result_dfs <- vector("list", length(election_years))

for(y in 1:length(election_years)) {
  # Get all file paths in directory corresponding to year
  filepaths <- list.files(paste0("data/raw/", election_years[y]), full.names = TRUE)
  
  # Loop over them, process individually and stick in a list
  result_dfs[[y]] <- vector("list", length(filepaths))
  
  # Before 2015 the encoding used was LATIN-1 for some reason
  file_encoding <- ifelse(election_years[y] <= 2011, "LATIN1", "UTF8")
  
  for(i in 1:length(filepaths)) {
    result_dfs[[y]][[i]] <- suppressMessages(suppressWarnings(read_csv(filepaths[i], locale = locale(encoding = file_encoding))) %>%
      dplyr::select(district_code = 1, district = 2, precinct = 4, candidate_last = 11, candidate_middle = 12, candidate_first = 13,
                    party = 14, incumbent = 16, votes = 18) %>%
      mutate(votes = as.numeric(votes),
             candidate = case_when(is.na(candidate_first) ~ paste(candidate_middle, candidate_last),
                                   is.na(candidate_middle) ~ paste(candidate_first, candidate_last),
                                   !is.na(candidate_first) & !is.na(candidate_middle) ~ paste(candidate_first, candidate_middle, candidate_last))) %>%
      group_by(district_code, district, party, candidate, incumbent) %>%
      summarise(votes = sum(votes, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(year = election_years[y],
             pct = votes / sum(votes)))
  }
  
  result_dfs[[y]] <- bind_rows(result_dfs[[y]])
}

# Create regional/provincial divisions
province_key <- tibble(province_code = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 60, 61, 62),
                       province = c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick", "Quebec",
                                    "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Yukon", "Northwest Territories",
                                    "Nunavut"),
                       province_abbr = c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU"),
                       region = c("Atlantic", "Atlantic", "Atlantic", "Atlantic", "Quebec", "Ontario", "Prairie", "Prairie", "Alberta",
                                  "British Columbia", "The frigid northlands", "The frigid northlands", "The frigid northlands"))

# Put together results
results_allparties <- results_2004 %>%
  bind_rows(bind_rows(result_dfs)) %>%
  arrange(district_code, party, year) %>%
  mutate(province_code = floor(district_code / 1000)) %>%
  left_join(province_key, by = "province_code") %>%
  mutate(party = case_when(party == "Conservative" ~ "Conservative",
                           party %in% c("Green Party", "Green") ~ "Green",
                           party == "Liberal" ~ "Liberal",
                           party %in% c("N.D.P.", "NDP-New Democratic Party", "NDP") ~ "NDP",
                           party %in% c("Bloc", "Bloc Québécois") ~ "Bloc",
                           party == "People's Party" ~ "People's",
                           party == "Independent" ~ "Independent")) 

natl_results <- results_allparties %>%
  group_by(year, party) %>%
  summarise(natl_votes = sum(votes)) %>%
  group_by(year) %>%
  mutate(natl_pct = natl_votes / sum(natl_votes)) %>%
  filter(!is.na(party))

regional_results <- results_allparties %>%
  group_by(year, region, party) %>%
  summarise(regional_votes = sum(votes)) %>%
  group_by(year, region) %>%
  mutate(region_pct = regional_votes / sum(regional_votes)) %>%
  filter(!is.na(party)) %>%
  ungroup()

district_results <- results_allparties %>%
  filter(party %in% c("Conservative", "Green", "Liberal", "NDP", "Bloc", "People's", "Independent")) %>%
  left_join(regional_results, by = c("year", "region", "party")) %>%
  left_join(natl_results, by = c("year", "party")) %>%
  group_by(district_code, party) %>%
  arrange(district_code, party, year) %>%
  mutate(pct_lag = lag(pct),
         region_lag = lag(region_pct),
         natl_lag = lag(natl_pct),
         incumbent_running = case_when(incumbent == "Y" ~ party),
         pct_change = pct - pct_lag,
         region_change = region_pct - region_lag,
         natl_change = natl_pct - natl_lag) %>%
  group_by(year, district_code) %>%
  mutate(incumbent_running = max(incumbent_running, na.rm = TRUE),
         incumbent_running = ifelse(is.na(incumbent_running), "Open", incumbent_running)) %>%
  group_by(district_code) %>%
  arrange(district_code, party, year) %>%
  mutate(incumbent_last = lag(incumbent_running)) %>%
  ungroup() %>%
  filter(year > 2004)
  
write_csv(district_results, "data/district_results.csv")

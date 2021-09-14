source("src/lib.R")

wikipage <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_Canadian_federal_election_by_constituency")

# Grab district poll tables
district_poll_tables <- html_nodes(wikipage, "table") %>%
  html_table()
district_poll_tables <- district_poll_tables[2:length(district_poll_tables)]

# Grab district names
district_poll_districts <- html_nodes(wikipage, "h4") %>%
  html_text()

district_poll_districts <- gsub("\\[edit\\]", "", district_poll_districts)
district_poll_districts <- gsub("’", "'", district_poll_districts)
district_poll_districts 

poll_table_formatter <- function(df) {
  df %>%
    rename(pollster = `Polling Firm`, Conservative = `Cons.`, `People's` = PPC, n = `SampleSize[2]`, mode = `Polling Method[3]`,
           median_date = `Last Dateof Polling`) %>%
    filter(!grepl("election", pollster, ignore.case = TRUE)) %>%
    mutate(median_date = as.Date(median_date, format = "%B %d, %Y") - 1) %>%
    dplyr::select(-Link, -Undecided, -`Marginof Error[1]`) %>%
    melt(id.vars = c("pollster", "median_date", "mode", "n"), variable.name = "party", value.name = "pct") %>%
    mutate(pct = as.numeric(pct) / 100) %>%
    as_tibble()
}

district_poll_list <- suppressWarnings(lapply(district_poll_tables, poll_table_formatter))

for(i in 1:length(district_poll_list)) {
  district_poll_list[[i]] <- district_poll_list[[i]] %>%
    mutate(district = district_poll_districts[i])
}

district_polls <- bind_rows(district_poll_list) %>%
  left_join(data_2021 %>% dplyr::select(district, district_code, province, region) %>% distinct(), by = "district") %>%
  mutate(n = as.numeric(n)) %>%
  dplyr::select(region, province, district_code, district, pollster, median_date, mode, n, party, pct) %>%
  as_tibble()
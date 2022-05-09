parking = read_csv('data/maindf.csv')
county_mean = parking %>%
  dplyr::select(County, State, 
                AvgTimeToPark:AvgTotalGeohashes, 
                PercentCar:PercentOther, 
                POPESTIMATE2020:RNETMIG2020) %>% 
  group_by(County, State) %>%
  summarize(across(everything(), mean)) %>%
  mutate(State = ifelse(
      test = State %in% state.name, 
      yes = state.abb[match(State, state.name)], 
      no = str_replace_all(gsub(" ", "", State), "[:lower:]+", ""))
  ) %>% 
  unique() %>%
  mutate(POPESTIMATE2020 = ifelse(is.na(POPESTIMATE2020), 0, POPESTIMATE2020)) %>%
  ungroup()

# load kriging output
krig = list()
for (c in c("Houston", "Los Angeles", "New York", "San Francisco", "Washington")) {
  krig[[c]] = read_csv(file.path("data", paste0(c, ".csv")))
}

varnames = c(
  "AvgTimeToPark", "AvgTimeToParkRatio", "TotalSearching", 
  "PercentSearching", "AvgUniqueGeohashes", "AvgTotalGeohashes",
  "PercentCar", "PercentMPV", "PercentLDT", "PercentMDT",
  "PercentHDT", "PercentOther")
ams_licenses <- read_csv("xxx.csv")

ams_licenses_clean <- ams_licenses %>% 
  clean_names() %>%
  mutate(date = dmy(date)) %>% 
  filter(date == ymd("2025.03.13")) %>% 
  select(about, taps, sport)

ams_licenses_check <- ams_licenses_clean %>% 
  # filter(taps %in% c("--", "DECARDED", NA, "TOURING ATHLETE", "NOT SUPPORTED")) %>% 
  filter(!(about %in% rnz_keepers))

ams_names <- ams_licenses_check %>% select(about) %>% unique() %>%  mutate(ams = TRUE)

add_to_ams <- ams_taps %>% 
  left_join(ams_names, by = "about") %>% 
  mutate(ams = case_when(ams == TRUE ~ TRUE,
                         .default = FALSE)) %>% 
  filter(ams == FALSE)

ams_licenses_clean %>% 
  group_by(taps) %>% 
  reframe(n = n())

rnz_keepers <- c(
  "Michael Brake",
  "Ethan Blight",
  "Kristen Froude",
  "Kobe Miller",
  "Jonte Wright",
  "Thomas Russel",
  "Matthew Dunham",
  "Blake Bradshaw",
  "Arie Magasiva",
  "Eva Hofmans",
  "Holly Gray",
  "Isabella-Hope Murray",
  "Amelia Barrell",
  "Annabel Wynn-Williams",
  "Nicole Vance",
  "Ella Hansen",
  "Brooke Pitchford",
  "James Hindle-Daniels",
  "Georgia Nugent-O'Leary")

athlete_groups <- ams_licenses %>% 
  clean_names() %>% 
  filter(!(paste(first_name, last_name, sep = " ") %in% test_group),
         !grepl("@teamworks.com|acc.connector", email_address)) %>% 
  
  select(-roles, most_recent_login) %>% 
  mutate(athlete_groups = str_replace_all(athlete_groups, "\\[|\\]", "")) %>% 
  separate_wider_delim(athlete_groups, delim = " | ", names_sep = "", too_few = "align_start")

ams_deactivation_modifiers_wide <- ams_deactivation_modifiers %>% 
  mutate(across(17:ncol(.), ~ case_when(grepl("rmd_|Paris|2023|2024|Rowing|MyAccount", .) ~ .,
                                        
                                        .default = NA_character_) )) %>% 
  unite("athlete_groups", 17:ncol(.), sep = "|", remove = TRUE, na.rm = TRUE) %>% 
  separate_wider_delim(athlete_groups, delim = "|", names_sep = "", too_few = "align_start") %>% 
  mutate(about = paste(first_name, last_name, sep = " ")) %>% 
  left_join(ams_taps %>%  select(about) %>% mutate(active = TRUE), by = "about") %>% 
  rename(ams_account_active = active.x,
         active = active.y) %>% 
  mutate(active = case_when(about %in% rnz_keepers ~ TRUE,
                            .default = active)) %>% 
  filter(is.na(active))


write.csv(ams_deactivation_modifiers_wide, file = "2025-03-13_clean-up.csv")


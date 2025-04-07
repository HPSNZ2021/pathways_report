library(neon)


ams_sb_taps_raw <-  pull_smartabase(form = "HPSNZ TAPS",
                                   start_date = "01/10/2022",
                                   end_date = "13/03/2025",
                                   filter_user_key   = "group",
                                   filter_user_value = "MyAccount") %>%
  clean_names()

ams_sb_taps_deactivate <- ams_sb_taps_raw %>% 
  select(about, start_date, sport, carding_status, sex, username, email, dob, user_id) %>% 
  mutate(start_date = dmy(as.character(start_date))) %>% 

  mutate(about = case_when(about == "Kergozou De La Boessiere Nicholas" ~ "Nicholas Kergozou De La Boessiere",
                           about == "Rosie Elliot"                      ~ "Rosie Elliott",
                           about == "van der Kaay Nicole"               ~ "Nicole van der Kaay",
                           about == "Melville Ives Campbell"            ~ "Campbell Melville Ives",
                           about == "Melville Ives Finley"              ~ "Finley Melville Ives",
                           about == "Lee Rush George"                   ~ "George Lee Rush",
                           about == "ten Have Veerle"                   ~ "Veerle ten Have",
                           about == "Da Silva Julien"                   ~ "Julien Da Silva",
                           about == "Van Veldhuizen Breeze"             ~ "Breeze Van Veldhuizen",
                           about == "Crisp Hughes Zoe"                  ~ "Zoe Crisp Hughes",
                           about == "De Jager Mia"                      ~ "Mia De Jager",
                           about == "Dunning Beck Logan"                ~ "Logan Dunning Beck",
                           about == "Landers Murphy Amanda"             ~ "Amanda Landers Murphy",
                           about == "De Villiers Moira"                 ~ "Moira De Villiers",
                           about == "Li Yanhao (Dwayne)"                ~ "Yanhao (Dwayne) Li",
                           about == "de Wit Amber"                      ~ "Amber de Wit",
                           
                           .default = about),
         
         sport = case_when(is.na(sport) ~"UNKNOWN",
                           sport == ""  ~ "UNKNOWN",
                           .default = sport),
         carding_status = case_when(carding_status %in% c("ELITE PERFORMANCE - CONVERT",
                                                          "ELITE PERFORMANCE - PROBABLE",
                                                          "PERFORMANCE POTENTIAL",
                                                          "FOUNDATION PERFORMANCE - OPPORTUNITY",
                                                          "TALENT CONFIRMATION",
                                                          "TALENT IDENTIFICATION",
                                                          "TRANSITION SUPPORTED",
                                                          "DUTY OF CARE") ~ carding_status,
                                    .default                              = "NOT SUPPORTED"),
         carding_status = factor(carding_status,
                                 levels = c("ELITE PERFORMANCE - CONVERT",
                                            "ELITE PERFORMANCE - PROBABLE",
                                            "PERFORMANCE POTENTIAL",
                                            "FOUNDATION PERFORMANCE - OPPORTUNITY",
                                            "TALENT CONFIRMATION",
                                            "TALENT IDENTIFICATION",
                                            "TRANSITION SUPPORTED",
                                            "DUTY OF CARE",
                                            "NOT SUPPORTED"
                                 )),
         
         active = case_when(as.numeric(carding_status) <= 8 ~ as.logical(TRUE),
                            .default = as.logical(FALSE))
  ) %>% 
  filter(!(about %in% test_group))  %>% 
  group_by(about) %>%
  fill(sport, carding_status,  .direction = "updown" ) %>% 
  filter(start_date == max(start_date)) %>% 
  ungroup() 

write.csv(ams_sb_taps_deactivate, file = "2025_03_12_ams_deactivate.csv")
       
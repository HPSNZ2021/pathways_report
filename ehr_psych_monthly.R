ams_psychology_consults_wrangled <- ams_psychology  %>% 
  rename(about = user_name_fixed,
         start_date = start_time) %>%
  mutate(start_date = round_date(start_date, "day"),
         start_date = ymd(as.character(start_date))
  ) %>%
  rbind(cleaned_nzoc_psychology_df) %>% 
  select(start_date, about, aps_discipline, entered_by_id) %>% 
  filter(!(entered_by_id %in% c(27431,
                                27436,
                                27511,
                                38356,
                                29836,
                                29845))) %>% 
  left_join(ams_users, by = c("entered_by_id" = "id" )) %>% 
  rename(about = about.x,
         entered_by = about.y)


ams_psych_monthly <- ams_psychology_consults_wrangled %>% 
  group_by(month = floor_date(start_date, unit = "months")) %>%
  reframe(n = n()) 

ams_psych_monthly_table <- ams_psych_monthly %>% 
  mutate(month_x = month(month, label = TRUE, abbr = FALSE),
         year = year(month)) %>% 
  select(-month) %>% 
  rename(month = month_x) %>% 
  arrange(month) %>% 
  pivot_wider(names_from = year,
              values_from = n) %>% 
  select(month, `2022`, `2023`, `2024`, `2025`) 

ams_psych_monthly_table_summary_stats <- ams_psych_monthly_table %>% 
  reframe(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(month = "Mean")

ams_psych_monthly_cost <- ams_psych_monthly_table_summary_stats %>% 
  reframe(across(where(is.numeric), ~ 1.7*(.x))) %>% 
  mutate(month = "Average monthly cost")


bind_rows(ams_psych_monthly_table, 
          ams_psych_monthly_table_summary_stats,
          ams_psych_monthly_cost) %>% 
  flextable() %>% 
  colformat_double(digits= 0,
                   na_str = "-") %>% 
  colformat_double(i = ~ month == "Average monthly cost", 
                   j = c("2022", "2023", "2024", "2025"),
                   digits= 2,
                   prefix = "$") %>% 
  hline(i = ~ before(month, "Mean"), border = fp_border_default()) %>% 
  autofit()

x <- ams_psych_monthly %>% 
  ggplot(aes(month, n)) +
  geom_col() 


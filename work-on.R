add <- aps_consults %>% filter(is.na(staff_region)) %>% select(entered_by) %>% unique() %>% pull()


xxx <- aps_weekly_caseload %>% 
  distinct(about) %>% 
  mutate(week = map2(as.Date(ymd(params$report_date_min)),
                               as.Date(ymd(params$report_date_max)), 
                               seq, 
                               by = "1 week")) %>% 
  # mutate(week = map2(as.Date(ymd(params$report_date_min)),
  #                    as.Date(ymd(params$report_date_max)), 
  #                    seq, 
  #                    by = "1 week"),
  #        week = ymd(as.character(week))) %>% 
  unnest(cols = c(week))  %>% 
  distinct(about, week) %>%
  ungroup() 

yyy <- xxx %>% 
  left_join(aps_weekly_caseload %>% ungroup() %>% mutate(week = ymd(week)), 
            by = join_by("about", "week"),
            keep = TRUE,
            relationship = "many-to-many") #%>% 
  rename(week = week.x, about = about.x) %>% 
  select(-week.y, - about.y) %>% #

    
    aps_weekly_caseload %>% 
    group_by(week, 
             aps_discipline) %>% 
    reframe(n = n()) %>% 
    filter(!is.na(aps_discipline)) %>% 
    ggplot(aes(week, n , fill = aps_discipline)) + 
    geom_col( colour = "#333F4880") +
    labs(title = "Overall Case Load",
         fill = "Support team",
         x = "Date",
         y = "Consults") +
    scale_y_continuous(breaks = ~round(unique(pretty(.)))) +
    scale_fill_aps() +
    theme(plot.background = element_rect(colour = "#000000", fill=NA, size=1),
          legend.justification='left',
          legend.position = "bottom")
  
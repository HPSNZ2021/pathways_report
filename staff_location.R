akl_staff <- tibble(staff_region     = "Auckland",
                    start_date = ymd("2022.01.01"),
                    end_date   = NA_Date_,
                    about      = c("Jane Knobloch",
                                   "Isobel Freeman",
                                   "Jennifer Scott",
                                   "Jordan Salesa",
                                   "Louise Johnson",
                                   "Mark Overington",
                                   "Rebecca Longhurst",
                                   "Craig Panther",
                                   "Dan Exeter",
                                   "Helen Fulcher",
                                   "Jane Probert",
                                   "Lynne Coleman",
                                   "Sam Wootton",
                                   "Stephen Kara",
                                   "Theo Dorfling",
                                   "Christel Dunshea-Mooij",
                                   "Glenn Kearney",
                                   "Jeff Rothschild",
                                   "Jeni Pearce",
                                   "Kelsey Paterson",
                                   "Craig Barretto",
                                   "Campbell Thompson",
                                   "Jhan Gavala",
                                   "John Sullivan",
                                   "Lucy Divers",
                                   "Sarah de Wattignar",
                                   "Carolyn Donaldson",
                                   "Christine Arthur",
                                   "Cushla Phillips",
                                   "Hannah McLean",
                                   "Nicola Lewis-Clifford",
                                   "Vicki Hudson",
                                   "Whare Kite",
                                   "Rachael Ward")) %>% 
  select(about, staff_region, start_date, end_date)

cambridge_staff <- tibble(staff_region     = "Cambridge",
                          start_date = ymd("2022.01.01"),
                          end_date   = NA_Date_,
                          about      = c("Brendan O'Neill",
                                         "Judith May",
                                         "Frances Stringfellow",
                                         "Deb Robinson",
                                         "Justin Ralph",
                                         "Rone Thompson",
                                         "Megan Munro",
                                         "Andrew Annear",
                                         "Lauren Shelley",
                                         "Riley Smith",
                                         "Katie Schofield",
                                         "Louise Davey",
                                         "Rachael Lockhart",
                                         "Christina Jacklin",
                                         "Tim Dovbysh",
                                         "Jacinta Horan",
                                         "Helena Graham",
                                         "Jessie Speedy",
                                         "Carla Johl",
                                         "Lucy Trollope",
                                         "Abi Chapman")) %>% 
  select(about, staff_region, start_date, end_date)

karapiro_staff <- tibble(staff_region     = "Karapiro",
                         start_date = ymd("2022.01.01"),
                         end_date   = NA_Date_,
                         about      = c("Judikje Scheffer",
                                        "Stuart Armstrong",
                                        "Chris Milne",
                                        "Kirstine Sutton",
                                        "Katie Snyman",
                                        "Melissa Gilbertson",
                                        "Katherine Rottier",
                                        "Cameron Boland",
                                        "Kara Thomas",
                                        "Stent Card",
                                        "Carey Pohl",
                                        "Craig Newlands",
                                        "Isobel Freeman",
                                        "Julia Bone",
                                        "Jane Knobloch",
                                        "Tina Manker",
                                        "Yolande Schyff")) %>% 
  select(about, staff_region, start_date, end_date)

chch_staff <- tibble(staff_region     = "Christchurch",
                     start_date = ymd("2022.01.01"),
                     end_date   = NA_Date_,
                     about      = c("Lesley Nicol",
                                    "Eloise Matthews",
                                    "Tamsin Chittock",
                                    "Amos Johnson",
                                    "Kelsi Parker",
                                    "Rebecca Cooke",
                                    "Sara Richardson",
                                    "Jason Yuill-Proctor",
                                    "Anna Simcic",
                                    "Ben Ardagh",
                                    "Jess Moulds",
                                    "Ellie Bell")) %>% 
  select(about, staff_region, start_date, end_date)

dunedin_staff <- tibble(staff_region     = "Dunedin",
                        start_date = ymd("2022.01.01"),
                        end_date   = NA_Date_,
                        about      =c("Peter Gallagher",
                                      "Helen Littleworth")) %>% 
  select(about, staff_region, start_date, end_date)

wanaka_staff <- tibble(staff_region     = "Wanaka",
                       start_date = ymd("2022.01.01"),
                       end_date   = NA_Date_,
                       about      = c("Sarah Beable",
                                      "Nat Anglem",
                                      "Sarah Gillespie",
                                      "Ginny Rutledge",
                                      "Carolyn Cruden",
                                      "Mariane Wray",
                                      "Helene Barron",
                                      "Carol Goodlass")) %>% 
  select(about, staff_region, start_date, end_date)

welly_staff <- tibble(staff_region     = "Wellington",
                      start_date = ymd("2022.01.01"),
                      end_date   = NA_Date_,
                      about      = c("Tania Don",
                                     "Waimarama Taumaunu",
                                     "Helen Regan",
                                     "Rebecca Jones")) %>% 
  select(about, staff_region, start_date, end_date)


# Natalie Fraser
# Daniella Cameron

staff_location <- akl_staff %>% 
  rbind(cambridge_staff, karapiro_staff, chch_staff, wanaka_staff, dunedin_staff, welly_staff)

staff_location <- staff_location %>% 
  mutate(start_date = case_when(about == "Isobel Freeman" & staff_region == "Auckland" ~ ymd("2025.01.01"),
                                about == "Jane Knobloch" & staff_region == "Auckland" ~ ymd("2024.09.01"),
                                .default = start_date),
         end_date = case_when(about == "Isobel Freeman" & staff_region == "Karapiro" ~ ymd("2024.12.31"),
                              about == "Jane Knobloch" & staff_region == "Karapiro" ~ ymd("2024.08.31"),
                                .default = end_date))



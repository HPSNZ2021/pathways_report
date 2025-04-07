library(tidyverse)
library(neon)


if (exists("EHR_group")){} else {EHR_group = "MyAccount"}

ehr_clinical_report <- tryCatch(
  
  {
    pull_smartabase(
      form                = "CLINICAL PSYCHOLOGY assessment",
      start_date          = "01/01/2016",
      end_date            = format(Sys.Date(), format = "%d/%m/%Y")  ,
      filter_user_key     = "group",
      filter_user_value   = EHR_group) 
    
  },
  
  error=function(cond) {
    message(paste("Did you load your libraries? Check your filter_user_name is in quotes? Dates are in `dd/mm/yyyy` format, OR... there is no data in Dx HRI"))
    
    return(NA)
  },
  
  warning=function(cond) {
    message(paste("Dx HRI caused a warning"))
    
    return(NULL)
  }
)


bulk_upload <- ehr_clinical_report

names(bulk_upload)

# NEED TO RUN 7:1
bulk_upload$entered_by_user_id %>% unique()

26834 32218 28003

push_smartabase(df   = bulk_upload %>% 
                  filter(entered_by_user_id == "28003") %>% 
                  select(-c(support_status, 
                            Name, 
                            Age, 
                            Ethnicity, 
                            Sport, 
                            TAPS, 
                            `Medical Alert`, 
                            Allergies, 
                            `Injury Status`, 
                            `Illness Status`, 
                            session_summary, 
                            url)),
                form = "CLINICAL PSYCHOLOGY assessment",
                edit_event = TRUE,
                table_fields = c(
                  "Topic Area",
                  "Topic",
                  "MAINTAINING FACTORS",
                  "PSYCHOLOGICAL SKILLS",
                  "INTERVENTIONS",
                  "PRIORITIES",
                  "PROGRESS",
                  "FOLLOW UP",
                  "Risk Type",
                  "Level",
                  #"Urgency",
                  "Risk Summary",
                  "Safety Plan"
                  )
                )









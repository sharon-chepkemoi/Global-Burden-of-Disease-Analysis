library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble)
library(forcats)

## Clean dataset

df_hararge_clean <- df_list[["Hararghe_Ethiopia_VA"]][["Hararghe_Ethiopia_Original_VA.dta"]] %>%
  dplyr::mutate(dob = as.Date("1960-01-01") + months(dob, abbreviate = FALSE) #Stata, a %tm variable is a monthly date
                , death_date = as.Date("1960-01-01") + months(death_date, abbreviate = FALSE) #Stata, a %tm variable is a monthly date
                , age_at_death = round(lubridate::time_length(difftime(death_date, dob, units = "auto")
                                                              , unit = "year"
                                                              ),2 #calculating age
                                       )
                , site = ifelse(site == "HU_HU", "Haramaya",
                                     ifelse(site == "H", "Harar",
                                            ifelse(site == "K", "Kersa", site
                                                   )
                                            )
                                     )
                , residence = ifelse(site == "Haramaya", "Peri-Urban",
                                     ifelse(site == "Harar", "Urban",
                                            ifelse(site == "Kersa", "Rural", site
                                                   )
                                            )
                                     )
                , cause1 = ifelse(cause1 == "", "va not done", cause1)
                , weath_sc = forcats::fct_recode(weath_sc
                                                 , "lower" = "poorest" #rename factor level "poorest"  to "lower"
                                                 , "middle lower" = "poor" #rename factor level "poor"  to "middle lower"
                                                 , "middle upper" = "rich" #rename factor level "rich"  to "middle upper"
                                                 , "upper" = "richest" #rename factor level "richest"  to "upper"
                                                 )
                , across(where(is.factor),  ~forcats::fct_drop(.x )) #drop unused factor levels
                ) %>%
  dplyr::rename(any_of(new_hdss_var_names[["hararghe_rename_vars_df"]]) #rename varaible names
                ) %>% 
  labelled::set_variable_labels(!!!new_hdss_labels[["hararghe_rename_vars_df"]][names(new_hdss_labels[["hararghe_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                ) 
#%>%
#  dplyr::select(any_of(select_common_vars_df$new_variable)
 #               )


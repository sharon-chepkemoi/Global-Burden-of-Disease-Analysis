library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble)
library(forcats)

## Clean dataset

df_meiru_clean <- df_list[["MEIRU_Malawi_VA"]][["MEIRU_Malawi_Original_VA.dta"]] %>%
  tidyr::drop_na(insilico_cod1) %>%
  dplyr::mutate(assetscore = as.factor(assetscore)
                , across(where(is.factor), ~ forcats::fct_recode(.x, 
                                                               NULL = "Missing"
                                                               , NULL = "99"
                                                               )
                         ) #recode levels to NULL
                , residence = "Rural"
                , location_name = "Karonga"
                , death_year = lubridate::year(dod)
                , age_at_death = round(lubridate::time_length(difftime(dod, birth_date, units = "auto")
                                                              , unit = "year"
                                                              ),2 #calculating age
                                       )
                , ses_quintile = forcats::fct_collapse(dwellscore
                                                       , "lower" = c("1", "2") # collapse levels to "lower"
                                                       , "middle lower" = c("3", "4") #collapse levels to "middle lower"
                                                       , "middle" = c("5", "6") #collapse levels to "middle"
                                                       , "middle upper" = c("7", "8") #collapse levels to "middle upper"
                                                       , "upper" = c("9", "10") #collapse levels to "upper"
                                                       )
                , across(where(is.factor),  ~forcats::fct_drop(.x )) #drop unused factor levels
                ) %>%
  dplyr::rename(any_of(new_hdss_var_names[["meiru_rename_vars_df"]]) #rename varaible names
                ) %>% 
  labelled::set_variable_labels(!!!new_hdss_labels[["meiru_hdss_rename_vars_df"]][names(new_hdss_labels[["meiru_hdss_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                ) 
#%>%
  #dplyr::select(any_of(select_common_vars_df$new_variable)
         #       )
  

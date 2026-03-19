library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble)
library(forcats)

##-----------------load, rename, label ,Clean dataset -----------------------------------------------

df_iganga_clean <- df_list[["Iganga Mayuge_Uganda_VA"]][["Iganga Mayuge_Uganda_VA.dta"]] %>%
  dplyr::mutate(across(where(is.factor),  ~forcats::fct_drop(.x )) #drop unused factor levels
                ) %>%
  dplyr::arrange(birthdate, dodyear, deathdate, sex) %>%
  dplyr::rename(any_of(new_hdss_var_names[["iganga_rename_vars_df"]]) #rename varaible names
                ) %>% 
  labelled::set_variable_labels(!!!new_hdss_labels[["iganga_rename_vars_df"]][names(new_hdss_labels[["iganga_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                ) 

#%>%
  #dplyr::select(any_of(select_common_vars_df$new_variable)
   #             )
  
 
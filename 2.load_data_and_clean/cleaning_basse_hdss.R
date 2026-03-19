#------------ load the necessary packages --------------------------------------------------------------

if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr,tibble)

#------------------------------------------ Load data set -------------------------------------------------

df_basse_clean <- df_list[["Basse_Gambia_VA"]][["Basse_Gambia_VA.dta"]] %>%
  dplyr::mutate(death_year = lubridate::year(dthdate)
    
    ,across(where(is.factor),  ~forcats::fct_drop(.x )) #drop unused factor levels
  ) %>%
  dplyr::rename(any_of(new_hdss_var_names[["basse_rename_vars_df"]]) #rename varaible names
  ) %>% 
  labelled::set_variable_labels(!!!new_hdss_labels[["basse_rename_vars_df"]][names(new_hdss_labels[["basse_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
  ) 


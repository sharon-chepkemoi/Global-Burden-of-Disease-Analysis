#------------ load the necessary packages --------------------------------------------------------------



### Main data
df_niakhar_clean <- df_list[["Niakhar_Senegal_VA"]][["Niakhar_Senegal_VA.dta"]] %>%
  dplyr::mutate(across(where(is.factor),  ~forcats::fct_drop(.x )) #drop unused factor levels
  ) %>%
  dplyr::rename(any_of(new_hdss_var_names[["niakhar_rename_vars_df"]]) #rename varaible names
  ) %>% 
  labelled::set_variable_labels(!!!new_hdss_labels[["niakhar_rename_vars_df"]][names(new_hdss_labels[["niakhar_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
  ) 







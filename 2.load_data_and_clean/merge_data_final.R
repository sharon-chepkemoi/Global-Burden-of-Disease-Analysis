library(dplyr)
library(forcats)
library(haven)
library(stringr)
library(janitor)
library(labelled)
library(writexl)
library(readr)


## Merging to get one dataset

### Check if output is true
janitor::compare_df_cols_same(df_match_merged_list
                                   )

df_final <- dplyr::bind_rows( df_match_merged_list
                                     ) %>%
  dplyr::mutate(across(c(gender, residence, death_year), ~as.factor(.x))
                , across(c(site_name, country), ~forcats::as_factor(.x))
                , gender = forcats::fct_collapse(gender
                                                 , "Female" = c("Female", "F", "female","f") # collapse levels to "Female"
                                                 , "Male" = c("Male", "M", "male","m") #collapse levels to "Male"
                                                 )
                , cause_of_death = stringr::str_to_lower(cause_of_death)
               
                , age_group_at_death = if_else(age_at_death < 5 , "Under 5",
                                               if_else(age_at_death < 15 , "5-14",
                                                       if_else(age_at_death < 50 , "15-49","50+"
                                                            
                                                               )
                                                       )
                                               )
                , age_group_at_death = factor(age_group_at_death, levels = c("Under 5", "5-14", "15-49",
                                                                             "50+")
                                              )
                ) 


#distinct causes of death

causes <- df_final %>%
  dplyr::distinct(cause_of_death)

  #writexl::write_xlsx(causes,
          #            path = base::file.path(output_Dir, paste0("causes_of_death.xlsx") )
 # )
                      
  # Combine all HDSS label lists into one
all_hdss_labels <- new_hdss_labels %>%
  purrr::reduce(c)

# Combine ALL labels (manual + dictionaries)
all_labels <- c(
  list(
    age_group_at_death = "Age group at Death (Years)",
    cause_of_death_new = "Cause of death",
    icd_10_cause_of_death = "ICD10 WHO group causes of death",
    general_cause_of_death = "General WHO Cause of death"
  ),
  new_labels,
  all_hdss_labels
)                    
                      
df_final2 <- df_final %>%
  left_join(causes_of_death_df %>%
            dplyr::select(cause_of_death, cause_of_death_new,
             icd_10_cause_of_death, general_cause_of_death) %>%
            dplyr::distinct(),
            by = "cause_of_death"
  ) %>%
  labelled::set_variable_labels(
    !!!all_labels[names(all_labels) %in% names(.)]
  )                      
                      


### creating data dictionary
#attribute <- as.data.frame(labelled::generate_dictionary(df_final2, labels = TRUE, values = TRUE)
                         #  )

### Saving data dictionary
# writexl::write_xlsx(attribute,
#                     path = base::file.path(output_Dir, paste0("VA_data_dictionary_merged_final.xlsx") )
#                     )

## saving merged dataset
 # haven::write_dta(data= df_final2, 
 #                 path = base::file.path(output_Dir, "VA_INSPIRE_all_merged_final.dta")
 #                  )

# haven::write_sav(data= df_final, 
#                  path = base::file.path(output_Dir, "VA_INSPIRE_merged_final.sav")
#                  )







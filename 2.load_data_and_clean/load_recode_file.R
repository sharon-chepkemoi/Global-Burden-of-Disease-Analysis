library(dplyr)
library(readxl)
library(tibble)
library(stringr)

working_directory

## Reading the recode file sheet

recode_file <- read_excel_allsheets("./2.load_data_and_clean/VA_recode_file.xlsx")

study_details <- recode_file[["study"]]

hararghe_rename_vars_df <- recode_file[["Hararghe vars"]] #df for renaming variable labels
meiru_rename_vars_df <- recode_file[["Meiru vars"]] #df for renaming variable labels
niakhar_rename_vars_df <- recode_file[["Niakhar vars"]] #df for renaming variable labels
iganga_rename_vars_df <- recode_file[["Iganga Mayuge vars"]] #df for renaming variable labels
ouagadougou_rename_vars_df <- recode_file[["Ouagadougou vars"]] #df for renaming variable labels
basse_rename_vars_df <- recode_file[["Basse vars"]]

vselect_common <- recode_file[["Common_vars"]] #df for selecting common variables

merged_common <- recode_file[["merge_common_vars_labels"]] #df for renaming variable labels

#selected_vars_df <- recode_file[["selected_vars"]] #df for choosing variables for analysis and plots

#drop_selected_vars_df <- recode_file[["drop_selected_vars"]] #df for dropping analysis variables not needed for modelling

causes_of_death_df <- recode_file[["causes_of_death_all"]] #df for collating causes of death


## Creating a named vector to quickly assign variable names and labels

#create labels
rename_vars_df <- sapply(ls(pattern = "_rename_vars_df$"), function(x){
  nn <- x 
  df_new <- get(x)
  #print("The df_new is", df_new)
  
  out <- df_new %>%
    dplyr::mutate(new_label = stringr::str_to_sentence(new_label))
  
}, simplify=FALSE)


new_hdss_var_names <-  sapply(names(rename_vars_df), function(x){ 
  out <- rename_vars_df[[x]] %>%
  dplyr::select(new_variable_name, new_names_janitor) %>%
  tibble::deframe()
  
}, simplify=FALSE)

new_hdss_labels <-  sapply(names(rename_vars_df), function(x){ 
  out <- rename_vars_df[[x]] %>%
  dplyr::select(new_variable_name, new_label) %>%
  tibble::deframe()
  
}, simplify=FALSE)

new_labels <- merged_common %>%
  dplyr::select(new_variable_name, new_label) %>%
  tidyr::drop_na() %>%
  tibble::deframe()




if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr,tibble)

#------------------------------------------ Load data set -------------------------------------------------

file_path <- file.path(data_Dir, "Niakhar_Senegal_VA.xlsx")
Niakha_VA <- read_excel_allsheets(file_path)


# Clean column names for all sheets first
Niakha_VA_clean <- lapply(Niakha_VA, function(df) {
  df %>% janitor::clean_names()
})


#-------------------------------------- Clean variable -------------------------------------------------

# Extract all unique column names across sheets
all_columns <- data.frame(unique(unlist(lapply(Niakha_VA_clean, names))))


#writexl::write_xlsx(all_columns, path = file.path(output_Dir, "Niakhar_all_columns.xlsx"))

#read for renaming
rename_df <- read_excel(file.path(output_Dir, "Niakhar_unique_variables.xlsx"))

#create a name vector
name_vector <- setNames(rename_df$new_name, rename_df$old_name)

Niakha_VA_clean <- lapply(Niakha_VA_clean, function(df) {
  # Only rename columns that exist in the data
  existing_cols <- intersect(names(df), names(name_vector))
  names(df)[match(existing_cols, names(df))] <- name_vector[existing_cols]
  return(df)

})

#convert to characters
Niakha_VA_clean <- lapply(Niakha_VA_clean, function(df) {
  df[] <- lapply(df, function(col) {
    if (is.factor(col)) {
      as.character(col)   # Convert factors safely
    } else {
      as.character(col)   # Convert numeric/other types safely
    }
  })
  return(df)
})




Niakha_VA_clean <- lapply(Niakha_VA_clean, function(df) {
  if("date_of_death" %in% names(df)) {

    x <- df$date_of_death
    x <- trimws(x)
    x[x %in% c("", "NA", "N/A", "n/a")] <- NA  # handle obvious missing

    # Detect numeric Excel dates
    is_excel_num <- grepl("^[0-9]+$", x) & !is.na(x)

    # Convert Excel numeric dates
    x[is_excel_num] <- as.character(as.Date(as.numeric(x[is_excel_num]), origin = "1899-12-30"))

    # Detect remaining valid character dates: skip NAs
    is_char_date <- !is_excel_num & !is.na(x)

    if(any(is_char_date)) {
      # Only parse entries that are actual strings
      parsed <- suppressWarnings(parse_date_time(x[is_char_date], orders = c("ymd","dmy","mdy")))
      x[is_char_date] <- as.character(parsed)
    }

    # Replace column and convert to Date
    df$date_of_death <- as.Date(x)

    # Add death_year
    df$death_year <- year(df$date_of_death)
  }

  return(df)
})

# Combine datasets
combined_df <- bind_rows(Niakha_VA_clean) %>%
  mutate(sex = case_when(
    sex %in% c("1", 1, "M", "m", "Male")    ~ "male",
    sex %in% c("2", 2, "F", "f", "Female")  ~ "female",
    sex %in% c("-1", -1)                     ~ NA_character_,
    TRUE                                      ~ NA_character_  # catch all other unexpected values
  )) %>%
  select(-all_of(c("x10","pathology_code_2")))


#write_dta(combined_df, path = file.path(data_Dir, "Niakhar_Senegal_VA.dta"))








library(nnet)

#------------------------------ update age and age group ------------------------------------------------
df <- df_final2 %>%
  mutate(
        birth_date = as.Date(birth_date, format = "%Y-%m-%d"),
        death_date = as.Date(death_date, format = "%Y-%m-%d"),
    # Calculate age_at_death if missing (in years)
        age_at_death = case_when(
                                is.na(age_at_death) & !is.na(birth_date) & !is.na(death_date) ~
                                as.numeric(difftime(death_date, birth_date, units = "days")) / 365.25,
      TRUE ~ age_at_death
    ),
    
    # Update age_group_at_death if missing
    age_group_at_death = case_when(
      is.na(age_group_at_death) & !is.na(age_at_death) & age_at_death < 5 ~ "Under 5",
      is.na(age_group_at_death) & !is.na(age_at_death) & age_at_death >= 5 & age_at_death < 15 ~ "5-14",
      is.na(age_group_at_death) & !is.na(age_at_death) & age_at_death >= 15 & age_at_death < 50 ~ "15-49",
      is.na(age_group_at_death) & !is.na(age_at_death) & age_at_death >= 50 ~ "50+",
      TRUE ~ age_group_at_death  # keep existing value if not missing
    ),
    
    #make age_group_at_death a factor with ordered levels
    age_group_at_death = factor(age_group_at_death, levels = c("Under 5", "5-14", "15-49", "50+"))
  )

#------------------------------ Select variables and exclude Gambia --------------------------------------

df2 <- df %>%
  # Select relevant columns
  dplyr::select(gender, age_at_death, cause_of_death_new, site_name, country, 
         death_year, age_group_at_death, icd_10_cause_of_death) %>%
  
  # Remove Gambia and drop unused country levels
  dplyr::filter(country != "Gambia") %>%
  dplyr::mutate(country = forcats::fct_drop(country)) %>%
  
  # Remove unwanted ICD-10 codes and select years
  filter(!icd_10_cause_of_death %in% c("Unknown", "VA not done", "Ill-defined diseases"),
         !is.na(icd_10_cause_of_death)) %>%
  
  # Recode ICD-10 causes
  mutate(icd_10_cause_of_death = case_when(
    icd_10_cause_of_death %in% c("Skin diseases", "Sense organ disorders", 
                                 "Other respiratory diseases", "Oral conditions") ~ "Other NCDs",
    icd_10_cause_of_death == "Endocrine disorders" ~ "Diabetes mellitus",
    icd_10_cause_of_death == "neuropsychiatric conditions" ~ "Neuropsychiatric conditions",
    icd_10_cause_of_death == "chronic respiratory disease" ~ "Chronic respiratory disease",
    icd_10_cause_of_death %in% c("Dengue", "Malaria") ~ "Malaria/dengue",
    icd_10_cause_of_death %in% c("Genitourinary diseases", "Kidney disorders") ~ "Kidney/genitourinary disorders",
    icd_10_cause_of_death %in% c("Acute lower respiratory infections", "Acute upper respiratory infection") ~ "Acute respiratory infections",
    TRUE ~ icd_10_cause_of_death  # keep all other values unchanged
  ))  %>%
  filter(!is.na(age_group_at_death))%>%
  filter(death_year %in% c(2015,2016,2017,2018,2019,2020,2021)) %>%
  dplyr::mutate(death_year = forcats::fct_drop(death_year)) 
  
#-------------------------------- Save modeling data --------------------------------------------------------
haven::write_dta(data= df2, 
                 path = base::file.path(output_Dir, "VA_modeling_data.dta")
)

#---------------------------------- END ---------------------------------------------------------------------

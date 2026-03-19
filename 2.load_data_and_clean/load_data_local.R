library(dplyr)
library(haven)
library(janitor)
library(tidyr)
library(writexl)
library(labelled)

working_directory

## Reading data from local folder

#--------------------------------------------

data_files <- list.files(path = data_Dir, pattern = "_VA$",  full.names = F)


df_list <- sapply(data_files, function(x){
     # For each folder
      nn <- x
    folder_name <- gsub("_VA$", "", nn)   # remove _VA
    data_subDir <- file.path(data_Dir, nn)
    # Only read dta and excel files
    data_subfiles <- list.files(path = data_subDir, pattern = "\\.dta$|\\.xlsx$",full.names = FALSE)
  
  out <- sapply(data_subfiles, function(y){
    
    file_path <- file.path(data_subDir, y)
    ext <- tools::file_ext(y)
    
    # -------------------------
    # Read file depending on type
    # -------------------------
    
    if(tolower(ext) == "dta") {
      
      df_raw <- haven::read_dta(file_path)
      # Convert labelled columns to factors
      df_raw <- df_raw %>%
        janitor::clean_names() %>%
        mutate(across(where(haven::is.labelled), ~ haven::as_factor(.x)))
      
    } else if(tolower(ext) %in% c("xls", "xlsx")) {
      
      df_raw <- readxl::read_excel(file_path)
      janitor::clean_names() 
      
    } else {
      return(NULL)
    }
    
    
    # -------------------------
    # Add site and country
    # -------------------------
    
    df <- df_raw %>%
      mutate(name = folder_name) %>%
      separate(name,into = c("site_name", "country"),sep = "_",remove = TRUE) %>%
      labelled::set_variable_labels(site_name = "Site Name",country   = "Country")
    
    df
    
  }, simplify = FALSE)
  
  out
  
}, simplify = FALSE)


# Optional: name the list by site for easy reference
#names(df_list) <- gsub("_VA$", "", basename(data_files))

#VA_data_to_clean<- list.files(df_list[[1]][[2]], df_list[[2]], df_list[[3]], df_list[[4]], df_list[[5]][[2]], df_list[[6]])

## creating data dictionary

raw_attribute <- sapply(names(df_list), function(x){
     nn <- x
    list <- names(df_list[[nn]])
  
    out <- sapply(list, function(y) {
    
    df <- base::as.data.frame(labelled::look_for(df_list[[nn]][[y]], labels = TRUE, values = TRUE)) %>%
      dplyr::mutate(file = y
                    #, across(c(levels, value_labels), ~as.character(.x))
                    ) 
    #df <- base::as.data.frame(labelled::generate_dictionary(df_list[[nn]][[y]], labels = TRUE, values = TRUE))
    
  }, simplify=FALSE)
  
   out <- dplyr::bind_rows(out)
  
  }, simplify=FALSE)
  


## Save raw dictionary

#writexl::write_xlsx(raw_attribute,
   #                 path = base::file.path(output_Dir, paste0("raw_attributes_dictionary_janitor_hdss.xlsx") )
   #                 )


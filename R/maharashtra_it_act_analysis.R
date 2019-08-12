library(rvest)
library(dplyr)

json_dir <- "mh_it_act_data/all_json"

extract_act_data <- function(case_id) {
  # case_id <- dir(json_dir)[[1]]
  case_file <- glue::glue("{json_dir}/{case_id}")
  case_id <-
    stringr::str_replace(case_id, pattern = "\\.json", replacement = "")
  case_json <- jsonlite::read_json(case_file)
  names(case_json)[] <-
    stringr::str_to_lower(stringr::str_trim(names(case_json)))
  if ('act' %in% names(case_json)) {
    act_data <- case_json[['act']]
    act_df <-
      act_data %>% read_html() %>% html_table() %>% bind_rows()
    if (nrow(act_df) > 0) {
      return(
        data.frame(
          'case_id' = case_id,
          'under_act' = as.character(act_df$`Under Act(s)`),
          'under_section' = as.character(act_df$`Under Section(s)`)
        )
      )
    } else {
      return(
        data.frame(
          'case_id' = case_id,
          'under_act' = 'not_available',
          'under_section' = 'not_available'
        )
      )
      
    } 
    
  }else {
    return(
      data.frame(
        'case_id' = case_id,
        'under_act' = 'no_act_key_present',
        'under_section' = 'no_act_key_present'
      ))
  }
  
}



# Run it for all case files -----------------------------------------------

all_case_files <- dir(json_dir)
acts_combined <- lapply(X = all_case_files, FUN = extract_act_data)
acts_combined_df <- dplyr::bind_rows(acts_combined)


# Search for section 66/66A related cases ---------------------------------
only_66_cases <- acts_combined_df[grepl(acts_combined_df$under_section, pattern = '66'),]
only_66_cases$under_section <- stringr::str_replace_all(string = only_66_cases$under_section,pattern = "\\.",replacement = "")
extract_section_numbers <- (regmatches(only_66_cases$under_section, gregexpr('\\(?[0-9.]+',text = only_66_cases$under_section)))

mark_66_cases <- function(case_list){
  if(66 %in% unlist(case_list)){
    return('yes')
  } else {
    return('no')
  }
}

flag_66 <- lapply(extract_section_numbers, mark_66_cases) %>% unlist()
only_66_cases$flag_66 <- flag_66
flag_a <- grepl(only_66_cases$under_section, pattern = "a",ignore.case = TRUE)
only_66_cases$flag_a <- flag_a

write.csv(only_66_cases, 'IT_section66_cases_MH.csv', row.names = FALSE)


# Manually label 66A cases and read it back -------------------------------
case_66A <- read.csv('IT_section66_cases_MH.csv', check.names = FALSE, stringsAsFactors = FALSE)
case_66A <- case_66A[case_66A$flag_66A == TRUE, ]
only_66A_ids <- unique(case_66A$case_id)
# case_id <- only_66A_ids[[1]]
prepare_66a_dataset <- function(case_id) {
  case_file <- glue::glue("{json_dir}/{case_id}.json")
  case_json <- jsonlite::read_json(case_file)
  date_of_filing <- case_json$date_of_filing
  cino <- as.character(case_json$cino)
  dt_regis <- as.character(case_json$dt_regis)
  reg_year <- as.character(case_json$reg_year)
  type_name <- as.character(case_json$type_name)
  state_code <- as.character(case_json$state_code)
  state_name <- as.character(case_json$state_name)
  district_code <- as.character(case_json$district_code)
  district_name <- as.character(case_json$district_name)
  return(
    data.frame(
      'date_of_filing' = date_of_filing,
      'cino' = cino,
      'dt_regis' = dt_regis,
      'reg_year' = reg_year,
      'type_name' = type_name,
      'state_code' = state_code,
      'state_name' = state_name,
      'district_code' = district_code,
      'district_name' = district_name
    )
  )
}

all_66A_meta <- lapply(only_66A_ids, prepare_66a_dataset)
all_66A_meta_df <- dplyr::bind_rows(all_66A_meta)

write.csv(all_66A_meta_df, 'details_66A_cases_mh.csv', row.names = FALSE)



# Check for PDF's ---------------------------------------------------------
all_json_path <- Sys.glob("mh_it_act_data/all_data/*/*/*.json")
all_path_components <- strsplit(all_json_path,split = "/")
district_comp <- all_path_components %>% purrr::map(3) %>% unlist()
directory_comp <- all_path_components %>% purrr::map(4) %>% unlist()
file_comp <- all_path_components %>% purrr::map(5) %>% unlist()
path_df <- data.frame('distc'= district_comp,'dirc' = directory_comp, 'filec' = file_comp)
path_df$filec <- stringr::str_replace_all(path_df$filec, pattern = '\\.json', replacement = '')
for_66a_cases <- path_df[path_df$filec %in% only_66A_ids,]
for_66a_cases$directory_path <- glue::glue("mh_it_act_data/all_data/{for_66a_cases$distc}/{for_66a_cases$dirc}")

# dir_path <- for_66a_cases$directory_path[[477]]
check_for_pdf <- function(dir_path){
  if(length(dir(dir_path)) > 0){
    all_files <- dir(dir_path)
    if(TRUE %in% grepl(all_files, pattern = "\\.pdf", ignore.case = TRUE)){
      return('has_pdf') 
    } else {
      return('no_pdf')
    }
  } else {
    return('no_pdf')
  }
}

pdf_available <- lapply(for_66a_cases$directory_path, check_for_pdf) %>% unlist()
for_66a_cases$pdf_available <- pdf_available

all_66A_meta_df <- left_join(all_66A_meta_df, for_66a_cases[,c(3,4,5)], by=c('cino' = 'filec'))
write.csv(all_66A_meta_df, 'details_66A_cases_mh_with_path.csv', row.names = FALSE)



# Arrange files -----------------------------------------------------------
base_dir <- "mh_it_act_data/all_data/"
all_districts <- unique(all_66A_meta_df$district_name)
arrange_files <- function(district_name){
dir.create(glue::glue("mh_it_act_data/sec_66a_cases/{district_name}"))
new_dir <- glue::glue("mh_it_act_data/sec_66a_cases/{district_name}")
get_dirs <- all_66A_meta_df$directory_path[all_66A_meta_df$district_name == district_name]
return(file.copy(get_dirs, new_dir, recursive = TRUE))
}

lapply(all_districts, arrange_files)

# Add google drive link -----------------------------------------------------------
all_66A_meta_df <- read.csv('details_66A_cases_mh_with_path.csv', stringsAsFactors = FALSE)

library(googledrive)
# 66a folder on GDrive - https://drive.google.com/open?id=1fsjrm92SHRo3BlCXUpDnHaMaJN0i0cU5
all_district_folders <- googledrive::drive_ls(path = as_id('https://drive.google.com/open?id=1fsjrm92SHRo3BlCXUpDnHaMaJN0i0cU5'))
drive_link_master <- data.frame('dirName' = c(), 'dirLink' = c())
pb <- progress::progress_bar$new(total = nrow(all_district_folders))
for(i in 1:nrow(all_district_folders)){
  # i <- 1
  get_district_contents <- googledrive::drive_ls(path = as_id(all_district_folders$id[i]))
  link_df <- data.frame('dirName' = get_district_contents$name, 'dirLink' = purrr::map(get_district_contents$drive_resource, 'webViewLink') %>% unlist())
  drive_link_master <- dplyr::bind_rows(drive_link_master, link_df)
  pb$tick()
}

# Create a master file with data and drive links --------------------------

all_66A_db <- all_66A_meta_df
all_66A_db$district_name[all_66A_db$district_name == "Mumbai City Civil Court"] <- "Mumbai CityCivil Court"
all_66A_db$district_name[all_66A_db$district_name == "Mumbai CMM Courts"] <- "Mumbai CMM Court"
all_66A_db$string_length <- nchar(glue::glue("mh_it_act_data/all_data/{all_66A_db$district_name}"))+2
all_66A_db$dirName <- substr(all_66A_db$directory_path, start = all_66A_db$string_length, stop = nchar(all_66A_db$directory_path))
all_66A_db$string_length <- NULL
all_66A_db <- left_join(all_66A_db, drive_link_master, by='dirName')
all_66A_db$directory_path <- NULL
all_66A_db$dirName <- NULL

write.csv(all_66A_db, 'db_66A_mh_drive_link.csv', row.names = FALSE)

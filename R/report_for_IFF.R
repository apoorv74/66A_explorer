

data_dir <- glue::glue("Data/information_tech_act")
all_states <- dir(data_dir)

get_file_stats <- function(state_name){
  
  pdf_dir <- glue::glue('{data_dir}/all_pdf/{state_name}')
  dir.create(pdf_dir)
  
  json_list <- Sys.glob(glue::glue("{data_dir}/{state_name}/*/*/*.json"))
  pdf_list <- Sys.glob(glue::glue("{data_dir}/{state_name}/*/*/*.pdf"))
  file.copy(from = pdf_list, to = pdf_dir)
  
  total_json <- length(json_list)
  total_pdf <- length(pdf_list)
  
  return(data.frame('state_name'=state_name, 'total_json' = total_json, 'total_pdf' = total_pdf))
  
  
}

all_state_stats <- lapply(all_states, get_file_stats)
all_state_stats <- dplyr::bind_rows(all_state_stats)

# Search for 66A reference within orders ----------------------------------
library(pdftools)
library(spacyr)
library(quanteda)
order_dir <- "assam_it_orders/"
all_orders <- dir(order_dir)
all_pdf <- glue::glue("{order_dir}{all_orders}")

combine_all_judgements <- function(pdf_name){
judgement <- list()
print(glue::glue("Processing judgement -> {pdf_name} ... "))
judgement_text <- pdftools::pdf_text(pdf_name)
judgement_text <- sub("[^_]+_([A-Za-z]+)$", "_\\1", judgement_text)
judgement$id <- stringr::str_replace_all(pdf_name,pattern = "\\.pdf",replacement = "")
read_all_paras <- paste0(judgement_text,collapse = " ")
# remove title from all places
# read_all_paras <- stringr::str_replace_all(string = read_all_paras,pattern = title_pattern_1, replacement = '') %>% stringr::str_squish()
# read_all_paras <- stringr::str_replace_all(string = read_all_paras,pattern = title_pattern_2, replacement = '') %>% stringr::str_squish()
# read_all_paras <- stringr::str_replace_all(string = read_all_paras,pattern = title_pattern_3, replacement = '') %>% stringr::str_squish()
judgement$text <- read_all_paras
judgement_language <- cld2::detect_language(read_all_paras)
judgement$language <- judgement_language
return(judgement)
}

x <- lapply(all_pdf, combine_all_judgements)
order_corpus <- quanteda::corpus(purrr::map(x, 'text') %>% unlist())
kwic_orders <- quanteda::kwic(order_corpus, pattern ='66', valuetype='regex') %>% data.frame() %>% View()

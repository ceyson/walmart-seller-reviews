### For a given seller collect back review text, star rating and review date
### and store to a data frame. Write to CSV for local inspection.


### Inits
libs <- c('tidyverse','stringr','rvest','tictoc','data.table')
new.libs <- libs[!(libs %in% installed.packages()[,"Package"])]
if(length(new.libs)) install.packages(new.libs)
lapply(libs, require, character.only = TRUE)

### Seller URL (Nobel Planet)
url_nb <- 'https://www.walmart.com/seller/4754'

### Last page index
indx_last_page <- rvest::read_html(url_nb) %>%
  rvest::html_nodes('.mv0') %>%
  rvest::html_children() %>%
  rvest::html_children() %>%
  rvest::html_children() %>%
  rvest::html_text() %>% 
  dplyr::nth(7) %>%
  as.numeric()

## Empty reviews data frame
df_reviews <- data.frame(date = as.Date(character()),
                         rating = numeric(),
                         review = character())

## Scraping
tictoc::tic('Execution')
for (n_page in c(1:indx_last_page)) {
  
  ## Feedback
  print(paste0('Scraping page ',n_page,' of ',indx_last_page))
  
  ## Nice
  sample(1:2,1) %>% Sys.sleep()
  
  ## Read page
  result <- paste0(url_nb,'?page=',n_page) %>% rvest::read_html()
  
  ## Review dates
  review_dates <- result %>%
    rvest::html_nodes('.w_HmLO') %>%
    rvest::html_nodes('.f7') %>%
    rvest::html_text() %>%
    as.Date(format = '%m/%d/%Y')
  
  ## Star ratings
  star_ratings <- result %>% 
    rvest::html_nodes('.cc-2') %>%
    rvest::html_nodes('.w_HmLO') %>%
    rvest::html_nodes('.w_iUH7') %>%
    rvest::html_text() %>%
    stringr::str_sub(1,1) %>%
    as.numeric()
  
  ## Reviews
  reviews <- result %>%
      rvest::html_nodes('.mb4') %>%
      rvest::html_nodes('.db-m') %>%
      rvest::html_text() %>%
      unique() %>%
      stringr::str_subset(pattern = "...See more", negate = TRUE)
  
  ## Append data
  df_tmp <- 
  df_reviews <- dplyr::bind_cols(
    review_dates
    , star_ratings
    , reviews
  ) %>% 
    stats::setNames(c('date','rating','review')) %>%
    dplyr::bind_rows()
  
}
tictoc::toc()

### Write to CSV
df_reviews %>% data.table::fwrite('./input/reviews.csv')


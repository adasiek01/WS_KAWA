library(httr)
library(rvest)
library(stringi)

base_url <- "https://www.konesso.pl/pol_m_Kawa_Rodzaj_Kawa-ziarnista-2160.html?counter="
titles <- c()
descriptions <- c()
specifications_list <- list()
prices <- c()
producers <- c()
ratings <- c()
reviews_counts <- c()
num_pages <- 65

process_product <- function(product) {
  title <- tryCatch(product %>% html_element("h5") %>% html_text(trim = TRUE), 
                    error = function(e) NA)
  titles <<- c(titles, title)
  
  description <- tryCatch({
    text <- product %>% html_element(".product-desc") %>% html_text(trim = TRUE)
    text <- stri_replace_all_fixed(text, "Przeczytaj dalej", "") %>%
      stri_replace_all_fixed("\n", " ") %>%
      stri_replace_all_regex("\\s+", " ") %>%
      stri_trim_both()
    text
  }, error = function(e) NA)
  descriptions <<- c(descriptions, description)
  
  specifications <- tryCatch({
    product %>% html_elements(".traits_info ul li") %>% html_text(trim = TRUE) %>%
      stri_replace_all_fixed("\n", " ") %>%
      stri_replace_all_regex("\\s+", " ") %>%
      stri_trim_both()
  }, error = function(e) NA)
  specifications_list[[length(specifications_list) + 1]] <<- specifications
  
  producer <- tryCatch(product %>% html_node(".product-info .info a strong") %>% html_text(trim = TRUE), 
                       error = function(e) NA)
  producers <<- c(producers, producer)
  
  rating <- tryCatch({
    text <- product %>% html_node(".info .avg") %>% html_text(trim = TRUE)
    stri_extract_first_regex(text, "\\d+\\.\\d+")
  }, error = function(e) NA)
  ratings <<- c(ratings, rating)
  
  reviews_count <- tryCatch({
    text <- product %>% html_node(".info .comments") %>% html_text(trim = TRUE)
    stri_extract_first_regex(text, "\\d+")
  }, error = function(e) "Brak opinii")
  reviews_counts <<- c(reviews_counts, reviews_count)
  
  price <- tryCatch({
    text <- product %>% html_node(".product_prices .price") %>% html_text(trim = TRUE)
    brutto_price <- stri_extract_first_regex(text, "(\\d+,\\d+)\\s*zł\\s*brutto")
    if (!is.na(brutto_price)) {
      brutto_price <- stri_replace_all_fixed(brutto_price, "brutto", "") %>%
        stri_replace_all_fixed(",", ".") %>%
        stri_replace_all_fixed(" zł", "")
    }
    brutto_price
  }, error = function(e) NA)
  prices <<- c(prices, price)
  
  Sys.sleep(1)
}

for (page_num in 1:num_pages) {
  url <- paste0(base_url, page_num)
  response <- tryCatch(
    GET(url, user_agent("I am a student of PWr. Problem? Write to stud@pwr.pl")),
    error = function(e) {
      message("Błąd podczas pobierania strony dla strony numer ", page_num, ": ", e)
      return(NULL)
    }
  )
  
  if (is.null(response) || status_code(response) != 200) next
  
  webpage <- tryCatch(content(response, "text", encoding = "UTF-8") %>% read_html(), 
                      error = function(e) {
                        message("Błąd podczas parsowania zawartości strony ", page_num, ": ", e)
                        return(NULL)
                      })
  
  if (is.null(webpage)) next
  
  product_containers <- webpage %>% html_nodes("div.product_wrapper")
  lapply(product_containers, process_product)
}

data <- data.frame(
  Title = titles,
  Description = descriptions,
  Price = prices,
  Producer = producers,
  Rating = ratings,
  Review_count = reviews_counts
)
data$Specifications <- sapply(specifications_list, function(specs) {
  if (length(specs) > 0) {
    paste(specs, collapse = " | ")
  } else {
    NA
  }
})

output_file <- "kawa.csv"
write.csv2(data, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")

output_file_with_bom <- "kawa_with_bom_ok.csv"
con <- file(output_file_with_bom, open = "wt", encoding = "UTF-8")
writeLines("\ufeff", con)
write.table(data, file = con, sep = ";", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")
close(con)

print(paste("Data saved to", output_file_with_bom))

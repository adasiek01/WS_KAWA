library(httr)
library(rvest)
library(stringi)

url <- "https://www.konesso.pl/pol_m_Kawa_Rodzaj_Kawa-ziarnista-2160.html"
response <- tryCatch(
  GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36")),
  error = function(e) stop("Błąd podczas pobierania strony: ", e)
)

if (status_code(response) != 200) stop("Nie udało się pobrać zawartości strony")
webpage <- tryCatch(
  content(response, "text", encoding = "UTF-8") %>% read_html(),
  error = function(e) stop("Błąd podczas parsowania zawartości strony: ", e)
)

product_containers <- webpage %>% html_nodes("div.product_wrapper")
titles <- c()
descriptions <- c()
specifications_list <- list()
prices <- c()
producers <- c()
ratings <- c()
reviews_counts <- c()

for (product in product_containers) {
  tryCatch({
    titles <- c(titles, product %>% html_element("h5") %>% html_text(trim = TRUE))
    
    description <- product %>% html_element(".product-desc") %>% html_text(trim = TRUE)
    description <- stri_replace_all_fixed(description, "Przeczytaj dalej", "") %>%
      stri_replace_all_fixed("\n", " ") %>%
      stri_replace_all_regex("\\s+", " ") %>%
      stri_trim_both()
    descriptions <- c(descriptions, description)
    
    specifications <- product %>% html_elements(".traits_info ul li") %>% html_text(trim = TRUE) %>%
      stri_replace_all_fixed("\n", " ") %>%
      stri_replace_all_regex("\\s+", " ") %>%
      stri_trim_both()
    specifications_list[[length(specifications_list) + 1]] <- specifications
    
    producers <- c(producers, product %>% html_node(".product-info .info a strong") %>% html_text(trim = TRUE))
    
    rating_text <- product %>% html_node(".info .avg") %>% html_text(trim = TRUE)
    rating <- stri_extract_first_regex(rating_text, "\\d+\\.\\d+")
    ratings <- c(ratings, ifelse(is.na(rating), NA, rating))
    
    reviews_text <- product %>% html_node(".info .comments") %>% html_text(trim = TRUE)
    reviews_count <- stri_extract_first_regex(reviews_text, "\\d+")
    reviews_counts <- c(reviews_counts, ifelse(is.na(reviews_count), "Brak opinii", reviews_count))
    
    price_text <- product %>% html_node(".product_prices .price") %>% html_text(trim = TRUE)
    brutto_price <- stri_extract_first_regex(price_text, "(\\d+,\\d+)\\s*zł\\s*brutto")
    if (!is.na(brutto_price)) {
      brutto_price <- brutto_price %>%
        stri_replace_all_fixed("brutto", "") %>%
        stri_replace_all_fixed(",", ".") %>%
        stri_replace_all_fixed(" zł", "")
    }
    prices <- c(prices, brutto_price)
    
    Sys.sleep(2)
  }, error = function(e) {
    message("Błąd podczas przetwarzania produktu: ", e)
  })
}

num_products <- min(5, length(titles))
data <- data.frame(
  Title = titles[1:num_products],
  Description = descriptions[1:num_products],
  Price = prices[1:num_products]
)
data$Specifications <- specifications_list[1:num_products]

print(data)

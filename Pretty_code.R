library(rvest)
library(stringi)

url <- "https://www.konesso.pl/pol_m_Kawa_Rodzaj_Kawa-ziarnista-2160.html"
webpage <- read_html(url)
product_containers <- webpage %>% html_nodes("div.product_wrapper")

titles <- c()
descriptions <- c()
specifications_list <- list()
prices <- c()
producers <- c()
ratings <- c()
reviews_counts <- c()

for (product in product_containers) {
  title <- product %>% html_element("h5") %>% html_text(trim = TRUE)
  titles <- c(titles, title)
  
  description <- product %>% html_element(".product-desc") %>% html_text(trim = TRUE)
  description <- stri_replace_all_fixed(description, "Przeczytaj dalej", "")
  description <- stri_replace_all_fixed(description, "\n", " ")
  description <- stri_replace_all_regex(description, "\\s+", " ")
  description <- stri_trim_both(description)
  descriptions <- c(descriptions, description)
  
  specifications <- product %>% html_elements(".traits_info ul li") %>% html_text(trim = TRUE)
  specifications <- stri_replace_all_fixed(specifications, "\n", " ")
  specifications <- stri_replace_all_regex(specifications, "\\s+", " ")
  specifications <- stri_trim_both(specifications)
  specifications_list[[length(specifications_list) + 1]] <- specifications
  
  producer <- product %>% html_node(".product-info .info a strong") %>% html_text(trim = TRUE)
  producers <- c(producers, producer)
  
  rating_text <- product %>% html_node(".info .avg") %>% html_text(trim = TRUE)
  rating <- stri_extract_first_regex(rating_text, "\\d+\\.\\d+")
  if (is.na(rating)) {
    ratings <- c(ratings, NA)
  } else {
    ratings <- c(ratings, rating)
  }
  
  reviews_text <- product %>% html_node(".info .comments") %>% html_text(trim = TRUE)
  reviews_count <- stri_extract_first_regex(reviews_text, "\\d+")
  if (is.na(reviews_count)) {
    reviews_counts <- c(reviews_counts, "Brak opinii")
  } else {
    reviews_counts <- c(reviews_counts, reviews_count)
  }
  
  price_text <- product %>% html_node(".product_prices .price") %>% html_text(trim = TRUE)
  brutto_price <- stri_extract_first_regex(price_text, "(\\d+,\\d+)\\s*zł\\s*brutto")
  if (!is.na(brutto_price)) {
    brutto_price <- stri_replace_all_fixed(brutto_price, "brutto", "")
    brutto_price <- stri_replace_all_fixed(brutto_price, ",", ".")
    brutto_price <- stri_replace_all_fixed(brutto_price, " zł", "")
    prices <- c(prices, brutto_price)
  } else {
    prices <- c(prices, NA)
  }
  
  Sys.sleep(2)
}

num_products <- 5
data <- data.frame(
  Title = titles[1:num_products],
  Description = descriptions[1:num_products],
  Price = prices[1:num_products]
)
data$Specifications <- specifications_list[1:num_products]

print(data)
print(titles[2])
print(descriptions[1])
print(prices[1])
print(specifications_list[[1]])
print(producers[1])
print(ratings[2])
print(reviews_counts[2])
print(specifications_list[[1]][2])

library(rvest)
library(stringi)

# Base URL
base_url <- "https://www.konesso.pl/pol_m_Kawa_Rodzaj_Kawa-ziarnista-2160.html?counter="

# Initialize containers
titles <- c()
descriptions <- c()
specifications_list <- list()
prices <- c()
producers <- c()
ratings <- c()
reviews_counts <- c()

# Number of pages to scrape
num_pages <- 30  # adjust num of products based on pages (1 page = 30 products)

# Loop through each page
for (page_num in 1:num_pages) {
  # Generate the URL for the current page
  url <- paste0(base_url, page_num)
  webpage <- read_html(url)
  product_containers <- webpage %>% html_nodes("div.product_wrapper")
  
  # Extract data for each product on the page
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
  }
  
  # Pause to avoid overloading the server
  Sys.sleep(2)
}

# Create the final data frame
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
    paste(specs, collapse = " | ")  # Combine list elements into a single string
  } else {
    NA  # If no specifications, return NA
  }
})

# Export the data to CSV with UTF-8-BOM encoding and semicolon separator
output_file <- "kawa.csv"
# Use write.csv2 for semicolon separator
write.csv2(data, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")

# Manually add BOM (if required)
output_file_with_bom <- "kawa_with_bom.csv"
con <- file(output_file_with_bom, open = "wt", encoding = "UTF-8")
writeLines("\ufeff", con)  # Add BOM
write.table(data, file = con, sep = ";", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")
close(con)

print(paste("Data saved to", output_file_with_bom))

# View or save the data
print(head(data))
print(titles)


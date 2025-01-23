library(rvest)

# URL strony z kawami
url <- "https://www.konesso.pl/pol_m_Kawa_Rodzaj_Kawa-ziarnista-2160.html"

# Pobierz zawartość strony
webpage <- read_html(url)

# Wyciągnij nazwy kaw
titles <- webpage %>% html_nodes("h5") %>% html_text(trim = TRUE)

# Wyciągnij opisy kaw
descriptions <- webpage %>% html_nodes(".product-desc") %>% html_text(trim = TRUE)

# Wyciągnij wszystkie szczegóły specyfikacji dla każdego produktu
specification_elements <- webpage %>% html_nodes(".traits_info ul li")
specifications <- specification_elements %>% html_text(trim = TRUE)

# Liczba produktów, które chcesz wyciągnąć (np. 5)
num_products <- 5

# Tworzenie ramki danych
data <- data.frame(
  Title = titles[1:num_products],
  Description = gsub("Przeczytaj dalej", "", descriptions[1:num_products])  # Usuń zbędne "Przeczytaj dalej"
)

# Podziel specyfikacje na grupy (każdy produkt ma swoją specyfikację)
specifications_list <- list()

start_idx <- 1
for (i in 1:num_products) {
  product_specifications <- c()
  
  # Określenie zakresu dla pierwszego produktu (do 12 punktów)
  if (i == 1) {
    end_idx <- start_idx + 11
  } else {
    # Dla kolejnych produktów, znajdź początek od "Skład:"
    end_idx <- start_idx
    while (end_idx <= length(specifications) && !grepl("^Skład:", specifications[end_idx])) {
      end_idx <- end_idx + 1
    }
  }
  
  # Przypisz specyfikacje dla produktu
  while (start_idx <= length(specifications) && (start_idx <= end_idx)) {
    product_specifications <- c(product_specifications, specifications[start_idx])
    start_idx <- start_idx + 1
  }
  
  # Przechodzimy do kolejnego produktu
  specifications_list[[i]] <- paste(product_specifications, collapse = "\n")
}

# Połącz tytuł, opis i specyfikacje w jedną ramkę danych
data$Specifications <- unlist(specifications_list)

# Wyświetl dane
print(data)

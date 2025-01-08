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
  # Specyfikacje są rozbite na 12 punktów dla każdego produktu
  end_idx <- start_idx + 17  # Dla każdego produktu, specyfikacje mają 12 punktów
  if (end_idx > length(specifications)) end_idx <- length(specifications)  # Jeśli specyfikacje się kończą
  specifications_list[[i]] <- paste(specifications[start_idx:end_idx], collapse = "\n")
  start_idx <- end_idx + 1
}

# Połącz tytuł, opis i specyfikacje w jedną ramkę danych
data$Specifications <- unlist(specifications_list)

# Wyświetl dane
print(data)

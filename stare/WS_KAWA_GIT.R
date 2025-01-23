library(rvest)

# URL strony z kawami
url <- "https://www.konesso.pl/pol_m_Kawa_Rodzaj_Kawa-ziarnista-2160.html"

# Pobierz zawartość strony
webpage <- read_html(url)

# Wyciągnij wszystkie kontenery produktów
product_containers <- webpage %>% html_nodes("div.product_wrapper")

# Inicjalizowanie zmiennych do przechowywania danych
titles <- c()
descriptions <- c()
specifications_list <- list()

# Iterujemy przez każdy kontener produktu
for (product in product_containers) {
  
  # Wyciągnij tytuł produktu
  title <- product %>% html_node("h5") %>% html_text(trim = TRUE)
  titles <- c(titles, title)
  
  # Wyciągnij opis produktu
  description <- product %>% html_node(".product-desc") %>% html_text(trim = TRUE)
  descriptions <- c(descriptions, description)
  
  # Wyciągnij specyfikacje produktu
  specifications <- product %>% html_nodes(".traits_info ul li") %>% html_text(trim = TRUE)
  
  # Dodaj specyfikacje produktu do listy
  specifications_list <- c(specifications_list, paste(specifications, collapse = "\n"))
}

# Liczba produktów, które chcesz wyciągnąć
num_products <- 5

# Tworzenie ramki danych
data <- data.frame(
  Title = titles[1:num_products],
  Description = gsub("Przeczytaj dalej", "", descriptions[1:num_products])  # Usuń zbędne "Przeczytaj dalej"
)

# Dodaj specyfikacje
data$Specifications <- specifications_list[1:num_products]

# Wyświetl dane
print(data)

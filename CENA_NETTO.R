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
prices <- c()

# Iterujemy przez każdy kontener produktu
for (product in product_containers) {
  
  # Wyciągnij tytuł produktu
  title <- product %>% html_node("h5") %>% html_text(trim = TRUE)
  titles <- c(titles, title)
  
  # Wyciągnij opis produktu
  description <- product %>% html_node(".product-desc") %>% html_text(trim = TRUE)
  
  # Usuń frazę "Przeczytaj dalej"
  description <- gsub("Przeczytaj dalej", "", description)
  
  # Usuń wszystkie znaki nowej linii \n i nadmiarowe spacje
  description <- gsub("\\n", " ", description)  # Zamień \n na pojedynczą spację
  description <- gsub("\\s+", " ", description)  # Zastąp nadmiarowe spacje pojedynczą spacją
  description <- trimws(description)  # Usuń nadmiarowe spacje z początku i końca
  
  descriptions <- c(descriptions, description)
  
  # Wyciągnij specyfikacje produktu
  specifications <- product %>% html_nodes(".traits_info ul li") %>% html_text(trim = TRUE)
  
  # Usuń \n z każdej specyfikacji
  specifications <- gsub("\n", " ", specifications)
  
  # Dodaj specyfikacje produktu jako lista do listy głównej
  specifications_list[[length(specifications_list) + 1]] <- specifications
  
  # Wyciągnij cenę produktu
  price <- product %>% html_node(".price .client-b2b .price-netto") %>% html_text(trim = TRUE)
  
  # Jeśli cena netto jest dostępna, dodaj ją (możesz zmodyfikować, aby uzyskać również cenę brutto)
  if (length(price) == 0) {
    price <- product %>% html_node(".price") %>% html_text(trim = TRUE)
  }
  
  # Dodaj cenę do listy
  prices <- c(prices, price)
}

# Liczba produktów, które chcesz wyciągnąć
num_products <- 5

# Tworzenie ramki danych dla tytułów, opisów i cen
data <- data.frame(
  Title = titles[1:num_products],
  Description = descriptions[1:num_products],  # Opisy bez "Przeczytaj dalej" i nadmiarowych znaków
  Price = prices[1:num_products]  # Ceny
)

# Dodaj specyfikacje
data$Specifications <- specifications_list[1:num_products]

# Wyświetl dane
print(data)

# Przykłady jak można uzyskać dostęp do tytułów, opisów, cen i specyfikacji:
print(titles[2])  # Drugi tytuł
print(descriptions[1])  # Pierwszy opis (bez "Przeczytaj dalej")
print(prices[1])  # Cena pierwszego produktu
print(specifications_list[[1]])  # Specyfikacje dla 1. produktu

# Uzyskanie 2. cechy specyfikacji dla 1. produktu
print(specifications_list[[1]][2])  # 2. element specyfikacji dla 1. produktu

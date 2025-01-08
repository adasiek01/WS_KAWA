library(rvest)
library(stringi)

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
producers <- c()
ratings <- c()
reviews_counts <- c()

# Iterujemy przez każdy kontener produktu
for (product in product_containers) {
  
  # Wyciągnij tytuł produktu
  title <- product %>% html_element("h5") %>% html_text(trim = TRUE)
  titles <- c(titles, title)
  
  # Wyciągnij opis produktu
  description <- product %>% html_element(".product-desc") %>% html_text(trim = TRUE)
  
  # Usuń frazę "Przeczytaj dalej"
  description <- stri_replace_all_fixed(description, "Przeczytaj dalej", "")
  
  # Usuń wszystkie znaki nowej linii \n
  description <- stri_replace_all_fixed(description, "\n", " ")  # Zamień \n na pojedynczą spację
  
  # Zastąp nadmiarowe spacje pojedynczą spacją
  description <- stri_replace_all_regex(description, "\\s+", " ")
  
  # Usuń nadmiarowe spacje z początku i końca
  description <- stri_trim_both(description)
  
  descriptions <- c(descriptions, description)
  
  # Wyciągnij specyfikacje produktu
  specifications <- product %>% html_elements(".traits_info ul li") %>% html_text(trim = TRUE)
  
  # Usuń znaki nowej linii w specyfikacjach
  specifications <- stri_replace_all_fixed(specifications, "\n", " ")  # Zamień \n na pojedynczą spację
  
  # Zastąp nadmiarowe spacje pojedynczą spacją
  specifications <- stri_replace_all_regex(specifications, "\\s+", " ")
  
  # Usuń nadmiarowe spacje z początku i końca
  specifications <- stri_trim_both(specifications)
  
  # Dodaj specyfikacje produktu jako lista do listy głównej
  specifications_list[[length(specifications_list) + 1]] <- specifications
  
  # Wyciągnij producenta produktu
  producer <- product %>% html_node(".product-info .info a strong") %>% html_text(trim = TRUE)
  producers <- c(producers, producer)
  
  # Wyciągnij ocenę produktu
  rating_text <- product %>% html_node(".info .avg") %>% html_text(trim = TRUE)
  rating <- stri_extract_first_regex(rating_text, "\\d+\\.\\d+")  # Wyciągnij wartość liczbową oceny
  if (is.na(rating)) {
    ratings <- c(ratings, NA)  # Jeśli brak oceny, dodajemy NA
  } else {
    ratings <- c(ratings, rating)
  }
  
  # Wyciągnij liczbę opinii lub tekst "Brak opinii"
  reviews_text <- product %>% html_node(".info .comments") %>% html_text(trim = TRUE)
  reviews_count <- stri_extract_first_regex(reviews_text, "\\d+")  # Wyciągnij liczbę opinii
  if (is.na(reviews_count)) {
    reviews_counts <- c(reviews_counts, "Brak opinii")  # Jeśli brak opinii, dodajemy tekst "Brak opinii"
  } else {
    reviews_counts <- c(reviews_counts, reviews_count)
  }
  
  # Wyciągnij pełny tekst z sekcji "price"
  price_text <- product %>% html_node(".product_prices .price") %>% html_text(trim = TRUE)
  
  # Zidentyfikuj cenę brutto, która znajduje się przed słowem "brutto"
  brutto_price <- stri_extract_first_regex(price_text, "(\\d+,\\d+)\\s*zł\\s*brutto")
  
  if (!is.na(brutto_price)) {
    # Jeśli znaleźliśmy cenę brutto, usuwamy "brutto" i zamieniamy przecinek na kropkę
    brutto_price <- stri_replace_all_fixed(brutto_price, "brutto", "")  # Usuń słowo "brutto"
    brutto_price <- stri_replace_all_fixed(brutto_price, ",", ".")  # Zamień przecinek na kropkę
    brutto_price <- stri_replace_all_fixed(brutto_price, " zł", "")  # Usuń spację i "zł"
    prices <- c(prices, brutto_price)   # Dodaj cenę do listy
  } else {
    prices <- c(prices, NA)  # Jeśli cena brutto nie została znaleziona, dodajemy NA
  }
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
print(producers[1]) #producent 1 kawy
print(ratings[2])
print(reviews_counts[2])

# Uzyskanie 2. cechy specyfikacji dla 1. produktu
print(specifications_list[[1]][2])  # 2. element specyfikacji dla 1. produktu



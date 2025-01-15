library(tidyverse)
library(stringi)

# Wczytanie pliku z odpowiednim separatorem
df_kawa <- read_csv2("kawa_with_bom_ok.csv", locale = locale(encoding = "UTF-8"))

# Sprawdzamy pierwsze wiersze, aby zobaczyć format danych
head(df_kawa$Specifications, 10)

# Funkcja, która przetwarza specyfikacje z jednej kawy
process_specifications <- function(specifications_text) {
  # Podziel tekst na poszczególne specyfikacje, oddzielając je znakiem " | "
  specs <- stri_split_fixed(specifications_text, " | ", simplify = TRUE)
  
  # Dzielimy każdą specyfikację na nazwę i wartość po dwukropku
  spec_names <- stri_extract(specs, regex = "^[^:]+(?=:)")
  spec_values <- stri_extract(specs, regex = "(?<=:).+$")
  
  # Tworzymy listę z nazwami i wartościami specyfikacji
  spec_list <- setNames(spec_values, spec_names)
  
  # Zwracamy wynik jako listę
  return(spec_list)
}

# Zastosowanie funkcji do każdej kawy w kolumnie 'Specifications'
specifications_processed <- lapply(df_kawa$Specifications, process_specifications)

# Zamiana wyników na data frame
specifications_df <- bind_rows(specifications_processed)

# Zidentyfikujmy unikalne nazwy kolumn (specyfikacje)
# Teraz 'specifications_df' zawiera te specyfikacje w postaci oddzielnych kolumn
# Sprawdzamy, jak wyglądają te kolumny
colnames(specifications_df)

# Dołączamy te kolumny do głównego data frame
df_kawa <- bind_cols(df_kawa, specifications_df)

# Usuwamy oryginalną kolumnę 'Specifications'
df_kawa <- df_kawa %>% select(-Specifications)

# Wyświetlamy wynikowy data frame
print(head(df_kawa, 10))

colnames(specifications_df)

# Przejdź przez każdą kolumnę w df_kawa i wypisz unikalne wartości
unique_values <- lapply(df_kawa, unique)

# Wyświetl unikalne wartości dla każdej kolumny
for (col in names(unique_values)) {
  cat("\nUnikalne wartości w kolumnie", col, ":\n")
  print(unique_values[[col]])
}


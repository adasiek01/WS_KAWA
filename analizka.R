library(tidyverse)
library(stringi)

# Wczytanie pliku z odpowiednim separatorem
df_kawa <- read_csv2("kawa_with_bom_ok.csv", locale = locale(encoding = "UTF-8"))

# Funkcja, która przetwarza specyfikacje z jednej kawy
process_specifications <- function(specifications_text) {
  specs <- stri_split_fixed(specifications_text, " | ", simplify = TRUE)
  spec_names <- stri_extract(specs, regex = "^[^:]+(?=:)")
  spec_values <- stri_extract(specs, regex = "(?<=:).+$")
  
  spec_list <- setNames(spec_values, spec_names)
  return(spec_list)
}

specifications_processed <- lapply(df_kawa$Specifications, process_specifications)

# Zamiana wyników na data frame
specifications_df <- bind_rows(specifications_processed)

# Dołączamy te kolumny do głównego data frame
df_kawa <- bind_cols(df_kawa, specifications_df)

# Usuwamy oryginalną kolumnę 'Specifications'
df_kawa <- df_kawa %>% select(-Specifications)

# Zidentyfikujmy liczbę nie-NA wartości w każdej kolumnie
non_na_counts <- colSums(!is.na(df_kawa))

# Filtrujemy kolumny, w których liczba nie-NA wartości jest większa lub równa 1000
df_kawa_filtered <- df_kawa[, non_na_counts >= 1000]

# Zmiana nazw kolumn na polski, podkreślenia zamiast spacji i myślników
colnames(df_kawa_filtered) <- c(
  "Tytuł", "Opis", "Cena", "Producent", "Ocena", "Liczba_opinii",
  "Skład", "Stopień_palenia", "Zawartość_kofeiny", "Rodzaj", "Palarnia", 
  "Przeznaczenie", "Opakowanie", "Sposób_przygotowania", "Crema", "Wyczuwalne_smaki", 
  "Intensywność_smaku", "Gorycz", "Kwasowość", "Słodycz", 
  "Pochodzenie_ziaren", "Blend_czy_Single", "Polecana_do", "Kawa_Specialty"
)

# Zamiana spacji oraz myślników na podkreślenia w nazwach kolumn
colnames(df_kawa_filtered) <- gsub(" |-", "_", colnames(df_kawa_filtered))

# Funkcja do konwersji wartości na skalę 1/5 do 5/5
convert_to_5_scale <- function(value) {
  # Usunięcie spacji i zamiana tekstów na odpowiednią skalę
  value <- gsub(" ", "", value)  # Usuwamy białe znaki
  if (grepl("Delikatna", value)) {
    return(0.2)
  } else if (grepl("Średnia", value)) {
    return(0.6)
  } else if (grepl("Mocna", value)) {
    return(1)
  }
  
  # Dla wartości numerycznych jak "2/5", "3/5"
  if (grepl("^\\d+/\\d+$", value)) {
    return(as.numeric(stri_extract(value, regex = "^\\d+")) / as.numeric(stri_extract(value, regex = "(?<=/).*")))
  }
  
  # Dla wartości numerycznych jak "1", "2", "3", "4", "5"
  if (grepl("^\\d+$", value)) {
    if (as.numeric(value) == 1) {
      return(0.2)
    } else if (as.numeric(value) == 2) {
      return(0.4)
    } else if (as.numeric(value) == 3) {
      return(0.6)
    } else if (as.numeric(value) == 4) {
      return(0.8)
    } else {
      return(1)
    }
  }
  
  return(NA)
}

# Zastosowanie funkcji do przekształcenia na skalę 1/5 do 5/5 w odpowiednich kolumnach
df_kawa_filtered$Gorycz <- sapply(df_kawa_filtered$Gorycz, convert_to_5_scale)
df_kawa_filtered$Kwasowość <- sapply(df_kawa_filtered$Kwasowość, convert_to_5_scale)
df_kawa_filtered$Słodycz <- sapply(df_kawa_filtered$Słodycz, convert_to_5_scale)
df_kawa_filtered$Intensywność_smaku <- sapply(df_kawa_filtered$Intensywność_smaku, convert_to_5_scale)

# Usuwamy atrybuty "names" za pomocą unname()
df_kawa_filtered$Gorycz <- unname(df_kawa_filtered$Gorycz)
df_kawa_filtered$Kwasowość <- unname(df_kawa_filtered$Kwasowość)
df_kawa_filtered$Słodycz <- unname(df_kawa_filtered$Słodycz)
df_kawa_filtered$Intensywność_smaku <- unname(df_kawa_filtered$Intensywność_smaku)

# Dzielimy cenę przez 100 i formatowanie jej z dwoma miejscami po przecinku
df_kawa_filtered$Cena <- as.numeric(df_kawa_filtered$Cena) / 100

# Jeśli chcesz, aby wartości były dokładnie z dwoma miejscami po przecinku, użyj formatowania
df_kawa_filtered$Cena <- format(df_kawa_filtered$Cena, nsmall = 2)


# Konwersja kolumny Ocena z typu chr na num (float)
df_kawa_filtered$Ocena <- as.numeric(df_kawa_filtered$Ocena)

# Sprawdzamy wynik po formatowaniu
# Sprawdzamy wynikowy data frame
str(df_kawa_filtered)


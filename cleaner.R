# Loading libraries
library(tidyverse)
library(stringi)
library(dplyr)

# Reading the file with the appropriate separator
df_kawa <- read_csv2("kawa_with_bom_ok.csv", locale = locale(encoding = "UTF-8"))

# Function that processes specifications for a single coffee
process_specifications <- function(specifications_text) {
  specs <- stri_split_fixed(specifications_text, " | ", simplify = TRUE)
  spec_names <- stri_extract(specs, regex = "^[^:]+(?=:)")
  spec_values <- stri_extract(specs, regex = "(?<=:).+$")
  
  spec_list <- setNames(spec_values, spec_names)
  return(spec_list)
}

# Processing specifications and creating additional columns
specifications_processed <- lapply(df_kawa$Specifications, process_specifications)

# Converting the results to a data frame
specifications_df <- bind_rows(specifications_processed)

# Adding these columns to the main data frame
df_kawa <- bind_cols(df_kawa, specifications_df)

# Removing the original 'Specifications' column
df_kawa <- df_kawa %>% select(-Specifications)

# Identifying the number of non-NA values in each column
non_na_counts <- colSums(!is.na(df_kawa))

# Filtering columns where the number of non-NA values is greater than or equal to 1000
df_kawa_filtered <- df_kawa[, non_na_counts >= 1000]

# Renaming columns to Polish, using underscores instead of spaces and dashes
colnames(df_kawa_filtered) <- c(
  "Tytuł", "Opis", "Cena", "Producent", "Ocena", "Liczba_opinii",
  "Skład", "Stopień_palenia", "Zawartość_kofeiny", "Rodzaj", "Palarnia", 
  "Przeznaczenie", "Opakowanie", "Sposób_przygotowania", "Crema", "Wyczuwalne_smaki", 
  "Intensywność_smaku", "Gorycz", "Kwasowość", "Słodycz", 
  "Pochodzenie_ziaren", "Blend_czy_Single", "Polecana_do", "Kawa_Specialty"
)

# Replacing spaces and dashes with underscores in column names
colnames(df_kawa_filtered) <- gsub(" |-", "_", colnames(df_kawa_filtered))

# Function to convert values to a scale from 1/5 to 5/5
convert_to_5_scale <- function(value) {
  value <- gsub(" ", "", value)  # Remove white spaces
  if (grepl("Delikatna", value)) {
    return(0.2)
  } else if (grepl("Średnia", value)) {
    return(0.6)
  } else if (grepl("Mocna", value)) {
    return(1)
  }
  
  # For numerical values like "2/5", "3/5"
  if (grepl("^\\d+/\\d+$", value)) {
    return(as.numeric(stri_extract(value, regex = "^\\d+")) / as.numeric(stri_extract(value, regex = "(?<=/).*")))
  }
  
  # For numerical values like "1", "2", "3", "4", "5"
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

# Applying the function to convert to a scale from 1/5 to 5/5 in the appropriate columns
df_kawa_filtered$Gorycz <- sapply(df_kawa_filtered$Gorycz, convert_to_5_scale)
df_kawa_filtered$Kwasowość <- sapply(df_kawa_filtered$Kwasowość, convert_to_5_scale)
df_kawa_filtered$Słodycz <- sapply(df_kawa_filtered$Słodycz, convert_to_5_scale)
df_kawa_filtered$Intensywność_smaku <- sapply(df_kawa_filtered$Intensywność_smaku, convert_to_5_scale)

# Removing the "names" attribute using unname()
df_kawa_filtered$Gorycz <- unname(df_kawa_filtered$Gorycz)
df_kawa_filtered$Kwasowość <- unname(df_kawa_filtered$Kwasowość)
df_kawa_filtered$Słodycz <- unname(df_kawa_filtered$Słodycz)
df_kawa_filtered$Intensywność_smaku <- unname(df_kawa_filtered$Intensywność_smaku)

# Dividing the price by 100 and formatting it to two decimal places
df_kawa_filtered$Cena <- as.numeric(df_kawa_filtered$Cena) / 100
df_kawa_filtered$Cena <- format(df_kawa_filtered$Cena, nsmall = 2)

# Converting the Rating column from character to numeric (float)
df_kawa_filtered$Ocena <- as.numeric(df_kawa_filtered$Ocena)

# Changing the packaging format
df_kawa_filtered$Opakowanie <- ifelse(df_kawa_filtered$Opakowanie == " 1000", 
                                      " 1000g", 
                                      df_kawa_filtered$Opakowanie)

# Processing weight and price, and calculating price ranges
df_kawa_filtered$Waga_g <- as.numeric(gsub("[^0-9]", "", df_kawa_filtered$Opakowanie))
df_kawa_filtered$Waga_g[grepl("Szklana butelka", df_kawa_filtered$Opakowanie)] <- NA
df_kawa_filtered$Cena <- as.numeric(gsub(",", ".", gsub("\\s", "", df_kawa_filtered$Cena)))
df_kawa_filtered <- df_kawa_filtered[!is.na(df_kawa_filtered$Waga_g) & !is.na(df_kawa_filtered$Cena), ]
df_kawa_filtered$Cena_za_gram <- df_kawa_filtered$Cena / df_kawa_filtered$Waga_g

df_kawa_filtered$Przedział_cenowy <- cut(
  df_kawa_filtered$Cena_za_gram,
  breaks = c(0, 0.05, 0.1, 0.2, Inf),
  labels = c("Budget (< 0.05)", "Mid-range (0.05 - 0.1)", "Premium (0.1 - 0.2)", "Luxury (> 0.2)"),
  right = FALSE
)

# Calculating the "Adjusted Rating"
df_kawa_filtered$Ocena[is.na(df_kawa_filtered$Ocena)] <- 0
df_kawa_filtered$Liczba_opinii[is.na(df_kawa_filtered$Liczba_opinii)] <- 0
df_kawa_filtered$Skorygowana_ocena <- df_kawa_filtered$Ocena * sqrt(df_kawa_filtered$Liczba_opinii)

# Saving the cleaned file to CSV
write.csv(df_kawa_filtered, "kawa_filtered.csv", row.names = FALSE)

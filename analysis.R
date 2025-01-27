# Loading libraries
library(tidyverse)
library(ggplot2)

# Reading data from "kawa_filtered.csv"
df_kawa_filtered <- read.csv("kawa_filtered.csv")

# General statistics
summary_stats <- df_kawa_filtered %>%
  select(Cena, Ocena, Liczba_opinii, Gorycz, Kwasowość, Słodycz, Intensywność_smaku, Cena_za_gram, Skorygowana_ocena) %>%
  summary()

# Additional statistics: mean, standard deviation, min, max
detailed_stats <- df_kawa_filtered %>%
  select(Cena, Ocena, Liczba_opinii, Gorycz, Kwasowość, Słodycz, Intensywność_smaku, Cena_za_gram, Skorygowana_ocena) %>%
  summarise(
    Mean_Cena = mean(Cena, na.rm = TRUE),
    SD_Cena = sd(Cena, na.rm = TRUE),
    Min_Cena = min(Cena, na.rm = TRUE),
    Max_Cena = max(Cena, na.rm = TRUE),
    
    Mean_Ocena = mean(Ocena, na.rm = TRUE),
    SD_Ocena = sd(Ocena, na.rm = TRUE),
    Min_Ocena = min(Ocena, na.rm = TRUE),
    Max_Ocena = max(Ocena, na.rm = TRUE),
    
    Mean_Liczba_opinii = mean(Liczba_opinii, na.rm = TRUE),
    SD_Liczba_opinii = sd(Liczba_opinii, na.rm = TRUE),
    Min_Liczba_opinii = min(Liczba_opinii, na.rm = TRUE),
    Max_Liczba_opinii = max(Liczba_opinii, na.rm = TRUE),
    
    Mean_Gorycz = mean(Gorycz, na.rm = TRUE),
    SD_Gorycz = sd(Gorycz, na.rm = TRUE),
    Min_Gorycz = min(Gorycz, na.rm = TRUE),
    Max_Gorycz = max(Gorycz, na.rm = TRUE),
    
    Mean_Kwasowość = mean(Kwasowość, na.rm = TRUE),
    SD_Kwasowość = sd(Kwasowość, na.rm = TRUE),
    Min_Kwasowość = min(Kwasowość, na.rm = TRUE),
    Max_Kwasowość = max(Kwasowość, na.rm = TRUE),
    
    Mean_Słodycz = mean(Słodycz, na.rm = TRUE),
    SD_Słodycz = sd(Słodycz, na.rm = TRUE),
    Min_Słodycz = min(Słodycz, na.rm = TRUE),
    Max_Słodycz = max(Słodycz, na.rm = TRUE),
    
    Mean_Intensywność_smaku = mean(Intensywność_smaku, na.rm = TRUE),
    SD_Intensywność_smaku = sd(Intensywność_smaku, na.rm = TRUE),
    Min_Intensywność_smaku = min(Intensywność_smaku, na.rm = TRUE),
    Max_Intensywność_smaku = max(Intensywność_smaku, na.rm = TRUE),
    
    Mean_Cena_za_gram = mean(Cena_za_gram, na.rm = TRUE),
    SD_Cena_za_gram = sd(Cena_za_gram, na.rm = TRUE),
    Min_Cena_za_gram = min(Cena_za_gram, na.rm = TRUE),
    Max_Cena_za_gram = max(Cena_za_gram, na.rm = TRUE)
  )

# Plotting distribution of price per gram
ggplot(df_kawa_filtered, aes(x = Cena_za_gram)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Price per Gram", x = "Price per Gram", y = "Number of Coffees")

# Scatter plot of price vs rating
ggplot(df_kawa_filtered, aes(x = Cena, y = Ocena)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Price vs Rating", x = "Price", y = "Rating")

# Boxplot by price range
ggplot(df_kawa_filtered, aes(x = Przedział_cenowy, y = Ocena)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Rating by Price Range", x = "Price Range", y = "Rating")

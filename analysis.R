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

# Identify Most Popular Producers
# Count reviews per producer and select top 5
popular_producers <- df_kawa_filtered %>%
  group_by(Producent) %>%
  summarise(Total_Reviews = sum(Liczba_opinii, na.rm = TRUE)) %>%
  arrange(desc(Total_Reviews)) %>%
  head(5)

# Bar plot of most popular producers
ggplot(popular_producers, aes(x = reorder(Producent, -Total_Reviews), y = Total_Reviews)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  coord_flip() +
  labs(title = "Top 5 Most Popular Producers by Total Reviews", x = "Producer", y = "Total Reviews")

#Identify Producers with Cheapest Products
# Average price per producer
cheapest_producers <- df_kawa_filtered %>%
  group_by(Producent) %>%
  summarise(Average_Price = mean(Cena_za_gram, na.rm = TRUE)) %>%
  arrange(Average_Price) %>%
  head(5)

# Bar plot for producers with cheapest products
ggplot(cheapest_producers, aes(x = reorder(Producent, Average_Price), y = Average_Price)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  coord_flip() +
  labs(title = "Top 5 Producers with Cheapest Products", x = "Producer", y = "Average Price per Gram")

#Producers with Best Reviews and most reviews
# Calculate average rating and total reviews per producer
producers_stats <- df_kawa_filtered %>%
  group_by(Producent) %>%
  summarise(
    Average_Rating = mean(Ocena, na.rm = TRUE),
    Total_Reviews = sum(Liczba_opinii, na.rm = TRUE)
  ) %>%
  arrange(desc(Average_Rating))

# Filter for the top producers (e.g., top 5 by average rating)
top_producers_stats <- producers_stats %>%
  head(10)

# Bar plot showing both average rating and total reviews with labels
ggplot(top_producers_stats, aes(x = reorder(Producent, -Average_Rating))) +
  geom_bar(aes(y = Average_Rating), stat = "identity", fill = "gold", alpha = 0.7) +
  geom_point(aes(y = Total_Reviews / 10), color = "blue", size = 3) + # Adjust scaling for clarity
  geom_line(aes(y = Total_Reviews / 10, group = 1), color = "blue", linetype = "dashed") +
  geom_text(aes(y = Total_Reviews / 10, label = Average_Rating), vjust = -0.5, color = "blue", size = 4) + # Add labels for reviews
  labs(
    title = "Top Producers: Rating and Number of Reviews",
    x = "Producer",
    y = "Average Rating (Bar) / Total Reviews (Scaled by 1/10)",
  ) +
  theme_minimal() +
  coord_flip()

#Analyze Review Counts and Ratings by Flavor Characteristics
# Correlation of characteristics with reviews and ratings
characteristics_correlation <- df_kawa_filtered %>%
  select(Gorycz, Kwasowość, Słodycz, Intensywność_smaku, Ocena, Liczba_opinii) %>%
  cor(use = "complete.obs")

# Heatmap of correlation
library(reshape2)
melted_correlation <- melt(characteristics_correlation)
ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "Variable", y = "Variable")





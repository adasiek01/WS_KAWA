library(tidyverse)
library(stringi)
library(dplyr)
library(ggplot2)


df_kawa_filtered <- read.csv("kawa_filtered.csv")
str(df_kawa_filtered)

#PRICE RANGE ANALYSIS
# Step 1: Standardize and clean the 'Opakowanie' column
df_kawa_filtered$Weight_g <- as.numeric(gsub("[^0-9]", "", df_kawa_filtered$Opakowanie))

# Handle specific cases
df_kawa_filtered$Weight_g[grepl("Szklana butelka", df_kawa_filtered$Opakowanie)] <- NA  # Non-numeric, set to NA

# Step 2: Clean 'Cena' and convert to numeric
df_kawa_filtered$Cena <- as.numeric(gsub(",", ".", gsub("\\s", "", df_kawa_filtered$Cena)))

# Step 3: Calculate "Price per Gram", excluding rows with NA in Weight_g or Cena
df_kawa_filtered <- df_kawa_filtered[!is.na(df_kawa_filtered$Weight_g) & !is.na(df_kawa_filtered$Cena), ]
df_kawa_filtered$Price_per_Gram <- df_kawa_filtered$Cena / df_kawa_filtered$Weight_g

# Step 4: Group coffees into price ranges
df_kawa_filtered$Price_Range <- cut(
  df_kawa_filtered$Price_per_Gram,
  breaks = c(0, 0.05, 0.1, 0.2, Inf),
  labels = c("Budget (< 0.05)", "Mid-range (0.05 - 0.1)", "Premium (0.1 - 0.2)", "Luxury (> 0.2)"),
  right = FALSE
)

# Step 5: Visualize the distribution of products in each price range
library(ggplot2)

ggplot(df_kawa_filtered, aes(x = Price_Range)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Products by Price Range",
    x = "Price Range (PLN per Gram)",
    y = "Number of Products"
  ) +
  theme_minimal()

#ADDING ADJUSTED RATINGS COLUMN
# Step 1: Handle NA values in 'Ocena' and 'Liczba_opinii'
df_kawa_filtered$Ocena[is.na(df_kawa_filtered$Ocena)] <- 0
df_kawa_filtered$Liczba_opinii[is.na(df_kawa_filtered$Liczba_opinii)] <- 0

# Step 2: Calculate the "Adjusted Rating" 
#- Multiplying the Ocena by the square root of the Liczba_opinii to give more weight to well-reviewed products.
df_kawa_filtered$Adjusted_Rating <- df_kawa_filtered$Ocena * sqrt(df_kawa_filtered$Liczba_opinii)

# Step 3: Preview the result
head(df_kawa_filtered[, c("Ocena", "Liczba_opinii", "Adjusted_Rating")])


#ANALYSING PRODUCERS, NUM OF OPINIONS AND RATINGS
# Step 1: Calculate average number of ratings and adjusted ratings for each producer
producer_analysis <- df_kawa_filtered %>%
  group_by(Producent) %>%
  summarise(
    Avg_Num_Ratings = mean(Liczba_opinii, na.rm = TRUE),
    Avg_Adjusted_Rating = mean(Adjusted_Rating, na.rm = TRUE),
    Count_Products = n()
  ) %>%
  arrange(desc(Avg_Num_Ratings))  # Sort by popularity

# Step 2: Top producers by average number of ratings
top_popular_producers <- producer_analysis %>%
  top_n(10, Avg_Num_Ratings)

# Step 3: Top producers by adjusted rating
top_adjusted_rating_producers <- producer_analysis %>%
  top_n(10, Avg_Adjusted_Rating)

# Step 4: Visualize popularity
library(ggplot2)

# Bar chart for average number of ratings
ggplot(top_popular_producers, aes(x = reorder(Producent, Avg_Num_Ratings), y = Avg_Num_Ratings)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top Producers by Average Number of Ratings",
    x = "Producer",
    y = "Average Number of Ratings"
  ) +
  theme_minimal()

# Bar chart for average adjusted rating
ggplot(top_adjusted_rating_producers, aes(x = reorder(Producent, Avg_Adjusted_Rating), y = Avg_Adjusted_Rating)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top Producers by Average Adjusted Rating",
    x = "Producer",
    y = "Average Adjusted Rating"
  ) +
  theme_minimal()

#CORELATION ANALYSIS OF COFFEE SPECIFICATION AND ADJUSTED RATING
# Step 1: Prepare data for correlation analysis
correlation_data <- df_kawa_filtered %>%
  select(Kwasowość, Gorycz, Słodycz, Intensywność_smaku, Adjusted_Rating) %>%
  na.omit()  # Remove rows with NA values

# Step 2: Compute the correlation matrix
correlation_matrix <- cor(correlation_data)

# Step 3: Visualize the correlation matrix
library(ggplot2)
library(reshape2)

# Convert matrix to long format for ggplot
correlation_long <- melt(correlation_matrix)

# Plot heatmap
ggplot(correlation_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10)
  ) +
  labs(
    title = "Correlation Heatmap",
    x = "Specifications",
    y = "Specifications"
  )

###




# Packages
library(tidyverse)
library(skimr)
library(dplyr)
library(pracma)
library(readr)

# Importing source files
df_nin <- read_csv("source/nintendo_discount_catalogue_with_info_02-06-2024.csv")
df_steam <- read.csv("source/steam_discount_catalogue_with_info_02-06-2024.csv")

# Uncleaned data
head(df_nin)
glimpse(df_nin)

head(df_steam)
glimpse(df_steam)

# Cleaning data

## Filling empty cells and "tbd values "with "N/A"
df_nin[df_nin == ""] <- "N/A"
df_steam[df_steam == ""] <- "N/A"
df_nin[df_nin == "tbd"] <- "N/A"
df_steam[df_steam == "tbd"] <- "N/A"

## Changing all "N/A" values to "NA" so it can be recognised by R.
df_nin[df_nin == "N/A"] <- "NA"
df_steam[df_steam == "N/A"] <- "NA"

## Column Data Type Standardisation
rating_columns <- c("meta_score", 
                    "meta_reviews", 
                    "user_score", 
                    "user_reviews")

for (rating_column in rating_columns) {
  df_nin[[rating_column]] <- as.double(df_nin[[rating_column]])
  df_steam[[rating_column]] <- as.double(df_steam[[rating_column]])
}

df_nin$release_date <- as.Date(df_nin$release_date, format = "%d-%m-%Y")
df_steam$release_date <- as.Date(df_steam$release_date, format = "%d-%m-%Y")

## Remove records with absolutely no ratings
df_nin_removed_na <- df_nin[!apply(df_nin[rating_columns], 1, function(x) all(is.na(x))), ]
df_steam_removed_na <- df_steam[!apply(df_steam[rating_columns], 1, function(x) all(is.na(x))), ]

## Standardising Currency to $AUD
df_nin_removed_na$discount_per <- round(
  ((df_nin_removed_na$original_price - df_nin_removed_na$special_price) / df_nin_removed_na$original_price) * 100, 
  2
)

df_nin_removed_na <- df_nin_removed_na[, c(
  names(df_nin_removed_na)[1:which(names(df_nin_removed_na) == "special_price")], 
  "discount_per", 
  names(df_nin_removed_na)[(which(names(df_nin_removed_na) == "special_price")+1):(ncol(df_nin_removed_na)-1)]
)]

df_steam_removed_na$discount_per <- round(
  ((df_steam_removed_na$original_price - df_steam_removed_na$special_price) / df_steam_removed_na$original_price) * 100, 
  2
)

df_steam_removed_na <- df_steam_removed_na[, c(
  names(df_steam_removed_na)[1:which(names(df_steam_removed_na) == "special_price")], 
  "discount_per", 
  names(df_steam_removed_na)[(which(names(df_steam_removed_na) == "special_price")+1):(ncol(df_steam_removed_na)-1)]
)]

df_nin_removed_na_aud <- df_nin_removed_na

exchange_rate_usd_to_aud <- 1.5046 # Set USD/AUD Closing Price here
df_steam_removed_na_aud <- df_steam_removed_na
df_steam_removed_na_aud$original_price <- df_steam_removed_na_aud$original_price * exchange_rate_usd_to_aud
df_steam_removed_na_aud$special_price <- df_steam_removed_na_aud$special_price * exchange_rate_usd_to_aud

## Removing Critics Columns
df_nin_meta_reviews_count <- sum(df_nin_removed_na_aud$meta_reviews, na.rm = TRUE)
df_steam_meta_reviews_count <- sum(df_steam_removed_na_aud$meta_reviews, na.rm = TRUE)

df_nin_user_reviews_count <- sum(df_nin_removed_na_aud$user_reviews, na.rm = TRUE)
df_steam_user_reviews_count <- sum(df_steam_removed_na_aud$user_reviews, na.rm = TRUE)

nin_reviews_counts <- c(df_nin_user_reviews_count, df_nin_meta_reviews_count)
steam_reviews_counts <- c(df_steam_user_reviews_count, df_steam_meta_reviews_count)

barplot(rbind(nin_reviews_counts, steam_reviews_counts), beside=TRUE,
        names.arg=c("Total critic reviews", "Total user reviews"),
        col=c("red", "blue"),
        legend.text=c("Nintendo", "Steam"),
        main="Comparison between Total Critic and User Reviews"
)

df_nin_removed_na_aud <- df_nin_removed_na_aud %>%
  select(-c(meta_score, meta_reviews))

df_steam_removed_na_aud <- df_steam_removed_na_aud %>%
  select(-c(meta_score, meta_reviews))

## Column Renaming, set unidentified publishers to "Independent Developers", and
## remove any leftover records with NA values
df_nin_cleaned <- df_nin_removed_na_aud %>%
  rename(rating = user_score, popularity = user_reviews) %>%
  mutate(popularity = ifelse(is.na(popularity), 0, popularity),
         publisher = ifelse(publisher == "NA", "Independent Developers", publisher)) %>%
  na.omit()

df_steam_cleaned <- df_steam_removed_na_aud %>%
  rename(rating = user_score, popularity = user_reviews) %>%
  mutate(popularity = ifelse(is.na(popularity), 0, popularity),
         publisher = ifelse(publisher == "NA", "Independent Developers", publisher)) %>%
  na.omit()

## Cleaned data overview
head(df_nin_cleaned)
head(df_steam_cleaned)

# Analysis
summary(df_nin_cleaned)
summary(df_steam_cleaned)
sapply(df_nin_cleaned[, c("original_price", "special_price", "discount_per", "popularity")], sd)
sapply(df_steam_cleaned[, c("original_price", "special_price", "discount_per", "popularity")], sd)

transparency = 0.7
nin_color = "red"
steam_color = "blue"

df_nin_cleaned$discount_abs <- df_nin_cleaned$original_price - df_nin_cleaned$special_price
df_steam_cleaned$discount_abs <- df_steam_cleaned$original_price - df_steam_cleaned$special_price

df_combined <- rbind(
  transform(df_nin_cleaned, source = "Nintendo"),
  transform(df_steam_cleaned, source = "Steam")
)

# Original and Special price plots
og_price_plot <- ggplot(df_combined, aes(x = source, y = original_price, fill = source, alpha = transparency)) +
  geom_boxplot() +
  ggtitle("Comparison between Original Prices") +
  ylab("Original Price") +
  scale_fill_manual(values = c("Nintendo" = nin_color, "Steam" = steam_color)) +
  theme(legend.position = "none")

sp_price_plot <- ggplot(df_combined, aes(x = source, y = special_price, fill = source, alpha = transparency)) +
  geom_boxplot() +
  ggtitle("Comparison between Special Prices") +
  ylab("Special Price") +
  scale_fill_manual(values = c("Nintendo" = nin_color, "Steam" = steam_color)) +
  theme(legend.position = "none")

discount_abs_plot <- ggplot(df_combined, aes(x = source, y = discount_abs, fill = source, alpha = transparency)) +
  geom_boxplot() +
  ggtitle("Comparison between Absolute Dollar Discount") +
  ylab("Absolute Dollar Discount") +
  scale_fill_manual(values = c("Nintendo" = nin_color, "Steam" = steam_color)) +
  theme(legend.position = "none")

og_sp_price_plot <- ggplot(df_combined, aes(x = original_price, y = special_price)) +
  geom_rect(aes(xmin = 50, xmax = 100, ymin = -Inf, ymax = Inf), 
            fill = "yellow", alpha = transparency) + 
  geom_point(aes(color = source), alpha = transparency) +
  geom_abline(intercept = 0, slope = 0.5, linetype = "dashed", color = "black") + 
  annotate("text", x = 100, y = 50, label = "50% discount line", color = "darkgreen") +
  ggtitle("Original Price vs. Special Price") +
  xlab("Original Price") +
  ylab("Special Price") +
  scale_color_manual(values = c("Nintendo" = nin_color, "Steam" = steam_color))

mean_discount_abs_nin <- mean(df_nin_cleaned$discount_abs, na.rm = TRUE)
mean_discount_abs_steam <- mean(df_steam_cleaned$discount_abs, na.rm = TRUE)

cat("Mean absolute dollar discount for Nintendo: ", mean_discount_abs_nin, "\n")
cat("Mean absolute dollar discount for Steam: ", mean_discount_abs_steam, "\n")

t_test_discount_abs <- t.test(df_nin_cleaned$discount_abs, df_steam_cleaned$discount_abs)
p_value_discount_abs <- t_test_discount_abs$p.value
cat("The p-value for the hypothesis test comparing discount absolute difference significane between Nintendo and Steam is:", p_value_discount_abs, "\n")
if (p_value_discount_abs < 0.05) {
  cat("The difference in absolute dollar discount between Nintendo and Steam is statistically significant.\n")
} else {
  cat("The difference in absolute dollar discount between between Nintendo and Steam is not statistically significant.\n")
}

og_price_plot
sp_price_plot
discount_abs_plot
og_sp_price_plot

## Percentage Discount
nin_discount_per_plot <- ggplot(df_nin_cleaned, aes(x=discount_per)) +
  geom_histogram(aes(y=..count../length(df_nin_cleaned$discount_per)), binwidth=10, fill=nin_color, alpha=transparency, boundary=0) +
  ggtitle("Nintendo Discount Percentage Distribution") +
  xlab("Discount Percentage") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))

steam_discount_per_plot <- ggplot(df_steam_cleaned, aes(x=discount_per)) +
  geom_histogram(aes(y=..count../length(df_steam_cleaned$discount_per)), binwidth=10, fill=steam_color, alpha=transparency, boundary=0) +
  ggtitle("Steam Discount Percentage Distribution") +
  xlab("Discount Percentage") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))

mean_discount_per_nin <- mean(df_nin_cleaned$discount_per, na.rm = TRUE)
mean_discount_per_steam <- mean(df_steam_cleaned$discount_per, na.rm = TRUE)

cat("Mean percentage discount for Nintendo: ", mean_discount_per_nin, "%\n")
cat("Mean percentage discount for Steam: ", mean_discount_per_steam, "%\n")

t_test_discount_per <- t.test(df_nin_cleaned$discount_per, df_steam_cleaned$discount_per)
p_value_discount_per <- t_test_discount_per$p.value
cat("The p-value for the hypothesis test comparing discount percentages significane between Nintendo and Steam is:", p_value_discount_per, "\n")
if (p_value_discount_per < 0.05) {
  cat("The difference in discount percentage between Nintendo and Steam is statistically significant.\n")
} else {
  cat("The difference in discount absolute between Nintendo and Steam is not statistically significant.\n")
}

nin_discount_per_plot
steam_discount_per_plot

# Rating Analysis
rating_plot <- ggplot(df_combined, aes(x = source, y = rating, fill = source, alpha = transparency)) +
  geom_boxplot() +
  ggtitle("Comparison between Game Ratings on Sale") +
  ylab("Rating") +
  scale_fill_manual(values = c("Nintendo" = nin_color, "Steam" = steam_color)) +
  theme(legend.position = "none")

rating_plot

# Popularity Analysis
popularity_plot <- ggplot(df_combined, aes(x = source, y = popularity, fill = source)) +
  geom_jitter(width = 0.1, height = 0, shape = 21, size = 3, alpha = transparency) +
  ggtitle("Comparison between Game Popularity on Sale") +
  ylab("Popularity") +
  scale_fill_manual(values = c("Nintendo" = nin_color, "Steam" = steam_color)) +
  theme(legend.position = "none")

popularity_plot

# Genre Analysis
nin_genre_top_10 <- df_nin_cleaned %>% 
  group_by(genre) %>% 
  summarize(count = n()) %>%
  top_n(10, wt=count)

steam_genre_top_10 <- df_steam_cleaned %>% 
  group_by(genre) %>% 
  summarize(count = n()) %>%
  top_n(10, wt=count)

nin_top_genre <- ggplot(nin_genre_top_10, aes(x = reorder(genre, -count), y = count)) +
  geom_bar(stat = "identity", fill = nin_color, alpha = transparency) +
  ggtitle("Genre Distribution of the Top 10 Nintendo Games on Sale") +
  xlab("Genre") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

steam_top_genre <- ggplot(steam_genre_top_10, aes(x = reorder(genre, -count), y = count)) +
  geom_bar(stat = "identity", fill = steam_color, alpha = transparency) +
  ggtitle("Genre Distribution of the Top 10 Steam Games on Sale") +
  xlab("Genre") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

nin_top_genre
steam_top_genre

# Publisher Analysis
nin_publisher_top_10 <- df_nin_cleaned %>%
  group_by(publisher) %>%
  summarise(count=n()) %>%
  top_n(10, wt=count)

steam_publisher_top_10 <- df_steam_cleaned %>%
  group_by(publisher) %>%
  summarise(count=n()) %>%
  top_n(10, wt=count)

nin_top_pub <- ggplot(nin_publisher_top_10, aes(x=reorder(publisher, -count), y=count)) +
  geom_bar(stat="identity", fill=nin_color, alpha=transparency) +
  ggtitle("Top Nintendo Publishers with Discounts") +
  xlab("Publisher") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

steam_top_pub <- ggplot(steam_publisher_top_10, aes(x=reorder(publisher, -count), y=count)) +
  geom_bar(stat="identity", fill=steam_color, alpha=transparency) +
  ggtitle("Top Steam Publishers with Discounts") +
  xlab("Publisher") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

nin_top_pub
steam_top_pub

# Regression Analysis
df_nin_cleaned$release_date_numeric <- as.numeric(df_nin_cleaned$release_date - as.Date("1970-01-01"))
df_steam_cleaned$release_date_numeric <- as.numeric(df_steam_cleaned$release_date - as.Date("1970-01-01"))

nin_model <- lm(discount_per ~ rating + popularity + release_date_numeric, df_nin_cleaned)
steam_model <- lm(discount_per ~ rating + popularity + release_date_numeric, df_steam_cleaned)

cat("Number of records in Nintendo model:", nrow(df_nin_cleaned), "\n")
summary(nin_model)

cat("Number of records in Steam model:", nrow(df_steam_cleaned), "\n")
summary(steam_model)

if (!dir.exists("output")) {
  dir.create("output", recursive = TRUE)
}

write.csv(df_nin_cleaned, file = "output/nintendo_cleaned.csv", row.names = FALSE)
write.csv(df_steam_cleaned, file = "output/steam_cleaned.csv", row.names = FALSE)
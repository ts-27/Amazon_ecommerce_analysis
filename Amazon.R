# Install data.table package
install.packages("data.table")

# Load it
library(data.table)

# Read your file

setwd("C:\\Users\\sthak\\Downloads")
df <- fread("amz_in_total_products_data_processed.csv", encoding = "UTF-8")

head(df)
str(df)
summary(df)


# View top prices
head(df[order(-price), ], 10)

# Or just check the highest price product
df[which.max(price), ]

sum(is.na(df))
sum(duplicated(df))



# Load required libraries
install.packages("dplyr")
library(dplyr)
library(psych)  # for describe()

# View first few rows
head(df)

# Summary statistics for entire dataset
summary(df)

# Summary for specific numeric columns (example: Price, Rating, Reviews)
summary(df$price)
summary(df$stars)
summary(df$reviews)

# Psych package gives more details (mean, sd, median, skewness, kurtosis)
describe(df)

unique(df$categoryName)


# Use psych's describe() for a detailed descriptive stats of the summary



library(psych)

# This assumes your dataset is called df and your category column is named Category
describeBy(df$price, group = df$categoryName)
describeBy(df$reviews, group = df$categoryName)

table(df$categoryName)

# Price stats by category
price_stats <- describeBy(df$price, group = df$Category, mat = TRUE) %>%
  select(group1, n, mean, sd, min, max, median) %>%
  rename(
    categoryName = group1,
    price_n = n,
    price_mean = mean,
    price_sd = sd,
    price_min = min,
    price_max = max,
    price_median = median
  )


library(dplyr)

category_summary <- df %>%
  group_by(categoryName) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    price_min = min(price, na.rm = TRUE),
    price_max = max(price, na.rm = TRUE),
    price_range = max(price, na.rm = TRUE) - min(price, na.rm = TRUE),
    avg_rating = mean(stars, na.rm = TRUE),
    total_reviews = sum(reviews, na.rm = TRUE),
    avg_reviews = mean(reviews, na.rm = TRUE),
    price_to_rating = avg_price / avg_rating,
    num_products = n(),
    reviews_per_product = total_reviews / num_products
  ) %>%
  arrange(desc(num_products))

head(category_summary)





library(dplyr)

# Group and summarise metrics
category_summary <- df %>%
  group_by(categoryName) %>%
  summarise(
    num_products = n_distinct(asin),
    avg_price = mean(price, na.rm = TRUE),
    avg_rating = mean(stars, na.rm = TRUE),
    avg_engagement = mean(reviews, na.rm = TRUE)
  )

# 1. Arrange by number of products
arrange_by_products <- category_summary %>%
  arrange(desc(num_products))

# 2. Arrange by average price
arrange_by_price <- category_summary %>%
  arrange(desc(avg_price))

# 3. Arrange by average engagement
arrange_by_engagement <- category_summary %>%
  arrange(desc(avg_engagement))

# 4. Arrange by average rating
arrange_by_rating <- category_summary %>%
  arrange(desc(avg_rating))

# Optional: View all side by side
list(
  By_Products = arrange_by_products,
  By_Price = arrange_by_price,
  By_Engagement = arrange_by_engagement,
  By_Rating = arrange_by_rating
)



##for tableau

##category segmentation
library(dplyr)

df %>%
  group_by(categoryName) %>%
  summarise(
    product_count = n(),
    avg_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    avg_rating = mean(stars, na.rm = TRUE),
    avg_reviews = mean(reviews, na.rm = TRUE),
    total_reviews = sum(reviews, na.rm = TRUE)
  ) %>%
  arrange(desc(product_count))   # Change to avg_price, avg_rating, total_reviews based on focus



##top and bottom categories
library(dplyr)

category_ratings <- df %>%
  group_by(categoryName) %>%
  summarise(
    avg_rating = mean(stars, na.rm = TRUE),
    num_products = n()
  ) %>%
  arrange(desc(avg_rating))

# Top 10 categories by average rating
top_10_categories <- head(category_ratings, 10)
print(top_10_categories)

# Bottom 10 categories by average rating
bottom_10_categories <- tail(category_ratings, 10)
print(bottom_10_categories)


##price bands
library(dplyr)

df <- df %>%
  mutate(price_band = cut(
    price,
    breaks = c(-Inf, 100, 250, 500, 750, 1000, 5000, 10000, 50000, 100000, Inf),
    labels = c(
      "<100",
      "100-250",
      "250-500",
      "500-750",
      "750-1000",
      "1K-5K",
      "5K-10K",
      "10K-50K",
      "50K-1Lakh",
      ">1 Lakh"
    ),
    right = FALSE
  ))

table(df$price_band)  # Check distribution

price_band_summary <- df %>%
  group_by(price_band) %>%
  summarise(
    num_products = n(),
    avg_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    avg_rating = mean(stars, na.rm = TRUE),
    avg_reviews = mean(reviews, na.rm = TRUE)
  )

print(price_band_summary)
#=>ratings and reviews don’t vary much across price bands, it means price alone might not be driving customer sentiment or engagement in this dataset.


#category + priceband
library(dplyr)

# Assuming df already has price_band column from previous step

category_price_summary <- df %>%
  group_by(categoryName, price_band) %>%
  summarise(
    num_products = n(),
    avg_price = mean(price, na.rm = TRUE),
    avg_rating = mean(stars, na.rm = TRUE),
    avg_reviews = mean(reviews, na.rm = TRUE)
  ) %>%
  arrange(categoryName, price_band)

print(category_price_summary)


##price correlation with ratings and reviews to check for customer engagement
# Basic correlation between price and rating
cor_price_rating <- cor(df$price, df$stars, use = "complete.obs")

# Correlation between price and reviews count
cor_price_reviews <- cor(df$price, df$reviews, use = "complete.obs")

cat("Correlation between Price and Rating:", cor_price_rating, "\n")
cat("Correlation between Price and Reviews:", cor_price_reviews, "\n")
#correlation values close to zero: Buyers may prioritize other factors (like brand, product type, quality, or features) more than just price.

library(ggplot2)

summary_df <- df %>%
  group_by(categoryName) %>%
  summarise(avg_price = mean(price, na.rm=TRUE),
            avg_rating = mean(stars, na.rm=TRUE))

ggplot(summary_df, aes(x = avg_price, y = avg_rating)) +
  geom_point() +
  labs(title="Avg Price vs Avg Rating by Category")




#Discount analysis
library(dplyr)

df <- df %>%
  mutate(
    discount_pct = ifelse(!is.na(listPrice) & listPrice > 0,
                          (listPrice - price) / listPrice * 100,
                          0)
  )

discount_summary <- df %>%
  summarise(
    avg_discount = mean(discount_pct, na.rm = TRUE),
    avg_rating = mean(stars, na.rm = TRUE),
    avg_reviews = mean(reviews, na.rm = TRUE)
  )

print(discount_summary)


# Group by discount bins for more detail
df <- df %>%
  mutate(discount_band = cut(
    discount_pct,
    breaks = c(-Inf, 0, 10, 25, 50, Inf),
    labels = c("No discount", "0-10%", "10-25%", "25-50%", ">50%")
  ))

discount_band_summary <- df %>%
  group_by(discount_band) %>%
  summarise(
    num_products = n(),
    avg_rating = mean(stars, na.rm = TRUE),
    avg_reviews = mean(reviews, na.rm = TRUE)
  ) %>%
  arrange(desc(num_products))

print(discount_band_summary)


#category wise discount
library(dplyr)

# Assuming discount_pct and discount_band already created as before
discount_category_summary <- df %>%
  group_by(categoryName, discount_band) %>%
  summarise(
    num_products = n(),
    avg_rating = mean(stars, na.rm = TRUE),
    avg_reviews = mean(reviews, na.rm = TRUE),
    avg_price = mean(price, na.rm = TRUE)
  ) %>%
  arrange(categoryName, discount_band)

print(discount_category_summary)



##bestseller vs nonbestseller
bestseller_summary <- df %>%
  group_by(isBestSeller) %>%
  summarise(
    num_products = n(),
    avg_price = mean(price, na.rm = TRUE),
    avg_rating = mean(stars, na.rm = TRUE),
    avg_reviews = mean(reviews, na.rm = TRUE),
    avg_discount = mean(discount_pct, na.rm = TRUE)
  )
print(bestseller_summary)

bestseller_category_summary <- df %>%
  group_by(categoryName, isBestSeller) %>%
  summarise(
    num_products = n(),
    avg_price = mean(price, na.rm = TRUE),
    avg_rating = mean(stars, na.rm = TRUE),
    avg_reviews = mean(reviews, na.rm = TRUE),
    avg_discount = mean(discount_pct, na.rm = TRUE)
  ) %>%
  arrange(categoryName, desc(isBestSeller))

print(bestseller_category_summary)


#boughtinlastmonth

recent_sales_summary <- df %>%
  group_by(boughtInLastMonth > 0) %>%
  summarise(
    num_products = n(),
    avg_price = mean(price, na.rm = TRUE),
    avg_rating = mean(stars, na.rm = TRUE),
    avg_reviews = mean(reviews, na.rm = TRUE),
    avg_discount = mean(discount_pct, na.rm = TRUE)
  ) %>%
  rename(recent_sales = `boughtInLastMonth > 0`)
print(recent_sales_summary)

recent_sales_category_summary <- df %>%
  mutate(recent_purchase = ifelse(boughtInLastMonth > 0, "Yes", "No")) %>%
  group_by(categoryName, recent_purchase) %>%
  summarise(
    num_products = n(),
    avg_price = mean(price, na.rm = TRUE),
    avg_rating = mean(stars, na.rm = TRUE),
    avg_reviews = mean(reviews, na.rm = TRUE),
    avg_discount = mean(discount_pct, na.rm = TRUE)
  ) %>%
  arrange(categoryName, desc(recent_purchase))

print(recent_sales_category_summary)




#Customer Segmentation
library(dplyr)
install.packages("tidyr")
library(tidyr)
library(cluster)
install.packages("factoextra")
library(factoextra)

# Select relevant columns and remove NAs
cluster_data <- df %>%
  select(price, stars, reviews, discount_pct) %>%
  drop_na()

# Scale data (important for clustering)
cluster_data_scaled <- scale(cluster_data)

set.seed(123)

# Elbow method to decide number of clusters
fviz_nbclust(cluster_data_scaled, kmeans, method = "wss") + 
  labs(subtitle = "Elbow method for optimal clusters")

set.seed(123)
k <- 4
kmeans_result <- kmeans(cluster_data_scaled, centers = k, nstart = 25)

# Add cluster labels to original data
df_clustered <- df %>%
  filter(!is.na(price) & !is.na(stars) & !is.na(reviews) & !is.na(discount_pct)) %>%
  mutate(cluster = kmeans_result$cluster)

df_clustered %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    avg_price = mean(price),
    avg_rating = mean(stars),
    avg_reviews = mean(reviews),
    avg_discount = mean(discount_pct)
  )



# exporting the clustered dataframe
write.csv(df_clustered, "amazon_clustered_data.csv", row.names = FALSE)

# Similarly export other summary tables, e.g.
write.csv(discount_category_summary, "discount_category_summary.csv", row.names = FALSE)
write.csv(bestseller_category_summary, "bestseller_category_summary.csv", row.names = FALSE)
write.csv(recent_sales_category_summary, "recent_sales_category_summary.csv", row.names = FALSE)







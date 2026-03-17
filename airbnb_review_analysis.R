###############################################
# 1. Load the dataset
###############################################

# Load the Airbnb reviews CSV.
reviews <- read.csv("reviews.csv", stringsAsFactors = FALSE)

# Inspect dataset shape (rows, columns)
dim(reviews)

# Preview the first few rows to confirm structure
head(reviews)

###############################################
# 2. Count missing values
###############################################

# Count how many NA values appear in each column.
colSums(is.na(reviews))

# Create a clean table for your report
missing_table <- data.frame(
  attribute = names(reviews),
  missing_values = colSums(is.na(reviews))
)

###############################################
# 3. Remove rows with missing comments
###############################################

# reviews with missing text are not useful for NLP.
reviews_clean <- subset(reviews, !is.na(comments))

# Confirm no more missing comments
sum(is.na(reviews_clean$comments))

###############################################
# 4. Add review length and remove short reviews
###############################################

# Compute the length (in characters) of each review.
reviews_clean$review_length <- nchar(reviews_clean$comments)

# See basic summary statistics for review lengths
summary(reviews_clean$review_length)

# Define a minimum allowed length for meaningful content.
threshold <- 20

# Count how many reviews are too short
sum(reviews_clean$review_length < threshold)

# Remove short, non-informative reviews
reviews_filtered <- subset(reviews_clean, review_length >= threshold)

# Check new dataset size
dim(reviews_filtered)

###############################################
# 5. Create summary statistics for your report
###############################################

# Generate descriptive statistics for review lengths
length_stats <- data.frame(
  min = min(reviews_filtered$review_length),
  q1 = quantile(reviews_filtered$review_length, 0.25),
  median = median(reviews_filtered$review_length),
  mean = mean(reviews_filtered$review_length),
  q3 = quantile(reviews_filtered$review_length, 0.75),
  max = max(reviews_filtered$review_length)
)

# Rounded table
round(length_stats, 2)

###############################################
# 6. Visualization:
###############################################

# 1: Histogram using base R to visualize the distribution of review lengths
hist(
  reviews_filtered$review_length,
  breaks = 50,
  main = "Distribution of Airbnb Review Lengths",
  xlab = "Review Length (characters)",
  col = "lightblue"
)

# 2: Boxplot to highlight the median, quartiles, and outliers in review lengths.
boxplot(
  reviews_filtered$review_length,
  main = "Boxplot of Airbnb Review Lengths",
  ylab = "Review Length (characters)",
  col = "lightgreen"
)

###############################################
# End of 1.3 Data Section Code
###############################################


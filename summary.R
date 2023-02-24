spl_data <- read.csv("2013-2023-5-Checkouts-SPL.csv")

library(ggplot2)
library(dplyr)

#my data

# --------------------------------------------------------------

# Values of Interest

# Value 1: Most checked out item for the year 2013 and year 2022.

checkouts_2013 <- spl_data %>%
  filter(CheckoutYear == 2013)

checkouts_2013_by_title <- checkouts_2013 %>%
  group_by(Title) %>%
  summarize(total_checkouts = sum(Checkouts))

most_checked_out_2013 <- checkouts_2013_by_title %>%
  slice_max(total_checkouts) %>%
  select(Title, total_checkouts)

checkouts_2022 <- spl_data %>%
  filter(CheckoutYear == 2022)

checkouts_2022_by_title <- checkouts_2022 %>%
  group_by(Title) %>%
  summarize(total_checkouts = sum(Checkouts))

most_checked_out_2022 <- checkouts_2022_by_title %>%
  slice_max(total_checkouts) %>%
  select(Title, total_checkouts)

# Value 2: Average number of checkouts per year

checkouts_by_year <- spl_data %>%
  group_by(CheckoutYear) %>%
  summarize(total_checkouts = sum(Checkouts))

avg_checkouts_per_year <- mean(checkouts_by_year$total_checkouts)

# Value 3: Which year between 2013-2022 had the most amount of check outs
checkouts_by_year <- spl_data %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

year_with_most_checkouts <- checkouts_by_year %>%
  slice_max(total_checkouts) %>%
  select(CheckoutYear)

# Value 4: The difference in checkout numbers between the years of 2013-2023
first_year_checkouts <- spl_data %>%
  filter(CheckoutYear == "2013") %>%
  summarize(total_checkouts = sum(Checkouts))

last_year_checkouts <- spl_data %>%
  filter(CheckoutYear == "2023") %>%
  summarize(total_checkouts = sum(Checkouts))

checkout_diff <- last_year_checkouts$total_checkouts - first_year_checkouts$total_checkouts

# Value 5: Most checked out item
item_checkouts <- spl_data %>%
  group_by(Title) %>%
  summarize(total_checkouts = sum(Checkouts)) %>%
  arrange(desc(total_checkouts))

most_checked_out_item <- item_checkouts[1,]

#---------------------------------------------------------------
# Trends Over Time Chart
# Total checkout numbers of books over 2013-2023 (linegraph)

book_checkouts <- spl_data %>%
  filter(MaterialType == "BOOK") %>%
  group_by(CheckoutYear) %>%
  summarize(total_checkouts = sum(Checkouts))

# Create a line graph showing the total checkout numbers for books over time
trend_over_time <- ggplot(book_checkouts, aes(x = CheckoutYear, y = total_checkouts)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "Total Checkouts", title = "Total Checkouts of Books") +
  scale_x_continuous(limits = c(2013, 2023), breaks = seq(2013, 2023, 1)) +
  scale_y_continuous(limits = c(0, max(book_checkouts$total_checkouts)), breaks = seq(0, max(book_checkouts$total_checkouts), by = 100000)) +
  theme_classic()

trend_over_time
#---------------------------------------------------------
# Variable Comparison Chart
# Trends of UsageClass overtime such as Digital and Physical (Bar Chart)

checkouts <- read.csv("2013-2023-5-Checkouts-SPL.csv")

# Create a bar chart of UsageClass over time
UsageClass_Barchart <- ggplot(checkouts, aes(x = CheckoutYear, fill = UsageClass)) + 
  geom_bar(position = "stack") + 
  theme(axis.text.x = element_text(size = 2, angle = 45)) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  scale_y_continuous(breaks = seq(0, 500000, by = 50000)) +
  labs(x = "Year", y = "Total Checkouts", fill = "Usage Class") +
  ggtitle("Trend of Usage Class Over Time") + 
  theme_minimal()

UsageClass_Barchart

#-----------------------------------------------------------------
# Overall usage of each Material Type such as Atlas, Books and DVDs etc. (Pie Chart)

total_checkouts <- spl_data %>%
  group_by(MaterialType) %>%
  summarise(TotalCheckouts = sum(Checkouts, na.rm = TRUE))

# Create a pie chart of the total checkouts by material type
MaterialType_Piechart <- ggplot(total_checkouts, aes(x = "", y = TotalCheckouts, fill = MaterialType)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Material Type") +
  ggtitle("Total Checkouts by Material Type")


MaterialType_Piechart

















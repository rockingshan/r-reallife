# Load required packages
library(dplyr)
library(janitor)
library(DBI)
library(RPostgres)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(grid)
library(svDialogs)

# Prompt user to choose file
file_path <- file.choose()

# Read and clean the CSV
raw_data <- read.csv(file_path, stringsAsFactors = FALSE) %>%
  janitor::clean_names()

# Select only necessary columns
data_clean <- raw_data %>%
  select(
    customer_nbr,
    entity_code,
    lco_city,
    service_name,
    plan_type
  ) %>%
  distinct()  # remove exact duplicates if any

# Replace specific LCO cities as per mapping
data_clean <- data_clean %>%
  mutate(lco_city = case_when(
    lco_city == "Bakrahat" ~ "Kolkata",
    lco_city == "Barasat" ~ "Kolkata",
    lco_city == "Howrah" ~ "Kolkata",
    lco_city == "Purulia" ~ "Bankura",
    TRUE ~ lco_city
  ))
# Normalize service names
data_clean <- data_clean %>%
  mutate(service_name = case_when(
    service_name == "DD" ~ "Bronze Basic",
    service_name == "Meghbela Basic Pack @ 155" ~ "Bronze Basic",
    TRUE ~ service_name
  ))

# Preview data
head(data_clean)

# Ask user to input the report date (in yyyy-mm-dd format)
report_date <- as.Date(dlgInput("Enter a date (YYYY-MM-DD):", default = format(Sys.Date(), "%Y-%m-%d"))$res)

# Validate date
if (is.na(report_date)) {
  stop("Invalid date format. Please enter as yyyy-mm-dd.")
}

# Create subscriber summary
subscribers_summary <- data_clean %>%
  distinct(customer_nbr, entity_code, lco_city) %>%
  group_by(entity_code, lco_city) %>%
  summarise(
    subscriber_count = n(),
    .groups = "drop"
  ) %>%
  mutate(report_date = report_date) %>%
  select(report_date, everything())  # reordering columns

# Preview the result
print(subscribers_summary)

# Filter and group for basic packages
basic_package_summary <- data_clean %>%
  filter(plan_type == "Basic") %>%
  group_by(entity_code, lco_city, service_name) %>%
  summarise(
    package_count = n(),
    .groups = "drop"
  ) %>%
  mutate(report_date = report_date) %>%
  select(report_date, entity_code, lco_city, service_name, package_count)

# Preview result
print(basic_package_summary)

# Filter and group for Alacarte services
alacarte_summary <- data_clean %>%
  filter(plan_type == "Alacarte") %>%
  group_by(lco_city, service_name) %>%
  summarise(
    service_count = n(),
    .groups = "drop"
  ) %>%
  mutate(report_date = report_date) %>%
  select(report_date, lco_city, service_name, service_count)

# Preview result
print(alacarte_summary)

####db operations ####

# Connect to PostgreSQL
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "meghbela",
  host = "192.168.1.160",
  port = 65432,  # change if needed
  user = "postgres",
  password = "hello12345"
)

# # SQL to create table if not exists
# create_subscribers_table_sql <- "
# CREATE TABLE IF NOT EXISTS subscribers_summary (
#   report_date DATE NOT NULL,
#   entity_code TEXT NOT NULL,
#   lco_city TEXT NOT NULL,
#   subscriber_count INTEGER NOT NULL
# );
# "
# 
# # Run the SQL
# dbExecute(con, create_subscribers_table_sql)
# 
# # SQL to create basic_package_summary table
# create_basic_table_sql <- "
# CREATE TABLE IF NOT EXISTS basic_package_summary (
#   report_date DATE NOT NULL,
#   entity_code TEXT NOT NULL,
#   lco_city TEXT NOT NULL,
#   service_name TEXT NOT NULL,
#   package_count INTEGER NOT NULL
# );
# "
# 
# # Run SQL to create table
# dbExecute(con, create_basic_table_sql)
# 
# # SQL to create alacarte_summary table
# create_alacarte_table_sql <- "
# CREATE TABLE IF NOT EXISTS alacarte_summary (
#   report_date DATE NOT NULL,
#   lco_city TEXT NOT NULL,
#   service_name TEXT NOT NULL,
#   service_count INTEGER NOT NULL
# );
# "
# 
# # Run SQL
# dbExecute(con, create_alacarte_table_sql)
# 


# Write the data to DB
dbWriteTable(
  con,
  name = "subscribers_summary",
  value = subscribers_summary,
  append = TRUE,
  row.names = FALSE
)

# Append today's basic package data
dbWriteTable(
  con,
  name = "basic_package_summary",
  value = basic_package_summary,
  append = TRUE,
  row.names = FALSE
)

# Append today's alacarte data
dbWriteTable(
  con,
  name = "alacarte_summary",
  value = alacarte_summary,
  append = TRUE,
  row.names = FALSE
)

# Disconnect
dbDisconnect(con)


####Plotting after fetching data ####

# Reconnect
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "meghbela",
  host = "192.168.1.160",
  port = 65432,  # change if needed
  user = "postgres",
  password = "hello12345"
)


# Get available report dates
available_dates <- dbGetQuery(con, "SELECT DISTINCT report_date FROM subscribers_summary ORDER BY report_date")

# Display available dates
print(available_dates)

# Get latest date
latest_date <- dbGetQuery(con, "
  SELECT MAX(report_date) as latest_date
  FROM subscribers_summary
")$latest_date

# Get total subscriber count for latest date
total_subs <- dbGetQuery(con, sprintf("
  SELECT SUM(subscriber_count) as total
  FROM subscribers_summary
  WHERE report_date = '%s'
", latest_date))

# Get last 7 dates' subscriber totals
trend_subs <- dbGetQuery(con, "
  SELECT report_date, SUM(subscriber_count) AS total_subscribers
  FROM subscribers_summary
  GROUP BY report_date
  ORDER BY report_date DESC
  LIMIT 7
")

# Order dates correctly (ascending for plotting)
trend_subs <- trend_subs %>%
  arrange(report_date)

# Query top 10 basic packages
top10_basic <- dbGetQuery(con, sprintf("
  SELECT service_name, SUM(package_count) AS total_subscribers
  FROM basic_package_summary
  WHERE report_date = '%s'
  GROUP BY service_name
  ORDER BY total_subscribers DESC
  LIMIT 10
", latest_date))

# Get all available dates in ascending order
all_dates <- dbGetQuery(con, "
  SELECT DISTINCT report_date
  FROM basic_package_summary
  ORDER BY report_date
")$report_date

# Ensure we have enough dates
if (length(all_dates) < 3) {
  stop("Not enough dates for 3-point trend.")
}

# Find 3 dates: latest + 2 more within 15 days range
latest <- as.Date(tail(all_dates, 1))

# Try to find other dates at least 7 days older, but not more than 15
valid_dates <- all_dates[as.Date(all_dates) >= (latest - 15) & as.Date(all_dates) <= latest]

# Pick 3 most recent from valid range
trend_dates <- tail(valid_dates, 3)

# Format for SQL IN clause
date_list_sql <- paste(sprintf("'%s'", trend_dates), collapse = ", ")

# Get top 5 basic packages on latest date
top5 <- dbGetQuery(con, sprintf("
  SELECT service_name, SUM(package_count) as subs
  FROM basic_package_summary
  WHERE report_date = '%s'
  GROUP BY service_name
  ORDER BY subs DESC
  LIMIT 5
", latest))

top5_packages <- sprintf("'%s'", top5$service_name)
top5_list_sql <- paste(top5_packages, collapse = ", ")

trend_data <- dbGetQuery(con, sprintf("
  SELECT report_date, service_name, SUM(package_count) AS count
  FROM basic_package_summary
  WHERE report_date IN (%s)
    AND service_name IN (%s)
  GROUP BY report_date, service_name
  ORDER BY report_date
", date_list_sql, top5_list_sql))

# SQL to get top 2 packages per city for the latest date
top2_city_sql <- sprintf("
  SELECT lco_city, service_name, total_count FROM (
    SELECT 
      lco_city,
      service_name,
      SUM(package_count) AS total_count,
      RANK() OVER (PARTITION BY lco_city ORDER BY SUM(package_count) DESC) AS rank
    FROM basic_package_summary
    WHERE report_date = '%s'
    GROUP BY lco_city, service_name
  ) sub
  WHERE rank <= 2
", latest)

# Run query
top2_city_packages <- dbGetQuery(con, top2_city_sql)

##Query Top 10 Alacarte Services
top10_alacarte <- dbGetQuery(con, sprintf("
  SELECT service_name, SUM(service_count) AS total
  FROM alacarte_summary
  WHERE report_date = '%s'
  GROUP BY service_name
  ORDER BY total DESC
  LIMIT 10
", latest))

##top 3 alacarte per city
top3_alacarte_city_sql <- sprintf("
  SELECT lco_city, service_name, total_count FROM (
    SELECT 
      lco_city,
      service_name,
      SUM(service_count) AS total_count,
      RANK() OVER (PARTITION BY lco_city ORDER BY SUM(service_count) DESC) AS rank
    FROM alacarte_summary
    WHERE report_date = '%s'
    GROUP BY lco_city, service_name
  ) sub
  WHERE rank <= 3
", latest)

top3_alacarte_city <- dbGetQuery(con, top3_alacarte_city_sql)


###plotting####
# Plot subscriber count as a title
plot1 <- ggplot() +
  annotate("text", x = 1, y = 1,
           label = paste("Total Subscribers on", latest_date, ":\n", total_subs$total),
           size = 8, fontface = "bold") +
  theme_void()

# Print plot
print(plot1)

# Create line plot of subscriber count trend
plot2 <- ggplot(trend_subs, aes(x = report_date, y = total_subscribers)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 3) +
  geom_text(aes(label = total_subscribers), vjust = -0.5, size = 4) +
  labs(
    title = "Subscriber Trend - Last 7 Days",
    x = "Date",
    y = "Total Subscribers"
  ) +
  theme_minimal()

# Show plot
print(plot2)

# Plot as horizontal bar chart
plot3 <- ggplot(top10_basic, aes(x = reorder(service_name, total_subscribers), y = total_subscribers)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = paste("Top 10 Basic Packages on", latest_date),
    x = "Package Name",
    y = "Subscribers"
  ) +
  theme_minimal()

# Show plot
print(plot3)

# Plot trend of top 5 packages
plot4 <- ggplot(trend_data, aes(x = as.Date(report_date), y = count, color = service_name, group = service_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Top 5 Basic Packages Trend (3 Dates, Spanning 7–15 Days)",
    x = "Date",
    y = "Subscriber Count",
    color = "Package"
  ) +
  theme_minimal()

# Show plot
print(plot4)

# Plot using facet_wrap to show each city separately
plot5 <- ggplot(top2_city_packages, aes(x = reorder(service_name, total_count), y = total_count, fill = service_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ lco_city, scales = "free_y") +
  coord_flip() +
  labs(
    title = paste("Top 2 Basic Packages per City on", latest),
    x = "Package",
    y = "Subscriber Count"
  ) +
  theme_minimal()

# Show plot
print(plot5)

##plot of top 10 alacarte
plot6 <- ggplot(top10_alacarte, aes(x = reorder(service_name, total), y = total)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(
    title = paste("Top 10 Alacarte Services on", latest),
    x = "Service Name",
    y = "Subscriber Count"
  ) +
  theme_minimal()

# Show plot
print(plot6)

##plot of top 3 alacarte per city
plot7 <- ggplot(top3_alacarte_city, aes(x = reorder(service_name, total_count), y = total_count, fill = service_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ lco_city, scales = "free_y") +
  coord_flip() +
  labs(
    title = paste("Top 3 Alacarte Services per City on", latest),
    x = "Service Name",
    y = "Subscriber Count"
  ) +
  theme_minimal()

# Show plot
print(plot7)

# Format today's date
today_str <- format(Sys.Date(), "%Y-%m-%d")

# Prepare PDF file name with today's date
pdf_file <- file.path("Output", paste0("Subscriber_Trends_Report_", today_str, ".pdf"))

cover <- grid.text(
  paste("Subscriber & Service Trends Report\nDate:", today_str),
  gp = gpar(fontsize = 20, fontface = "bold")
)
title1 <- grid.text("Plot 1: Overall Subscriber Count (Latest Date)", gp = gpar(fontsize = 16, fontface = "bold"))
insight1 <- grid.text("Insight: Shows total active subscriber base as of the latest report date.", gp = gpar(fontsize = 12))


pdf(pdf_file, width = 11, height = 8.5)

# Cover page
grid.newpage()
grid.text(paste("Subscriber Trends Report", "\nDate:", today_str), gp = gpar(fontsize = 22, fontface = "bold"))

# Plot 1
grid.newpage(); grid.draw(title1); grid.newpage(); print(plot1); grid.newpage(); grid.draw(insight1)

# Plot 2
grid.newpage(); grid.text("Plot 2: Subscriber Trend (Last 7 Dates)", gp = gpar(fontsize = 16, fontface = "bold"))
print(plot2)
grid.newpage(); grid.text("Insight: Shows how subscriber base is increasing or decreasing over time.", gp = gpar(fontsize = 12))

# Plot 3
grid.newpage(); grid.text("Plot 3: Top 10 Basic Packages", gp = gpar(fontsize = 16, fontface = "bold"))
print(plot3)
grid.newpage(); grid.text("Insight: Highlights the most subscribed basic packages as of the latest date.", gp = gpar(fontsize = 12))

# Plot 4
grid.newpage(); grid.text("Plot 4: Top 5 Basic Packages Trend", gp = gpar(fontsize = 16, fontface = "bold"))
print(plot4)
grid.newpage(); grid.text("Insight: Visualizes growth or decline in popularity of leading packages over time.", gp = gpar(fontsize = 12))

# Plot 5
grid.newpage(); grid.text("Plot 5: Top 2 Packages Per City", gp = gpar(fontsize = 16, fontface = "bold"))
print(plot5)

# Plot 6
grid.newpage(); grid.text("Plot 6: Top 10 Alacarte Services", gp = gpar(fontsize = 16, fontface = "bold"))
print(plot6)

# Plot 7
grid.newpage(); grid.text("Plot 7: Top 3 Alacarte Services Per City", gp = gpar(fontsize = 16, fontface = "bold"))
print(plot7)

dev.off()


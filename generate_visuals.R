
library(ggplot2)
library(dplyr)
library(randomForest)
library(lubridate)

dataset <- read.csv("~/Desktop/DATA3010/project/UK+Train+Rides/railway.csv")
# Ticket Price Distribution
price_plot <- ggplot(dataset, aes(x = Price)) +
  geom_histogram(binwidth = 10, fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = 'Distribution of Ticket Prices', x = 'Price ($)', y = 'Frequency') +
  theme_minimal()
price_plot


# Top 5 Departure Stations
top_departure_stations <- dataset %>%
  group_by(Departure.Station) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:5)

station_plot <- ggplot(top_departure_stations, aes(x = reorder(Departure.Station, -count), y = count)) +
  geom_bar(stat = 'identity', fill = 'green', color = 'black', alpha = 0.7) +
  labs(title = 'Top 5 Departure Stations', x = 'Station', y = 'Number of Departures') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
station_plot


# Journey Status Distribution
status_count <- dataset %>%
  group_by(Journey.Status) %>%
  summarise(count = n())

status_plot <- ggplot(status_count, aes(x = reorder(Journey.Status, -count), y = count)) +
  geom_bar(stat = 'identity', fill = 'red', color = 'black', alpha = 0.7) +
  labs(title = 'Journey Status Distribution', x = 'Status', y = 'Count') +
  theme_minimal()
status_plot




# Railcard Usage by Ticket Class - Stacked Bar Chart
railcard_stacked_plot <- dataset %>%
  filter(Railcard != "None") %>%
  group_by(Railcard, Ticket.Class) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Railcard, y = count, fill = Ticket.Class)) +
  geom_bar(stat = 'identity', color = 'black', alpha = 0.7) +
  labs(title = 'Railcard Usage by Ticket Class', x = 'Railcard Type', y = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
railcard_stacked_plot


# Alternative 2: Refund Requests by Journey Status - Heatmap
refund_heatmap_data <- dataset %>%
  filter(Journey.Status %in% c("Delayed", "Cancelled")) %>%
  group_by(Refund.Request, Journey.Status) %>%
  summarise(count = n())

refund_heatmap_plot <- ggplot(refund_heatmap_data, aes(x = Journey.Status, y = Refund.Request, fill = count)) +
  geom_tile(color = 'white') +
  scale_fill_gradient(low = 'lightblue', high = 'darkblue') +
  labs(title = 'Refund Requests by Journey Status (Heatmap)', x = 'Journey Status', y = 'Refund Request') +
  theme_minimal()
refund_heatmap_plot





#Revenue by Ticket Type - Pie Chart
revenue_by_type <- dataset %>%
  group_by(Ticket.Type) %>%
  summarise(total_revenue = sum(Price))

pie_chart_plot <- ggplot(revenue_by_type, aes(x = "", y = total_revenue, fill = Ticket.Type)) +
  geom_bar(stat = 'identity', width = 1, color = 'black') +
  coord_polar(theta = "y") +
  labs(title = 'Revenue by Ticket Type (Pie Chart)', x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
pie_chart_plot




# Average Ticket Price by Ticket Type and Class
avg_price <- dataset %>%
  group_by(Ticket.Type, Ticket.Class) %>%
  summarise(avg_price = mean(Price)) %>%
  arrange(desc(avg_price))

avg_price_plot <- ggplot(avg_price, aes(x = Ticket.Type, y = avg_price, fill = Ticket.Class)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black', alpha = 0.7) +
  labs(title = 'Average Ticket Price by Type and Class', x = 'Ticket Type', y = 'Average Price ($)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
avg_price_plot


# Calculate total revenue by route line graph
route_revenue <- dataset %>%
  group_by(Departure.Station, Arrival.Destination) %>%
  summarise(total_revenue = sum(Price)) %>%
  arrange(desc(total_revenue))

# Combine departure and arrival into a single column for visualization
route_revenue <- route_revenue %>%
  mutate(Route = paste(Departure.Station, "to", Arrival.Destination))

# Filter for Top 10 Routes
top_routes <- route_revenue %>% 
  head(10) # Select the first 10 rows
# Bubble Chart for Top 10 Routes
line_graph <- ggplot(top_routes, aes(x = reorder(Route, -total_revenue), y = total_revenue, group = 1)) +
  geom_line(color = "grey", size = 1) +
  geom_point(size = 3, color = "black") +
  labs(title = 'Top 10 Revenue-Generating Routes (Line Graph)', x = 'Route', y = 'Total Revenue ($)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
line_graph



# Load necessary libraries (Predicted vs actual arrival time)
library(randomForest)
library(lubridate)
dataset <- read.csv("~/Desktop/DATA3010/project/UK+Train+Rides/railway.csv")

# Step 1: Replace Missing Values in `Reason.for.Delay`
dataset$Reason.for.Delay[is.na(dataset$Reason.for.Delay)] <- "On Time"

# Replace invalid time entries with "00:00:00"
dataset$Arrival.Time[!grepl("^\\d{1,2}:\\d{2}:\\d{2}$", dataset$Arrival.Time)] <- "00:00:00"
dataset$Actual.Arrival.Time[!grepl("^\\d{1,2}:\\d{2}:\\d{2}$", dataset$Actual.Arrival.Time)] <- "00:00:00"
dataset$Departure.Time[!grepl("^\\d{1,2}:\\d{2}:\\d{2}$", dataset$Departure.Time)] <- "00:00:00"

# Step 2: Convert Time Columns to Numeric Values (Minutes Past Midnight)
dataset$Arrival.Time <- as.numeric(hms(dataset$Arrival.Time)) / 60
dataset$Actual.Arrival.Time <- as.numeric(hms(dataset$Actual.Arrival.Time)) / 60
dataset$Departure.Time <- as.numeric(hms(dataset$Departure.Time)) / 60

# Step 3: Convert Categorical Variables to Factors
dataset$Ticket.Class <- as.factor(dataset$Ticket.Class)
dataset$Ticket.Type <- as.factor(dataset$Ticket.Type)
dataset$Journey.Status <- as.factor(dataset$Journey.Status)
dataset$Reason.for.Delay <- as.factor(dataset$Reason.for.Delay)
dataset$Refund.Request <- as.factor(dataset$Refund.Request)

# Step 5: Split Data into Training and Testing Sets
set.seed(42)
train_idx <- sample(1:nrow(dataset), size = 0.8 * nrow(dataset))
train_data <- dataset[train_idx, ]
test_data <- dataset[-train_idx, ]

# Step 6: Train the Random Forest Model
rf_model <- randomForest(Actual.Arrival.Time ~ Arrival.Time + Departure.Time + 
                           Ticket.Class + Ticket.Type + Journey.Status + 
                           Reason.for.Delay + Refund.Request,
                         data = train_data, ntree = 200, importance = TRUE)

# Step 7: Print Model Summary
print(rf_model)

# Step 8: Predict on the Test Set
test_data$Predicted.Arrival.Time <- predict(rf_model, newdata = test_data)

# Step 9: Visualize Predicted vs Actual Arrival Times
ggplot(test_data, aes(x = Actual.Arrival.Time, y = Predicted.Arrival.Time)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Arrival Time",
       x = "Actual Arrival Time (minutes past midnight)",
       y = "Predicted Arrival Time (minutes past midnight)") +
  theme_minimal()



# Group by railcard and calculate usage (railcard usage distrubution 30 %)
railcard_usage <- dataset %>%
  group_by(Railcard) %>%
  summarise(count = n())

# Create pie chart
ggplot(railcard_usage, aes(x = "", y = count, fill = Railcard)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Railcard Usage Distribution", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())



# Group by Purchase.Type and calculate counts and percentages
purchase_type_counts <- dataset %>%
  group_by(Purchase.Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100) # Calculate percentage

# Create bar chart for Purchase.Type with percentage labels
ggplot(purchase_type_counts, aes(x = Purchase.Type, y = count, fill = Purchase.Type)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, size = 5) + # Add percentage labels above the bars
  labs(title = "Online vs Station Ticket Purchases", 
       x = "Purchase Type", 
       y = "Number of Tickets") +
  theme_minimal() +
  theme(legend.position = "none")

dataset <- read.csv("~/Desktop/DATA3010/project/UK+Train+Rides/railway.csv")
# Convert time columns to minutes past midnight
dataset <- dataset %>%
  mutate(
    Scheduled_Arrival_Minutes = as.numeric(format(as.POSIXct(Arrival.Time, format = "%H:%M:%S"), "%H")) * 60 +
      as.numeric(format(as.POSIXct(Arrival.Time, format = "%H:%M:%S"), "%M")),
    Actual_Arrival_Minutes = as.numeric(format(as.POSIXct(Actual.Arrival.Time, format = "%H:%M:%S"), "%H")) * 60 +
      as.numeric(format(as.POSIXct(Actual.Arrival.Time, format = "%H:%M:%S"), "%M"))
  ) %>%
  filter(!is.na(Scheduled_Arrival_Minutes) & !is.na(Actual_Arrival_Minutes)) %>%
  mutate(Deviation = Actual_Arrival_Minutes - Scheduled_Arrival_Minutes)

# Group by hourly intervals
deviation_by_hour <- dataset %>%
  mutate(Hour = Scheduled_Arrival_Minutes %/% 60) %>%
  group_by(Hour) %>%
  summarise(Average_Deviation = mean(Deviation, na.rm = TRUE))

# Plotting
ggplot(deviation_by_hour, aes(x = Hour, y = Average_Deviation)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average Deviation by Time of Day", x = "Hour of the Day (24-hour format)", y = "Average Deviation (minutes)") +
  theme_minimal()

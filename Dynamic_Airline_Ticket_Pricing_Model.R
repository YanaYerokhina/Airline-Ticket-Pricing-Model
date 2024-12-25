# Load required libraries
library(tidyverse)
library(caret)
library(lubridate)
library(xgboost)
library(forecast)
library(ggplot2)
library(plotly)

# Data preprocessing function
preprocess_data <- function(file_path) {
  data <- read.csv(file_path)
  
  # Basic preprocessing from before
  data$sales_channel <- as.factor(data$sales_channel)
  data$trip_type <- as.factor(data$trip_type)
  data$route <- as.factor(data$route)
  data$booking_origin <- as.factor(data$booking_origin)
  data$flight_day <- as.factor(data$flight_day)
  
  # Enhanced time-based features
  data$is_weekend <- ifelse(data$flight_day %in% c("Saturday", "Sunday"), 1, 0)
  data$is_peak_hour <- ifelse(data$flight_hour >= 6 & data$flight_hour <= 9 |
                                data$flight_hour >= 16 & data$flight_hour <= 19, 1, 0)
  
  # Add seasonal features
  data$month <- month(as.Date(data$flight_day, format="%A"))
  data$season <- case_when(
    data$month %in% c(12, 1, 2) ~ "Winter",
    data$month %in% c(3, 4, 5) ~ "Spring",
    data$month %in% c(6, 7, 8) ~ "Summer",
    TRUE ~ "Fall"
  )
  
  # Holiday periods (major US holidays as example)
  holidays <- c("2024-01-01", "2024-12-25", "2024-07-04", "2024-11-28")
  data$is_holiday_period <- 0
  for(holiday in holidays) {
    holiday_date <- as.Date(holiday)
    # Mark 3 days before and after holiday as holiday period
    data$is_holiday_period <- ifelse(
      abs(as.numeric(difftime(as.Date(data$flight_day, format="%A"), 
                              holiday_date, units="days"))) <= 3,
      1, data$is_holiday_period
    )
  }
  
  return(data)
}

# Main pricing function
dynamic_pricing <- function(data) {
  # Initialize price vector
  prices <- numeric(nrow(data))
  
  for(i in 1:nrow(data)) {
    # Calculate base price
    base_price <- calculate_base_price(
      data$route[i],
      data$flight_duration[i]
    )
    
    # Calculate demand multiplier
    demand_mult <- calculate_demand_multiplier(
      data$purchase_lead[i],
      data$is_weekend[i],
      data$is_peak_hour[i]
    )
    
    # Calculate extras cost
    extras <- calculate_extras_cost(
      data$wants_extra_baggage[i],
      data$wants_preferred_seat[i],
      data$wants_in_flight_meals[i]
    )
    
    # Calculate final price
    prices[i] <- (base_price * demand_mult * data$num_passengers[i]) + extras
  }
  
  return(prices)
}

# Function to train predictive model for booking probability
train_booking_model <- function(data) {
  # Prepare features for modeling
  features <- c("num_passengers", "purchase_lead", "length_of_stay", 
                "flight_hour", "flight_duration", "is_weekend", "is_peak_hour")
  
  # Create training data
  X <- data[features]
  y <- data$booking_complete
  
  # Convert to matrix format for xgboost
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(X), label = y)
  
  # Set parameters
  params <- list(
    objective = "binary:logistic",
    eta = 0.1,
    max_depth = 6,
    nrounds = 100
  )
  
  # Train model
  model <- xgboost::xgb.train(params = params, data = dtrain, nrounds = 100)
  
  return(model)
}

# Demand modeling function
model_demand <- function(data) {
  # Create time series of bookings by date
  daily_bookings <- data %>%
    group_by(flight_day) %>%
    summarise(bookings = n())
  
  # Fit SARIMA model to capture seasonality
  ts_bookings <- ts(daily_bookings$bookings, frequency=7)
  demand_model <- auto.arima(ts_bookings, seasonal=TRUE)
  
  # Generate demand forecast
  forecast_periods <- 30
  demand_forecast <- forecast(demand_model, h=forecast_periods)
  
  return(list(
    model = demand_model,
    forecast = demand_forecast
  ))
}

# Competitor price tracking simulation
track_competitor_prices <- function(route, date) {
  # Simulate competitor prices (in practice, this would pull from real competitor data)
  base_competitor_price <- runif(1, 200, 800)
  
  # Add random variation for different competitors
  competitor_prices <- data.frame(
    competitor = c("Comp1", "Comp2", "Comp3"),
    price = base_competitor_price + rnorm(3, 0, 50)
  )
  
  return(competitor_prices)
}

# Enhanced seasonal adjustment function
calculate_seasonal_multiplier <- function(season, is_holiday_period) {
  # Base seasonal multipliers
  season_mult <- case_when(
    season == "Summer" ~ 1.3,
    season == "Winter" ~ 1.2,
    season == "Spring" ~ 1.1,
    TRUE ~ 1.0
  )
  
  # Holiday period adjustment
  holiday_mult <- if(is_holiday_period) 1.4 else 1.0
  
  return(season_mult * holiday_mult)
}

# Enhanced pricing optimization
optimize_prices <- function(base_prices, booking_prob, competitor_prices, demand_forecast) {
  # Calculate competitor price statistics
  comp_mean <- mean(competitor_prices$price)
  comp_min <- min(competitor_prices$price)
  
  # Calculate demand pressure
  demand_pressure <- as.numeric(demand_forecast$mean[1]) / mean(demand_forecast$mean)
  
  # Define price bounds
  min_price <- pmax(base_prices * 0.7, comp_min * 0.9)
  max_price <- base_prices * 1.4
  
  # Optimize prices based on multiple factors
  optimized_prices <- base_prices * (
    1 + 
      (0.5 - booking_prob) * 0.4 +  # Booking probability adjustment
      (demand_pressure - 1) * 0.3 +  # Demand pressure adjustment
      (comp_mean / base_prices - 1) * 0.3  # Competitor price adjustment
  )
  
  # Ensure prices stay within bounds
  optimized_prices <- pmax(min_price, pmin(max_price, optimized_prices))
  
  return(optimized_prices)
}

# New visualization functions
generate_pricing_visualizations <- function(results, demand_forecast) {
  # Price distribution plot
  price_dist <- ggplot(results, aes(x=final_price)) +
    geom_histogram(fill="skyblue", bins=30) +
    theme_minimal() +
    labs(title="Distribution of Final Prices",
         x="Price ($)", y="Count")
  
  # Demand forecast plot
  demand_plot <- autoplot(demand_forecast) +
    theme_minimal() +
    labs(title="Demand Forecast",
         x="Time Period", y="Expected Bookings")
  
  # Price vs. Booking Probability
  price_prob_plot <- ggplot(results, 
                            aes(x=booking_probability, y=final_price)) +
    geom_point(alpha=0.5) +
    geom_smooth(method="lm") +
    theme_minimal() +
    labs(title="Price vs. Booking Probability",
         x="Booking Probability", y="Final Price ($)")
  
  return(list(
    price_distribution = price_dist,
    demand_forecast = demand_plot,
    price_probability = price_prob_plot
  ))
}

# Generate pricing report
generate_pricing_report <- function(results, demand_forecast, competitor_data) {
  # Calculate summary statistics
  summary_stats <- list(
    mean_price = mean(results$final_price),
    median_price = median(results$final_price),
    price_range = range(results$final_price),
    expected_demand = mean(demand_forecast$mean),
    competitor_price_avg = mean(competitor_data$price)
  )
  
  # Create route-specific analysis
  route_analysis <- results %>%
    group_by(route) %>%
    summarise(
      avg_price = mean(final_price),
      avg_booking_prob = mean(booking_probability),
      price_variance = var(final_price)
    )
  
  # Generate markdown report
  report <- sprintf("
# Pricing Analysis Report

## Summary Statistics
- Average Price: $%.2f
- Median Price: $%.2f
- Price Range: $%.2f - $%.2f
- Expected Demand: %.0f bookings
- Average Competitor Price: $%.2f

## Route Analysis
%s

## Recommendations
1. Routes with low booking probabilities (<0.4) might benefit from price adjustments
2. Monitor routes with high price variance for stability
3. Adjust prices based on competitor positioning
",
                    summary_stats$mean_price,
                    summary_stats$median_price,
                    summary_stats$price_range[1],
                    summary_stats$price_range[2],
                    summary_stats$expected_demand,
                    summary_stats$competitor_price_avg,
                    capture.output(print(route_analysis))
  )
  
  return(report)
}

# Enhanced main execution function
run_pricing_system <- function(file_path) {
  # Load and preprocess data
  data <- preprocess_data(file_path)
  
  # Model demand
  demand_analysis <- model_demand(data)
  
  # Calculate initial prices
  base_prices <- dynamic_pricing(data)
  
  # Get competitor prices
  competitor_prices <- track_competitor_prices(data$route[1], Sys.Date())
  
  # Train booking probability model
  booking_model <- train_booking_model(data)
  
  # Predict booking probabilities
  features <- c("num_passengers", "purchase_lead", "length_of_stay", 
                "flight_hour", "flight_duration", "is_weekend", "is_peak_hour")
  booking_prob <- predict(booking_model, as.matrix(data[features]))
  
  # Calculate seasonal adjustments
  seasonal_mult <- mapply(calculate_seasonal_multiplier,
                          data$season,
                          data$is_holiday_period)
  
  # Optimize prices
  final_prices <- optimize_prices(base_prices * seasonal_mult,
                                  booking_prob,
                                  competitor_prices,
                                  demand_analysis$forecast)
  
  # Prepare results
  results <- data.frame(
    route = data$route,
    base_price = base_prices,
    seasonal_adjusted_price = base_prices * seasonal_mult,
    booking_probability = booking_prob,
    final_price = final_prices
  )
  
  # Generate visualizations
  plots <- generate_pricing_visualizations(results, demand_analysis$forecast)
  
  # Generate report
  report <- generate_pricing_report(results, demand_analysis$forecast, competitor_prices)
  
  return(list(
    results = results,
    plots = plots,
    report = report,
    demand_forecast = demand_analysis$forecast
  ))
}

# Run the enhanced pricing system
results <- run_pricing_system("customer_booking.csv")

# View different components
head(results$results)  # View pricing results
results$plots$price_distribution  # View price distribution plot
cat(results$report)  # View the pricing report

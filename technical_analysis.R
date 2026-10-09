# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 2: Technical Analysis using R, Preliminary Stage
# ==============================================================================

# ------------------------------------------------------------------------------
# Section 3: Install and Load Required Packages
# ------------------------------------------------------------------------------
required_packages <- c("quantmod", "TTR")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ------------------------------------------------------------------------------
# Section 6 Utility: Mode Calculation Helper
# ------------------------------------------------------------------------------
# R does not have a built-in function for statistical mode, so we define one.
calculate_mode <- function(x) {
  x <- na.omit(x)
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# ------------------------------------------------------------------------------
# Section 5: Function to Import and Load Stocks Data
# ------------------------------------------------------------------------------
load_stock_data <- function(file_path = "portfolio.txt", from_date = "2023-01-01") {
  if (!file.exists(file_path)) {
    stop("Error: Portfolio file not found at ", file_path)
  }
  
  # Read stock symbols line by line
  symbols <- readLines(file_path, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[symbols != ""] # Remove empty lines
  
  stock_data_list <- list()
  
  for (symbol in symbols) {
    cat("Fetching data for:", symbol, "...\n")
    tryCatch({
      # Download stock data using quantmod
      data <- getSymbols(symbol, src = "yahoo", from = from_date, auto.assign = FALSE)
      stock_data_list[[symbol]] <- data
    }, error = function(e) {
      warning(paste("Failed to fetch data for symbol:", symbol, "-", e$message))
    })
  }
  
  return(stock_data_list)
}

# ------------------------------------------------------------------------------
# Section 6: Function to Compute Basic Statistics
# ------------------------------------------------------------------------------
calculate_statistics <- function(stock_xts) {
  # Extract Adjusted Closing Prices
  adj_prices <- Cl(stock_xts) # or Ad(stock_xts) for Adjusted Close
  adj_vector <- as.numeric(na.omit(adj_prices))
  
  # Calculate 20-day Simple Moving Average (SMA) using TTR
  sma_20 <- SMA(adj_prices, n = 20)
  latest_sma <- tail(na.omit(sma_20), 1)
  
  # Compute statistical measures
  stats <- list(
    Mean               = mean(adj_vector),
    Median             = median(adj_vector),
    Mode               = calculate_mode(round(adj_vector, 2)),
    Std_Dev            = sd(adj_vector),
    Latest_20Day_SMA   = as.numeric(latest_sma),
    Min_Price          = min(adj_vector),
    Max_Price          = max(adj_vector)
  )
  
  return(stats)
}

# ------------------------------------------------------------------------------
# Section 7: Utilities to Display Output
# ------------------------------------------------------------------------------
display_summary_table <- function(stock_list) {
  summary_df <- data.frame(
    Symbol = character(),
    Mean = numeric(),
    Median = numeric(),
    Mode = numeric(),
    Std_Dev = numeric(),
    Latest_20Day_SMA = numeric(),
    Min_Price = numeric(),
    Max_Price = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (symbol in names(stock_list)) {
    stats <- calculate_statistics(stock_list[[symbol]])
    summary_df <- rbind(summary_df, data.frame(
      Symbol = symbol,
      Mean = round(stats$Mean, 2),
      Median = round(stats$Median, 2),
      Mode = round(stats$Mode, 2),
      Std_Dev = round(stats$Std_Dev, 2),
      Latest_20Day_SMA = round(stats$Latest_20Day_SMA, 2),
      Min_Price = round(stats$Min_Price, 2),
      Max_Price = round(stats$Max_Price, 2)
    ))
  }
  
  cat("\n=======================================================\n")
  cat("           SUMMARY OF TECHNICAL STATISTICS             \n")
  cat("=======================================================\n")
  print(summary_df)
  return(summary_df)
}

# ------------------------------------------------------------------------------
# Execution & Display
# ------------------------------------------------------------------------------
# 1. Load Stock Data
portfolio_data <- load_stock_data("portfolio.txt", from_date = "2023-01-01")

# 2. Display Head of Loaded Data (Yahoo Finance Style)
cat("\n--- Preview of Raw Imported Data (First Stock) ---\n")
if (length(portfolio_data) > 0) {
  print(head(portfolio_data[[1]]))
}

# 3. Compute & Display Calculated Statistics Table
summary_table <- display_summary_table(portfolio_data)

# 4. Visualization (Candlestick Chart with Moving Average)
if (length(portfolio_data) > 0) {
  first_symbol <- names(portfolio_data)[1]
  chartSeries(
    portfolio_data[[first_symbol]], 
    name = paste("Technical Chart:", first_symbol),
    theme = chartTheme("white"),
    TA = "addSMA(n=20, col='blue'); addEMA(n=50, col='red')"
  )
}

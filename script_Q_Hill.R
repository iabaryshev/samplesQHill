
# The working directory must contain a file named sample.csv, in which the data is provided as a single column.
# The first cell is the column header "x". The decimal separator must be a dot (`.`).
# The main output of the script is the calculation of the number of samples that need to be collected and processed to obtain a result with known precision.
# The result is written to a file in the working directory named "Number of samples.txt"

# Step 1: Load the table from the sample.csv file
data <- read.csv("sample.csv")

# Step 2: Calculate the mean of the data
a <- mean(data[[1]], na.rm = TRUE)  # If data is a dataframe with one column

# Step 3: Create a table with 1000 shuffled columns
n_rows <- nrow(data)  # Number of rows in the original data
n_cols <- 1000        # Number of new columns
# Create a new table (each column is a random permutation of the original data)
random_data <- as.data.frame(
  replicate(n_cols, sample(data[[1]], size = n_rows, replace = FALSE))
)
# Name the columns col_1, col_2, ..., col_1000
colnames(random_data) <- paste0("col_", 1:n_cols)

# Step 4: Create a table of cumulative means
# Create a table with cumulative means
cumulative_means <- as.data.frame(
  lapply(random_data, function(col) {
    cumsum(col) / seq_along(col)
  })
)

# Preserve column names
colnames(cumulative_means) <- colnames(random_data)

# View the first rows of the result
# head(cumulative_means)

# Step 5: Calculate percentiles for three significance levels
# The arguments k were set to 0.15 (lower bound) and 0.85 (upper bound) for 70% probability;
# 0.05 and 0.95 for 90% probability; 0.025 and 0.975 for 95% probability.

# For 70% probability
# Create a percentil70 table with percentiles
percentil70 <- data.frame(
  p15 = apply(cumulative_means, 1, quantile, probs = 0.15, na.rm = TRUE),
  p85 = apply(cumulative_means, 1, quantile, probs = 0.85, na.rm = TRUE)
)
# Add a new column with the calculation (p85 - p15)/a
percentil70$normalized_range <- (percentil70$p85 - percentil70$p15) / a * 100

# For 90% probability
# Create a percentil90 table with percentiles
percentil90 <- data.frame(
  p05 = apply(cumulative_means, 1, quantile, probs = 0.05, na.rm = TRUE),
  p95 = apply(cumulative_means, 1, quantile, probs = 0.95, na.rm = TRUE)
)
# Add a new column with the calculation (p95 - p05)/a
percentil90$normalized_range <- (percentil90$p95 - percentil90$p05) / a * 100

# For 95% probability
# Create a percentil95 table with percentiles
percentil95 <- data.frame(
  p025 = apply(cumulative_means, 1, quantile, probs = 0.025, na.rm = TRUE),
  p975 = apply(cumulative_means, 1, quantile, probs = 0.975, na.rm = TRUE)
)
# Add a new column with the calculation (p975 - p025)/a
percentil95$normalized_range <- (percentil95$p975 - percentil95$p025) / a * 100

# Create a combined table with interval widths
intervals70_90_95 <- data.frame(
  row_num = 1:nrow(percentil70),  # Row numbers
  
  # From the percentil70 table take the last column
  norm_range_70 = percentil70[[ncol(percentil70)]],
  
  # From the percentil90 table take the last column
  norm_range_90 = percentil90[[ncol(percentil90)]],
  
  # Assuming we have a similar percentil95
  norm_range_95 = percentil95[[ncol(percentil95)]]
)

# Remove the first 5 rows!!!
intervals70_90_95 <- intervals70_90_95[-(1:5), ]

# Check the structure
str(intervals70_90_95)
# Display the first 10 rows
head(intervals70_90_95, 10)

#
# Next, the regression itself (modified version)
#

library(ggplot2)
library(dplyr)

# Function to fit a 3-parameter Hill equation
fit_hill_3param <- function(x, y) {
  start_values <- list(
    top = max(y),
    EC50 = median(x),
    n = 1
  )
  
  hill_func <- function(x, top, EC50, n) {
    top / (1 + (EC50/x)^n)
  }
  
  nls(
    y ~ hill_func(x, top, EC50, n),
    start = start_values,
    control = nls.control(maxiter = 500, warnOnly = TRUE)
  )
}

# Function to calculate R²
calculate_r_squared <- function(model, x, y) {
  predicted <- predict(model, newdata = data.frame(x = x))
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - predicted)^2)
  1 - (ss_residual/ss_total)
}

# Prepare data
data <- intervals70_90_95
n_rows <- nrow(data)

# Create a list to store models and their parameters
results <- list()

# Analysis for each confidence interval
for (col in c("norm_range_70", "norm_range_90", "norm_range_95")) {
  # Filter data (remove NA if present)
  clean_data <- data.frame(
    x = data[[col]],
    y = data$row_num
  ) %>% na.omit()
  
  # Fit the model
  model <- fit_hill_3param(clean_data$x, clean_data$y)
  
  # Calculate R² and AIC
  r_squared <- calculate_r_squared(model, clean_data$x, clean_data$y)
  aic_value <- AIC(model)
  
  # Save results
  params <- coef(model)
  results[[col]] <- list(
    model = model,
    params = params,
    r_squared = r_squared,
    aic = aic_value,
    equation = sprintf("y = %.2f / (1 + (%.2f/x)^%.2f)", params["top"], params["EC50"], params["n"])
  )
}

# Create a table with parameters of all models
model_summary <- data.frame(
  Model = names(results),
  top = sapply(results, function(x) x$params["top"]),
  EC50 = sapply(results, function(x) x$params["EC50"]),
  n = sapply(results, function(x) x$params["n"]),
  R_squared = sapply(results, function(x) x$r_squared),
  AIC = sapply(results, function(x) x$aic),
  Equation = sapply(results, function(x) x$equation),
  row.names = NULL
)

# Save model parameters to a file
write.table(
  model_summary,
  file = "hill_models_summary.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

# Print the table nicely in the console
cat("\nModeling results:\n")
cat("-----------------------------------------\n")
print(model_summary)
cat("-----------------------------------------\n")

# Save the table to a well-formatted text file
sink("hill_models_summary_formatted.txt")

cat("Hill model parameters with coefficients of determination and AIC\n")
cat("=========================================\n\n")

for(i in 1:nrow(model_summary)) {
  cat("Model: ", model_summary$Model[i], "\n")
  cat("-----------------------------------------\n")
  cat("Upper asymptote (top):    ", round(model_summary$top[i], 4), "\n")
  cat("Half-saturation point (EC50): ", round(model_summary$EC50[i], 4), "\n")
  cat("Slope coefficient (n):   ", round(model_summary$n[i], 4), "\n")
  cat("R-squared:                 ", round(model_summary$R_squared[i], 4), "\n")
  cat("AIC:                       ", round(model_summary$AIC[i], 2), "\n")
  cat("Model equation:          ", model_summary$Equation[i], "\n\n")
}

cat("Note:\n")
cat("- top: maximum y value (upper asymptote)\n")
cat("- EC50: x value at which half of top is achieved\n")
cat("- n: slope coefficient of the curve\n")
cat("- R-squared: proportion of explained variance (0-1)\n")
cat("- AIC: Akaike Information Criterion (smaller is better)\n")

sink()

# Show save message
cat("\nParameter table saved to files:\n")
cat("- hill_models_summary.txt (tabular format)\n")
cat("- hill_models_summary_formatted.txt (formatted version)\n")

#
# Now calculate the number of samples for the required confidence intervals and plot the graph
#

# Define the number of samples
# Create models from model_summary
models <- lapply(1:nrow(model_summary), function(i) {
  list(
    coefficients = c(
      top = model_summary$top[i],
      EC50 = model_summary$EC50[i],
      n = model_summary$n[i]
    ),
    formula = model_summary$Equation[i]
  )
})
names(models) <- model_summary$Model

# Function to calculate y
calculate_y <- function(model, x) {
  with(as.list(model$coefficients), {
    top / (1 + (EC50/x)^n)
  })
}

# Calculate values for x = 20, 30, 40, 50, 60
x_values <- c(20, 30, 40, 50, 60)
results <- sapply(models, function(model) {
  round(calculate_y(model, x_values), 2)
})
rownames(results) <- paste0("x=", x_values)

# Round all values to integers
results_rounded <- round(results)

# Print the result
print(results_rounded)

# Save the table to a txt file
sink("Number of samples.txt")
cat("Calculation results (rounded to integers)\n")
cat("=========================================\n")
cat("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")
print(results_rounded, right = FALSE, row.names = FALSE)
cat("\nNote:\n")
cat("- All values are rounded to integers\n")
cat("- Calculations performed using the Hill equation\n")
cat("- x - confidence interval width in % relative to the mean\n")
sink()

# Create the plot
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Define the hill_curve function
hill_curve <- function(x, top, EC50, n) {
  top / (1 + (EC50/x)^n)
}

# 1. Prepare data for model curves
plot_data <- data.frame()
for (i in 1:nrow(model_summary)) {
  model <- model_summary[i, ]
  x_values <- seq(1, 100, length.out = 200)  # Limit x to 100
  y_values <- hill_curve(x_values, model$top, model$EC50, model$n)
  
  plot_data <- rbind(plot_data, data.frame(
    x = x_values,
    y = y_values,
    Model = model$Model,
    Equation = model$Equation
  ))
}

# 2. Prepare points from intervals70_90_95
points_data <- intervals70_90_95 %>%
  pivot_longer(
    cols = starts_with("norm_range"),
    names_to = "Model",
    values_to = "W"
  ) %>%
  mutate(
    Model = gsub("norm_range_", "", Model),
    n = row_num
  ) %>%
  filter(W <= 100)  # Filter points so that W does not exceed 100

# 3. Build the plot
ggplot() +
  # Model curves (semi-transparent for better point visibility)
  geom_line(data = plot_data, 
            aes(x = x, y = y, color = Model),
            linewidth = 1.0, alpha = 0.9) +
  
  # Data points (large with outline)
  geom_point(data = points_data,
             aes(x = W, y = n, fill = Model),
             size = 1.8, shape = 21, color = "black", stroke = 0.3) +
  
  # Limit X axis
  coord_cartesian(xlim = c(0, 100)) +
  
  # Styling options
  labs(
    title = "Hill curves with empirical data",
    subtitle = "Comparison of model and actual values",
    x = "Confidence interval width (W)",
    y = "Sample size (n)",
    color = "Model",
    fill = "Data"
  ) +
  scale_color_manual(
    values = c("70" = "darkblue", "90" = "darkgreen", "95" = "darkred"),
    labels = c("70% CI", "90% CI", "95% CI")
  ) +
  scale_fill_manual(
    values = c("70" = "darkblue", "90" = "darkgreen", "95" = "darkred"),
    labels = c("70% CI", "90% CI", "95% CI"),
    guide = "none"  # Hide duplicate legend
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank()
  ) +
  # Vertical reference lines
  geom_vline(xintercept = seq(20, 80, by = 20), 
             linetype = "dotted", 
             color = "gray60",
             alpha = 0.5)

# Save the plot
ggsave("combined_hill_plot.png", width = 11, height = 9, dpi = 300, bg = "white")

# Print the result
print(model_summary)
print(results_rounded)

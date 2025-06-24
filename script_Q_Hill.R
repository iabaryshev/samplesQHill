# В рабочей директории должен быть файл sample.csv , в котором выборка идет столбцом. 
# При этом первая ячейка – название столбца «x», знак десятичной дроби – точка.
# Основной результат скрипта - это расчёт количества проб которые необходимо 
# собрать и обработать для получения результата с известной точностью.
# Результат записывается в файл в рабочую директорию как "Number of sumples.txt"
# The working directory must contain a file sample.csv with data in a single column
# The first cell should be column name "x", with decimal point as separator
# The main script output is calculating the number of samples required
# to obtain results with known precision
# Results are saved to "Number of samples.txt" in the working directory

# Шаг 1: Загрузка таблицы из файла sample.csv
# Step 1: Load data from sample.csv
data <- read.csv("sample.csv")

# Шаг 2: Вычисление среднего значения от data
# Step 2: Calculate mean of data
a <- mean(data[[1]], na.rm = TRUE)  # Если data - это датафрейм с одним столбцом
# If data is a single-column dataframe

# Шаг 3: Создание таблицы с 1000 перемешанными столбцами
# Step 3: Create table with 1000 shuffled columns
n_rows <- nrow(data)  # Количество строк в исходных данных
# Number of rows in original data
n_cols <- 1000        # Количество новых столбцов
# Number of new columns
# Создаем новую таблицу (каждый столбец - случайная перестановка исходных данных)
# Create new table (each column is random permutation of original data)
random_data <- as.data.frame(
  replicate(n_cols, sample(data[[1]], size = n_rows, replace = FALSE))
)
# Даем столбцам имена col_1, col_2, ..., col_1000
# Name columns as col_1, col_2, ..., col_1000
colnames(random_data) <- paste0("col_", 1:n_cols)

# Шаг 4: Создание таблицы накопления средних
# Step 4: Create cumulative means table
# Создаем таблицу с накопленными средними
# Create table with cumulative means
cumulative_means <- as.data.frame(
  lapply(random_data, function(col) {
    cumsum(col) / seq_along(col)
  })
)

# Сохраняем имена столбцов
# Keep column names
colnames(cumulative_means) <- colnames(random_data)

# Просмотр первых строк результата
# View first rows of results
# head(cumulative_means)

# Шаг5 Расчет перцентилей для трех уровней значимости
# Step 5: Calculate percentiles for three confidence levels
# В качестве аргумента k были указаны значения 0.15 (нижняя граница) и 0.85 (верхняя граница) для 70% вероятности; 
# Using k values 0.15 (lower bound) and 0.85 (upper bound) for 70% probability
# 0.05 и 0.95 для 90% вероятности; 0.025 и 0.975 для 95% вероятности. 
# 0.05 and 0.95 for 90% probability; 0.025 and 0.975 for 95% probability

# для 70% вероятности
# for 70% probability
# Создаем таблицу percentil70 с перцентилями
# Create percentil70 table with percentiles
percentil70 <- data.frame(
  p15 = apply(cumulative_means, 1, quantile, probs = 0.15, na.rm = TRUE),
  p85 = apply(cumulative_means, 1, quantile, probs = 0.85, na.rm = TRUE)
)
# Добавляем новый столбец с расчетом (p85 - p15)/a
# Add new column with (p85 - p15)/a calculation
percentil70$normalized_range <- (percentil70$p85 - percentil70$p15) / a * 100


# для 90% вероятности
# for 90% probability
# Создаем таблицу percentil90 с перцентилями
# Create percentil90 table with percentiles
percentil90 <- data.frame(
  p05 = apply(cumulative_means, 1, quantile, probs = 0.05, na.rm = TRUE),
  p95 = apply(cumulative_means, 1, quantile, probs = 0.95, na.rm = TRUE)
)
# Добавляем новый столбец с расчетом (p95 - p05)/a
# Add new column with (p95 - p05)/a calculation
percentil90$normalized_range <- (percentil90$p95 - percentil90$p05) / a * 100

# для 95% вероятности
# for 95% probability
# Создаем таблицу percentil90 с перцентилями
# Create percentil95 table with percentiles
percentil95 <- data.frame(
  p025 = apply(cumulative_means, 1, quantile, probs = 0.025, na.rm = TRUE),
  p975 = apply(cumulative_means, 1, quantile, probs = 0.975, na.rm = TRUE)
)
# Добавляем новый столбец с расчетом (p975 - p025)/a
# Add new column with (p975 - p025)/a calculation
percentil95$normalized_range <- (percentil95$p975 - percentil95$p025) / a *100

# Создаем объединенную таблицу с шириной интервалов
# Create combined table with interval widths
intervals70_90_95 <- data.frame(
  row_num = 1:nrow(percentil70),  # Номера строк
  # Row numbers
  
  # Из таблицы percentil70 берем последний столбец
  # Take last column from percentil70
  norm_range_70 = percentil70[[ncol(percentil70)]],
  
  # Из таблицы percentil90 берем последний столбец
  # Take last column from percentil90
  norm_range_90 = percentil90[[ncol(percentil90)]],
  
  # Предполагая, что у нас есть аналогичная percentil95
  # Assuming we have similar percentil95
  norm_range_95 = percentil95[[ncol(percentil95)]]
)

#Удаляем 5 первых строк!!!
# Remove first 5 rows!!!
intervals70_90_95 <- intervals70_90_95[-(1:5), ]

# Проверяем структуру
# Check structure
str(intervals70_90_95)
# Выводим первые 10 строк
# Show first 10 rows
head(intervals70_90_95, 10)

# [Previous code remains unchanged until "Далее собственно регрессия"]

#
# Далее собственно регрессия (модифицированная версия)
# Actual regression (modified version)
#

library(ggplot2)
library(dplyr)

# Функция для подбора уравнения Хилла с 3 параметрами
# Function for 3-parameter Hill equation fitting
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

# Функция для расчета R²
# Function for R² calculation
calculate_r_squared <- function(model, x, y) {
  predicted <- predict(model, newdata = data.frame(x = x))
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - predicted)^2)
  1 - (ss_residual/ss_total)
}

# Подготовка данных
# Data preparation
data <- intervals70_90_95
n_rows <- nrow(data)

# Создаем список для хранения моделей и их параметров
# Create list to store models and their parameters
results <- list()

# Анализ для каждого доверительного интервала
# Analysis for each confidence interval
for (col in c("norm_range_70", "norm_range_90", "norm_range_95")) {
  # Фильтруем данные (убираем NA если есть)
  # Filter data (remove NA if present)
  clean_data <- data.frame(
    x = data[[col]],
    y = data$row_num
  ) %>% na.omit()
  
  # Подгоняем модель
  # Fit model
  model <- fit_hill_3param(clean_data$x, clean_data$y)
  
  # Рассчитываем R² и AIC
  # Calculate R² and AIC
  r_squared <- calculate_r_squared(model, clean_data$x, clean_data$y)
  aic_value <- AIC(model)
  
  # Сохраняем результаты
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

# Создаем таблицу с параметрами всех моделей
# Create table with all model parameters
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

# Сохраняем параметры моделей в файл
# Save model parameters to file
write.table(
  model_summary,
  file = "hill_models_summary.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

# Красиво выводим таблицу в консоль
# Print formatted table to console
cat("\nРезультаты моделирования:\n")
cat("Modeling results:\n")
cat("-----------------------------------------\n")
print(model_summary)
cat("-----------------------------------------\n")

# Сохраняем таблицу в хорошо оформленный текстовый файл
# Save table to well-formatted text file
sink("hill_models_summary_formatted.txt")

cat("Параметры моделей Хилла с коэффициентами детерминации и AIC\n")
cat("Hill model parameters with R-squared and AIC\n")
cat("=========================================\n\n")

for(i in 1:nrow(model_summary)) {
  cat("Модель: ", model_summary$Model[i], "\n")
  cat("Model: ", model_summary$Model[i], "\n")
  cat("-----------------------------------------\n")
  cat("Верхняя асимптота (top):    ", round(model_summary$top[i], 4), "\n")
  cat("Upper asymptote (top):      ", round(model_summary$top[i], 4), "\n")
  cat("Точка полунасыщения (EC50): ", round(model_summary$EC50[i], 4), "\n")
  cat("Half-saturation point (EC50):", round(model_summary$EC50[i], 4), "\n")
  cat("Коэффициент наклона (n):   ", round(model_summary$n[i], 4), "\n")
  cat("Slope coefficient (n):     ", round(model_summary$n[i], 4), "\n")
  cat("R-квадрат:                 ", round(model_summary$R_squared[i], 4), "\n")
  cat("R-squared:                ", round(model_summary$R_squared[i], 4), "\n")
  cat("AIC:                       ", round(model_summary$AIC[i], 2), "\n")
  cat("AIC:                      ", round(model_summary$AIC[i], 2), "\n")
  cat("Уравнение модели:          ", model_summary$Equation[i], "\n\n")
  cat("Model equation:           ", model_summary$Equation[i], "\n\n")
}

cat("Примечание:\n")
cat("Notes:\n")
cat("- top: максимальное значение y (верхняя асимптота)\n")
cat("- top: maximum y value (upper asymptote)\n")
cat("- EC50: значение x, при котором достигается половина от top\n")
cat("- EC50: x value where half of top is reached\n")
cat("- n: коэффициент наклона кривой\n")
cat("- n: curve slope coefficient\n")
cat("- R-квадрат: доля объясненной дисперсии (0-1)\n")
cat("- R-squared: explained variance proportion (0-1)\n")
cat("- AIC: информационный критерий Акаике (чем меньше, тем лучше)\n")
cat("- AIC: Akaike Information Criterion (lower is better)\n")

sink()

# [Rest of the code remains unchanged]

# Показываем сообщение о сохранении
# Show save message
cat("\nТаблица параметров сохранена в файлы:\n")
cat("Parameter table saved to files:\n")
cat("- hill_models_summary.txt (табличный формат)\n")
cat("- hill_models_summary.txt (table format)\n")
cat("- hill_models_summary_formatted.txt (форматированный вариант)\n")
cat("- hill_models_summary_formatted.txt (formatted version)\n")

#
# Осталось расчитать значения количество проб для нужных дов интервалов и нарисовать график
# Need to calculate number of samples for required confidence intervals and plot graph
#

# Определяем количество проб
# Determine number of samples
# Создаем models из model_summary
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

# Функция для расчета y
# Function to calculate y
calculate_y <- function(model, x) {
  with(as.list(model$coefficients), {
    top / (1 + (EC50/x)^n)
  })
}

# Рассчитываем значения для x = 20, 30, 40, 50, 60
# Calculate values for x = 20, 30, 40, 50, 60
x_values <- c(20, 30, 40, 50, 60)
results <- sapply(models, function(model) {
  round(calculate_y(model, x_values), 2)
})
rownames(results) <- paste0("x=", x_values)

# Округляем все значения до целых
# Round all values to integers
results_rounded <- round(results)

# Выводим результат
# Print results
print(results_rounded)

# Сохраняем таблицу в txt файл
# Save table to txt file
sink("Number of sumples.txt")
cat("Результаты расчетов (округленные до целых)\n")
cat("Calculation results (rounded to integers)\n")
cat("=========================================\n")
cat("Дата: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")
cat("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")
print(results_rounded, right = FALSE, row.names = FALSE)
cat("\nПримечание:\n")
cat("\nNotes:\n")
cat("- Все значения округлены до целых чисел\n")
cat("- All values rounded to integers\n")
cat("- Расчеты выполнены по уравнению Хилла\n")
cat("- Calculations based on Hill equation\n")
cat("- x - ширина доверительного интервала в % с среднему\n")
cat("- x - confidence interval width as % of mean\n")
sink()

# Создаем график
# Create plot
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Определяем функцию hill_curve
# 1. Define hill_curve function
hill_curve <- function(x, top, EC50, n) {
  top / (1 + (EC50/x)^n)
}

# 1. Подготовка данных для кривых моделей
# 1. Prepare data for model curves
plot_data <- data.frame()
for (i in 1:nrow(model_summary)) {
  model <- model_summary[i, ]
  x_values <- seq(1, 100, length.out = 200)  # Ограничиваем x до 100
  # Limit x to 100
  y_values <- hill_curve(x_values, model$top, model$EC50, model$n)
  
  plot_data <- rbind(plot_data, data.frame(
    x = x_values,
    y = y_values,
    Model = model$Model,
    Equation = model$Equation
  ))
}

# 2. Подготовка точек из intervals70_90_95
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
  filter(W <= 100)  # Фильтруем точки, чтобы W не превышало 100
# Filter points where W doesn't exceed 100

# 3. Построение графика
# 3. Plot creation

ggplot() +
  # Кривые моделей
  geom_line(data = plot_data, 
            aes(x = x, y = y, color = Model),
            linewidth = 1.2, alpha = 0.8) +
  
  # Точки из данных - используем ТОЛЬКО color для единообразия
  geom_point(data = points_data,
             aes(x = W, y = n, color = Model),
             size = 2.0, alpha = 0.6) +
  
  coord_cartesian(xlim = c(0, 100)) +
  
  labs(
    title = "Hill curves with empirical data",
    subtitle = "Comparison of model and actual values",
    x = "Confidence interval width (W)", 
    y = "Number of Samples (n)",
    color = "Confidence Level"  # Единое название для легенды
  ) +
  scale_color_manual(
    values = c("70" = "#FF6B6B", "90" = "#4ECDC4", "95" = "#3A86FF"),
    labels = c("70% CI", "90% CI", "95% CI")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank()
  ) +
  geom_vline(xintercept = seq(20, 80, by = 20), 
             linetype = "dotted", 
             color = "gray60",
             alpha = 0.5)

# Сохраняем график
# Save plot
ggsave("combined_hill_plot.png", width = 11, height = 7, dpi = 300, bg = "white")

# Выводим результат
# Print results
print(model_summary)
print(results_rounded)

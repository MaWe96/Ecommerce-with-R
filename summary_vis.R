library(tidyverse)

source("script1.R")

summarize_orders <- function(data, group_var, value_var) {
  data %>%
    group_by({{group_var}}) %>%
    summarise(
      avg = mean({{value_var}}, na.rm = TRUE),
      median = median({{value_var}}, na.rm = TRUE),
      sd = sd({{value_var}}, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
}

plot_boxplot <- function(data, group_var, value_var) {
  ggplot(data, aes(x = {{group_var}}, y = {{value_var}})) +
    geom_boxplot() +
    labs(
      title = "Distibution",
      x = deparse(substitute(group_var)),
      y = deparse(substitute(value_var))
    )
}


ggplot(df_clean, aes(x = customer_segment)) +
  geom_bar() +
  labs(
    title = "Antal kunder per segment",
    x = "Segment",
    y = "Antal kunder"
  )

plot_avg_bar <- function(data, group_var, value_var) {
  data %>%
    group_by({{group_var}}) %>%
    summarise(
      avg_value = mean({{value_var}}, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = {{group_var}}, y = avg_value)) +
    geom_col() +
    labs(
      titel = paste("Average", deparse(substitute(group_var)), "by", deparse(substitute(value_var))),
      x = deparse(substitute(group_var)),
      y = paste("Average", deparse(substitute(value_var)))
    )
}

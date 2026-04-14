# nr 3 Försäljning över tid: totalt, regionalt och kundtyp?

library(tidyverse)

source("script1.R")

df_clean <- df_clean %>% 
  mutate(
    order_month = as.Date(format(order_date, "%Y-%m-01"))
  )


ordermonth <- df_clean %>%
  group_by(order_month) %>%
  summarise(
    total_sales = sum(discounted_orderval, na.rm = TRUE),
    orders = n()
  )

monthregion <- df_clean %>%
  group_by(order_month, region) %>%
  summarise(
    sales = sum(discounted_orderval, na.rm = TRUE),
    amount = n(),
    .groups = "drop_last"
  )

monthcustomer <- df_clean %>%
  group_by(order_month, customer_type) %>%
  summarise(
    sales = sum(discounted_orderval, na.rm = TRUE),
    amount = n(),
    .groups = "drop_last"
  )

# total (ordermonth)
plot1 <- ggplot(
  df_clean %>%
    group_by(order_month) %>%
    summarise(sales = sum(discounted_orderval, na.rm = TRUE), .groups = "drop_last"),
  aes(x = order_month, y = sales)
) +
  geom_line() +
  labs(
    title = "Försäljning över tid per region",
    x = "Månad",
    y = "Försäljning"
  )


plot2 <- ggplot(
  df_clean %>%
    group_by(order_month, region) %>%
    summarise(sales = sum(discounted_orderval, na.rm = TRUE), .groups = "drop_last"),
  aes(x = order_month, y = sales, color = region)
) +
  geom_line() +
  labs(
    title = "Försäljning över tid per region",
    x = "Månad",
    y = "Försäljning"
  )

plot3 <- ggplot(
  df_clean %>%
    group_by(order_month, customer_type) %>%
    summarise(sales = sum(discounted_orderval, na.rm = TRUE), .groups = "drop_last"),
  aes(x = order_month, y = sales, color = customer_type)
) +
  geom_line() +
  labs(
    title = "Försäljning över tid per kundtyp",
    x = "Månad",
    y = "Försäljning"
  )

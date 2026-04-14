library(tidyverse)
df <- read_csv("ecommerce_orders.csv")
df_clean <- df %>% 
  mutate(
    order_id = str_replace(order_id, "O", ""),
    order_id = as.integer(order_id),
    
    order_date = as.Date(order_date),
    
    customer_id = str_replace(customer_id, "C", ""),
    customer_id = as.integer(customer_id),
    
    customer_segment = str_trim(customer_segment),
    customer_type = str_trim(customer_type),
    
    region = str_trim(region),
    
    city = str_trim(city),
    city = str_to_lower(city),
    city = str_replace(city, "malmo", "malmö"),
    city = str_replace(city, "gothenburg", "göteborg"),
    city = str_replace(city, "orebro", "örebro"),
    city = str_replace(city, "vasteras", "västerås"),
    city = str_replace(city, "linkoping", "linköping"),
    city = str_replace(city, "boras", "borås"),
    city = str_replace(city, "norrkoping", "norrköping"),
    city = str_to_title(city),
    
    product_category = str_trim(product_category),
    product_subcategory = str_trim(product_subcategory),
    
    payment_method = str_trim(payment_method),
    payment_method = str_to_title(payment_method),
    
    campaign_source = str_trim(campaign_source),
    campaign_source = str_to_title(campaign_source),
    
    quantity = as.integer(quantity),
    shipping_days = as.integer(shipping_days),
    
    
    discount_pct = replace_na(discount_pct, 0),
    
    returned = case_when(
      returned == "Yes" ~ TRUE,
      returned == "No" ~FALSE
    ),
    
    # Nya variabler
    discounted_price = unit_price * (1-discount_pct /100),
    discounted_orderval = quantity * discounted_price
    
  )

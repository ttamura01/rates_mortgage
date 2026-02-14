setwd("/Users/takayukitamura/Documents/R_Computing/rates_mortgage")
# Install/load required packages
library(tidyverse)
library(lubridate)
library(scales)
library(quantmod)   # for Fed Funds
library(fredr) 

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

treasury_10y <- fredr(series_id = "DGS10") %>% 
  select(date, "long_term_yield" = value) 
  # filter(date >= "2023-01-01")

head(treasury_10y)
tail(treasury_10y)

# treasury_10y <- treasury_10y[-708,]

# updates <- tribble(~date, ~long_term_yield,
#                    "2026-02-12", 4.104)

updates$date <- as.Date(updates$date)

treasury_10y <- rbind(treasury_10y, updates)

# treasury_10y <- treasury_10y %>% mutate(long_term_yield = if_else(date == "2025-09-25", 4.168, long_term_yield))

mortgage_30y <- fredr(series_id = "MORTGAGE30US") %>% 
  select(date, "mortgage_rate" = value) #%>% 
  # filter(date >= "2023-01-01") 

tail(mortgage_30y)

# updates <- tribble(~date, ~mortgage_rate,
#                    "2025-10-23", 6.26)

updates$date <- as.Date(updates$date)

mortgage_30y <- rbind(mortgage_30y, updates)
mortgage_30y <- mortgage_30y[4848,]

head(mortgage_30y)
tail(mortgage_30y)

ds_10y_30y <- treasury_10y %>% 
  left_join(mortgage_30y, by = "date") 

tail(ds_10y_30y)

write_csv(ds_10y_30y,"data/us_10y_30y.csv")

# 
# read_csv("us_10y_30y.csv")

# dgs10_weekly <- treasury_10y %>% 
#   mutate(week = floor_date(date, "week", week_start = 7)) %>% 
#   group_by(week) %>% 
#   summarise(dgs10_wk = mean(long_term_yield, na.rm = TRUE )) %>% 
#   ungroup() 

# ggplot() +
#   geom_line(data = treasury_10y, aes(x = date, y = long_term_yield), color = "blue") +
#   geom_line(data = mortgage_30y, aes(x = date, y = mortgage_rate), color = "red") +
#   geom_text(aes(x = as.Date("2025-09-18"), 
#                 y = c(4, 6.2),
#                 label = c("4.1%", "6.26%"),
#                 colour = c("red", "blue"))) +
#   scale_y_continuous(limits = c(3.2, 8),
#    breaks = seq(4, 8, 2),
#    labels = label_comma(accuracy = 0.1)) 
   
ds_10y_30y <- ds_10y_30y %>%
  arrange(date) %>%                        # make sure data is in chronological order
  mutate(
    long_term_yield = zoo::na.locf(long_term_yield, na.rm = FALSE),
    mortgage_rate   = zoo::na.locf(mortgage_rate,   na.rm = FALSE)
  )

ds_10y_30y$latest_data <- if_else(ds_10y_30y$date == max(ds_10y_30y$date), TRUE, FALSE) 

latest_data <- ds_10y_30y %>% slice_tail(n = 1)
latest_date <- latest_data$date    
latest_10_y <- latest_data$long_term_yield 
latest_mortgage_rate <- latest_data$mortgage_rate


ds_10y_30y %>% 
  na.omit() %>% 
  filter(date >= "2022-01-01") %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = long_term_yield, colour = "10-Year Yield"), color = "#0136b2") +
  geom_line(aes(y = mortgage_rate, colour = "30-Year Mortgage"), color = "#f85033") +
  geom_vline(xintercept = as.Date("2025-09-17"), linetype = "dashed", colour = "black") +
  annotate("text",
           x = as.Date("2025-09-17"),
           y = 7.75,
           label = "Rate-cut\n(9/17/25)", size = 5, colour = "black", fontface = "bold",
           hjust = 0.5, vjust = 0.85) +
  annotate("text",
           x = latest_date,
           y = (latest_10_y) * 0.965,
           label = paste0("10-Year Treasury: ", latest_10_y, "%"),
           color = "#0136b2",
           size = 5,
           hjust = 0.85, vjust = 1) +
  annotate("text",
           x = latest_date,
           y = (latest_mortgage_rate) * 0.975,
           label = paste0("30-Year Mortgage: ", latest_mortgage_rate, "%"),
           color = "#f85033",
           size = 5,
           hjust = 0.85, vjust = 1) +
  coord_cartesian(clip = "off", expand = TRUE) +
  scale_y_continuous(limits = c(NA, NA),
                     labels = label_comma(accuracy = 0.1)) + 
  labs(title = "10-Year Treasury Yeild and 30-Year Mortgage Rate",
       caption = "source: FRED, WSJ, by Takayuki Tamura",
       x = NULL,
       y = "percentage") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "gray80"),
    panel.grid.minor.y = element_line(colour = "gray90", linewidth = 0.3, linetype = "dashed"))
  
ggsave("figures/us_30y_10y.png", height = 6, width = 6)  





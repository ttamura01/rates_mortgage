setwd("/Users/takayukitamura/Documents/R_Computing/rates_mortgage")

# Install/load required packages
library(tidyverse)
library(lubridate)
library(scales)
library(quantmod)   # for Fed Funds
library(fredr) 
library(ggtext)

ds_10y_30y <-  read_csv("data/us_10y_30y.csv")

ds_10y_30y <- ds_10y_30y %>%
  arrange(date) %>%                        # make sure data is in chronological order
  mutate(
    long_term_yield = zoo::na.locf(long_term_yield, na.rm = FALSE),
    mortgage_rate   = zoo::na.locf(mortgage_rate,   na.rm = FALSE),
    spread = mortgage_rate - long_term_yield
  )

ds_10y_30y$latest_data <- if_else(ds_10y_30y$date == max(ds_10y_30y$date), TRUE, FALSE) 

latest_data <- ds_10y_30y %>% slice_tail(n = 1)
latest_date <- latest_data$date    
latest_10_y <- latest_data$long_term_yield 
latest_mortgage_rate <- latest_data$mortgage_rate
latest_spread <- latest_data$spread

##historical yields and spread
ds_10y_30y %>% 
  filter(date >= "2022-01-01") %>% 
  mutate(spread = mortgage_rate - long_term_yield) %>% 
  ggplot(aes(x = date)) +
  # geom_line(show.legend = FALSE) +
  geom_line(aes(y = long_term_yield, colour = "10-Year Yield"), color = "#0136b2") +
  geom_line(aes(y = mortgage_rate, colour = "30-Year Mortgage"), color = "#f85033") +
  geom_line(aes(y = spread, colour = "Risk Premium"), color = "#1e8932") +
  geom_vline(xintercept = as.Date("2025-09-17"), linetype = "dashed", colour = "black") +
  annotate("text",
           x = as.Date("2025-09-17"),
           y = 7.75,
           label = "Rate-cut\n(9/17/25)", size = 5, colour = "black", fontface = "bold",
           hjust = 0.5, vjust = 0.85) +
  annotate("text",
           x = latest_date,
           y = (latest_10_y) * 0.925,
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
  annotate("text",
           x = latest_date,
           y = (latest_spread) * 0.9,
           label = paste0("Risk Premium: ", latest_spread, "%"),
           color = "#1e8932",
           size = 5,
           hjust = 0.85, vjust = 1) +
  coord_cartesian(clip = "off", expand = TRUE) +
  scale_y_continuous(limits = c(NA, NA),
                     labels = label_comma(accuracy = 0.1)) + 
  labs(title = "Treasury Yeild, Mortgage Rate and Spread",
       caption = "source: FRED, WSJ, by Takayuki Tamura",
       x = NULL,
       y = "Yield & Spread (%)") +
  theme(
    legend.position = "none",
    
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "gray80"),
    panel.grid.minor.y = element_line(colour = "gray90", linewidth = 0.3, linetype = "dashed"),
    plot.margin = margin_auto(10, 10))

ggsave("tnote_mortgage_spread.png", width = 6, height = 6)

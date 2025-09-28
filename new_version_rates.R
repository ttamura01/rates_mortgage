setwd("/Users/takayukitamura/Documents/R_Computing/rates_mortgage")
library(tidyverse)
library(scales)
library(ggtext)
library(glue)

rates_m30  <- read.csv("data/us_10y_30y.csv") %>% 
  rename(treasury10 = long_term_yield, mortgage30 = mortgage_rate)

rates_m30 <- rates_m30[-c(2786:2840), ]

df_rates <- rates_m30 %>% 
  mutate(rate_change_wow = c(NA, diff(treasury10)),
         rate_change_status = if_else(is.na(rate_change_wow), NA_character_,
                                      if_else(rate_change_wow > 0, "rose",
                                              if_else(rate_change_wow < 0, "fell", "flat"))),
         mortgage_change_wow = c(NA, diff(mortgage30)),
         mortgage_change_status = if_else(is.na(mortgage_change_wow), NA_character_,
                                          if_else(mortgage_change_wow > 0, "rose",
                                                  if_else(mortgage_change_wow < 0, "fell", "flat"))),
         latest_week = if_else(date == max(date), TRUE, FALSE)) %>% 
  mutate(date = as.Date(date))


latest_data <- df_rates %>% 
  filter(latest_week == TRUE)

latest_date <- latest_data$date

latest_treasury_rate <- latest_data$treasury10

latest_mortgage_rate <- latest_data$mortgage30

latest_treasury_rate_change <- latest_data$rate_change_wow

latest_treasury_rate_status <- latest_data$rate_change_status

latest_mortgage_rate_change <- latest_data$mortgage_change_wow

latest_mortgage_rate_status <- latest_data$mortgage_change_status

df_rates %>% 
  select(date, long_term_rate = treasury10, mortgage_rate = mortgage30) %>% 
  pivot_longer(cols = -date, names_to = "rates", values_to = "percentage") %>% 
  filter(date >= "2025-01-01") %>% 
  ggplot(aes(x = date, y = percentage, color = rates)) +
  geom_line(show.legend = FALSE) +
  annotate("label",
           x = latest_date,
           y = c(latest_mortgage_rate, latest_treasury_rate),
           label = c(latest_mortgage_rate, latest_treasury_rate),
           color = c("red", "blue"),
           vjust = -0.75,
           hjust = c(1, 1)
  ) +
  # annotate("label",
  #          x = as.Date("2018-09-18"),
  #          y = c(latest_mortgage_rate, latest_treasury_rate),
  #          label = c(latest_mortgage_rate, latest_treasury_rate),
  #          color = c("red", "blue"),
  #          vjust = -0.75,
  #          hjust = c(1, 1)
  # ) +
  annotate("label",
           x = as.Date("2025-07-31"),
           y = c(7.0, 4.75),
           label = c("30-Year Mortgage Rate",
                     "10-Year Treasury Yield"),
           color = c("red", "blue")
           ) +
  scale_y_continuous(limits = c(3.5, 7.25),
                     labels = label_comma(accuracy = 0.1)) +
  scale_color_manual(breaks = c("long_term_rate", "mortgage_rate"),
                     values = c("blue", "red")) +
  labs(x = NULL,
       y = "Interest Rate(%)",
       caption = "Freddie Mac, FRED, by Takayuuki Tamura",
       title = glue("Average 30_Year mortgage rate {latest_mortgage_rate_status} to {latest_mortgage_rate}% in this week"),
       subtitle =
       "-Reflecting heightening market expectations for Fed's rate cuts, 99.7% of markets expectiong 25bps of cut on Sep 17 and 45% expecting the Fed rate down to 3.5-3.75% or .75% cuts toward end-2025") +
  theme_classic() +
  theme(
    legend.key = element_blank(),
    legend.title = element_blank(),
    panel.grid.major = element_line(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(face = "bold", size = 16, margin = margin(t = 6, b = 1)),
    plot.subtitle = element_textbox_simple(face = "italic", size = 12)
  )

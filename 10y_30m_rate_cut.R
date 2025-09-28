setwd("/Users/takayukitamura/Documents/R_Computing/rates_mortgage")
library(tidyverse)
library(scales)
library(ggtext)
library(glue)

rates_m30 <- read.csv("data/us_10y_30y.csv") %>% 
  rename("treasury10" = long_term_yield, "mortgage30" = mortgage_rate) %>% 
  na.omit()

# rates_m30[2834,2] <- 4.57
tail(rates_m30)

# remove the wrong entries
rates_m30 <- rates_m30[-2835, ]
# 
# rates_m30[-2828,]

sapply(rates_m30, class)

# update the wrong data with correct numbers

# rates_m30 <- rates_m30 %>%
#   mutate(treasury10 = ifelse(date == "2024-12-26", 4.58, treasury10))
# 
updates <- tribble(~date, ~mortgage30, ~treasury10,
                   "2025-09-05", 6.50, 4.08)
# 
rates_m30 <- rbind(rates_m30, updates)
# 
tail(rates_m30)
# 
write_csv(rates_m30, "/Users/takayukitamura/Documents/R_Computing/us_rates/data/us_10y_30y.csv")

rates_m30$date <- as.Date(rates_m30$date, format = "%Y-%m-%d")

#filtering with date

rates_m30 <- rates_m30 %>% 
  filter(date >= "2024-01-01")

rates_m30 %>% 
  pivot_longer(cols = -date, names_to = "rates", values_to = "percentage") %>% 
  ggplot(aes(x = date, y = percentage, color = rates)) +
  geom_line() +
  # annotate("text", x = as.Date("2024-09-18"), y = 7.04,
  #           label = "Sep-18,24", vjust = -0.5) +
  # annotate("text", x = as.Date("2024-09-18"), y = 7.5, label = "Rate-cut(9/18/24)",
  #          vjust = -0.5,hjust = 1, size = 5) +
  # geom_vline(xintercept = as.Date("2024-09-18"), linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(3.6, 7.4),
                     labels = label_comma(accuracy = 0.1)) +
  # scale_color_manual(breaks = c(mortgage30, treasury10),
  #                    values = c("steelblue", "green")) +
  labs(x = NULL,
       y = "Interest Rate(%)",
       caption = "Freddie Mac, FRED",
       title = "Average 30_Year mortgage rate slightly declined to 6.89% in this week") +
  theme_classic() +
  theme(
    legend.key = element_blank(),
    legend.title = element_blank(),
    panel.grid.major = element_line(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(face = "bold", size = 16, margin = margin(t = 6, b = 1))
  )

# Define scaling factors
min_treasury <- min(rates_m30$treasury10, na.rm = TRUE)
max_treasury <- max(rates_m30$treasury10, na.rm = TRUE)
min_mortgage <- min(rates_m30$mortgage30, na.rm = TRUE)
max_mortgage <- max(rates_m30$mortgage30, na.rm = TRUE)


# Transformatiokn function for secondary axis
trans_mortgage <- function(x) {
  ((x - min_mortgage) / (max_mortgage - min_mortgage)) * (max_treasury - min_treasury) + min_treasury
}

inv_trans_mortgage <- function(x) {
  ((x - min_treasury) / (max_treasury - min_treasury)) * (max_mortgage - min_mortgage) + min_mortgage
}

rates_m30 %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = treasury10, color = "10-Year Treasury Yield", linewidth = 0.1)) +
  geom_line(aes(y = trans_mortgage(mortgage30), color = "30-Year Mortgage Rate", linewidth = 0.1)) +
  # geom_vline(xintercept = as.Date("2024-09-18"), linetype = "dashed", color = "red") +
  # annotate("text", x = as.Date("2024-09-18"), y = 4.75, label = "Sep-18: Fed's rate-cut", vjust = -0.5, size = 5, fontface = "bold")+
  scale_y_continuous(
    name = "10-Year Treasury Yield (%)",
    limits = c(3.5, 5),
    sec.axis = sec_axis(~ inv_trans_mortgage(.), name = "30-Year Mortgage Rate (%)")
  ) +
  labs(
    title = glue("The 30-Year U.S. Mortgage Rates declined to {6.63}%, the lowest level in 2025, with heightening expectation for Fed's Rate cuts, after the WEAKER-than-expected Job-Data)",
    x = NULL, 
    color = "Rate Type",
    caption = "source: FRED(Federal Reserve Economic Data"
  )) +
  scale_color_manual(values = c("10-Year Treasury Yield" = "blue", "30-Year Mortgage Rate" = "red")) +
  theme_classic() +
  theme(
    axis.title.y.left = element_text(color = "blue", size = 16, face = "bold"),
    axis.title.y.right = element_text(color = "red", size = 16, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"), 
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 16, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line()
  )

ggsave("10y_30m_rate_cut.png", height = 6, width = 6)

##########################################################################################
#Flexible time span
rates_m30 <- read_csv("data/us_10y_30y.csv")

# rates_m30$date <- as.Date(rates_m30$date, format = "%Y-%m-%d")

rates_m30 <- rates_m30 %>% 
  filter(date > "2022-01-01") 

head(rates_m30)
tail(rates_m30)

# Define scaling factors
min_treasury <- min(rates_m30$treasury10)
max_treasury <- max(rates_m30$treasury10)
min_mortgage <- min(rates_m30$mortgage30)
max_mortgage <- max(rates_m30$mortgage30)

# Transformatiokn function for secondary axis
trans_mortgage <- function(x) {
  ((x - min_mortgage) / (max_mortgage - min_mortgage)) * (max_treasury - min_treasury) + min_treasury
}

inv_trans_mortgage <- function(x) {
  ((x - min_treasury) / (max_treasury - min_treasury)) * (max_mortgage - min_mortgage) + min_mortgage
}

rates_m30 %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = treasury10, color = "10-Year Treasury Yield")) +
  geom_line(aes(y = trans_mortgage(mortgage30), color = "30-Year Mortgage Rate", linewidth = 0.1)) +
  geom_vline(xintercept = as.Date("2024-09-18"), linetype = "dashed", color = "red") +
  annotate("text", x = as.Date("2024-09-18"), y = 4.75, label = "Sep-18: Fed's rate-cut", vjust = -0.5,hjust = 0.6, size = 6, fontface = "bold")+
  scale_y_continuous(
    name = "10-Year Treasury Yield (%)",
    limits = c(1.5, 5.25),
    sec.axis = sec_axis(~ inv_trans_mortgage(.), name = "30-Year Mortgage Rate (%)",
                        breaks = seq(3, 8, 0.5))
  ) +
  labs(
    title = "Bond Yield & Mortgage Rate both up, despite total 1.0% of Fed's Rate Cuts since Sep-18, 2024",
    x = NULL,
    color = "Rate Type",
    caption = "source: FRED(Federal Reserve Economic Data"
  ) +
  scale_color_manual(values = c("10-Year Treasury Yield" = "blue", "30-Year Mortgage Rate" = "red")) +
  theme_classic() +
  theme(
    axis.title.y.left = element_text(color = "blue", size = 16, face = "bold"),
    axis.title.y.right = element_text(color = "red", size = 16, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"), 
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 16, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line()
  )

rates_m30 %>% 
  select(date, mortgage30) %>% 
  ggplot(aes(x = date, y = mortgage30)) +
  geom_line()

rates_m30 %>% 
  select(date, treasury10) %>% 
  ggplot(aes(x = date, y = treasury10)) +
  geom_line()


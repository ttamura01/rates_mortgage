setwd("/Users/takayukitamura/Documents/R_Computing/rates_mortgage")
library(tidyverse)
library(glue)
library(ggtext)
library(patchwork)
library(scales)

# import data 
rates <- read.csv("data/us_10y_30y.csv") %>% 
  rename("treasury10" = long_term_yield, "mortgage30" = mortgage_rate) %>% 
  na.omit()

head(rates)
tail(rates)

# update the data 
# change the data observation 
rates[2840,] <- list(date = "2025-09-11", treasury10 = 4.03, mortgage30 = 6.35)

# eliminate data row 
rates <- rates[-2837, ]

# adding new row(s) 
updates <- tribble(~date, ~mortgage30, ~treasury10,
                   "2025-09-18", 6.26, 4.10)

rates <- rbind(rates, updates)
rates$date <- as.Date(rates$date, format = "%Y-%m-%d")
sapply(rates, class)
head(rates)
tail(rates)

rates

write_csv(rates, "/Users/takayukitamura/Documents/R_Computing/us_rates/data/us_10y_30y.csv")

average_rates <- rates %>% 
  # filter(date >= "2025-04-01" & date <= "2025-8-31") %>% 
  mutate(average_10y = mean(treasury10, na.rm = TRUE),
         average_m30 = mean(mortgage30, na.rm = TRUE))

# historical 30 year mortgage rates 

#### This is the final one! ####

# filtered_date <- as.Date("1971-01-01")
# filtered_date2 <- as.Date("2025-12-31")

# rates <- rates %>% 
  # select(date, treasury10, mortgage30) %>% 
  # filter(date >= filtered_date & date < filtered_date2)

annotation <- rates %>% 
  mutate(average_rate = round(mean(mortgage30), 2)) %>% 
  slice_max(date)

latest_date <- annotation$date
latest_10y <- annotation$treasury10
latest_30y <- annotation$mortgage30

annotaion0 <-  rates %>% 
  mutate(average_rate = round(mean(mortgage30), 2)) %>% 
  slice_min(date)
start_year <- year(annotaion0$date)

rates_longer <- rates %>% 
  pivot_longer(cols = -date, names_to = "class", values_to = "rates") 

ggplot(rates_longer, aes(x = date, y = rates, color = class)) +
  geom_line(show.legend = TRUE) +
  scale_color_manual(breaks = c("mortgage30", "treasury10"),
                     values = c("#D81B60", "#1E88E5")) +
  # geom_vline(xintercept = as.Date("2024-09-18"), linetype = "dashed", color = "red") +
  # annotate("text", x = as.Date("2024-09-18") - 15, y = 7.2, label = "30-Year mortgage rate",size = 8, vjust = -0.5, color = "red") +
  # annotate("text", x = as.Date("2024-09-18") - 15, y = 5, label = "10-Year treasury note", size= 8, vjust = -0.5, color = "blue") +
  annotate("text", x = latest_date, y = latest_10y,
           label = glue("{latest_10y}%"),
           color = c("#1E88E5"),
           size = 5,
           vjust = 1,
           hjust =0.25) +
  annotate("text", x = latest_date, y = latest_30y,
           label = glue("{latest_30y}%"),
           color = c("#D81B60"),
           size = 5,
           vjust = 1,
           hjust = 0.25) +
  # scale_x_continuous(limits = as.Date(c("2023-01-01", "2025-10-01"))) +
  scale_y_continuous(limits = c(NA, NA),
                     # breaks = seq(0, 20, 2),
                     labels = label_comma(accuracy = 0.1)) +
  labs(title = "10-Years Treasury Note Yield and 30-Years Mortgage Rates",
       x = NULL, y = "percentage",
       caption = "FRED(Federal Reserve Economic Data)") +
  coord_cartesian(clip = "off", expand = TRUE) +
  # scale_color_brewer(type = div, palette = "Set1")+
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 18),
    panel.background = element_blank(),
    text = element_text(face = "bold"), 
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.75, 0.75)
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/us_rates/us10_m30/figures/10y_note_30y_mortgage.png", width = 6, height = 4)

# t_note_sep <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DGS10&scale=left&cosd=2023-11-21&coed=2024-11-21&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-11-23&revision_date=2024-11-23&nd=1962-01-02") %>%
#   select(date =  DATE, yield = DGS10) 
# 
# non_numeric <- t_note_sep %>% filter(is.na(as.numeric(yield)))
# 
# t_note_sep <- t_note_sep %>% 
#   mutate(yield = as.numeric(yield))
# head(t_note_sep)
# tail(t_note_sep)

 
# updates <- tribble(~date, ~yield,
 #                    "2024-10-17", 4.10)
 # 
 # updates$date <- as.Date(updates$date)
 # 
 # t_note_sep$date = as.Date(t_note_sep$date, format = "%m/%d/%y")

 # updates$date <- as.Date(updates$date)
 # 
 # t_note_sep <- rbind(updates, t_note_sep)
 # 
 # head(t_note_sep)
 # tail(t_note_sep)
 # 
 # t_note_sep %>% 
 #   slice_min(yield)

# updates <- tribble(~date, ~yield,
#                    "2024-09-23", 3.77)


# t_note_sep <- rbind(updates, t_note_sep)

# t_note_sep %>% 
#   filter(date >= "2024-09-01") %>% 
#   ggplot(aes(x = date, y = yield)) +
#   geom_line() +
#   geom_vline(xintercept = as.Date("2024-09-18"), linetype = "dashed", color = "red") +
#   annotate("text", x = as.Date("2024-09-18"), y = 3.825, label = "Sep-18: Fed's rate-cut", vjust = -0.5)+
#   labs(title = "10-Year Treasury Note Yield up desptie the Fed's bigger rate-cut",
#        x = NULL,
#        y = "10-year treasury. bond yield",
#        caption = "Source: WSJ")+
#   theme(
#     plot.title.position = "plot",
#     plot.title = element_textbox_simple()
#   )
# ggsave("/Users/takayukitamura/Documents/R_Computing/us_rates/us10_m30/figures/us_10y.png", height = 4, width=5)

# p <- t_note_sep %>% 
#   ggplot(aes(x = date, y = yield)) +
#   geom_line() +
#   geom_vline(xintercept = as.Date("2024-09-18"))
# 
# p + geom_vline(xintercept = 2024-09-18)

rates %>% 
  ggplot(aes(x=date, y=mortgage30)) +
  geom_line() +
  geom_hline(yintercept = annotation$average_rate, linetype = "dashed", color = "blue") +
  # geom_text(aes(x=max(date), y=average_rate_rounded, label = glue("average: {average_rate_rounded}%")),
  #           vjust = -1, hjust = 1, color = "blue")+
  geom_text(data = annotation, 
            aes(x = date, 
                y = average_rate,
                label = glue("average:{average_rate}%")),
            vjust = -1, hjust = 1, color = "blue") +
  # scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, 5),
                     labels = label_comma(accuracy = 0.1)) +
  labs(title = glue("US 30-Year Fixed Rate Mortgage in the United States, since {start_year}"), 
       #The Average Rate is {average_rate_rounded}%"),
       # subtitle = glue("Average rate over 50 years is {average_rate_rounded}%"),
       caption = "Source: FRED(Federal Reserve Economic Data",
       y = "30-Year Fixed Rate(%)",
       x = NULL) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple()
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/us_rates/us10_m30/figures/30_year_mortgage.png", height = 4, width = 6)

## Regression Analysis 

####
latest_data <- tail(rates,1)

# rates$latest <- ifelse(rates$date==max(rates$date), TRUE, FALSE)
latest_TB_rate <- tail(rates$treasury10,1)
# latest_TB_rate <- 4.2
latest_mortgage_rate <- tail(rates$mortgage30,1)

#Linear regression model
lm_model <- lm(mortgage30~treasury10, rates)

#Coefficients
coefficients <- coef(lm_model)
intercept <- coefficients[1]
slope <- coefficients[2]

intercept_rounded <- round(intercept,2)
slope_rounded <- round(slope,2)

##implied mortgage rate
theoritical_mortgage_rate <- slope*latest_TB_rate + intercept
theoritical_data <- tribble(~mortgage30, ~treasury10,
                            theoritical_mortgage_rate, latest_TB_rate)

theoritical_mortgage_rate_rounded <- round(theoritical_mortgage_rate,2)

#R-squared value
r2 <- summary(lm_model)$r.squared
r2_rounded <- round(r2,2)

#start_year
start_year <- rates %>% 
  head(1) %>% 
  separate(date, into = c("year", "month", "day")) %>% 
  select(year)

rates %>% 
  ggplot(aes(x = treasury10, y = mortgage30)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "blue") +
  geom_vline(xintercept = annotation$treasury10, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = annotation$mortgage30, linetype = "dashed") +
  geom_hline(yintercept = slope*annotation$treasury10 + intercept, linetype = "dashed", 
             color = "red" ) +
  geom_point(data = annotation, aes(x = treasury10, y = mortgage30), color = "green", 
             size = 3) +
  geom_point(data = theoritical_data, aes(x= treasury10,theoritical_mortgage_rate), 
             color = "red",
             size=3) +
  annotate("text", x = latest_data$treasury10, y = theoritical_mortgage_rate, 
           label = glue("TheoriticalRate: {theoritical_mortgage_rate_rounded}%"), 
           hjust = -0.1, vjust = 1.25, color = "black", size = 5)  +
  labs(title = glue("Implied 30-Year Fixed Mortgage Rate is {theoritical_mortgage_rate_rounded}%,
                   vs. Current at {latest_data$mortgage30}%"),
       subtitle = glue("<i>(y = {slope_rounded}X + {intercept_rounded}, R2 = {r2_rounded}, based on regression analysis since {start_year})"),
       caption = "Source: FRED(Federal Reserve Economic Data by St.Louis FED), WSJ",
       x="10_Year Treasury Rate (%)",
       y="30-Year Fixed Mortgage Rate(%)") +
  theme(
  plot.title.position = "plot",
  plot.title = element_textbox_simple(),
  plot.subtitle = element_markdown()
  ) 
ggsave("/Users/takayukitamura/Documents/R_Computing/us_rates/us10_m30/figures/implied_30_year_mortgage.png", height = 5, width = 5)

rates %>% 
  ggplot(aes(x = treasury10, y = mortgage30)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "blue") +
  # geom_vline(xintercept = annotation$treasury10, linetype = "dashed", color = "blue") +
  # geom_hline(yintercept = annotation$mortgage30, linetype = "dashed") +
  # geom_hline(yintercept = slope*annotation$treasury10 + intercept, linetype = "dashed", 
  #            color = "red" ) +
  # geom_point(data = annotation, aes(x = treasury10, y = mortgage30), color = "green", 
  #            size = 3) +
  # geom_point(data = theoritical_data, aes(x= treasury10,theoritical_mortgage_rate), 
  #            color = "red",
  #            size=3) +
  # annotate("text", x = latest_data$treasury10, y = theoritical_mortgage_rate, 
  #          label = glue("TheoriticalRate: {theoritical_mortgage_rate_rounded}%"), 
  #          hjust = -0.1, vjust = 1.25, color = "black", size = 5)  +
  labs(title = glue("US 30-Year Fixed Mortgage Rate has very high histrical correlation to the US 10-Year Treasury Yield"),
       subtitle = glue("<i>(y = {slope_rounded}X + {intercept_rounded}, R2 = {r2_rounded}, based on regression analysis since {start_year})"),
       caption = "Source: FRED(Federal Reserve Economic Data by St.Louis FED), WSJ",
       x="10_Year Treasury Rate (%)",
       y="30-Year Fixed Mortgage Rate(%)") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    plot.subtitle = element_markdown()
  ) 
ggsave("/Users/takayukitamura/Documents/R_Computing/us_rates/us10_m30/figures/implied_30_year_mortgage.png", height = 5, width = 5)


#### This is the final one! ####

# filtered_date <- as.Date("2022-01-01")
# filtered_date2 <- as.Date("2024-12-31")
# 
# rates <- rates %>% 
#   select(date, treasury10, mortgage30) %>% 
#   filter(date >= filtered_date & date < filtered_date2)

annotation <- rates %>% 
  mutate(average_rate = round(mean(mortgage30), 2)) %>% 
  slice_max(date)

annotaion0 <-  rates %>% 
  mutate(average_rate = round(mean(mortgage30), 2)) %>% 
  slice_min(date)
start_year <- year(annotaion0$date)

rates_longer <- rates %>% 
  pivot_longer(cols = -date, names_to = "class", values_to = "rates") 

ggplot(rates_longer, aes(x = date, y = rates, color = class)) +
  geom_line(show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 18),
                     breaks = seq(0, 18, 5),
                     labels = label_comma(accuracy = 0.1)) +
  labs(title = "10-Years Treasury Note Yield and 30-Years Mortgage Rates",
       x = NULL, y = "percentage") +
  scale_color_brewer(type = div, palette = "Set1")+
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    text = element_text(face = "bold"), 
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank()
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/us_rates/us10_m30/figures/10y_note_30y_mortgage.png", width = 6, height = 4)

rates %>% 
  ggplot(aes(x=date, y=mortgage30)) +
  geom_line() +
  geom_hline(yintercept = annotation$average_rate, linetype = "dashed", color = "blue") +
  # geom_text(aes(x=max(date), y=average_rate_rounded, label = glue("average: {average_rate_rounded}%")),
  #           vjust = -1, hjust = 1, color = "blue")+
  geom_text(data = annotation, 
            aes(x = date, 
                y = average_rate,
                label = glue("average:{average_rate}%")),
            vjust = -1, hjust = 1, color = "blue") +
  # scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, 5),
                     labels = label_comma(accuracy = 0.1)) +
  labs(title = glue("US 30-Year Fixed Rate Mortgage in the United States, since {start_year}"), 
       #The Average Rate is {average_rate_rounded}%"),
       # subtitle = glue("Average rate over 50 years is {average_rate_rounded}%"),
       caption = "Source: FRED(Federal Reserve Economic Data",
       y = "30-Year Fixed Rate(%)",
       x = NULL) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple()
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/us_rates/us10_m30/figures/30_year_mortgage.png", height = 4, width = 6)

## Regression Analysis 

####
latest_data <- tail(rates,1)

# rates$latest <- ifelse(rates$date==max(rates$date), TRUE, FALSE)
latest_TB_rate <- tail(rates$treasury10,1)
# latest_TB_rate <- 4.2
latest_mortgage_rate <- tail(rates$mortgage30,1)

#Linear regression model
lm_model <- lm(mortgage30~treasury10, rates)

#Coefficients
coefficients <- coef(lm_model)
intercept <- coefficients[1]
slope <- coefficients[2]

intercept_rounded <- round(intercept,2)


slope_rounded <- round(slope,2)

##implied mortgage rate
theoritical_mortgage_rate <- slope*latest_TB_rate + intercept
theoritical_data <- tribble(~mortgage30, ~treasury10,
                            theoritical_mortgage_rate, latest_TB_rate)

theoritical_mortgage_rate_rounded <- round(theoritical_mortgage_rate,2)

#R-squared value
r2 <- summary(lm_model)$r.squared
r2_rounded <- round(r2,2)

#start_year
start_year <- rates %>% 
  head(1) %>% 
  separate(date, into = c("year", "month", "day")) %>% 
  select(year)

rates %>% 
  ggplot(aes(x = treasury10, y = mortgage30)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "blue") +
  geom_vline(xintercept = annotation$treasury10, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = annotation$mortgage30, linetype = "dashed") +
  geom_hline(yintercept = slope*annotation$treasury10 + intercept, linetype = "dashed", 
             color = "red" ) +
  geom_point(data = annotation, aes(x = treasury10, y = mortgage30), color = "green", 
             size = 3) +
  geom_point(data = theoritical_data, aes(x= treasury10,theoritical_mortgage_rate), 
             color = "red",
             size=3) +
  annotate("text", x = latest_data$treasury10, y = theoritical_mortgage_rate, 
           label = glue("Theoritical Rate: {theoritical_mortgage_rate_rounded}%", size =15,face = "bold"), 
           hjust = -0.1, vjust = 1.25, color = "black", size = 5)  +
  labs(title = glue("Implied 30-Year Fixed Mortgage Rate is {theoritical_mortgage_rate_rounded}%
                   vs. Current at {latest_data$mortgage30}%"),
       subtitle = glue("<i>(y = {slope_rounded}X + {intercept_rounded}, R2 = {r2_rounded}, based on regression analysis since {start_year})"),
       caption = "Source: FRED(Federal Reserve Economic Data by St.Louis FED), WSJ, by Takayuki Tamura",
       x="10_Year Treasury Rate (%)",
       y="30-Year Fixed Mortgage Rate(%)") +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 16, face = "bold"),
    plot.subtitle = element_markdown(),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line()
  ) 

ggsave("/Users/takayukitamura/Documents/R_Computing/us_rates/us10_m30/figures/implied_30_year_mortgage_2.png", height = 5, width = 5)


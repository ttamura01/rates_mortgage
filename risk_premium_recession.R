setwd("/Users/takayukitamura/Documents/R_Computing/rates_mortgage")
library(tidyverse)
library(scales)
library(ggtext)
library(glue)
library(quantmod)   # for Fed Funds
library(fredr) 
library(plotly)
library(patchwork)


#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

## download data from csv file
rates <- read.csv("data/us_10y_30y.csv") %>% 
  rename("treasury10" = long_term_yield, "mortgage30" = mortgage_rate) %>% 
  drop_na()

head(rates)
tail(rates)

sapply(rates, class)

rates$date <- as.Date(rates$date, format = "%Y-%m-%d")
## Add spread to the data frame
spread <- rates$mortgage30 - rates$treasury10

rates$spread <- spread
head(rates, 10)
tail(rates, 10)

rates %>% 
  arrange(-spread)

dim(rates)

##understand the data
rates <- rates %>% 
  filter(date>="1971-01-01") 

summary <- rates %>% 
  summarise(minimum = min(spread, round(2)),
            median = median(spread, round(2)),
            mean = mean(spread, round(2)),
            maximum = max(spread, round(2)),
            "25th percentile" = quantile(spread,0.25), 
            "75th percentile" = quantile(spread, 0.75),
            sd = sd(spread, round(3)))

mean_spread <- summary$mean
mean_sd <- round(summary$sd + mean_spread, 2)
mean_2sd <- round((summary$sd)*2 + mean_spread, 2)

initial_data <- rates %>% 
  filter(date == min(date))

initial_date <- initial_data$date

max(rates$date)
latest_data <- rates %>% 
  filter(date == max(date))

max_mortgage <- max(rates$mortgage30)

latest_date <- latest_data$date
latest_mortgage_30 <- latest_data$mortgage30
latest_treasury_10 <- latest_data$treasury10
latest_spread <- latest_data$spread

##transform the data to longer
long_data <- rates %>% 
  pivot_longer(cols = -date, names_to = "yield", values_to = "percentage")

# Reorder the lebels of classes based on the latest value

latest_vals <- long_data %>% 
  filter(date == max(date)) %>% 
  arrange(desc(percentage))

long_data <- long_data %>% 
  mutate(yield = factor(yield, levels = latest_vals$yield))

##historical yields and spread
long_data %>% 
  filter(date >= as.Date("1971-01-01")) %>% 
  ggplot(aes(x =date, y = percentage, color = yield)) +
  geom_line(show.legend = TRUE) +
  geom_hline(yintercept = 1.66, color = "#0DE4C1") +
  geom_hline(yintercept = 2.2, color = "#0DE4C1") +
  geom_hline(yintercept = 2.74, color = "#0DE4C1") +
  scale_color_manual(breaks = c("mortgage30", "treasury10", "spread"),
                     values = c("#D81B60", "#1E88E5", "#483E21" )) +
  # annotate("text",
  #          x = latest_date,
  #          y = latest_mortgage_30,
  #          label = glue("{latest_mortgage_30}%")) +
  # annotate("text",
  #          x = latest_date,
  #          y = latest_treasury_10,
  #          label = glue("{latest_treasury_10}%")) +
  # annotate("text",
  #          x = latest_date,
  #          y = latest_spread,
  #          label = glue("{latest_spread}%")) +
  # annotate("label",
  #          x = 0.5,
  #          y = latest_mortgage_30,
  #          label = glue("{latest_mortgage_30}%")) +
  annotate("label",
           x = as.Date("2010-01-01"), 
           y = 15,
           label = "Spread:\nmean = 1.66%\nsd = 0.54%\n
          mean + sd = 2.2%\nmean + 2sd = 2.74%",
           fontface = "bold") +
  annotate("text",
           x = initial_date,
           y = mean_spread,
           label = "mean",
           hjust = 0.5) +
  annotate("text",
           x = initial_date,
           y = mean_sd,
           label = "mean + sd",
           hjust = 0.27) +
  annotate("text",
           x = initial_date,
           y = mean_2sd,
           label = "mean + 2sd",
           hjust = 0.27) +
  # annotate("text",
  #          x = initial_date,
  #          y = max_mortgage * 1.1,
  #          label = "interest rate\n spread (%)") +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_comma(accuracy = 0.1)) +
  labs(title = "US 30-year mortgage yield spread over 10-year treasury notes",
       caption = "FRED(Federal Reserve Economic Data), WSJ, by Takayuki Tamura",
       x = NULL, y = NULL) +
  theme(
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "top",
    # legend.position.inside = c(0.75, 0.85),
    panel.background = element_blank(),
    axis.line.x = element_line(colour = "gray"),
    axis.line.y = element_line(colour = "gray"),
    plot.title = element_textbox_simple(size = 18, face = "bold", margin = margin(b=10, t=10)),
    plot.title.position = "plot"
  )

ggsave("figures/us_30y_10y_spread.png", height = 6, width = 6)  

## adding high yield spread
high_yld <- fredr(series_id = "BAMLH0A0HYM2") %>% 
  select(date, high_yield = value)

avg_rates <- rates %>% 
  select(date, spread) %>% 
  left_join(., high_yld, by = "date") %>% 
  filter(date >= "2005-12-31") %>% 
  mutate(avg_spread = mean(spread, na.rm = TRUE), avg_high_yield = mean(high_yield, na.rm = TRUE)) 

avg_spread <- round(avg_rates$avg_spread, 3)
avg_high_yield <- round(avg_rates$avg_high_yield, 3)

#  pivot_longer(-date, names_to = "assets", values_to = "spread") 

spread_comp <- avg_rates %>% 
  select(date, spread, high_yield) %>% 
  pivot_longer(-date, names_to = "assets", values_to = "spread")

p <- spread_comp %>% 
  na.omit() %>% 
  ggplot(aes(x = date, y = spread, colour = assets))+
  geom_line(show.legend = FALSE) +
  geom_hline(yintercept = avg_spread, color = "red", linetype = "dashed") +
  geom_hline(yintercept = avg_high_yield, color = "blue", linetype = "dashed") +
  annotate("text",
           x = as.Date("2015-01-01"),
           y = 7,
           label = "High-Yield Spread") +
  annotate("text",
           x = as.Date("2015-01-01"),
           y = 2.5,
           label = "30-Year Mortgage Spread") +
  scale_color_manual(breaks = c("spread", "high_yield"),
                     values = c("red", "blue")
  ) +
  scale_y_continuous(limits = c(0, NA),
                     breaks = seq(0, 20, 5),
                     labels = label_comma(accuracy = 0.1)) +
  labs(title = "US 30-Year Mortgage and US High-Yield Bonds yield spread over 10-year treasury notes",
       subtitle = "The BAML High-Yield OAS(Option-Adjusted Spread) already compressed to the Post-GFC low",
       caption = "FRED(Federal Reserve Economic Data), WSJ, by Takayuki Tamura",
       x = NULL, y = NULL) +
  theme(
    panel.background = element_blank(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 16, margin = margin(b=10, t=5)),
    plot.subtitle = element_textbox_simple()
  )
p
ggsave("figures/us_30y_hi_yield_spread.png", height = 6, width = 6)  

  ## Fed balancesheet & recession
  # Set FRED API key if needed
  # fredr_set_key("your_api_key_here")
  
  # --- Download Data ---
  fed_bs <- fredr(series_id = "WALCL") %>%
    select(date, balance_sheet = value) %>% 
    filter(date >= "2006-01-05")
  
  usrec <- fredr(series_id = "USREC") %>%
    select(date, recession = value)%>% 
    filter(date >= "2006-01-05")
  
  
  recessions <- usrec %>%
    mutate(
      start = recession == 1 & lag(recession, default = 0) == 0,
      end   = recession == 0 & lag(recession, default = 0) == 1
    )
  
  rec_start <- recessions %>%
    filter(start) %>%
    pull(date)
  
  rec_end <- recessions %>%
    filter(end) %>%
    pull(date)
  
  # If last recession is ongoing, close it with last Fed BS date
  if(length(rec_end) < length(rec_start)){
    rec_end <- c(rec_end, max(fed_bs$date))
  }
  
  recession_df <- tibble(
    start = rec_start,
    end   = rec_end
  )
  
p1 <-   ggplot() +
    
    # recession shading
    geom_rect(data = recession_df,
              aes(xmin = start, xmax = end,
                  ymin = -Inf, ymax = Inf),
              fill = "gray70", alpha = 0.4) +
    
    # Fed balance sheet
    geom_line(data = fed_bs,
              aes(x = date, y = balance_sheet/1e6),
              color = "#1f77b4",
              linewidth = 1) +
    
    scale_y_continuous(
      labels = label_dollar(suffix = "T"),
      name = "Balance Sheet (Trillions USD)"
    ) +
    
    labs(
      title = "Federal Reserve Balance Sheet vs Recessions",
      subtitle = "WALCL with NBER recession shading",
      caption = "Source: FRED, by Takayuki Tamura"
    ) +
    
    theme_bw(base_size = 14) +
    theme(panel.grid = element_blank())

p / p1 + plot_layout(heights = c(4, 1))

# adding recession on the p
p <- ggplot() +
  
  # recession shading
  geom_rect(data = recession_df,
            aes(xmin = start, xmax = end,
                ymin = -Inf, ymax = Inf),
            fill = "gray70", alpha = 0.4) +
  
  # Fed balance sheet
  geom_line(data = spread_comp,
            aes(x = date, y = spread, colour = assets),
            show.legend = F,
            linewidth = 1) +
  
  annotate(geom = "text",
           x = as.Date("2015-01-01"),
           y = 10.0,
           label = "high yield bond",
           color = "blue",
           fontface = "italic",
           hjust = 0.5) +
  annotate(geom = "text",
           x = as.Date("2015-01-01"),
           y = 2.75,
           label = "30-Year mortgage",
           color = "red",
           fontface = "italic",
           hjust = 0.5) +
  
  scale_color_manual(values = c("blue","red")) +
  
  scale_y_continuous(
    labels = label_number_auto(),
    name = "Risk Premium (%)"
  ) +
  
  labs(
    title = "Risk Premium of HY-Bonds & Mortgage vs Recessions",
    subtitle = "WALCL with NBER recession shading",
    caption = "Source: FRED, by Takayuki Tamura"
  ) +
  
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.position = c("2015-01-01", 15))

p / p1 + plot_layout(heights = c(4, 1))



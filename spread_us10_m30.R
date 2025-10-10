setwd("/Users/takayukitamura/Documents/R_Computing/rates_mortgage")
library(tidyverse)
library(scales)
library(ggtext)
library(glue)
library(quantmod)   # for Fed Funds
library(fredr) 

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

## download data from csv file
rates <- read.csv("data/us_10y_30y.csv") %>% 
  rename("treasury10" = long_term_yield, "mortgage30" = mortgage_rate) %>% 
  na.omit()

tail(rates)

## add the rarest data 
# updates <- tribble(~date, ~mortgage30, ~treasury10,
#                    "2024-12-26", 6.85, 4.58)

##Combine the two data
# rates <- rbind(rates, updates)
rates$date <- as.Date(rates$date, format = "%Y-%m-%d")
sapply(rates, class)

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
  

spread_comp %>% 
  na.omit() %>% 
  ggplot(aes(x = date, y = spread, colour = assets))+
  geom_line(show.legend = FALSE) +
  geom_hline(yintercept = avg_spread, color = "red", linetype = "dashed") +
  geom_hline(yintercept = avg_high_yield, color = "blue", linetype = "dashed") +
  annotate("label",
           x = as.Date("2015-01-01"),
           y = 7,
           label = "High-Yield Spread") +
  annotate("label",
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

ggsave("figures/us_30y_hi_yield_spread.png", height = 6, width = 6)  


##historical spread with latest data
rates$latest_data <- ifelse(rates$date == max(rates$date), TRUE, FALSE)

rates %>% 
  ggplot(aes(x = date, y = spread, color = latest_data, fill = latest_data)) +
  geom_col(show.legend = FALSE) +
  scale_colour_manual(breaks = c(TRUE, FALSE),
                    values = c("red", "#AAAAAA")) +
  geom_text(data = subset(rates, latest_data == TRUE),
            aes(label = glue("{spread}%")), vjust=-.5, hjust = 0.5, color = "red") +
  labs(title = "US 30-year mortgage spread over 10-year treasury yield",
       x = NULL, y = NULL)

ggsave("figures/historical_spread_latest_data.png", height = 4.5, width = 5)
  
##boxplot

rates$year <- format(rates$date, "%Y")
sapply(rates, class)
rates$year = as.numeric(rates$year)

rates$period <- cut(rates$year,
                    breaks = c(1970, 2008, 2009, 2019, 2024, 2025),
                    labels = c("1971-2008", "2009-2019", "2020-2021", "2022-2024", "2025"))

d <- ggplot(rates, aes(x = period, y = spread, fill = period)) +
  geom_boxplot(show.legend = FALSE) +
  # facet_wrap(~period) +
  scale_y_continuous(limits = c(-0.5, 6),
                     breaks = seq(0, 6, 2),
                     labels = label_comma(accuracy = 0.1)) +
  labs(x = NULL,
       y = "spread")


d +  labs(title = "30 years mortgage yield spread over 10 years treasuary notes",
          subtitle = "The spread widened since 2022, with unwinding of MBS by Fed?", 
          y = "spread",
          x = NULL) +
  theme(plot.title.position = "plot",
        plot.subtitle = element_text(face = "italic"))

ggsave("figures/boxplots of spread indicating some impacts from Fed's MBS unwinding.png", width = 5, height = 4)

## Add z-score on the data_frame
z_score <- (spread - mean(spread))/sd(spread)

rates$z_score <- z_score
head(rates)
tail(rates)
model <- lm(mortgage30 ~ treasury10, data = rates)
lm(model)
summary(model)

predicted_30y <- predict(model, data.frame(treasury10 = c(4.5)))
attributes(model)

cor(rates$treasury10, rates$mortgage30)
model$coefficients
model$residuals
model$effects
model$rank
model$fitted.values

coefficients(model)
confint(model)
anova(model)

rate_stats <- rates %>% 
  summarise(
    mean_rate = mean(spread),
    SD_rate = sd(spread),
    med_est = median(spread),
    conf_25 = quantile(spread, 0.25),
    conf_75 = quantile(spread, 0.75),
    conf_low = quantile(spread, 0.025),
    conf_high = quantile(spread, 0.975)
  )

conf_low <- rate_stats %>% 
  select(conf_low)
conf_high <- rate_stats %>% 
  select(conf_high)

# Plot summary

latest_spread <- slice_tail(rates)[4]

slice_tail(rates)[4]


e <- ggplot(rates, aes(x = spread)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30, alpha = .3) +
  geom_vline(data = rate_stats,
             aes(xintercept = mean_rate),
             size = 1, color = "red") +
  geom_vline(data = rate_stats,
             aes(xintercept = mean_rate + SD_rate),
             linetype = "dashed", color = "red") +
  geom_vline(data = rate_stats,
             aes(xintercept = mean_rate - SD_rate),
             linetype = "dashed", color = "red") +
  geom_vline(data = rate_stats,
             aes(xintercept = mean_rate + SD_rate*2),
             linetype = "dotted", color = "red") +
  geom_vline(data = rate_stats,
             aes(xintercept = mean_rate - SD_rate*2),
             linetype = "dotted", color = "red") +
  scale_x_continuous(limits=c(NA, 4),
                     breaks = c(0, 4, 2)) +
  labs(title = "The 30-year mortgage spread over 10y treasury yield",
      # subtitle = glue("Current spread at {latest_spread}% is outside of the 95% confidence interval(=2SD)"),
       x = "spread(%)",
       caption = "Source: FRED(Federal Reserve Economic Data") +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    plot.subtitle = element_text(face = "italic"
  ))

e

# Add labels
z1 <-  round((mean(rates$spread) + sd(rates$spread)),2)
z2 <- round((mean(rates$spread) + sd(rates$spread)*2),2)
z3 <- round((mean(rates$spread) - sd(rates$spread)),2)
z4 <- round((mean(rates$spread) - sd(rates$spread)*2),2)

e + geom_text(aes(x = 1.7, y = 450, label = round(rate_stats$mean_rate, 2))) +
  geom_text(aes(x = z1, y = 400, label = z1)) +
  geom_text(aes(x = z2, y = 350, label = z2)) +
  geom_text(aes(x = z3, y = 400, label = z3)) +
  geom_text(aes(x = z4, y = 350, label = z4)) 

ggsave("figures/30y_mortgage_spread_10TB.png", height = 5, width = 5)

e + geom_text(aes(x = 1.7, y = 470, label = glue("mean = {round(rate_stats$mean_rate, 2)}"))) +
  geom_text(aes(x = z1 + 0.5, y = 425, label = glue("mean + SD = {z1}"))) +
  geom_text(aes(x = z2, y = 350, label = glue("mean + 2SD = {z2}"))) +
  geom_text(aes(x = z3 -0.7, y = 400, label = glue("mean - SD = {z3}"))) +
  geom_text(aes(x = z4, y = 350, label = glue("mean - 2SD = {z4}"))) 

ggsave("figures/30y_mortgage_spread_10TB.png", height = 5, width = 6)


f <- ggplot(rates, aes(x = spread)) +
  geom_histogram(fill = "blue", color = "white") +
  geom_rect(data = rate_stats,
            aes(
              ymin = 0, ymax = Inf,
              x = med_est, xmin = conf_low, xmax = conf_high),
            alpha = 0.2, fill = "green"
  ) +
  geom_vline(data = rate_stats, aes(
    xintercept = med_est),
    size = 1, color = "red"
  ) +
  labs(title = "Histogram - how the spread is distributed",
  subtitle = glue("95% confidence interval is the stread between {conf_low}% and {conf_high}%"),
  caption = "FRED(Federal Reserve Econimic Data"
  ) +
  theme(
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic")
  )
f
ggsave("figures/histogram_stread.png", width = 5, height = 5)






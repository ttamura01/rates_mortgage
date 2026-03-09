setwd("/Users/takayukitamura/Documents/R_Computing/rates_mortgage")

# Install/load required packages
library(tidyverse)
library(lubridate)
library(scales)
library(quantmod)   # for Fed Funds
library(fredr) 
library(ggtext)

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

cre <- fredr(series_id = "DRCRELEXFACBS") %>% 
  select(date, CRE_Loans = value)

credit_card <- fredr(series_id = "DRCCLACBS")%>% 
  select(date, Credit_Card_Loans = value)

consumer_loans <- fredr(series_id = "DRCLACBS") %>% 
  select(date, Consumer_Loans = value)

other_loans <- fredr(series_id = "DROCLACBS") %>% 
  select(date, Other_Loans = value)

delinquency <- credit_card %>% 
  left_join(., cre, by = "date") %>% 
  left_join(., consumer_loans, by = "date") %>% 
  left_join(., other_loans, by = "date") 

delinquency_long_data <- delinquency %>% 
  pivot_longer(-date, names_to = "loans", values_to = "deliquency")

delinquency_long_data %>% 
  filter(date >= "2006-01-01") %>% 
  ggplot(aes(x = date, y = deliquency, colour = loans)) +
  geom_line() +
  labs(title = "Diliquency Rate - Loan Stress is quietly rising again",
       subtitle = "Deliquency Rate by Loan Type",
       x = NULL, y = "Deliquency Rate (%)",
       caption = "Source: FRED by Takayuki Tamura",
       color = NULL) + 
  theme_minimal() +
  theme(
    legend.position = "top"
  )

ggsave("deliquency.png", width = 6, height = 5)  
  


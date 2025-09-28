library(tidyverse)
library(glue)
t_note <- read.csv("/Users/takayukitamura/Documents/R_Computing/US10&MG30/ust_10.csv") %>% 
  rename_all(tolower) %>% 
  rename(yld_10y = dgs10) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  group_by(year, month) 

t_note$yld_10y <- as.numeric(t_note$yld_10y)
sapply(t_note, class)

t_note_m <- t_note %>%
  na.omit(yld_10y) %>% 
  summarise(avg_rates = mean(yld_10y)) %>%
  mutate(month = ymd(glue("{year}-{month}-01"))) 
  
tail(t_note_m)
  
sapply(t_note, class)  
  
tail(t_note)  
cpi_ccpi <- read.csv("/Users/takayukitamura/Documents/R_Computing/cpi_treasury/cpi_ccpi_03_2024.csv") %>% 
  select(-X) %>% 
  separate(date, sep="-", into = c("year", "month", "day")) %>% 
  mutate(month = ymd(glue("{year}-{month}-01")))
tail(cpi_ccpi)
sapply(cpi_ccpi, class)

us10y_cpi <- full_join(t_note_m, cpi_ccpi, by = "month")
us10y_cpi %>% 
  select(month, avg_rates, cpi) %>% 
  pivot_longer(cols = -month, names_to = "index", values_to = "percentage") %>% 
  ggplot(aes(x = month, y = percentage, color = index)) +
  geom_line()

us10y_cpi %>% 
  mutate(spread = avg_rates - cpi) %>% 
  ggplot(aes(x = month, y = spread)) +
  geom_line()

us10y_cpi %>% 
  filter(month > "1988-01-01") %>% 
  ggplot(aes(x = cpi, y = avg_rates)) +
  geom_point() + 
  geom_smooth(method = "lm")

model = lm(cpi ~ avg_rates, us10y_cpi)
lm(model)
summary(model)

library(tidyverse)
library(lubridate)
library(scales)
library(fredr)

fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

#------------------------------------------------------------
# Helper: pull a single FRED series and standardize
#------------------------------------------------------------
get_z1 <- function(series_id, sector){
  fredr(series_id = series_id) %>%
    transmute(date = as.Date(date),
              sector = sector,
              value = value)
}

#------------------------------------------------------------
# Ownership series from Z.1 L.211 (Agency- and GSE-backed securities)
# Units: $ billions (level)
#
# NOTE:
# - These are the "Asset, Level" series by sector (holders)
# - Names follow L.211 line titles
#------------------------------------------------------------

df <- bind_rows(
  get_z1("BOGZ1FL893061705Q", "All sectors (total)") ,              # total assets
  get_z1("BOGZ1FL713061705Q", "Federal Reserve (Monetary authority)"),
  get_z1("BOGZ1FL763061705Q", "US-chartered depository institutions (banks)"),
  get_z1("BOGZ1FL753061703Q", "Foreign banking offices in the US"),
  get_z1("BOGZ1FL473061705Q", "Credit unions"),
  get_z1("BOGZ1FL513061705Q", "Property-casualty insurers"),
  get_z1("BOGZ1FL543061705Q", "Life insurers"),
  get_z1("BOGZ1FL573061705Q", "Private pension funds"),
  get_z1("BOGZ1FL343061705Q", "Federal government retirement funds")
)

#------------------------------------------------------------
# OPTIONAL: add the Rest of the World series if you want explicit foreign ownership
# (Depending on releases, FRED has a Rest of World holder series for L.211.
# If this errors, tell me and I’ll pull the correct one for your exact need.)
#------------------------------------------------------------
# df <- bind_rows(df,
#   get_z1("BOGZ1FL263061705Q", "Rest of the world (foreign holders)")
# )

#------------------------------------------------------------
# 1) Latest quarter table
#------------------------------------------------------------
latest_q <- max(df$date)

latest_tbl <- df %>%
  filter(date == latest_q) %>%
  filter(sector != "All sectors (total)") %>%
  arrange(desc(value)) %>%
  mutate(share = value / sum(value, na.rm = TRUE)) %>%
  mutate(share = percent(share, accuracy = 0.1),
         value = comma(value, accuracy = 1)) %>%
  select(Sector = sector, `Holdings ($bn)` = value, Share = share)

print(latest_q)
print(latest_tbl)

#------------------------------------------------------------
# 2) Line chart (levels)
#------------------------------------------------------------
df_chart <- df %>%
  filter(sector != "All sectors (total)") %>%
  filter(date >= as.Date("2000-01-01"))  # change start date (e.g., 1990)

ggplot(df_chart, aes(date, value, color = sector)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = comma) +
  labs(title = "Ownership of US Agency & GSE-Backed Securities (Z.1 L.211)",
       subtitle = "Holdings by sector, level ($ billions), quarterly",
       x = NULL, y = "$ billions",
       color = NULL,
       caption = "Source: Federal Reserve Financial Accounts (Z.1) Table L.211 via FRED") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

#------------------------------------------------------------
# 3) OPTIONAL: stacked ownership share chart
#------------------------------------------------------------
df_total <- df %>%
  filter(sector == "All sectors (total)") %>%
  select(date, total = value)

df_share <- df %>%
  filter(sector != "All sectors (total)") %>%
  left_join(df_total, by = "date") %>%
  mutate(share = value / total) %>%
  filter(date >= as.Date("2000-01-01"))

ggplot(df_share, aes(date, share, fill = sector)) +
  geom_area() +
  scale_y_continuous(labels = percent) +
  labs(title = "US Agency MBS / GSE-Backed Securities Ownership Share",
       subtitle = "Share of total outstanding by sector (quarterly)",
       x = NULL, y = "Share of total",
       fill = NULL,
       caption = "Source: Federal Reserve Financial Accounts (Z.1) Table L.211 via FRED by Takayuki Tamura") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

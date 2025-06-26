library(dplyr)
library(ggplot2)
library(TTR)
library(tidyquant)
library(patchwork)
library(weathermetrics)

# bring in data
climate_df <- read.csv("Data/climate_data.csv")

## max temp -------
max_df <- climate_df %>% filter(Metric %in% "Avg_yearly_max_temp")

# convert temps to celsius
max_df$ValueC <- fahrenheit.to.celsius(max_df$Value)

# plot 
max_temp <- ggplot(max_df, aes(x = Year, y = ValueC)) +
  geom_smooth(se = F, size = 3, method = "loess") +
  geom_line() + 
  #geom_ma(ma_fun = SMA, n = 5, color = "red") + 
  labs(x = "Year", y = "Average annual maximum temperature (°C)") +
  theme_minimal() +
  theme(text = element_text(size = 20))+
  scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990,2000, 2010,2020), limits = c(1945,2020) )

plot(max_temp)

## min temp -----
min_df <- climate_df %>% filter(Metric %in% "Avg_yearly_min_temp")

# convert temps to celsius
min_df$ValueC <- fahrenheit.to.celsius(min_df$Value)

min_temp <- ggplot(min_df, aes(x = Year, y = ValueC)) +
  geom_line() + 
  geom_smooth(se = F, size = 3, method = "loess") +
  labs(x = "Year", y = "Average annual minimum temperature (°C)") +
  theme_minimal() +
  theme(text = element_text(size = 20))+
  scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990,2000, 2010,2020), limits = c(1945,2020) )

plot(min_temp)

## precip ------
precip_df <- climate_df %>% filter(Metric %in% "Precip_cm")

precip <- ggplot(precip_df, aes(x = Year, y = Value)) +
  geom_line() + 
  geom_smooth(se = F, size = 3, method = "loess") +
  labs(x = "Year", y = "Annual precipitation (cm)") +
  theme_minimal() +
  theme(text = element_text(size = 20))+
  scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990,2000, 2010,2020), limits = c(1945,2020) )

plot(precip)

# combine 

climate_fig <- max_temp + min_temp + precip

plot(climate_fig)

ggsave("Figures/climate_fig.pdf", height = 7, width = 17)

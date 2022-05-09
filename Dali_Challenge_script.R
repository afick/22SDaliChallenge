# DALI Application Spring 2022
# Applying for 22F
#
# Name: Alex Fick
# Date: May 8, 2022

# Load Packages and Initial Settings --------------------------------------

library(tidyverse)
library(ggthemes)
library(patchwork)

fig_h <- 8
fig_w <- 11
source <- "Source: World Income Inequality Database (WIID)"

# Load data ---------------------------------------------------------------

data <- read_csv("wiid.csv") %>% 
  mutate(country = ifelse(str_detect(country, "United States"), 
                          "USA",
                          country))

data2 <- read_csv("sample_superstore.csv") 

names(data2) <- names(data2) %>%  
  str_replace_all("[\\s-]+","_") %>% 
  str_to_lower()

# Figure 1 & 2 ------------------------------------------------------------

world_coordinates <- map_data("world")

# Represent quality of life as numeric, and consider only years after 2000
mediangini <- data %>% 
  filter(year > 2000) %>% 
  group_by(country) %>% 
  mutate(quality = case_when(quality == "Average" ~ 1,
                             quality == "High" ~ 2,
                             quality == "Low" ~ 0),
         quality = as.numeric(quality)) %>% 
  summarise(gini = median(gini_reported, na.rm = T), quality = median(quality, 
                                                            na.rm = T))

mediangini <- full_join(mediangini,
                        world_coordinates,
                        by = c("country" = "region"))

# Create map where fill of the countries are based on their gini index
gini <- ggplot(mediangini, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = gini), color = "lightgray",
               size = .25) +
  theme_map() +
  coord_fixed() +
  scale_fill_gradient(low = "green",
                      high = "black",
                      na.value = "white",
                      name = "Gini Index",
                      breaks = c(60, 45, 30),
                      labels = c("60\n(less equality)",
                                 "45",
                                 "30\n(more equality)")) +
  labs(title = "Median Gini Index in Each Country during the 21st century") +
  theme(legend.position = "bottom", legend.justification = "center")

# Create map where fill of the countries are based on their quality of life
qual <- ggplot(mediangini, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = quality), color = "lightgray",
               size = .25) +
  theme_map() +
  coord_fixed() +
  scale_fill_gradient(low = "black",
                      high = "lightblue", 
                      na.value = "white",
                      name = "Quality of Life",
                      breaks = c(0, 1, 2),
                      labels = c("Low",
                                 "Average", 
                                 "High")) +
  labs(title = "Median Quality of Life in Each Country during the 21st century",
       caption = "Note: Countries/regions in white indicate no data collected\n\nSource: World Income Inequality Database (WIID)") +
  theme(legend.position = "bottom", legend.justification = "center")

# Show the graphs next to each other
gini / qual

ggsave("figures/figure1.pdf", width = fig_h, height = fig_w)

# Figure 3 ----------------------------------------------------------------

# Choose one row to show what kind of data is contained in the set
ug <- data %>% 
  filter(id == 10094)

# Pull data on key information in the dataset
country <- ug %>% pull(country)
quality <- ug %>% pull(quality) %>% str_to_lower()
group <- ug %>% pull(incomegroup) %>% str_to_lower()
population <- formatC(ug %>% pull(population), format = "d", big.mark = ",")
year <- ug %>% pull(year)
index <- ug %>% pull(gini_reported)

uglong <- ug %>% 
  pivot_longer(d1:d10, names_to = "decile",
               values_to = "value") %>% 
  mutate(decile = as.numeric(str_remove(decile, "d")))

# Visualize the data
ggplot(uglong, aes(decile, value)) +
  geom_col(fill = "navyblue") +
  geom_line(color = "navyblue") +
  scale_x_continuous(breaks = c(1,10),
                     labels = c("Poorest 10%",
                                "Wealthiest 10%")) +
  labs(title = paste0("Distribution of Wealth in ", country,
                      " in ", year),
       subtitle = paste0("The population was ",
                      population, ", the Gini index was ", index,
                      ",\nthe quality of life was ", quality, 
                      ", and the country as a whole was ", group),
       x = "Decile Income Group",
       y = "Percentage of Country's Wealth Held by the Decile",
       caption = source) +
  theme_few()

ggsave("figures/figure1&2.pdf", width = fig_w, height = fig_h)

# Figure 4 ----------------------------------------------------------------

# Create dataframe of top 10 countries by GDP, removing countries with 
# missing data
t10 <- data %>% 
  filter(country != "Equatorial Guinea" & country != "Saudi Arabia") %>% 
  group_by(country) %>% 
  summarise(mediangdp = median(gdp_ppp_pc_usd2011)) %>% 
  slice_max(order_by = mediangdp, n = 10)

top10gdp <- data %>% 
  filter(country %in% t10$country) %>% 
  mutate(quality = case_when(quality == "Average" ~ 1,
                                     quality == "High" ~ 2,
                                     quality == "Low" ~ 0) %>% 
           as.numeric(quality)) %>% 
  group_by(country) %>% 
  summarise(gdp = median(gdp_ppp_pc_usd2011), q1 = median(q1, na.rm = T), 
            q2 = median(q2, na.rm = T), q3 = median(q3, na.rm = T), 
            q4 = median(q4, na.rm = T), q5 = median(q5, na.rm = T),
            gini_reported = median(gini_reported), quality = mean(quality)) %>% 
  pivot_longer(q1:q5, names_to = "quintile",
               values_to = "perc") %>% 
  mutate(country = fct_reorder(country, desc(gdp)))

# Create dataframe that will be used for annotating the graph
text <- top10gdp %>% 
  group_by(country) %>% 
  summarise(gdp, x = "q3", y = 40) %>% 
  distinct()

# Visualize data
ggplot(top10gdp, aes(quintile, perc)) +
  geom_col(aes(fill = gini_reported)) +
  geom_text(aes(x, y, label = paste0("GDPpC:\n$", scales::comma(gdp))), text,
            size = 3) +
  facet_wrap(~ country, nrow = 2) +
  scale_x_discrete(breaks = c("q1", "q5"), 
                   labels = c("           Bottom 20%", "Top 20%      ")) +
  scale_fill_gradient(low = "#50C878", high = "black",
                      na.value = "gray", limits = c(25,55),
                      breaks = c(30, 40, 50), 
                      labels = c("30\nMore Equality", "40", 
                                 "50\n Less Equality")) +
  labs(x = "Quintles", y = "Percentage of Wealth",
       title = "Distribution of Wealth of Countries in Top 10 of GDP per Capita",
       subtitle = "In order of GDP per Capita",
       fill = "Gini Index",
       caption = source) +
  theme_few() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, 'cm')) +
  guides(fill = guide_colorbar(title.vjust = .9))

ggsave("figures/figure3.pdf", width = fig_w, height = fig_h)


# Challenge 2 -------------------------------------------------------------

# Create 10 Year Yearly Profit and Lifetime Profit Projections ------------

#Calculate total profits, sales, and quantities for each year
data2year <- data2 %>% mutate(year = as.numeric(str_extract(order_date, "\\d{4}$"))) %>% 
  group_by(year) %>% 
  summarise(totalprofits = sum(profit), totalsales = sum(sales),
            totalquantity = sum(quantity), disc = mean(discount),
            y0 = year - 2014) %>% 
  distinct()

# Create regression models
profitsinc <- lm(totalprofits ~ year, data2year)
summary(profitsinc)
salesinc <- lm(totalsales ~ year, data2year)
summary(salesinc)
quantinc <- lm(totalquantity ~ year, data2year)
summary(quantinc)

# Create dataframe of next 10 years predicted stats
next10 <- tibble(year = seq(2018, 2027, 1))
next10stats <- next10 %>% 
  mutate(totalprofits = predict(profitsinc, next10),
         totalsales = predict(salesinc, next10),
         totalquantity = predict(quantinc, next10))

# Combine current stats with predicted stats
currandnext <- bind_rows(data2year,
                         next10stats) %>% 
  group_by() %>% 
  mutate(lifetimeprofit = cumsum(totalprofits))

# Visualize the lifetime profit
ggplot(currandnext, aes(year, lifetimeprofit)) +
  geom_rect(aes(xmin = -Inf, xmax = 2017,
                ymin = -Inf, ymax = Inf),
            fill = "lightgray") +
  geom_rect(aes(xmin = 2017, xmax = Inf,
                ymin = -Inf, ymax = Inf),
            fill = "lightgreen") +
  geom_point(size = 2) +
  geom_smooth(se = F) +
  scale_x_continuous(breaks = c(2014, 2017, 2027)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_few() +
  labs(x = "Year", y = "Lifetime Profits",
       title = "Current and Projected Lifetime Profits of Superstore") +
  annotate(geom = "text", x = 2015.15, y = 1750000,
           label = "Lifetime Profits\nof First 4 Years") +
  annotate(geom = "text", x = 2022.5, y = 1750000,
           label = "Projected Lifetime Profits in\nNext 10 Years")

ggsave("figures/projlifetimeprofits.pdf", width = fig_w, height = fig_h)


# Visualize yearly profits
ggplot(currandnext, aes(year, totalprofits)) +
  geom_rect(aes(xmin = -Inf, xmax = 2017,
                ymin = -Inf, ymax = Inf),
            fill = "lightgray") +
  geom_rect(aes(xmin = 2017, xmax = Inf,
                ymin = -Inf, ymax = Inf),
            fill = "lightgreen") +
  geom_point(size = 2) +
  geom_smooth(se = F) +
  scale_x_continuous(breaks = c(2014, 2017, 2027)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_few() +
  labs(x = "Year", y = "Total Profits",
       title = "Current and Projected Yearly Total Profits of Superstore") +
  annotate(geom = "text", x = 2015.15, y = 120000,
           label = "Total Profits\nin First 4 Years") +
  annotate(geom = "text", x = 2022.5, y = 120000,
           label = "Projected Total Profits in\nNext 10 Years")
  
ggsave("figures/projyearlyprofits.pdf", width = fig_w, height = fig_h)


# Appliance Profit Model --------------------------------------------------

# Create dataframe that only contains appliance sales
appls <- data2 %>% filter(sub_category == "Appliances")

# Polynomial and linear regression model for appliance sales
profitmodel <- lm(profit ~ poly(sales, degree = 3) + discount + quantity, data = appls)
summary(profitmodel)
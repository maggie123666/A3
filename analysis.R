# Maggie Li
# BF

# Load the tidyverse package
library("tidyverse")
library("maps")
library("mapproj")


# Load the data
data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# I want to analyze the relationship between urbanicity and the black jail 
# population over these year. 

# What is the average value for black jail population in urban area in 2018?
black_pop_urban_2018 <- data %>% 
  select(year, black_jail_pop, urbanicity) %>% 
  filter(urbanicity == "urban") %>% 
  filter(year == "2018") %>%
  pull(black_jail_pop)

ave_black_pop_urban_2018 <- mean(black_pop_urban_2018, na.rm = T)

# What is the average value for black jail population in rural area in 2018?
black_pop_rural_2018 <- data %>% 
  select(year, black_jail_pop, urbanicity) %>% 
  filter(urbanicity == "rural") %>% 
  filter(year == "2018") %>%
  pull(black_jail_pop)

ave_black_pop_rural_2018 <- mean(black_pop_rural_2018, na.rm = T)

# Which county has the highest black jail population through the years?
highest_black_county <- data %>% 
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>% 
  pull(county_name)

# How much has the average value for black jail population in urban area change 
# over the last 20 years?
black_pop_urban_1998 <- data %>% 
  select(year, black_jail_pop, urbanicity) %>% 
  filter(urbanicity == "urban") %>% 
  filter(year == "1998") %>%
  pull(black_jail_pop)

ave_black_pop_urban_1998 <- mean(black_pop_urban_1998, na.rm = T)
ave_change <- ave_black_pop_urban_2018 - ave_black_pop_urban_1998

# What is the difference between highest black jail population in urban area and
# small/mid area?
highest_black_urban <- data %>% 
  filter(urbanicity == "rural") %>% 
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(black_jail_pop)

highest_black_small <- data %>% 
  filter(urbanicity == "small/mid") %>% 
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(black_jail_pop)

difference <- highest_black_urban - highest_black_small

# Chart
# What is the black jail population change over 40 years with different 
# urbanicity?

# collect data in 1988
black_pop_change_1988 <- data %>% 
  select(year, black_jail_pop, urbanicity) %>% 
  filter(year == "1988")

rural_1988 <- black_pop_change_1988 %>% 
  filter(urbanicity == "rural") %>% 
  summarise(rural_1988 = sum(black_jail_pop, na.rm = T))

urban_1988 <- black_pop_change_1988 %>% 
  filter(urbanicity == "urban") %>% 
  summarise(urban_1988 = sum(black_jail_pop, na.rm = T))


# collect data in 1998
black_pop_change_1998 <- data %>% 
  select(year, black_jail_pop, urbanicity) %>% 
  filter(year == "1998")

rural_1998 <- black_pop_change_1998 %>% 
  filter(urbanicity == "rural") %>% 
  summarise(rural_1998 = sum(black_jail_pop, na.rm = T))

urban_1998 <- black_pop_change_1998 %>% 
  filter(urbanicity == "urban") %>% 
  summarise(urban_1998 = sum(black_jail_pop, na.rm = T))

# collect data in 2008
black_pop_change_2008 <- data %>% 
  select(year, black_jail_pop, urbanicity) %>% 
  filter(year == "2008")

rural_2008 <- black_pop_change_2008 %>% 
  filter(urbanicity == "rural") %>% 
  summarise(rural_2008 = sum(black_jail_pop, na.rm = T))

urban_2008 <- black_pop_change_2008 %>% 
  filter(urbanicity == "urban") %>% 
  summarise(urban_2008= sum(black_jail_pop, na.rm = T))

# collect data in 2018
black_pop_change_2018 <- data %>% 
  select(year, black_jail_pop, urbanicity) %>% 
  filter(year == "2018")

rural_2018 <- black_pop_change_2018 %>% 
  filter(urbanicity == "rural") %>% 
  summarise(rural_2018= sum(black_jail_pop, na.rm = T))

urban_2018 <- black_pop_change_2018 %>% 
  filter(urbanicity == "urban") %>% 
  summarise(urban_2018= sum(black_jail_pop, na.rm = T))

year_data <- data.frame(
  "Year" = c(1988, 1998, 2008, 2018))

rural_years <- rbind.data.frame(
  "Rural" = c(rural_1988, rural_1998, rural_2008, rural_2018))

rural_years <- rural_years %>% 
  pivot_longer(
  rural_1988 | rural_1998 | rural_2008 | rural_2018, names_to = "year", 
  values_to = "rural")

urban_years <- rbind.data.frame(
  "urban" = c(urban_1988, urban_1998, urban_2008, urban_2018))

urban_years <- urban_years %>% 
  pivot_longer(
    urban_1988 | urban_1998 | urban_2008 | urban_2018, names_to = "Year", 
    values_to = "urban")

chart_one_data <- data.frame(urban_years$urban, rural_years$rural, year_data)

# Plot
chart_one <- ggplot(chart_one_data, aes(x=Year)) + 
  geom_line(aes(y = urban_years.urban), color = "darkred", size = 1.5) + 
  geom_line(aes(y = rural_years.rural), color="steelblue", size = 1.5) +
  labs(
    title = "Rural area vs urban area with black jail population" ,
    subtitle = 
      "The black jail population difference between rural area and urban area over 40 years" ,
    x = "Year",
    y = "Population")


# Chart 2
white_pop_urban_2018 <- data %>% 
  select(year, white_jail_pop, county_name, urbanicity) %>% 
  filter(urbanicity == "urban") %>% 
  filter(year == "2018") %>%
  pull(white_jail_pop, county_name)

black_pop_urban_2018 <- data %>% 
  select(year, black_jail_pop, urbanicity, county_name) %>% 
  filter(urbanicity == "urban") %>% 
  filter(year == "2018") %>%
  pull(black_jail_pop, county_name)

urban_county_name <- data %>% 
  select(urbanicity, county_name, year) %>% 
  filter(urbanicity == "urban") %>% 
  filter(year == "2018") %>%
  pull(county_name)

chart_two_data <- data.frame(
  white_pop_urban_2018, black_pop_urban_2018, urban_county_name)


bar_chart <- barplot(cbind(
  chart_two_data$white_pop_urban_2018, chart_two_data$black_pop_urban_2018), 
  main="white jail population vs black jail population", 
  ylab="Population", beside=TRUE)


# Chart 3
black_pop_county_2018 <- data %>% 
  select(year, black_jail_pop, urbanicity, county_name) %>% 
  filter(year == "2018") %>%
  select(black_jail_pop, county_name)

names(black_pop_county_2018)[2] <- "county"
county_shape <- map_data("county") %>%
  rename(county = region)

county_shapes <- left_join(county_shape, black_pop_county_2018, by = "county")

county_map <- ggplot(county_shapes) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "white", 
    size = .1 
  ) +
  coord_map() + 
  scale_fill_continuous(low = "White", high = "Red") +
  labs(
    title = "Black jail population in US county",
    fill = "Total Number of Population"
  )
  
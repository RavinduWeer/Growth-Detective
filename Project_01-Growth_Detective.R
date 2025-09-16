# Project 1 - Growth Detective (Data Preparation) 
# Part 1: Data Collection and Preparation
# Countries: Canada (advanced), Paraguay (emerging), Lesotho (low-income)
# Variables: GDP per Capita, Population growth, Investment rate, Inflation rate
###################################################

# Install required packages needed for the code
install.packages(c("WDI", "dplyr", "ggplot2", "zoo"))

library(WDI)
library(dplyr)
library(ggplot2)
library(zoo)

# Step 1: Define countries
countries <- c("CAN", "PRY", "LSO")  # Canada, Paraguay, Lesotho

# Step 2: Define variables (WDI codes)
# - GDP per capita (constant 2015 US$): NY.GDP.PCAP.KD
# - Population growth (annual %): SP.POP.GROW
# - Investment rate (Gross capital formation, % of GDP): NE.GDI.TOTL.ZS
# - Inflation (annual %): FP.CPI.TOTL.ZG
indicators <- c(
  "NY.GDP.PCAP.KD",   # GDP per capita
  "SP.POP.GROW",      # Population growth
  "NE.GDI.TOTL.ZS",   # Investment rate
  "FP.CPI.TOTL.ZG"    # Inflation
)

# Step 3: Download data (1980–2025)
data_raw <- WDI(
  country = countries,
  indicator = indicators,
  start = 1980,
  end   = 2025,
  extra = FALSE
)

# View data_raw sheet
view(data_raw)

# Step 4: Rename columns for readability
data_clean <- data_raw %>%
  rename(
    gdp_per_capita   = NY.GDP.PCAP.KD,
    pop_growth       = SP.POP.GROW,
    investment_rate  = NE.GDI.TOTL.ZS,
    inflation        = FP.CPI.TOTL.ZG,
    Country          = country,
    Year             = year
  ) %>%
  arrange(Country, Year)
view(data_clean)


# Step 5: Handle missing values via interpolation
data_clean <- data_clean %>%
  group_by(Country) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(across(
    c(gdp_per_capita, pop_growth, investment_rate, inflation),
    ~ na.approx(., x = Year, na.rm = FALSE, rule = 2)
  )) %>%
  ungroup()

# View cleaned data
view(data_clean)

###################################################
# Part 2: Comparing Growth Across Countries
# Countries: Canada, Paraguay, Lesotho
# Plotting variables country by country
# Variables: GDP per Capita, Population growth,
# Investment rate, Inflation rate
###################################################

# Function to plot all four variables for a single country
plot_country_all_vars <- function(cn){
  df <- data_clean %>% filter(Country == cn)
  
  p1 <- ggplot(df, aes(x = Year, y = gdp_per_capita)) +
    geom_line(color = "blue", size = 1) +
    labs(title = paste("GDP per capita -", cn),
         x = "Year", y = "GDP per capita (constant 2015 US$)") +
    theme_minimal()
  
  p2 <- ggplot(df, aes(x = Year, y = pop_growth)) +
    geom_line(color = "darkgreen", size = 1) +
    labs(title = paste("Population Growth -", cn),
         x = "Year", y = "Annual % growth") +
    theme_minimal()
  
  p3 <- ggplot(df, aes(x = Year, y = investment_rate)) +
    geom_line(color = "purple", size = 1) +
    labs(title = paste("Investment Rate -", cn),
         x = "Year", y = "% of GDP") +
    theme_minimal()
  
  p4 <- ggplot(df, aes(x = Year, y = inflation)) +
    geom_line(color = "red", size = 1) +
    labs(title = paste("Inflation -", cn),
         x = "Year", y = "Inflation (%)") +
    theme_minimal()
  
  print(p1); print(p2); print(p3); print(p4)
}

# Generate plots for each of the three countries 
plot_country_all_vars("Canada")
plot_country_all_vars("Paraguay")
plot_country_all_vars("Lesotho")


# CROSS-COUNTRY COMPARISON GRAPHS 

# GDP per capita comparison
ggplot(data_clean, aes(x = Year, y = gdp_per_capita, color = Country)) +
  geom_line(size = 1) +
  labs(title = "GDP_PC_Comparison (1980–2025)",
       x = "Year", y = "GDP per capita (constant 2015 US$)") +
  theme_minimal()

# Population growth comparison
ggplot(data_clean, aes(x = Year, y = pop_growth, color = Country)) +
  geom_line(size = 1) +
  labs(title = "Population Growth Comparison (1980–2025)",
       x = "Year", y = "Annual % growth") +
  theme_minimal()

# Investment rate comparison
ggplot(data_clean, aes(x = Year, y = investment_rate, color = Country)) +
  geom_line(size = 1) +
  labs(title = "Investment Rate Comparison(1980–2025)",
       x = "Year", y = "% of GDP") +
  theme_minimal()

# Inflation comparison
ggplot(data_clean, aes(x = Year, y = inflation, color = Country)) +
  geom_line(size = 1) +
  labs(title = "Inflation Comparison (1980–2025)",
       x = "Year", y = "Inflation (%)") +
  theme_minimal()


###################################################
# Part 3: GDP per Capita Growth per Country 
###################################################

# Ensure GDP per capita growth is computed
data_clean <- data_clean %>%
  group_by(Country) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(gdp_capita_growth = (gdp_per_capita / lag(gdp_per_capita) - 1) * 100) %>%
  ungroup()
view(data_clean)
# Function to plot GDP per capita growth for a single country
plot_country_growth <- function(cn){
  df <- data_clean %>% filter(Country == cn)
  
  ggplot(df, aes(x = Year, y = gdp_capita_growth)) +
    geom_line(color = "blue", size = 1) +
    labs(title = paste("GDP per capita Growth Rate -", cn),
         x = "Year", y = "Growth rate (%)") +
    theme_minimal()
}

###################################################
# Comparison: GDP per capita Growth vs Other Variables
# Countries: Canada, Paraguay, Lesotho
###################################################

# (a) GDP per capita vs GDP per capita growth
ggplot(data_clean, aes(x = gdp_per_capita, y = gdp_capita_growth, color = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP per Capita vs GDP per Capita Growth",
       x = "GDP per capita (constant 2015 US$)",
       y = "GDP per Capita Growth (%)") +
  theme_minimal()

# (b) Population growth vs GDP per capita growth
ggplot(data_clean, aes(x = pop_growth, y = gdp_capita_growth, color = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Population Growth vs GDP per Capita Growth",
       x = "Population Growth (annual %)",
       y = "GDP per Capita Growth (%)") +
  theme_minimal()

# (c) Investment Rate vs GDP per capita growth
ggplot(data_clean, aes(x = investment_rate, y = gdp_capita_growth, color = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Investment Rate vs GDP per Capita Growth",
       x = "Investment Rate (% of GDP)",
       y = "GDP per Capita Growth (%)") +
  theme_minimal()

# (d) Inflation vs GDP per capita growth (original extended)
ggplot(data_clean, aes(x = inflation, y = gdp_capita_growth, color = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Inflation vs GDP per Capita Growth",
       x = "Inflation (annual %)",
       y = "GDP per Capita Growth (%)") +
  theme_minimal()


###############################################################################
# Plots per country
plot_country_growth("Canada")
plot_country_growth("Paraguay")
plot_country_growth("Lesotho")


###################################################
#(e) Cross-country Comparison of Growth 
###################################################

ggplot(data_clean, aes(x = Year, y = gdp_capita_growth, color = Country)) +
  geom_line(size = 0.75) +
  labs(title = "GDP PC Growth Cmprson(1980–2025)",
       x = "Year", y = "Growth rate (%)") +
  theme_minimal()



###################################################
# Part 3: Growth Changes in a Specific Country
# Country: Canada
###################################################

# Step 1: Select Canada data 
canada <- data_clean %>%
  filter(Country == "Canada") %>%
  arrange(Year)

# Step 2: Divide into periods 
canada <- canada %>%
  mutate(period = case_when(
    Year <= 1989 ~ "1980–1989",
    Year <= 2007 ~ "1990–2007",
    TRUE ~ "2008–2025"
  ))

# Step 3: Calculate average GDP growth per period 
canada_summary <- canada %>%
  group_by(period) %>%
  summarise(avg_growth = mean(gdp_capita_growth, na.rm = TRUE))

print(canada_summary)

# Step 4: Visualizations 

# (a) Growth trajectory with shaded periods
ggplot(canada, aes(x = Year, y = gdp_capita_growth)) +
  geom_line(color = "blue", size = 1) +
  geom_vline(xintercept = c(1990, 2008), linetype = "dashed", color = "red") +
  labs(title = "Canada:Avg GDP PC Growth by Period",
       x = "Year", y = "Growth rate (%)") +
  annotate("text", x = 1985, y = max(canada$gdp_capita_growth, na.rm=TRUE),
           label = "Pre-1990", color = "black") +
  annotate("text", x = 1998, y = max(canada$gdp_capita_growth, na.rm=TRUE),
           label = "1990–2007", color = "black") +
  annotate("text", x = 2015, y = max(canada$gdp_capita_growth, na.rm=TRUE),
           label = "2008–2025", color = "black") +
  theme_minimal()

# (b) Bar chart of average growth by period
ggplot(canada_summary, aes(x = period, y = avg_growth, fill = period)) +
  geom_col() +
  labs(title = "Canada:Avg GDP PC Growth by Period",
       x = "Period", y = "Average growth rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# (c) Timeline comparison of GDP per capita and inflation for context
ggplot(canada, aes(x = Year)) +
  geom_line(aes(y = gdp_per_capita/1000), color = "blue", size = 1) +
  geom_line(aes(y = inflation), color = "red", size = 1, linetype = "dashed") +
  labs(title = "Canada: GDP PC (scaled) vs Inflation",
       x = "Year", y = "GDP per capita (000s, blue) & Inflation % (red dashed)") +
  theme_minimal()

################################################################################
# Part 4: Determinants of Growth
# Data: OECD countries
################################################################################
install.packages("corrplot")
library(corrplot)

# Step 1: Define OECD countries (ISO codes are in WDI)
# Quick pull of all OECD economies
oecd_codes <- c("AUS","AUT","BEL","CAN","CHL","COL","CZE","DNK","EST","FIN",
                "FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR",
                "LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT","SVK",
                "SVN","ESP","SWE","CHE","TUR","GBR","USA")

# Step 2: Define indicators 
indicators <- c(
  "NY.GDP.PCAP.KD",   # GDP per capita (constant 2015 US$)
  "SP.POP.GROW",      # Population growth
  "NE.GDI.TOTL.ZS",   # Investment rate (% of GDP)
  "FP.CPI.TOTL.ZG"    # Inflation
)

# Step 3: Download data (1980–2025) 
oecd_raw <- WDI(country = oecd_codes,
                indicator = indicators,
                start = 1980, end = 2025)
view(oecd_raw)

oecd_clean <- oecd_raw %>%
  rename(
    gdp_per_capita = NY.GDP.PCAP.KD,
    pop_growth     = SP.POP.GROW,
    investment_rate= NE.GDI.TOTL.ZS,
    inflation      = FP.CPI.TOTL.ZG,
    Country        = country,
    Year           = year
  ) %>%
  group_by(iso2c) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(gdp_percap_growth = (gdp_per_capita/lag(gdp_per_capita)-1)*100) %>%
  ungroup()

# Step 4: Compute annual averages by country 
oecd_avg <- oecd_clean %>%
  group_by(Country) %>%
  summarise(
    avg_growth = mean(gdp_percap_growth, na.rm = TRUE),
    avg_pop = mean(pop_growth, na.rm = TRUE),
    avg_invest = mean(investment_rate, na.rm = TRUE),
    avg_inflation = mean(inflation, na.rm = TRUE)
  )

print(head(oecd_avg))

# Step 5: Scatter plots of Growth vs Determinants 
ggplot(oecd_avg, aes(x = avg_pop, y = avg_growth)) +
  geom_point(color = "blue") + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP Growth vs Population Growth(OECD)",
       x = "Avg Population Growth (%)", y = "Avg GDP per capita Growth (%)") +
  theme_minimal()

ggplot(oecd_avg, aes(x = avg_invest, y = avg_growth)) +
  geom_point(color = "purple") + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP Growth vs Investment Rate (OECD)",
       x = "Avg Investment Rate (% of GDP)", y = "Avg GDP per capita Growth (%)") +
  theme_minimal()

ggplot(oecd_avg, aes(x = avg_inflation, y = avg_growth)) +
  geom_point(color = "red") + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP Growth vs Inflation (OECD)",
       x = "Avg Inflation (%)", y = "Avg GDP per capita Growth (%)") +
  theme_minimal()

# Step 6: Correlation Matrix 
oecd_corr <- cor(oecd_avg[, c("avg_growth","avg_pop","avg_invest","avg_inflation")],
                 use="pairwise.complete.obs")

corrplot(oecd_corr, method="ellipse", type="upper", tl.col="black", tl.srt=45)


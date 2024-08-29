# Load necessary libraries
library(quantmod)
library(ggplot2)
library(readxl)


########################################### GDP ######################################################
# Download GDP data from FRED
getSymbols("GDP", src = "FRED")

# Convert the data to a data frame
gdp_data <- data.frame(Date = index(GDP), GDP = coredata(GDP))

# Filter the data for the years 1967 to 2023
gdp_data <- gdp_data[gdp_data$Date >= "1970-01-01" & gdp_data$Date <= "2022-12-31",]

# Create the plot with more breaks on the y-axis and x-axis
gdp_plot <- ggplot(gdp_data, aes(x = Date, y = GDP)) +
  geom_line(color = "black") +
  labs(x = "Year",
       y = "US GDP (Billions of Dollars)") +
  scale_x_date(date_breaks = "11 years", date_labels = "%Y") +  # Breaks every 10 years
  scale_y_continuous(breaks = seq(0, max(gdp_data$GDP), by = 5000)) +  # Y-axis breaks every 5000 billion
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "gainsboro", color = NA),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),  # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),  # Remove vertical minor grid lines
  )

# Display the plot
print(gdp_plot)


################################################ CPI ######################################################
# Download CPI data from FRED
getSymbols("CPIAUCSL", src = "FRED")

# Convert the data to a data frame
cpi_data <- data.frame(Date = index(CPIAUCSL), CPI = as.numeric(CPIAUCSL))

# Filter the data for the years 1970 to 2022
cpi_data <- cpi_data[cpi_data$Date >= "1970-01-01" & cpi_data$Date <= "2022-12-31",]

# Calculate the year-over-year percentage change (inflation rate)
cpi_data$Inflation_Rate <- (cpi_data$CPI / lag(cpi_data$CPI, 12) - 1) * 100

# Remove the first 12 months (NA values) due to the lag calculation
cpi_data <- cpi_data[!is.na(cpi_data$Inflation_Rate), ]

# Create the plot with more breaks on the y-axis and x-axis
cpi_plot <- ggplot(cpi_data, aes(x = Date, y = Inflation_Rate)) +
  geom_line(color = "black") +
  labs(x = "Year",
       y = "Inflation Rate (%)") +
  scale_x_date(date_breaks = "6 years", date_labels = "%Y") +  # Breaks every 6 years
  scale_y_continuous(breaks = seq(-5, max(cpi_data$Inflation_Rate, na.rm = TRUE), by = 2)) +  # Y-axis breaks every 2%
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "gainsboro", color = NA),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),  # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),  # Remove vertical minor grid lines
  )

# Display the plot
print(cpi_plot)


################################################ POLICY RATE ######################################################

# Download Federal Funds Rate data from FRED
getSymbols("FEDFUNDS", src = "FRED")

# Convert the data to a data frame
fedfunds_data <- data.frame(Date = index(FEDFUNDS), FEDFUNDS = as.numeric(FEDFUNDS))

# Filter the data for the years 1970 to 2022
fedfunds_data <- fedfunds_data[fedfunds_data$Date >= "1970-01-01" & fedfunds_data$Date <= "2022-12-31",]

# Create the plot with more breaks on the y-axis and x-axis
fedfunds_plot <- ggplot(fedfunds_data, aes(x = Date, y = FEDFUNDS)) +
  geom_line(color = "black") +
  labs(x = "Year",
       y = "Federal Funds Rate (%)",) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +  # Breaks every 6 years
  scale_y_continuous(breaks = seq(0, max(fedfunds_data$FEDFUNDS, na.rm = TRUE), by = 2)) +  # Y-axis breaks every 2%
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "gainsboro", color = NA),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),  # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),  # Remove vertical minor grid lines
  )

# Display the plot
print(fedfunds_plot)


################################################ EM RATIO ######################################################

# Download Employment-Population Ratio data from FRED
getSymbols("EMRATIO", src = "FRED")

# Convert the data to a data frame
emratio_data <- data.frame(Date = index(EMRATIO), EMRATIO = as.numeric(EMRATIO))

# Filter the data for the years 1970 to 2022
emratio_data <- emratio_data[emratio_data$Date >= "1970-01-01" & emratio_data$Date <= "2022-12-31",]

# Create the plot with more breaks on the y-axis and x-axis
p_emratio <- ggplot(emratio_data, aes(x = Date, y = EMRATIO)) +
  geom_line(color = "black") +
  labs(x = "Year",
       y = "Employment-Population Ratio (%)",) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +  # Breaks every 6 years
  scale_y_continuous(breaks = seq(min(emratio_data$EMRATIO, na.rm = TRUE), max(emratio_data$EMRATIO, na.rm = TRUE), by = 2)) +  # Y-axis breaks every 2%
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "gainsboro", color = NA),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),  # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),  # Remove vertical minor grid lines
  )

# Display the plot
print(p_emratio)

################################################ WTI OIL ######################################################

# Download WTI Spot Oil Price data from FRED
getSymbols("WTISPLC", src = "FRED")

# Convert the data to a data frame
wti_data <- data.frame(Date = index(WTISPLC), WTISPLC = as.numeric(WTISPLC))

# Filter the data for the years 1970 to 2022
wti_data <- wti_data[wti_data$Date >= "1970-01-01" & wti_data$Date <= "2022-12-31",]

# Create the plot with more breaks on the y-axis and x-axis
wti_plot <- ggplot(wti_data, aes(x = Date, y = WTISPLC)) +
  geom_line(color = "black") +
  labs(x = "Year",
       y = "WTI Spot Price (USD per Barrel)",
       caption = "Source: FRED") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +  # Breaks every 6 years
  scale_y_continuous(breaks = seq(0, max(wti_data$WTISPLC, na.rm = TRUE), by = 20)) +  # Y-axis breaks every 20 USD
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "gainsboro", color = NA),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),  # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),  # Remove vertical minor grid lines
  )

# Display the plot
print(wti_plot)


################################################ MONEY ######################################################

# Download M2 Money Supply data from FRED
getSymbols("M2SL", src = "FRED")  # "M2SL" is the FRED code for the M2 Money Stock

# Convert the data to a data frame
m2_data <- data.frame(Date = index(M2SL), M2 = as.numeric(M2SL))

# Filter the data for the years you are interested in
m2_data <- m2_data[m2_data$Date >= "1970-01-01" & m2_data$Date <= "2022-12-31",]

# Create the plot to show the increase in M2 money supply over time
m2_plot <- ggplot(m2_data, aes(x = Date, y = M2)) +
  geom_line(color = "black") +
  labs(x = "Year",
       y = "M2 Money Supply (Billions of USD)",) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +  # Breaks every 6 years
  scale_y_continuous(labels = scales::comma) +  # Use comma format for large numbers
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "gainsboro", color = NA),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),  # Optionally remove vertical major grid lines
    panel.grid.minor.x = element_blank(),  # Optionally remove vertical minor grid lines
  )

# Display the plot
print(m2_plot)


################################################ GOV SPENDING ######################################################

# Download Federal Government Current Expenditures data from FRED
getSymbols("FGEXPND", src = "FRED")  # "FGEXPND" is the FRED code for Federal Government Expenditures

# Convert the data to a data frame
fgexpend_data <- data.frame(Date = index(FGEXPND), FGEXPND = as.numeric(FGEXPND))

# Filter the data for the years you are interested in
fgexpend_data <- fgexpend_data[fgexpend_data$Date >= "1970-01-01" & fgexpend_data$Date <= "2022-12-31",]

# Create the plot to show the increase in federal government expenditures over time
gov_plot <- ggplot(fgexpend_data, aes(x = Date, y = FGEXPND)) +
  geom_line(color = "black") +
  labs(x = "Year",
       y = "Expenditures (Billions of USD)",) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +  # Breaks every 6 years
  scale_y_continuous(labels = scales::comma) +  # Use comma format for large numbers
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "gainsboro", color = NA),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),  # Optionally remove vertical major grid lines
    panel.grid.minor.x = element_blank(),  # Optionally remove vertical minor grid lines
  )

# Display the plot
print(gov_plot)

################################################ DEBT ######################################################

# Load the Excel file (replace with the correct file path)
debt_data <- read_csv("US_Debt.csv")

debt_data

# Combine the YEAR and MONTH columns into a Date column
debt_data <- debt_data %>%
  mutate(Date = as.Date(paste(YEAR, MONTH, "01", sep = "-")))

# Convert DEBT to numeric (remove commas if necessary)
debt_data$DEBT <- as.numeric(gsub(",", "", debt_data$DEBT))

# Create the line chart
ggplot(debt_data, aes(x = Date, y = DEBT)) +
  geom_line(color = "black", size = 1.0) +
  labs(x = "Year",
       y = "Total Gross Debt (USD Trillions)",) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +  # Breaks every 6 years
  scale_y_continuous(labels = scales::comma) +  # Use comma format for large numbers
  scale_y_continuous(labels = function(x) x / 1e12) +  # Convert to trillions
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "gainsboro", color = NA),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),  # Optionally remove vertical major grid lines
    panel.grid.minor.x = element_blank(),  # Optionally remove vertical minor grid lines
  )
# Plots for NJ Phase 2 reopening, shows percent change in number of visits, per county & region, per day since June 15th.


library(SafeGraphR)
library(tidyverse)
library(lubridate)


# Read in previously built phase 2 dataset
phase2 <- read_csv('phase2-safegraph-by-county.csv')

# Scale the data to be relative to June 15th reopening
scaled <- phase2 %>% scale_to_date(adj_vars = 'visits_by_day',
                                   date = lubridate::ymd('2020-06-15'),
                                   by = 'county_fips',
                                   growth = TRUE)


# Create regional plots

# Add up all county values by region
sum <- scaled %>% group_by(date,REGION) %>% summarise(visits_by_day=sum(visits_by_day))

# Create 7 day moving average
sum$ma <- ma(sum$visits_by_day)

# Ungroup so plotly doesn't break
sum <- ungroup(sum)


# BY REGION Plot using raw visit count percent change
fig1 <- plot_ly(sum, x = ~date, y = ~visits_by_day, type = 'scatter', mode = 'lines', color = ~REGION) %>% 
  layout(
    title = "NJ Phase 2: Change in POI Visits By Region",
    yaxis = list(title = "Percent Change Since June 15th",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(sum$date), max(sum$date))
    ))

# BY REGION Plot using 7-day moving average to account for weekend effects
fig2 <- plot_ly(sum, x = ~date, y = ~ma, type = 'scatter', mode = 'lines', color = ~REGION) %>% 
  layout(
  title = "NJ Phase 2: Change in POI Visits By Region",
  yaxis = list(title = "Percent Change Since June 15th",
               tickformat = "%"),
  xaxis = list(
    type = "date",
    title = "Month",
    range=c(min(sum$date), max(sum$date))
  ))

# Sum all numbers by county group
sum_county <- scaled %>% group_by(date,COUNTY) %>% summarise(visits_by_day=sum(visits_by_day))

# Create 7 day moving average
sum_county$ma <- ma(sum_county$visits_by_day)

# Ungroup for plotly
sum_county <- ungroup(sum_county)

# Create individual county plots

# BY COUNTY Plot using raw visit count percent change
fig3 <- plot_ly(sum_county, x = ~date, y = ~visits_by_day, type = 'scatter', mode = 'lines', color = ~COUNTY) %>% 
  layout(
    title = "NJ Phase 2: Change in POI Visits By Region",
    yaxis = list(title = "Percent Change Since June 15th",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(sum$date), max(sum$date))
    ))

# BY COUNTY Plot using 7 day ma percent change
fig4 <- plot_ly(sum_county, x = ~date, y = ~ma, type = 'scatter', mode = 'lines', color = ~COUNTY) %>% 
  layout(
    title = "NJ Phase 2: Change in POI Visits By Region",
    yaxis = list(title = "Percent Change Since June 15th",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(sum$date), max(sum$date))
    ))




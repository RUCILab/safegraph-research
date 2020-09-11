library(SafeGraphR)
library(tidyverse)
library(lubridate)

setwd("E:/NJ Phase 2")
counties <- read_csv('nj_counties.csv')

# Read in all Safegraph patterns from 6-15-20 to 8-10-20, filter to NJ and aggregate daily visit count by county FIPS code

patterns <- read_many_patterns("E:/Safegraph/patterns/",
                               recursive = TRUE, 
                               by = 'county_fips',
                               expand_int = 'visits_by_day',
                               filter = 'state_fips == 34')

nj_county_patterns <- patterns %>% select(county_fips,visits_by_day,day,start_date,date)

# Normalize the data # Doesnt work

norm <- read_many_csvs(dir = "E:/NJ Phase 2/normalization_stats/",
                       recursive = TRUE,
                       makedate = TRUE,
                       fill = TRUE)


# Create a 7 day moving average based upon daily visit count
data.table::setorder(patterns, county_fips, date)
patterns[, avg_visits := ma(visits_by_day), by = 'county_fips']


# Scale to date to calculate raw percent change

patterns_scaled <- patterns %>% scale_to_date(adj_vars = 'visits_by_day',
                                              date = lubridate::ymd('2020-06-15'),
                                              by = 'county_fips')
                                              
patterns_scaled_subset <- patterns_scaled %>% select(date,county_fips,visits_by_day)

county_percent_change <- patterns_scaled_subset %>% 
  group_by(date,county_fips) %>% 
  summarise(total_pct_change=sum(visits_by_day))

joined <- county_percent_change %>% left_join(counties,by=c('county_fips' ='FIPSCO'))


njphase2 <- nj_county_patterns %>% left_join(counties,by=c('county_fips' = 'FIPSCO'))

# County
county <-joined %>% group_by(date,COUNTY) %>% summarise(total_pct_change)

# Region
region <- joined %>% group_by(date,REGION) %>% summarise(total_pct_change)


ggplot2::ggplot(region,
                ggplot2::aes(x = date, y = total_pct_change, color = REGION)) +
ggplot2::geom_line(size = 2) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 text = ggplot2::element_text(size = 15, family = 'serif')) +
  ggplot2::labs(x = 'Date',
                y = 'Change Since June 15th',
                title = 'SafeGraph: Change in Visits Since June 15, 2020') +
  ggplot2::guides(color = FALSE) +
  directlabels::geom_dl(ggplot2::aes(x = date + .1, label = REGION),
                        method = list('last.bumpup', cex = 1.3)) +
  ggplot2::scale_x_date(limits = c(min(region$date), max(region$date)+2)) +
  ggplot2::scale_y_continuous(labels = scales::percent)
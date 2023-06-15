library(tidyverse)

chloropleth_map <- function(df, ind, year_date, title, name){
  df %>%
    filter(indicator == ind,
           year == year_date) %>%
    mutate(label_v = number(value, big.mark = ",")) %>%
  hcmap(
    map = 'custom/world-robinson-lowres',
    download_map_data = TRUE,
    value = "value",
    name = name,
    joinBy = c('iso-a3','iso3c'),
    borderWidth = 0,
    nullColor = "#d3d3d3",
    tooltip = list(
      pointFormat = "{point.country}: {point.label_v}"
    )
  ) %>%
    hc_colorAxis(
      stops = color_stops(colors = viridisLite::inferno(10, begin = 0.1)),
      max = 10^8,
      labels = list(
        overflow = 'allow'
      )
    ) %>%
    hc_title(text = title)
}

chloropleth_map(
  df,
  'International tourism, number of arrivals',
  2018,
  'Which countries recieve the most Tourists?',
  'Number of Arriving Tourists')

chloropleth_map(
  df,
  'International tourism, number of departures',
  2020,
  'Which countries are loses',
  'Number of leaving tourists'
)


df %>%
  filter(
    indicator == 'International tourism, number of arrivals') %>%
  group_by(country) %>%
  mutate(growth_rate = (value - lag(value)) / lag(value),
         label_gr = percent(growth_rate, accuracy = 0.01)) %>%
  mutate(prev_year_value = lag(value)) %>%
  ungroup() %>%
  filter(year == 2020) %>%
  filter(!is.na(growth_rate)) %>%
  arrange(-prev_year_value) %>%
  mutate(label_value = number(value, big.mark = ","),
         label_pyv = number(prev_year_value, big.mark = ",")) %>%
  slice_head(n= 10) %>%
  mutate(country = as_factor(country)) %>%
  hchart(
    'bar',
    hcaes(x = country,
          y = growth_rate),
    color = "#CC4248FF",
    tooltip = list(
      useHTML = TRUE,
      pointFormat = 
        "Growth Rate: {point.label_gr}<br>2019 Tourists: {point.label_pyv}<br> 2020 Tourists: {point.label_value}"
    )
  ) %>%
  hc_xAxis(
    title = list(text = "")
  ) %>%
  hc_yAxis(
    title = list(text = "Percent Change")
  ) %>%
  hc_title(
    text = 'Percent Change - 2019 to 2020'
  )

pc_bar_graph <- function(df, ind, order_var, sort_fun){
  df %>%
    filter(indicator == ind) %>%
    group_by(country) %>%
    mutate(percent_change = (value - lag(value)) / lag(value),
           prev_year_value = lag(value)) %>%
    ungroup() %>%
    filter(year == 2020) %>%
    filter(!is.na(percent_change)) %>%
    arrange(!!order_var, sort_fun(!!order_var)) %>%
    mutate(label_value = number(value, big.mark = ","),
           label_pyv = number(prev_year_value, big.mark = ","),
           label_pc = percent(percent_change, accuracy = 0.01)) %>%
    slice_head(n=10) %>%
    mutate(country = as_factor(country)) %>%
      hchart(
        'bar',
        hcaes(x = country,
              y = percent_change),
        color = "#CC4248FF",
        tooltip = list(
          useHTML = TRUE,
          pointFormat = 
            "Percent Change: {point.label_pc}<br>2019 Tourists: {point.label_pyv}<br> 2020 Tourists: {point.label_value}"
        )
      ) %>%
      hc_xAxis(
        title = list(text = "")
      ) %>%
      hc_yAxis(
        title = list(text = "Percent Change")
      ) %>%
      hc_title(
        text = 'Percent Change - 2019 to 2020'
      )
}

pc_bar_graph(
  df,
  'International tourism, number of arrivals',
  sym("value"),
  sort
)

library(tidyverse)
library(highcharter)
library(rlang)

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

arrange_dir <- function(df, order_var, direction){
  order_var <- sym(order_var)
  order_var <- enquo(order_var)
  if(direction == 'desc'){
    return(arrange(df, desc(!!order_var)))
  }
  if(direction == 'aesc'){
    return(arrange(df, !!order_var))
  }
}

pc_bar_graph <- function(df, ind, order_var, direction){
  df %>%
    filter(indicator == ind) %>%
    group_by(country) %>%
    mutate(percent_change = (value - lag(value)) / lag(value),
           prev_year_value = lag(value)) %>%
    ungroup() %>%
    filter(year == 2020) %>%
    filter(!is.na(percent_change)) %>%
    arrange_dir(order_var, direction) %>%
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
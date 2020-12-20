library(tidyverse)

library(readr)
all_data <- read_delim("data/rep_mon.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)

# PPA / Democracy Index Plot ----------------------------------------------

plot(int_dollars ~ score, all_data, type = "p", col = const_form)
legend("topleft",
       legend = levels(all_data$const_form),
       col = levels(all_data$const_form) %>% as.numeric,
       bty = "n")

library(ggplot2)

p <- ggplot(aes(x = score, y = int_dollars, color = const_form),
            data = all_data) +
  geom_point() + geom_text(aes(label = country ), hjust = 0, vjust = 0) +
  labs(x = "Democracy Index 2018", 
       y = "Per Capita GDP (PPA International Dollars)")
p

## 
library(highcharter)

colors <- c(
  "#FB1108", "#FD150B", "#FA7806", "#FBE426", "#FCFB8F",
  "#F3F5E7", "#C7E4EA", "#ABD6E6", "#9AD2E1"
)

all_data$color <- highcharter::colorize(log(as.numeric(all_data$const_form)+1), colors)
all_data <- all_data %>% 
  mutate(shape = ifelse(const_form == "monarchy", "circle", "square"))
x <- c("Country:", 
       "Democracy Index 2018:", 
       "Regime Type:", 
       "Const. Form:", 
       "Per Capita PPA (int$):",
       "Population: ")
y <- c("{point.country}", 
       "{point.score:.2f}",
       "{point.regime_type}",
       "{point.const_form}",
       "{point.int_dollars:.0f}",
       "{point.pop:.0f}")

tltip <- tooltip_table(x, y)

hchart(
  all_data,
  "scatter",
  hcaes(x = score, 
        y = int_dollars, 
        color = color,
        size = pop)#,
  # marker = list(symbol = "triangle")
  # minSize = 2,
  # maxSize = 20
) %>%
  hc_chart(
    # backgroundColor = "black",
    zoomType = "xy",
    backgroundColor = hex_to_rgba("black", 0.5),
    divBackgroundImage = "http://www.wired.com/images_blogs/underwire/2013/02/xwing-bg.gif"
  ) %>% 
  hc_plotOptions(
    series = list(stickyTracking = FALSE)
  ) %>%
  hc_xAxis(
    title = list(text = "Democracy Index 2018"),
    type = "logarithmic",
    gridLineWidth = 0# ,
    # reversed = TRUE
  ) %>%
  hc_yAxis(
    title = list(text = "Per Capita PPA (international $)"),
    type = "logarithmic", 
    gridLineWidth = 0
  ) %>%
  hc_title(
    style = list(color = hex_to_rgba("white", 0.5)),
    text = "Democracy Level, Constitutional Form and Economic Development"
  ) %>%
  hc_subtitle(
    style = list(color = hex_to_rgba("white", 0.5)),
    text = "(Logarithmic Axis)"
  ) %>%
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "",
    pointFormat = tltip
  ) # %>%
# hc_size(
#   height = 700
# )

# PPA / Gimo Index Plot ---------------------------------------------------
hchart(
  all_data,
  "scatter",
  hcaes(
    x = gini_index, 
    y = int_dollars, 
    color = color,
    size = pop
  )
  # minSize = 2,
  # maxSize = 20
) %>%
  hc_chart(
    # backgroundColor = "black",
    zoomType = "xy",
    backgroundColor = hex_to_rgba("black", 0.5),
    divBackgroundImage = "http://www.wired.com/images_blogs/underwire/2013/02/xwing-bg.gif"
  ) %>% 
  hc_plotOptions(
    series = list(stickyTracking = FALSE)
  ) %>%
  hc_xAxis(
    title = list(text = "Gini Index"),
    type = "logarithmic",
    gridLineWidth = 0,
    reversed = TRUE
  ) %>%
  hc_yAxis(
    title = list(text = "Per Capita PPA (international $)"),
    type = "logarithmic", 
    gridLineWidth = 0
  ) %>%
  hc_title(
    style = list(color = hex_to_rgba("white", 0.5)),
    text = "Income Equality, Constitutional Form and Economic Development"
  ) %>%
  hc_subtitle(
    style = list(color = hex_to_rgba("white", 0.5)),
    text = "(Logarithmic Axis)"
  ) %>%
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "",
    pointFormat = tltip
  ) # %>%
# hc_size(
#   height = 700
# )


###

# DI - Gini Plot ----------------------------------------------------------


library(highcharter)

colors <- c(
  "#FB1108", "#FD150B", "#FA7806", "#FBE426", "#FCFB8F",
  "#F3F5E7", "#C7E4EA", "#ABD6E6", "#9AD2E1"
)

all_data$color <- highcharter::colorize(log(all_data$int_dollars), colors)
# all_data <- all_data %>% 
#   mutate(shape = ifelse(const_form == "monarchy", "circle", "square"))

# n <- 4
# 
# stops <- data.frame(
#   q = 0:n/n,
#   # c = colors[c(1,3,5,7,9)],
#   c = colors,
#   stringsAsFactors = FALSE
# )
# 
# stops <- list_parse2(stops)
stops <- color_stops(5, colors)

x <- c("Country:", 
       "Democracy Index 2018:", 
       "Regime Type:", 
       "Const. Form:", 
       "Per Capita GDP (PPA int$):",
       "Gini Index:",
       "Population: ")
y <- c("{point.country}", 
       "{point.score:.2f}",
       "{point.regime_type}",
       "{point.const_form}",
       "{point.int_dollars:.0f}",
       "{point.gini_index:.2f}",
       "{point.pop:.0f}")

tltip <- tooltip_table(x, y)

highchart() %>% 
  hc_add_series(all_data %>% filter(const_form == "monarchy"), 
                hcaes(x = score, # Democracy Index
                      y = gini_index, 
                      color = color, 
                      # value = all_data %>% 
                      #   filter(const_form == "monarchy") %>% 
                      #   pull(int_dollars) %>% 
                      #   log(),
                      size = pop),
                marker = list(symbol = "square"),
                type = "scatter",
                name = "Monarchies") %>% 
  hc_add_series(all_data %>% filter(const_form == "republic"), 
                hcaes(x = score, 
                      y = gini_index, 
                      color = color, 
                      # value = all_data %>% 
                      #   filter(const_form == "republic") %>% 
                      #   pull(int_dollars) %>% 
                      #   log(),
                      size = pop),
                marker = list(symbol = "circle"),
                type = "scatter",
                name = "Republics") %>%
  hc_chart(
    backgroundColor = "black",
    zoomType = "xy"# ,
    # backgroundColor = hex_to_rgba("black", 0.5),
    # divBackgroundImage = "http://www.wired.com/images_blogs/underwire/2013/02/xwing-bg.gif"
  ) %>% 
  hc_plotOptions(
    series = list(stickyTracking = FALSE)
  ) %>%
  hc_xAxis(
    title = list(text = "Democracy Index 2018", style = list(color = "white")),
    # type = "logarithmic",
    gridLineWidth = 0
  ) %>%
  hc_yAxis(
    title = list(text = "Gini Index", style = list(color = "white")),
    # type = "logarithmic", 
    gridLineWidth = 0,
    reversed = TRUE
  ) %>%
  hc_title(
    # style = list(color = hex_to_rgba("white", 0.5)),
    style = list(color = "white"),
    text = "Comparing Republics vs. Monarchies"
  ) %>%
  hc_subtitle(
    # style = list(color = hex_to_rgba("white", 0.5)),
    style = list(color = "white"),
    text = "Democracy Level, Income Equality and Economic Development"
  ) %>%
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "",
    pointFormat = tltip
  ) %>% 
  hc_colorAxis(type = "logarithmic",
               stops = stops) %>%
  hc_legend(enabled = TRUE,
            # title = "Per Capita GDP (PPA international dollars)"#,
            title = list(text = "Per Capita GDP (PPA international $)"),
            # color = hex_to_rgba("white", 0.5),
            color = "white",
            # layout = "vertical", 
            # align = "right", 
            verticalAlign = "top"#,
            # floating = TRUE
            # bubbleLegend = list(enabled = TRUE, color = '#e4d354')
  ) %>% 
  hc_add_theme(hc_theme_monokai())
# hc_add_theme(hc_theme_chalk())
# hc_add_theme(hc_theme_flatdark())
# hc_add_theme(hc_theme_darkunica())
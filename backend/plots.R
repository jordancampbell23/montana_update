#----Historical-----

# Excess Contributions



# saveRDS(contributions, "contributions.rds")


# Amo Interest




# saveRDS(amo_plot, "amo_plot.rds")



# Gainn Loss




# saveRDS(gl_plot, "gl_plot.rds")



# Asset Allocation

asset_allocation <- readr::read_csv("data/historical/MPERS_allocation.csv")

asset_allocation <- asset_allocation %>%
  dplyr::mutate(year = as.character(year)) %>%
  dplyr::mutate(`Fixed Income` = Fixed.Income + Cash.Equivalents) |>
  dplyr::mutate(alternatives = Real.Estate + Hedge.Funds + Commodities)

asset <- asset_allocation %>%
  echarts4r::e_chart(year, stack = "group", areaStyle = list(opacity = 1)) %>%
  echarts4r::e_area(name = "Fixed Income", `Fixed Income`, symbol = "none") %>%
  echarts4r::e_area(name = "Public Equities", Total.Public.Equity, symbol = "none") %>%
  echarts4r::e_area(name = "Private Equity", Private.Equity, symbol = "none") %>%
  echarts4r::e_area(name = "Other Alternatives", alternatives, symbol = "none") %>%
  echarts4r::e_y_axis(max = 1,
                      axisLabel = list(fontSize = 14),
                      margin = 0,
                      splitLine = list(show = FALSE),
                      formatter = echarts4r::e_axis_formatter(style = "percent", digits = 0)) %>%
  echarts4r::e_x_axis(axisLabel = list(fontSize = 14),
                      axisTick = list(alignWithLabel = T)) %>%
  echarts4r::e_legend(type = "scroll",
                      textStyle = list(fontSize = 14),
                      icons = c("roundRect", "roundRect", "roundRect", "roundRect")) %>%
  echarts4r::e_tooltip(trigger = "axis",
                       axisPointer = list(type = 'shadow',
                                          shadowStyle = list(color = 'rgba(14, 43, 72, 0.1)')),
                       formatter = echarts4r::e_tooltip_pointer_formatter(style = "percent",
                                                                          digits = 1),
                       axisPointer = list(type = 'shadow', shadowStyle = list(opacity = 0.5))) %>%
  echarts4r::e_axis_pointer(label = list(show = F)) |>
  echarts4r::e_theme_custom("echarts_theme.json") %>%
  echarts4r::e_grid(left = "15%", bottom = "10%")

# saveRDS(asset, "data/historical/MPERS_asset.rds")


# Density Ridgeline - Probability Analysis

SimulationData <- read_csv("data/historical/MPERS_simulation_data.csv")


br <- SimulationData |>
  as.data.frame() |>
  filter(`Plan Name` == "BlackRock") |>
  select(1) |>
  rename(BlackRock = Data)


ra <- SimulationData |>
  as.data.frame() |>
  filter(`Plan Name` == "Research Affiliates") |>
  select(1) |>
  rename(`Research Affiliates` = Data)

jp <- SimulationData |>
  as.data.frame() |>
  filter(`Plan Name` == "JP Morgan") |>
  select(1) |>
  rename(`JP Morgan` = Data)


bny <- SimulationData |>
  as.data.frame() |>
  filter(`Plan Name` == "BNY Mellon") |>
  select(1) |>
  rename(`BNY Mellon` = Data)


h10 <- SimulationData |>
  as.data.frame() |>
  filter(`Plan Name` == "Horizon10") |>
  select(1) |>
  rename(Horizon10 = Data)


h20 <- SimulationData |>
  as.data.frame() |>
  filter(`Plan Name` == "Horizon20") |>
  select(1) |>
  rename(Horizon20 = Data)


pa <- SimulationData |>
  as.data.frame() |>
  filter(`Plan Name` == "Plan Assumptions") |>
  select(1) |>
  rename(`Plan Assumptions` = Data)


hist <- SimulationData |>
  as.data.frame() |>
  filter(`Plan Name` == "Historical") |>
  select(1) |>
  rename(Historical = Data)


sim_data <- cbind(br, jp, bny, h10, h20, ra, pa, hist)


# saveRDS(sim_data, "ndpers_density_data.rds")

# sim_data <- readRDS("data/historical/ndpers_density_data.rds")

density <- sim_data |>
  mutate(row = 1:nrow(sim_data)) |>
  e_chart(row,
          silent = T) |>
  e_density(BlackRock,
            areaStyle = list(opacity = 0.75),
            lineStyle = list(width = 0),
            symbol = "none",
            color = "#ff6633") |>
  e_density(`Plan Assumptions`,  x_index = 1, y_index = 1,
            areaStyle = list(opacity = 0.75),
            lineStyle = list(width = 0),
            symbol = "none",
            color = "#e15f36") %>%
  e_density(Horizon20, x_index = 2, y_index = 2, 
            areaStyle = list(opacity = 0.75),
            lineStyle = list(width = 0),
            symbol = "none",
            color = "#c35938") %>%
  e_density(Horizon10, x_index = 3, y_index = 3, 
            areaStyle = list(opacity = 0.75),
            lineStyle = list(width = 0),
            symbol = "none",
            color = "#a5523b") %>%
  e_density(Historical, x_index = 4, y_index = 4, 
            areaStyle = list(opacity = 0.75),
            lineStyle = list(width = 0),
            symbol = "none",
            color = "#884b3d") %>%
  e_density(`BNY Mellon`, x_index = 5, y_index = 5, 
            areaStyle = list(opacity = 0.75),
            lineStyle = list(width = 0),
            symbol = "none",
            color = "#6a4440") %>%
  e_density(`JP Morgan`, x_index = 6, y_index = 6, 
            areaStyle = list(opacity = 0.75),
            lineStyle = list(width = 0),
            symbol = "none",
            color = "#4c3e42") %>%
  e_density(`Research Affiliates`, x_index = 7, y_index = 7, 
            areaStyle = list(opacity = 0.75),
            lineStyle = list(width = 0),
            symbol = "none",
            color = "#2e3745") %>%
  e_grid(height = "25%",
         # top = "100%",
         bottom = "77%",
         left = "25%") %>%
  e_grid(height = "25%", 
         bottom = "67%",
         left = "25%") %>%
  e_grid(height = "25%", 
         bottom = "57%",
         left = "25%") %>%
  e_grid(height = "25%", 
         bottom = "47%",
         left = "25%") %>%
  e_grid(height = "25%", 
         bottom = "37%",
         left = "25%") %>%
  e_grid(height = "25%", 
         bottom = "27%",
         left = "25%") %>%
  e_grid(height = "25%", 
         bottom = "17%",
         left = "25%") %>%
  e_grid(height = "25%",
         bottom = "7%",
         left = "25%") %>%
  e_tooltip(trigger = "item") %>%
  # e_legend(bottom = 1, textStyle = list(color = "#bdbdbd")) %>%
  # e_title(text = "Expected Returns",
  #         textStyle = list(fontSize = 22)) %>%
  e_y_axis(max = 30, gridIndex = 0, index = 0,
           axisLine = list(show = F, lineStyle = list(width = 0)),
           splitLine = list(show = F),
           axisTick = list(show = F),
           axisLabel = list(show = F)) %>% # add the grids
  e_x_axis(min = -0.05, max = 0.15, gridIndex = 0, index = 0,
           name = "BlackRock",
           nameLocation = "start",
           nameGap = 5,
           nameTextStyle = list(color = "#333", fontStyle = "bolder", fontSize = 14),
           axisLabel = list(show = F),
           axisTick = list(show = F),
           splitLine = list(show = F)
  ) %>%
  e_y_axis(max = 30, gridIndex = 1, index = 1,
           axisLine = list(show = F, lineStyle = list(width = 0)),
           splitLine = list(show = F),
           axisTick = list(show = F),
           axisLabel = list(show = F)) %>%
  e_x_axis(min = -0.05, max = 0.15, gridIndex = 1, index = 1,
           name = "Plan Assumptions",
           nameLocation = "start",
           nameGap = 5,
           nameTextStyle = list(color = "#333", fontStyle = "bolder", fontSize = 14),
           axisLabel = list(show = F),
           axisTick = list(show = F),
           # splitLine = list(show = F)
  ) %>%
  e_y_axis(max = 30, gridIndex = 2, index = 2,
           axisLine = list(show = F, lineStyle = list(width = 0)),
           splitLine = list(show = F),
           axisTick = list(show = F),
           axisLabel = list(show = F)) %>%
  e_x_axis(min = -0.05, max = 0.15, gridIndex = 2, index = 2,
           name = "Horizon 20-Year",
           nameLocation = "start",
           nameGap = 5,
           nameTextStyle = list(color = "#333", fontStyle = "bolder", fontSize = 14),
           axisLabel = list(show = F),
           axisTick = list(show = F),
           # splitLine = list(show = F)
  ) %>%
  e_y_axis(max = 30, gridIndex = 3, index = 3,
           axisLine = list(show = F, lineStyle = list(width = 0)),
           splitLine = list(show = F),
           axisTick = list(show = F),
           axisLabel = list(show = F)) %>%
  e_x_axis(min = -0.05, max = 0.15, gridIndex = 3, index = 3,
           name = "Horizon 10-Year",
           nameLocation = "start",
           nameGap = 5,
           nameTextStyle = list(color = "#333", fontStyle = "bolder", fontSize = 14),
           axisLabel = list(show = F),
           axisTick = list(show = F),
           # splitLine = list(show = F)
  ) %>%
  e_y_axis(max = 30, gridIndex = 4, index = 4,
           axisLine = list(show = F, lineStyle = list(width = 0)),
           splitLine = list(show = F),
           axisTick = list(show = F),
           axisLabel = list(show = F)) %>%
  e_x_axis(min = -0.05, max = 0.15, gridIndex = 4, index = 4,
           name = "Historical",
           nameLocation = "start",
           nameGap = 5,
           nameTextStyle = list(color = "#333", fontStyle = "bolder", fontSize = 14),
           axisLabel = list(show = F),
           axisTick = list(show = F),
           # splitLine = list(show = F)
  ) %>%
  e_y_axis(max = 30, gridIndex = 5, index = 5,
           axisLine = list(show = F, lineStyle = list(width = 0)),
           splitLine = list(show = F),
           axisTick = list(show = F),
           axisLabel = list(show = F)) %>%
  e_x_axis(min = -0.05, max = 0.15, gridIndex = 5, index = 5,
           name = "BNY Mellon",
           nameLocation = "start",
           nameGap = 5,
           nameTextStyle = list(color = "#333", fontStyle = "bolder", fontSize = 14),
           axisLabel = list(show = F),
           axisTick = list(show = F),
           # splitLine = list(show = F)
  ) %>%
  e_y_axis(max = 30, gridIndex = 6, index = 6,
           axisLine = list(show = F, lineStyle = list(width = 0)),
           splitLine = list(show = F),
           axisTick = list(show = F),
           axisLabel = list(show = F)) %>%
  e_x_axis(min = -0.05, max = 0.15, gridIndex = 6, index = 6,
           name = "JP Morgan",
           fontSize = 14,
           nameLocation = "start",
           nameGap = 5,
           nameTextStyle = list(color = "#333", fontStyle = "bolder", fontSize = 14),
           axisLabel = list(show = F),
           axisTick = list(show = F)) %>%
  e_y_axis(max = 30, gridIndex = 7, index = 7,
           axisLine = list(show = F, lineStyle = list(width = 0)),
           splitLine = list(show = F),
           axisTick = list(show = F),
           axisLabel = list(show = F)) %>%
  e_x_axis(min = -0.05, max = 0.15, gridIndex = 7, index = 7,
           name = "Research Affiliates",
           nameLocation = "start",
           nameGap = 5,
           nameTextStyle = list(color = "#333", fontStyle = "bolder", fontSize = 14),
           axisTick = list(show = F),
           formatter = echarts4r::e_axis_formatter(style = "percent"),
           axisLabel = list(fontSize = 14, color = "#333", fontStyle = "bold")
  ) %>%
  echarts4r::e_mark_line(data = list(xAxis = 0.0765,
                                     lineStyle = list(color = "#d3d3d3",
                                                      width = 2,
                                                      cap = "butt",
                                                      type = "solid")),
                         title = "",
                         precision = 100,
                         label = list(fontStyle = "bold",
                                      textBorderWidth = 4,           
                                      textBorderColor = "#fff",
                                      fontSize = 12),
                         symbol = "none",
                         zlevel = 0) %>%
  e_legend(show = F) |>
  e_text_g(top = "7%",
           left = "62%",
           z = 999,
           background = "#fff",
           style = list(text = "7.65%",
                        fontStyle = "bolder",
                        fontSize = 15))


# saveRDS(density, "data/historical/MPERS_density.rds")


# Table - Probability Analysis

probability <- readr::read_csv("data/historical/MPERS_return_probabilities.csv")

probability <- probability |>
  select(Return, `Plan Assumptions`, Historical, Horizon10, `BNY Mellon`, `JP Morgan`, `Research Affiliates`, Horizon20, BlackRock)

table <- reactable::reactable(
  probability,
  striped = T,
  highlight = T,
  columnGroups = list(
    reactable::colGroup(name = "System Assumptions & Experience", columns = c("Plan Assumptions", "Historical"),
                        headerStyle = list(background = "#9dc3e6")),
    reactable::colGroup(name = "Short-Term Market Forecast", columns = c("Horizon10", "BNY Mellon", "JP Morgan", "Research Affiliates"),
                        headerStyle = list(background = "#c5e0b4")),
    reactable::colGroup(name = "Long-Term Market Forecast", columns = c("Horizon20", "BlackRock"),
                        headerStyle = list(background = "#f8cbad")),
    reactable::colGroup(name = "", columns = "Return",
                        headerStyle = list(background = "#333f50", borderColor = "#333f50"))
  ),
  rowStyle = function(index) {
    if (probability[index, "Return"] == 0.0765) {
      list(borderColor = "#ff6633")
    }
  },
  defaultColDef = reactable::colDef(
    header = function(value) gsub(".", " ", value, fixed = T),
    headerStyle = list(background = "#333f50", color = "#FFF"),
    align = "center"),
  
  columns = list(
    Return = reactable::colDef(
      format = reactable::colFormat(percent = T, digits = 2),
      headerStyle = list(position = "sticky", left = 0, background = "#333f50", color = "#FFF", zIndex = 1,
                         borderRight = "1px solid #eee"),
      style = function(value) {
        if (value > 0.0765) {
          color <- "#008000"
        } else if (value < 0.0765) {
          color <- "#e00000"
        } else {
          color <- "#333"
        }
        list(color = color, fontWeight = "bold", position = "sticky", background = "#d6dce5", left = 0, color = "#FFF", zIndex = 1,
             borderRight = "1px solid #eee", stripedColor = "#d6dce5")
        
      }
    ),
    `Plan Assumptions` = reactable::colDef(
      width = 120,
      format = reactable::colFormat(percent = T, digits = 0),
      style = function(value) {
        if (value > 0.48 & value < 0.52) {
          background <- "#f4b183"
          list(background = background)
        } else {
        }
      }
    ),
    Horizon20 = reactable::colDef(
      format = reactable::colFormat(percent = T, digits = 0),
      name = "Horizon 20-Year Market Forecast",
      style = function(value) {
        if (value > 0.45 & value < 0.54) {
          background <- "#f4b183"
          list(background = background)
        } else {
        }
      }
    ),
    Horizon10 = reactable::colDef(
      format = reactable::colFormat(percent = T, digits = 0),
      name = "Horizon 10-Year Forecast",
      style = function(value) {
        if (value > 0.45 & value < 0.52) {
          background <- "#f4b183"
          list(background = background)
        } else {
        }
      }
    ),
    Historical = reactable::colDef(
      format = reactable::colFormat(percent = T, digits = 0),
      style = function(value) {
        if (value > 0.46 & value < 0.54) {
          background <- "#f4b183"
          list(background = background)
        } else {
        }
      }
    ),
    `BNY Mellon` = reactable::colDef(
      format = reactable::colFormat(percent = T, digits = 0),
      name = "BNY Mellon 10-Year Forecast",
      style = function(value) {
        if (value > 0.42 & value < 0.56) {
          background <- "#f4b183"
          list(background = background)
        } else {
        }
      }
    ),
    `JP Morgan` = reactable::colDef(
      format = reactable::colFormat(percent = T, digits = 0),
      name = "JP Morgan 10-15 Year Forecast",
      style = function(value) {
        if (value > 0.42 & value < 0.58) {
          background <- "#f4b183"
          list(background = background)
        } else {
        }
      }
    ),
    `Research Affiliates` = reactable::colDef(
      format = reactable::colFormat(percent = T, digits = 0),
      name = "Research Affiliates 10-Year Forecast",
      style = function(value) {
        if (value > 0.45 & value < 0.52) {
          background <- "#f4b183"
          list(background = background)
        } else {
        }
      }
    ),

    BlackRock = reactable::colDef(
      format = reactable::colFormat(percent = T, digits = 0),
      name = "BlackRock 20-Year Forecast",
      style = function(value) {
        if (value > 0.47 & value < 0.54) {
          background <- "#f4b183"
          list(background = background)
        } else {
        }
      }
    )
    
  ),
  theme = reactable::reactableTheme(
    background = "#d6dce5",
    stripedColor = "#adb9ca",
    highlightColor = "#f4b183",
    #backgroundColor = "#d6dce5",
    #cellStyle = list(background = "#d6dce5"),
    cellPadding = "8px 10px",
    style = list(fontFamily = "'Open Sans', sans-serif"),
    searchInputStyle = list(width = "100%")
  )
)


# saveRDS(table, "data/historical/MPERS_table.rds")




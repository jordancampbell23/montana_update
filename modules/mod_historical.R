#--------------------------------Login for App---------------------------------#


# UI

mod_historical_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
            div(
        style = "margin: 0px 40px 0 40px; padding-top: 50px;",
        h2("Assets & Liabilities", style = "margin-bottom: -15px;"),
      fluidRow(
                         fluidRow(
                           style = "margin: 0; height: 45%",
                           
                           column(
                             width = 9,
                             style = "height: 100%; padding-top: 0px;",
                             text_card(
                               shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart1"), height = "35vh"))
                             )
                           ),
                           
                           column(
                             width = 3,
                             align = "left",
                             style = "padding-top: 20px;",
                             
                             radioGroupButtons(
                               inputId = ns("ava_mva_i"),
                               label = NULL,
                               justified = T,
                               selected = "AVA",
                               choices = c("AVA", "MVA")),
                             
                             # data_card(
                               h4(strong("Unfunded Liabilities")),
                               
                               p("This plot shows unfunded accrued liability (UAL) in billions of dollars and the plan's funded ratio. The button above switches between AVA and MVA.",
                                 "AVA is the actuarial value of assets which is the value of a plan’s assets with gains and losses that are smoothed over time.",
                                 "MVA is the market value of assets.", style = "font-size: 14px")
                             # )
                           )
                         ),
                         
                         fluidRow(
                           style = "margin: 0; height: 41%",
                           
                           column(
                             width = 9,
                             style = "height: 100%; padding-top: 0px; margin-bottom: -50px;",
                             data_card(
                               shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart2"), height = "35vh"))
                             )
                           ),
                           
                           column(
                             width = 3,
                             align = "left",
                             style = "padding-top: 20px; margin-bottom: -50px;",
                             
                             radioGroupButtons(
                               inputId = ns("ava_mva_ii"),
                               label = NULL,
                               justified = T,
                               selected = "AVA",
                               choices = c("AVA", "MVA")),
                             
                             # text_card(
                               h4(strong("Assets v. Liability")),
                               p("This plot displays actuarial value of assets (AVA) or market value of assets (MVA) and actuarial accrued liability (AAL). The button above switches between between AVA and MVA.", style = "font-size: 14px")
                             # )
                           )
                         ))),
      
      br(),
      hr(),
      div(
        style = "margin: 50px 50px 0 50px;",
        h2("Debt", style = "margin-bottom: -15px;"),
      fluidRow(
        fluidRow(
          style = "margin: 0; height: 45%",

          column(
            width = 3,
            align = "left",
            style = "height: 75vh; margin-top: 25px; margin-bottom: -5px; overflow: auto; font-size: 14px; background-color: #fff;",
            # data_card(
              h4(strong("Drivers of PERS Debt\n(2000-2021)")),
              p(strong("Investment Performance"), "below projects has added $1.16 billion to unfunded liabilities."),
              p(strong("Changes to Actuarial Methods & Assumptions"), " updated actuarial methods and assumptions revealed $540 million in additional unfunded liabilities."),
              p(strong("Changes to Benefits"), " changes have added a net $508 million in unfunded liabilities."),
              p(strong(a("Negative Amortization", href = "https://www.investopedia.com/terms/n/negativeamortization.asp", style = "text-decoration: underline; color: #ff6633")), " has resulted in interest on PERS debt exceeding the actual debt payments (negative amortization), adding $321.6 million in unfunded liabilities."),
              p(strong("Other"), " changes have added a net $177 million in unfunded liabilities."),
              p(strong("Unclassified Liability Gain/Loss"), " changes have added a net $56 million in unfunded liabilities."),
              p(strong("Deviations from Demographic Assumptions"), " updated demographic assumptions exposed $39 million in previously unrecognized unfunded liabilities."),
              p(strong("Gains from Expected Pay Increases Not Given"), " reduced unfunded liabilities by $200 million."),
              p(strong("Net Change"), ", from 2000 to 2021, unfunded liabilities increased by $2.6 billion")
              
            # )
          ),
          
          column(
            width = 9,
            style = "height: 100%; padding-top: -10px;",
            data_card(
              shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart3"), height = "75vh"))
            )
          )
        ),
        
        fluidRow(
            style = "margin: 0; height: 40%",
            column(
              width = 3,
              align = "left",
              style = "padding-top: 20px;",

              # data_card(
              h4(strong("Placeholder")),
              p("...", style = "font-size: 14px")
              # )
            ),
            column(
              width = 9,
              align = "left",
              style = "padding-top: -10px;",
              data_card(
                shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart4"), height = "35vh"))
              )
            )
          ),
          fluidRow(
            style = "margin: 0; height: 40%",
            column(
              width = 3,
              align = "left",
              style = "padding-top: 20px; margin-bottom: -50px",
              h4(strong("Placeholder")),
              p("...", style = "font-size: 14px")
            ),
            column(
              width = 9,
              style = "height: 100%; padding-top: -10px; margin-bottom: -50px",
              data_card(
                shinycssloaders::withSpinner(shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart5"), height = "35vh")))
              )
            )
          )
      )),
      
            br(),
      hr(),
      div(
        style = "margin: 50px 50px 0 50px;",
        h2("Assets & Returns", style = "margin-bottom: -15px;"),
      fluidRow(
        fluidRow(
          style = "margin: 0; height: 40%",
          column(
            width = 9,
            style = "height: 100%; padding-top: -10px;",
            text_card(
              shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart6"), height = "60vh"))
            )
          ),
          column(
            width = 3,
            align = "left",
            style = "margin-top: 25px; height: 60vh; overflow: auto; font-size: 14px;  background-color: #fff;",
            
            h4(strong("Investment Returns")),
            
            shinyWidgets::pickerInput(
              inputId = ns("rolling"),
              label = strong("Rolling return period:"),
              selected = "10-Year",
              choices = c("5-Year", "10-Year", "15-Year")
            ),
            
            # data_card(
            
            p("The plot to the left compares assumed returns with market, actuarial, and geometric rolling returns. The table provides the geomtetric average return for the last 5, 10, 15, and 20 years.", style = "font-size: 14px"),
            reactable::reactableOutput(ns("table"))
            # )
          )
        ),
        
        fluidRow(
          style = "margin: 0; height: 41%",
          
          column(
            width = 9,
            style = "height: 100%; padding-top: 0px; margin-bottom: -50px;",
            data_card(
              shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart7"), height = "35vh"))
            )
          ),
          
          column(
            width = 3,
            align = "left",
            style = "padding-top: 20px; margin-bottom: -50px; height: 36vh; overflow: auto; font-size: 14px",

            h4(strong("Asset Allocation")),
            
            p(strong("Fixed Income: "), "Includes government bonds, corporate bonds, and cash."),
            p(strong("Public Equities: "), "Includes a variety of public equities."),
            p(strong("Private Equity: "), "Private equity holdings."),
            p(strong("Other Alternatives: "), "Includes real estate, commodities, and other alternative investments.")

          )
        )
      )
      ),
      
      br(),
      hr(),
        h2("Return Probability Analysis"),
        br(),
      fluidRow(
        div(
          align = "center",
          style = "height: 20px; margin-top: 10px; margin-botton: -10px;",
        
        fluidRow(
          style = "margin: 0;",
          
          fluidRow(
            column(1),
            column(
              width = 10,
              align = "left",
              style = "margin-top: -10px; overflow: auto; font-family: 'Open Sans', sans-serif; background-color: #f5f5f5;",
                div(
                  style = "background-color: #9dc3e6; border-width: 2px; border-color: #333333; border-style: solid;",
                  h4(strong("System Assumptions & Experience"), align = "left", style = "margin: 10px")
                ),
                br(),
                tags$ul(
                  tags$li("A probability analysis of PERS historical returns over the past 21 years (2001-2021) indicate a 25% chance of hitting the plan’s 7.65% assumed return.", style = "font-family: 'Open Sans', sans-serif; font-family: 'Roboto', sans-serif; font-size: 14px;"),
                  br(),
                  tags$li("PERS’s own investment return assumptions imply a 51% chance of achieving their investment return target over the next 20 years.", style = "font-family: 'Open Sans', sans-serif; font-family: 'Roboto', sans-serif; font-size: 14px;")
                ),
                br(),
                div(
                  style = "background-color: #c5e0b4; border-width: 2px; border-color: #333333; border-style: solid;",
                  h4(strong("Short-Term Market Forecast"), align = "left", style = "margin: 10px")
                ),
                br(),
                tags$ul(
                  tags$li("Returns over the short to medium term can have significant negative effects on funding outcomes for mature pension plans with large negative cash flows like PERS", style = "font-family: 'Open Sans', sans-serif; font-family: 'Roboto', sans-serif; font-size: 14px;"),
                  br(),
                  tags$li("Analysis of capital market assumptions publicly reported by the leading financial firms (BlackRock, JP Morgan, BNY Mellon, and Research Affiliates) suggests that over a 10-15 year period, PERS returns are likely to fall short of their assumption.", style = "font-family: 'Open Sans', sans-serif; font-family: 'Roboto', sans-serif; font-size: 14px;")
                ),
                br(),
                div(
                  style = "background-color: #f8cbad; border-width: 2px; border-color: #333333; border-style: solid;",
                  h4(strong("Long-Term Market Forecast"), align = "left", style = "margin: 10px")
                ),
                br(),
                tags$ul(
                  tags$li("Longer-term projections typically assume PERS investment returns will revert back to historical averages.", style = "font-family: 'Open Sans', sans-serif; font-family: 'Roboto', sans-serif; font-size: 14px;"),
                  tags$ul(
                    tags$li("The “reversion to mean” assumption should be viewed with caution given historical changes in interest rates and a variety of other market conditions that increase uncertainty over longer projection periods, relative to shorter ones.", style = "font-family: 'Open Sans', sans-serif; font-family: 'Roboto', sans-serif; font-size: 14px;")),
                  br(),
                  tags$li("Forecasts showing long-term returns near 7.65% as likely also show a significant chance that the actual long-term average return will fall below that figure.", style = "font-family: 'Open Sans', sans-serif; font-family: 'Roboto', sans-serif; font-size: 14px;"),
                  tags$ul(
                    tags$li("For example, according to the BlackRock’s 20-year forecast, while the probability of achieving an average return of 8% or higher is 52%, the probability of earning a rate of return below 6% is about 24%.", style = "font-family: 'Open Sans', sans-serif; font-family: 'Roboto', sans-serif; font-size: 14px;"))
                )
            ),
            column(1)
          ),

          fluidRow(
            column(1),
            column(10,
              h3("Table")
            ),
            column(1)
          ),

          fluidRow(
            column(1),
            column(
              width = 10,
              style = "margin-top: -10px;",
            data_card(
              shinycssloaders::withSpinner(reactable::reactableOutput(ns("prob_table")))
              )
            ),
            column(1)
          ),
          br(),
          fluidRow(
            column(1),
            column(10,
              h3("Return Distributions")
            ),
            column(1)
          ),
          fluidRow(
            
            column(2),

            column(
              width = 8,
              style = "margin-top: -20px; z-index: 0",
              data_card(
                shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart8"), height = "45vh"))
            )),
            
            column(2)
          )
        )
        )
      )
    )
  
}


# Server

mod_historical_server <- function(id, state) {
  server <- function(input, output, session) {
    ns <- session$ns
    
    
    output$chart1 <- echarts4r::renderEcharts4r({
      
      df <- readr::read_csv("data/historical/MPERS_mva_ava_aal.csv")
      
      df <- df |>
        dplyr::mutate(year = as.character(year))
      
      if(input$ava_mva_i == "AVA") {
        
        df <- df |>
          dplyr::mutate(`Funding Ratio` = ava/aal) |>
          dplyr::mutate(`Unfunded Accrued Liability` = round((aal - ava)/1e9, 3))
        
        df |>
          echarts4r::e_charts(year, areaStyle = list(opacity = 1)) |>
          echarts4r::e_line(name = "Funded Ratio (AVA)",
                            `Funding Ratio`,
                            y_index = 1,
                            symbol = "none",
                            color = "#6699CC",
                            lineStyle = list(width = 4),
                            z = 999) |>
          echarts4r::e_y_axis(name = "Billions",
                              axisLabel = list(fontSize = 14),
                              nameLocation = "middle",
                              nameTextStyle = list(
                                fontSize = 14,
                                fontStyle = 'bold',
                                padding = c(2, 2, 40, 2)),
                              formatter = echarts4r::e_axis_formatter(style = "currency", digits = 1)) |>
          echarts4r::e_y_axis(splitLine = list(show = FALSE),
                              axisLabel = list(fontSize = 14),
                              index = 1,
                              formatter = echarts4r::e_axis_formatter(style = "percent",
                                                                      digits = 0))  |>
          echarts4r::e_x_axis(axisLabel = list(fontSize = 14),
                              axisTick = list(alignWithLabel = T)) |>
          echarts4r::e_area(name = "Unfunded Accrued Liability (AVA)",
                            `Unfunded Accrued Liability`, symbol = "none", 
                            color = "#cc0000",
                            lineStyle = list(width = 0.1, shadowBlur = 0)) |>
          echarts4r::e_legend(icons = c('roundRect', 'roundRect', 'roundRect'),
                              selectedMode = F,
                              textStyle = list(fontSize = 14)) |>
          echarts4r::e_tooltip(trigger = "axis",
                               # axisPointer = list(lineStyle = list(type = 'solid')),
                               formatter = htmlwidgets::JS("
             function(params){
             if (params[1].value[1] != '-')
             return(
             params[0].value[0] + '<br/>' +
             params[0].marker + 'Funded Ratio (AVA): ' + (params[0].value[1] * 100).toFixed(2) + '%' +  '<br/>' +
             params[1].marker + 'UAL (AVA): ' + '$' +  echarts.format.addCommas(params[1].value[1]) + '<br/>'
             )}")
          ) |>
          echarts4r::e_theme_custom("echarts_theme.json") |>
          echarts4r::e_grid(left = "15%", bottom = "10%")
        
      } else {
        
        df <- df |>
          dplyr::mutate(`Funding Ratio` = mva/aal) |>
          dplyr::mutate(`Unfunded Accrued Liability` = round((aal - mva)/1e9, 3))
        
        df |>
          echarts4r::e_charts(year, areaStyle = list(opacity = 1)) |>
          echarts4r::e_line(name = "Funded Ratio (MVA)",
                            `Funding Ratio`,
                            y_index = 1,
                            symbol = "none",
                            color = "#6699CC",
                            lineStyle = list(width = 4),
                            z = 999) |>
          echarts4r::e_y_axis(name = "Billions",
                              axisLabel = list(fontSize = 14),
                              nameLocation = "middle",
                              nameTextStyle = list(
                                fontSize = 14,
                                fontStyle = 'bold',
                                padding = c(2, 2, 40, 2)),
                              formatter = echarts4r::e_axis_formatter(style = "currency", digits = 1)) |>
          echarts4r::e_y_axis(splitLine = list(show = FALSE),
                              axisLabel = list(fontSize = 14),
                              index = 1,
                              formatter = echarts4r::e_axis_formatter(style = "percent",
                                                                      digits = 0))  |>
          echarts4r::e_x_axis(axisLabel = list(fontSize = 14),
                              axisTick = list(alignWithLabel = T)) |>
          echarts4r::e_area(name = "Unfunded Accrued Liability (MVA)",
                            `Unfunded Accrued Liability`, symbol = "none", 
                            color = "#cc0000",
                            lineStyle = list(width = 0.1, shadowBlur = 0)) |>
          echarts4r::e_legend(icons = c('roundRect', 'roundRect', 'roundRect'),
                              selectedMode = F,
                              textStyle = list(fontSize = 14)) |>
          echarts4r::e_tooltip(trigger = "axis",
                               # axisPointer = list(lineStyle = list(type = 'solid')),
                               formatter = htmlwidgets::JS("
             function(params){
             if (params[1].value[1] != '-')
             return(
             params[0].value[0] + '<br/>' +
             params[0].marker + 'Funded Ratio (MVA): ' + (params[0].value[1] * 100).toFixed(2) + '%' +  '<br/>' +
             params[1].marker + 'UAL (MVA): ' + '$' +  echarts.format.addCommas(params[1].value[1]) + '<br/>'
             )}")
          ) |>
          echarts4r::e_theme_custom("echarts_theme.json") |>
          echarts4r::e_grid(left = "15%", bottom = "10%")
      }
      
    })
    
    output$chart2 <- echarts4r::renderEcharts4r({
      
      df <- readr::read_csv("data/historical/MPERS_mva_ava_aal.csv")
      
      df <- df |>
        dplyr::mutate(year = as.character(year)) |>
        dplyr::mutate(`Actuarially Accrued Liability` = round(aal/1e9, 3)) |>
        dplyr::mutate(`Market Value of Assets` = round(mva/1e9, 3)) |>
        dplyr::mutate(`Actuarial Value of Assets` = round(ava/1e9, 3))
      
      if(input$ava_mva_ii == "AVA") {
        
        df |>
          echarts4r::e_chart(year) |>
          echarts4r::e_line(`Actuarial Value of Assets`,
                            symbol = "none",
                            color = "#FF6633",
                            lineStyle = list(width = 3)) |>
          echarts4r::e_line(`Actuarially Accrued Liability`,
                            name = "Actuarial Accrued Liability",
                            symbol = "none",
                            color = "#2879CB",
                            lineStyle = list(width = 3)) |>
          echarts4r::e_band2(`Actuarial Value of Assets`, `Actuarially Accrued Liability`, name = "Unfunded Liability",
                             legend = F,
                             color = "#D3D3D3",
                             itemStyle = list(borderWidth = 0)) |>
          echarts4r::e_tooltip(trigger = "axis",
                               # axisPointer = list(lineStyle = list(type = 'solid')),
                               formatter = htmlwidgets::JS("
             function(params){
             return(
             params[0].value[0]
             + '<br/>' +
             params[1].marker + ' AAL: ' + '$' + echarts.format.addCommas(params[1].value[1]) + '<br/>' +
             params[0].marker + ' AVA: ' + '$' + echarts.format.addCommas(params[0].value[1]) + '<br/>' +
             params[2].marker + 'Unfunded Liability: ' + '$' + echarts.format.addCommas((params[1].value[1] - params[0].value[1]).toFixed(3)) 
             )}")
          ) |>
          echarts4r::e_legend(icons = c('roundRect', 'roundRect'),
                              selectedMode = F,
                              textStyle = list(fontSize = 14)) |>
          echarts4r::e_y_axis(name = "Billions",
                              axisLabel = list(fontSize = 14),
                              nameLocation = "middle",
                              formatter = echarts4r::e_axis_formatter(style = "currency",
                                                                      digits = 0),
                              nameTextStyle = list(
                                fontSize = 14,
                                fontStyle = 'bold',
                                padding = c(2, 2, 20, 2))) |>
          echarts4r::e_x_axis(axisLabel = list(fontSize = 14),
                              axisTick = list(alignWithLabel = T)) |>
          echarts4r::e_theme_custom("echarts_theme.json") |>
          echarts4r::e_grid(left = "15%", bottom = "10%")
        
      } else {
        
        df |>
          echarts4r::e_chart(year) |>
          echarts4r::e_line(`Market Value of Assets`,
                            symbol = "none",
                            color = "#FF6633",
                            lineStyle = list(width = 3)) |>
          echarts4r::e_line(`Actuarially Accrued Liability`,
                            symbol = "none",
                            color = "#2879CB",
                            lineStyle = list(width = 3)) |>
          echarts4r::e_band2(`Market Value of Assets`, `Actuarially Accrued Liability`, name = "Unfunded Liability",
                             legend = F,
                             color = "#D3D3D3",
                             itemStyle = list(borderWidth = 0)) |>
          echarts4r::e_tooltip(trigger = "axis",
                               # axisPointer = list(lineStyle = list(type = 'solid')),
                               formatter = htmlwidgets::JS("
             function(params){
             return(
             params[0].value[0]
             + '<br/>' +
             params[1].marker + ' AAL: ' + '$' + echarts.format.addCommas(params[1].value[1]) + '<br/>' +
             params[0].marker + ' MVA: ' + '$' + echarts.format.addCommas(params[0].value[1]) + '<br/>' +
             params[2].marker + 'Unfunded Liability: ' + '$' + echarts.format.addCommas((params[1].value[1] - params[0].value[1]).toFixed(3))
             )}")
          ) |>
          echarts4r::e_legend(icons = c('roundRect', 'roundRect'),
                              selectedMode = F,
                              textStyle = list(fontSize = 14)) |>
          echarts4r::e_y_axis(name = "Billions",
                              axisLabel = list(fontSize = 14),
                              nameLocation = "middle",
                              formatter = echarts4r::e_axis_formatter(style = "currency",
                                                                      digits = 0),
                              nameTextStyle = list(
                                fontSize = 14,
                                fontStyle = 'bold',
                                padding = c(2, 2, 20, 2))) |>
          echarts4r::e_x_axis(axisLabel = list(fontSize = 14),
                              axisTick = list(alignWithLabel = T)) |>
          echarts4r::e_theme_custom("echarts_theme.json") |>
          echarts4r::e_grid(left = "15%", bottom = "10%")
        
      }
      
    })
    
    
    output$chart3 <- echarts4r::renderEcharts4r({
    
      gainloss <- readr::read_csv("data/historical/MPERS_gain_loss.csv")
      # gainloss <-gainloss[1:7, ]
      
      gainloss|>
        mutate(name_label = stringr::str_wrap(name_label, 12)) |>
        rowwise() |>
        mutate(total = sum(abs(gain), abs(gain_ii), abs(loss), abs(loss_ii), net, na.rm = T)) |>
        e_charts(name_label, stack = "group",  barCategoryGap = "10%")|>
        e_bar(base, color = "#1C00ff00", legend = F, tooltip = list(show = F))|>
        e_bar(name = "Increase UAAL", loss, color = "#CC0000")|>
        e_bar(name = "Decrease UAAL", gain, color = "#669900")|>
        e_bar(name = "Increase UAAL < 0", loss_ii, color = "#CC0000", legend = F)|>
        e_bar(name = "Decrease UAAL < 0", gain_ii, color = "#669900", legend = F)|>
        e_bar(name = "Net Change in UAAL", net, color = "#FF6633")|>
        e_bar(total, color = "#1C00ff00", legend = F)|>
        e_y_axis(name = "Millions",
                 axisLabel = list(fontSize = 12),
                 nameLocation = "middle",
                 max = 3000,
                 formatter = e_axis_formatter(style = "currency",
                                              digits = 0),
                 nameTextStyle = list(
                   fontSize = 14,
                   fontStyle = 'bold',
                   padding = c(2, 2, 40, 2)))|>
        e_x_axis(axisLabel = list(interval = 0L, fontSize = 12))|>
        e_tooltip(trigger = "axis",
                  confine = T,
                  axisPointer = list(type = 'shadow',
                                     shadowStyle = list(color = 'rgba(14, 43, 72, 0.1)')),
                  formatter = htmlwidgets::JS("function (params) {return params[0].value[0] + '</br>'
                                            + ' $' + echarts.format.addCommas((1 * params[5].value[1]).toFixed(3)) + ' M'}"))|>
        e_legend(icons = c('roundRect', 'roundRect', 'roundRect', 'roundRect', 'roundRect'),
                 selectedMode = F,
                 textStyle = list(fontSize = 14))|>
        e_grid(left = "12%", top = "10%", bottom = "23%") |>
        e_theme_custom("echarts_theme.json")
      
    })
    
    
    output$chart5 <- echarts4r::renderEcharts4r({
      
      cash_flow <- readr::read_csv("data/historical/MPERS_excess_contribution.csv")
      
      cash_flow <- cash_flow |>
        dplyr::filter(`Year` >= 2004) |>
        dplyr::mutate(year = as.character(Year)) |>
        dplyr::mutate(Contributions = Contributions/1e3) |>
        dplyr::mutate(Interest = Interest/1e3) |>
        dplyr::mutate(`Negative Amo` = `Negative Amo`/1e3)
      
      contributions <- cash_flow |>
        echarts4r::e_charts(year, barGap = "0%") |>
        echarts4r::e_bar(Contributions,
                         name = "Amortization Payment",
                         color = "#2879CB",
                         barWidth = "25%") |>
        echarts4r::e_bar(Interest,
                         name = "Interest on Debt",
                         color = "#ff6633", 
                         barWidth = "25%") |>
        echarts4r::e_bar(`Negative Amo`,
                         name = "Net Amortization",
                         color = "#00b0f0", 
                         barWidth = "25%") |>
        echarts4r::e_y_axis(
          name = "Millions",
          nameLocation = "middle",
          axisLabel = list(fontSize = 14),
          nameTextStyle = list(
            fontSize = 14,
            fontStyle = 'bold',
            padding = c(2, 2, 30, 2)),
          formatter = echarts4r::e_axis_formatter(style = "currency",
                                                  digits = 0)) |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 14),
                            axisTick = list(alignWithLabel = T)) |>
        echarts4r::e_legend(icons = c('roundRect', 'roundRect', 'roundRect'),
                            textStyle = list(fontSize = 14)) |>
        echarts4r::e_tooltip(trigger = "axis",
                             confine = T,
                             axisPointer = list(type = 'shadow',
                                                shadowStyle = list(color = 'rgba(14, 43, 72, 0.1)')),
                             formatter = echarts4r::e_tooltip_pointer_formatter(style = "currency",
                                                                                digits = 3)) |>
        echarts4r::e_axis_pointer(label = list(show = F)) |>
        echarts4r::e_theme_custom("echarts_theme.json") |>
        # echarts4r::e_title("Interest on Debt vs. Amortization Payments") |>
        echarts4r::e_grid(left = "15%", bottom = "10%")
      
      
    })
    
    
    output$chart4 <- echarts4r::renderEcharts4r({
      
      amo_interest <- readr::read_csv("data/historical/MPERS_net_interest.csv")
      
      amo_interest <- amo_interest |>
        mutate(Year = as.character(Year)) |>
        dplyr::mutate(`Net Amo` = `Net Amo` / 1e3) |>
        dplyr::mutate(`Remaining UAL` = `Remaining UAL` / 1e3)
      
      amo_plot <- amo_interest |>
        echarts4r::e_chart(Year, stack = "group", areaStyle = list(opacity = 1)) |>
        echarts4r::e_area(`Remaining UAL`, 
                          name = "Unfunded Liabilities from Other Sources",
                          symbol = "none",
                          color = "#ff6633") |>
        echarts4r::e_area(`Net Amo`,
                          name = "Cumulative Unpaid Interest on UAAL",
                          symbol = "none", 
                          color = "#7a8ca9") |>
        echarts4r::e_y_axis(name = "Millions",
                            axisLabel = list(fontSize = 14),
                            nameLocation = "middle",
                            nameTextStyle = list(
                              fontSize = 14,
                              fontStyle = 'bold',
                              padding = c(2, 2, 40, 2)),
                            formatter = echarts4r::e_axis_formatter(style = "currency", digits = 0)) |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 14),
                            axisTick = list(alignWithLabel = T)) |>
        echarts4r::e_legend(icons = c('roundRect', 'roundRect', 'roundRect', 'roundRect'),
                            textStyle = list(fontSize = 14)) |>
        echarts4r::e_tooltip(trigger = "axis",
                             confine = T,
                             formatter = echarts4r::e_tooltip_pointer_formatter(style = "currency",
                                                                                digits = 3)) |>
        echarts4r::e_axis_pointer(label = list(show = F)) |>
        echarts4r::e_theme_custom("echarts_theme.json") |>
        # echarts4r::e_title("Net Interest on UAAL") |>
        echarts4r::e_grid(left = "15%", bottom = "10%")
      
    })
    
    
    
    output$table <- reactable::renderReactable({
      
      geomReturn <- function(returns){
        prod(1 + returns)^(1/length(returns)) - 1 
      }
      
      
      returns <- readr::read_csv("data/historical/investment_returns.csv")
      
      five <- geomReturn(returns$mva[17:21])
      ten <- geomReturn(returns$mva[12:21])
      fifteen <- geomReturn(returns$mva[7:21])
      twenty <- geomReturn(returns$mva[2:21])
      
      df <- data.frame(`Time Range` = c("5 Years", "10 Years", "15 Years", "20 Years"),
                       `Geometric Average Return` = c(five, ten, fifteen, twenty))
      
      reactable::reactable(
        df,
        striped = T,
        highlight = T,
        sortable = F,
        defaultColDef = reactable::colDef(
          header = function(value) gsub(".", " ", value, fixed = T),
          headerStyle = list(background = "#333f50", color = "#FFF"),
          align = "center",
          format = reactable::colFormat(percent = T, digits = 2),
          style = list(color = "#333")),
        columns = list(
          `Time.Range` = reactable::colDef(minWidth = 80),
          `Average.Return` = reactable::colDef(minWidth = 80)
        ),
        theme = reactable::reactableTheme(
          #borderColor = "#dfe2e5",
          stripedColor = "#adb9ca",
          highlightColor = "#f4b183",
          background = "#d6dce5",
          #backgroundColor = "#d6dce5",
          #cellStyle = list(background = "#d6dce5"),
          cellPadding = "8px 10px",
          style = list(fontFamily = "'Open Sans', sans-serif"),
          searchInputStyle = list(width = "100%")
        )
      )
    })
    
    
    output$chart6 <- echarts4r::renderEcharts4r({
      
      geomReturn <- function(returns){
        prod(1 + returns)^(1/length(returns)) - 1 
      }
      
      
      returns <- readr::read_csv("data/historical/MPERS_investment_returns.csv")
      
      returns <- returns |>
        dplyr::filter(year != 2000) |>
        dplyr::mutate(`5-Year Rolling Avg.` = zoo::rollapply(mva, 5, geomReturn, align = "right", partial = F, fill = NA)) |>
        dplyr::mutate(`10-Year Rolling Avg.` = zoo::rollapply(mva, 10, geomReturn, align = "right", partial = F, fill = NA)) |>
        dplyr::mutate(`15-Year Rolling Avg.` = zoo::rollapply(mva, 15, geomReturn, align = "right", partial = F, fill = NA)) |>
        dplyr::mutate(year = as.character(year))
      
      
      
      e1 <- returns |>
        echarts4r::e_chart(year) |>
        echarts4r::e_line(name = "Assumed Return",
                          arr,
                          symbol = "none",
                          lineStyle = list(width = 4)) |>
        echarts4r::e_line(name = "Market Return",
                          mva, 
                          symbol = "none",
                          lineStyle = list(width = 4)) |>
        echarts4r::e_line(name = "Actuarial Return",
                          ava,
                          symbol = "none",
                          lineStyle = list(width = 4))
      
      
      if (input$rolling == "5-Year") {
        
        e1 |>
          echarts4r::e_line(name = "5-Year Geometric Rolling Return", `5-Year Rolling Avg.`, symbol = "none",
                            lineStyle = list(width = 4)) |>
          echarts4r::e_y_axis(axisLabel = list(fontSize = 14),
                              formatter = echarts4r::e_axis_formatter(style = c("percent"))) |>
          echarts4r::e_x_axis(min = "2001",
                              axisLabel = list(fontSize = 14),
                              axisTick = list(alignWithLabel = T)) |>
          echarts4r::e_legend(type = "scroll",
                              textStyle = list(fontSize = 14),
                              icons = c("roundRect", "roundRect", "roundRect", "roundRect"),
                              selector = list(
                                list(type = 'inverse', title = 'Invert Selection'),
                                list(type = 'all', title = 'Reset')
                              )) |>
          echarts4r::e_tooltip(trigger = "axis",
                               confine = T,
                               formatter = echarts4r::e_tooltip_pointer_formatter(style = c("percent"),
                                                                                  digits = 2)) |>
          echarts4r::e_axis_pointer(label = list(show = F)) |>
          echarts4r::e_theme_custom("echarts_theme.json") |>
          echarts4r::e_grid(left = "15%", bottom = "10%")
        
      } else if (input$rolling == "10-Year") {
        
        e1 |>
          echarts4r::e_line(name = "10-Year Geometric Rolling Return",`10-Year Rolling Avg.`, symbol = "none",
                            lineStyle = list(width = 4)) |>
          echarts4r::e_y_axis(axisLabel = list(fontSize = 14),
                              formatter = echarts4r::e_axis_formatter(style = c("percent"))) |>
          echarts4r::e_x_axis(min = "2001",
                              axisLabel = list(fontSize = 14),
                              axisTick = list(alignWithLabel = T)) |>
          echarts4r::e_legend(type = "scroll",
                              textStyle = list(fontSize = 14),
                              icons = c("roundRect", "roundRect", "roundRect", "roundRect"),
                              selector = list(
                                list(type = 'inverse', title = 'Invert Selection'),
                                list(type = 'all', title = 'Reset')
                              )) |>
          echarts4r::e_tooltip(trigger = "axis",
                               confine = T,
                               formatter = echarts4r::e_tooltip_pointer_formatter(style = c("percent"),
                                                                                  digits = 2)) |>
          echarts4r::e_axis_pointer(label = list(show = F)) |>
          echarts4r::e_theme_custom("echarts_theme.json") |>
          echarts4r::e_grid(left = "15%", bottom = "10%")
        
      } else {
        
        e1 |>
          echarts4r::e_line(name = "15-Year Geometric Rolling Return", `15-Year Rolling Avg.`, symbol = "none",
                            lineStyle = list(width = 4)) |>
          echarts4r::e_y_axis(axisLabel = list(fontSize = 14),
                              formatter = echarts4r::e_axis_formatter(style = c("percent"))) |>
          echarts4r::e_x_axis(min = "2001",
                              axisLabel = list(fontSize = 14),
                              axisTick = list(alignWithLabel = T)) |>
          echarts4r::e_legend(type = "scroll",
                              textStyle = list(fontSize = 14),
                              icons = c("roundRect", "roundRect", "roundRect", "roundRect"),
                              selector = list(
                                list(type = 'inverse', title = 'Invert Selection'),
                                list(type = 'all', title = 'Reset')
                              )) |>
          echarts4r::e_tooltip(trigger = "axis",
                               confine = T,
                               formatter = echarts4r::e_tooltip_pointer_formatter(style = c("percent"),
                                                                                  digits = 2)) |>
          echarts4r::e_axis_pointer(label = list(show = F)) |>
          echarts4r::e_theme_custom("echarts_theme.json") |>
          echarts4r::e_grid(left = "15%", bottom = "10%")
        
      }
      
    })
    
    
    output$chart7 <- echarts4r::renderEcharts4r({
      
      asset <- readRDS("data/historical/MPERS_asset.rds")
      
      asset
      
    })
    
    
    
    output$prob_table <- reactable::renderReactable({
      
      table <- readRDS("data/historical/MPERS_table.rds")
      
      table
    })
    
    
    output$chart8 <- echarts4r::renderEcharts4r({
      
      density <- readRDS("data/historical/MPERS_density.rds")
      
      density
      
    })
    
    
    
    
  }
  moduleServer(id, server)
}

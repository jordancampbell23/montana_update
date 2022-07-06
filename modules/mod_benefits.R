#--------------------------------Login for App---------------------------------#


# UI

mod_benefits_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    
    div(id = ns("control-panel"),
        style = control_div_style,
        
        actionButton(ns("controlbutton_close"),
                     label = NULL,
                     icon = icon("close")
        ),
        
        div(
          style = "margin-top: 10px; margin-left: 20px; margin-right: 20px; margin-bottom: 30px;",
          
          fluidRow(
            
            column(4),
            
            column(4, 
                   style = 'margin-top: -10px; font-size: 14px;',
                   align = "center",
                   shinyWidgets::radioGroupButtons(
                     ns("nclass"),
                     label = "Number of Scenarios:",
                     choices = c(1, 2, 3, 4),
                     selected = 1,
                     # icon = icon("chart-line"),
                     size = "normal"
                   )
            ),
            
            column(4)
            
          ),
          
          
          fluidRow(
            column(12,
                   div(
                     align = "center",
                     radioGroupButtons(
                       ns("ui_inputs"),
                       label = "",
                       choices = c("Plan Design",
                                   "Defined Benefit",
                                   "Defined Contribution"),
                       selected = c("Plan Design")
                     ))
            )
          ),
          
          fluidRow(
            
            column(1),
            
            column(2,  p("Name Scenario:"), style = name_scenario_style),
            
            conditionalPanel(
              condition = 'input.nclass >= 1', ns = ns,
              column(2, style = "margin-top: -20px;", align = "center",
                     p( textInput(ns("scen_name_1"), label = "", value = "Scenario 1"), style = "font-weight: 800")
              )),
            
            conditionalPanel(
              condition = 'input.nclass >= 2', ns = ns,
              column(2, style = "margin-top: -20px;", align = "center",
                     p(textInput(ns("scen_name_2"), label = "", value = "Scenario 2"), style = "font-weight: 800")
              )),
            
            conditionalPanel(
              condition = 'input.nclass >= 3', ns = ns,
              column(2, style = "margin-top: -20px;", align = "center",
                     p(textInput(ns("scen_name_3"), label = "", value = "Scenario 3"), style = "font-weight: 800")
              )),
            
            conditionalPanel(
              condition = 'input.nclass == 4', ns = ns,
              column(2, style = "margin-top: -20px;", align = "center",
                     p(textInput(ns("scen_name_4"), label = "", value = "Scenario 4"), style = "font-weight: 800")
              ))
            
          ),
          
          conditionalPanel(
            condition = "input.ui_inputs == 'Plan Design'", ns = ns,
          
          fluidRow(
            
            style = dynamic_input_style_top,
            
            column(1),
            
            column(2,
             hover_text(
                      title = "Retirement Plan:",
                      text = "Defined Benefit Plan (DB): New hires share normal cost\n\nDefined Contribution Plan (DC): New hires do not share normal cost\n\nHybrid: Normal cost represents a single valuation year's portion of\nthe value of actuarial liabilities",
                      position = "right"),
              style = "margin-top: 10px; font-size: 14px; font-family: 'Open Sans', sans-serif;"),
            
            conditionalPanel(
              condition = 'input.nclass >= 1', ns = ns,
            column(2, style = "margin-top: 10px;",
                   shinyWidgets::pickerInput(
                     ns("Output_1"),
                     choices = c("DB", "DC", "Hybrid"),
                     selected = "DB"
                   )
            )),
            
            conditionalPanel(
              condition = 'input.nclass >= 2', ns = ns,
            column(2, style = "margin-top: 10px;",
                   shinyWidgets::pickerInput(
                     ns("Output_2"),
                     choices = c("DB", "DC", "Hybrid"),
                     selected = "DB"
                   )
            )),
            
            conditionalPanel(
              condition = 'input.nclass >= 3', ns = ns,
            column(2, style = "margin-top: 10px;",
                   shinyWidgets::pickerInput(
                     ns("Output_3"),
                     choices = c("DB", "DC", "Hybrid"),
                     selected = "DB"
                   )
                   )),
            
            conditionalPanel(
              condition = 'input.nclass == 4', ns = ns,
            column(2, style = "margin-top: 10px;",
                   shinyWidgets::pickerInput(
                     ns("Output_4"),
                     choices = c("DB", "DC", "Hybrid"),
                     selected = "DB"
                   )
            ))
            
          ),
          
          fluidRow(
            style = dynamic_input_style_bottom,
            
            column(1),
            
            column(2,
              hover_text(
                      title = "Hiring Age:",
                      text = "Age selection options.",
                      position = "right"),
              style = "margin-top: 25px; font-size: 14px; font-family: 'Open Sans', sans-serif;"),
            
            conditionalPanel(
              condition = 'input.nclass >= 1', ns = ns,
              column(2,
                     shinyWidgets::pickerInput(
                       ns("HiringAge_1"),
                       label = "",
                       choices = c(SalaryEntry$entry_age),
                       selected = 22,
                       options = list(size = 8)
                     )
              )),
            
            conditionalPanel(
              condition = 'input.nclass >= 2', ns = ns,
              column(2,
                     shinyWidgets::pickerInput(
                       ns("HiringAge_2"),
                       label = "",
                       choices = c(SalaryEntry$entry_age),
                       selected = 22,
                       options = list(size = 8)
                     )
              )),
            
            conditionalPanel(
              condition = 'input.nclass >= 3', ns = ns,
              column(2,
                     shinyWidgets::pickerInput(
                       ns("HiringAge_3"),
                       label = "",
                       choices = c(SalaryEntry$entry_age),
                       selected = 22,
                       options = list(size = 8)
                     )
              )),
            
            conditionalPanel(
              condition = 'input.nclass == 4', ns = ns,
              
              column(2, 
                     shinyWidgets::pickerInput(
                       ns("HiringAge_4"),
                       label = "",
                       choices = c(SalaryEntry$entry_age),
                       selected = 22,
                       options = list(size = 8)
                     )
              )) 
          )
          ),
          
          conditionalPanel(
            condition = "input.ui_inputs == 'Defined Benefit'", ns = ns,
          
          fluidRow(
            style = dynamic_input_style_top,
            
            column(1),
            
            column(2,
             hover_text(
                      title = "Assumed Rate of Return:",
                      text = "The rate of return adopted by the\nboard as its assumption of what\nthe DB plan will return on average\nin the long run.",
                      position = "right"), 
             style = margin_top_25),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & (input.Output_1 == 'DB' | input.Output_1 == 'Hybrid')", ns = ns,
            column(2,
                   fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("ARR_1"),
                              label = "",
                              min = 0,
                              max = 50,
                              step = 0.25,
                              value = ARR*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
            )),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & input.Output_1 == 'DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
                     )),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & (input.Output_2 == 'DB' | input.Output_2 == 'Hybrid')", ns = ns,
            column(2, 
              fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("ARR_2"),
                              label = "",
                              min = 0,
                              max = 50,
                              step = 0.25,
                              value = ARR*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
            )),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & input.Output_2 == 'DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & (input.Output_3 == 'DB' | input.Output_3 == 'Hybrid')", ns = ns,
            column(2,  
              fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("ARR_3"),
                              label = "",
                              min = 0,
                              max = 50,
                              step = 0.25,
                              value = ARR*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
            )),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & input.Output_3 == 'DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )),
            
            conditionalPanel(
              condition = "input.nclass == 4 & (input.Output_4 == 'DB' | input.Output_4 == 'Hybrid')", ns = ns,
            
            column(2, 
              fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("ARR_4"),
                              label = "",
                              min = 0,
                              max = 50,
                              step = 0.25,
                              value = ARR*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
            )),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & input.Output_4 == 'DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              ))
            
          ),
          
          
          fluidRow(
            style = dynamic_input_style_middle,
            
            column(1),
            
            column(2,
            hover_text(
                      title = "DB Employee Contribution:",
                      text = "The rate of employee contribution into the DB plan.",
                      position = "right"),
            style = margin_top_25),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & (input.Output_1 == 'DB' | input.Output_1 == 'Hybrid')", ns = ns,
              column(2,
                     fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DB_EE_cont_1"),
                              label = "",
                              step = 0.25,
                              value = DB_EE_cont * 100
                     )

                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & input.Output_1 == 'DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )),
            
            conditionalPanel(
              condition =  "input.nclass >= 2 & (input.Output_2 == 'DB' | input.Output_2 == 'Hybrid')", ns = ns,
              column(2,
                  fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DB_EE_cont_2"),
                              label = "",
                              step = 0.25,
                              value = DB_EE_cont * 100
                     )

                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & input.Output_2 == 'DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )),
            
            conditionalPanel(
              condition =  "input.nclass >= 3 & (input.Output_3 == 'DB' | input.Output_3 == 'Hybrid')", ns = ns,
              column(2,
                fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DB_EE_cont_3"),
                              label = "",
                              step = 0.25,
                              value = DB_EE_cont * 100
                     )

                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & input.Output_3 == 'DC'", ns = ns,
              column(2, style = margin_top_25,
                     "Not an input for DC"
              )),
            
            conditionalPanel(
              condition =  "input.nclass >= 4 & (input.Output_4 == 'DB' | input.Output_4 == 'Hybrid')", ns = ns,
              column(2, 
                fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DB_EE_cont_4"),
                              label = "",
                              step = 0.25,
                              value = DB_EE_cont * 100
                     )

                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & input.Output_4 == 'DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              ))
            
          ),
          
          fluidRow(
            style = dynamic_input_style_middle,
            
            column(1),
            
            column(2,
            hover_text(
                      title = "DB Benefit Multiplier <10 YOS:",
                      text = "A DB benefit multiplier is used, typically with\nfinal average salary and years of service (YOS)\nto determine retiree benefit. <10 YOS indicates\nless than 10 years of service.",
                      position = "right"),
            style = margin_top_25),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & (input.Output_1 =='DB' | input.Output_1 == 'Hybrid')", ns = ns,
              column(2, 
                fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult1_1"),
                              label = "",
                              step = 0.25,
                              value = BenMult1*100
                                )

                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & input.Output_1 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & (input.Output_2 =='DB' | input.Output_2 == 'Hybrid')", ns = ns,
              column(2, 
                fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult1_2"),
                              label = "",
                              step = 0.25,
                              value = BenMult1*100
                                )

                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & input.Output_2 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & (input.Output_3 =='DB' | input.Output_3 == 'Hybrid')", ns = ns,
              column(2,
                fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult1_3"),
                              label = "",
                              step = 0.25,
                              value = BenMult1*100
                                )

                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & input.Output_3 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & (input.Output_4 =='DB' | input.Output_4 == 'Hybrid')", ns = ns,
              column(2,
                  fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult1_4"),
                              label = "",
                              step = 0.25,
                              value = BenMult1*100
                                )

                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & input.Output_4 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            )
            
          ),
          
          fluidRow(
            style = dynamic_input_style_middle,
            
            column(1),
            
            column(2,
             hover_text(
                      title = "DB Benefit Multiplier 10-30 YOS:",
                      text = "A DB benefit multiplier is used, typically with\nfinal average salary and years of service (YOS)\nto determine retiree benefit. <10 YOS indicates\n10 to 30 years of service.",
                      position = "right"),
             style = margin_top_25),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & (input.Output_1 =='DB' | input.Output_1 == 'Hybrid')", ns = ns,
              column(2,
                     fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult2_1"),
                              label = "",
                              step = 0.25,
                              value = BenMult2*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & input.Output_1 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & (input.Output_2 =='DB' | input.Output_2 == 'Hybrid')", ns = ns,
              column(2,
                  fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult2_2"),
                              label = "",
                              step = 0.25,
                              value = BenMult2*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & input.Output_2 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & (input.Output_3 =='DB' | input.Output_3 == 'Hybrid')", ns = ns,
              column(2,
                  fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult2_3"),
                              label = "",
                              step = 0.25,
                              value = BenMult2*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & input.Output_3 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & (input.Output_4 =='DB' | input.Output_4 == 'Hybrid')", ns = ns,
              column(2,
                 fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult2_4"),
                              label = "",
                              step = 0.25,
                              value = BenMult2*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & input.Output_4 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            )
            
          ),
          
          fluidRow(
            style = dynamic_input_style_middle,
            
            column(1),
            
            column(2,
            hover_text(
                      title = "DB Benefit Multiplier >30 YOS:",
                      text = "A DB benefit multiplier is used, typically with\nfinal average salary and years of service (YOS)\nto determine retiree benefit. <10 YOS indicates\ngreater than 30 years of service.",
                      position = "right"),
             style = margin_top_25),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & (input.Output_1 =='DB' | input.Output_1 == 'Hybrid')", ns = ns,
              column(2,
                     fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult3_1"),
                              label = "",
                              step = 0.25,
                              value = BenMult3*100
                     )
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & input.Output_1 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & (input.Output_2 =='DB' | input.Output_2 == 'Hybrid')", ns = ns,
              column(2,
                  fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult3_2"),
                              label = "",
                              step = 0.25,
                              value = BenMult3*100
                     )
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & input.Output_2 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & (input.Output_3 =='DB' | input.Output_3 == 'Hybrid')", ns = ns,
              column(2,
                fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult3_3"),
                              label = "",
                              step = 0.25,
                              value = BenMult3*100
                     )
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & input.Output_3 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & (input.Output_4 =='DB' | input.Output_4 == 'Hybrid')", ns = ns,
              column(2,
                fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("BenMult3_4"),
                              label = "",
                              step = 0.25,
                              value = BenMult3*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & input.Output_4 =='DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            )
          ),
          
          fluidRow(
            style = dynamic_input_style_bottom,
            
            column(1),
            
            column(2,
            hover_text(
                      title = "COLA:",
                      text = "An annual change to a pension benefit for retirees,\nusually tied to a measure of inflation.",
                      position = "right"),
              style = margin_top_25),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & (input.Output_1 == 'DB' | input.Output_1 == 'Hybrid')", ns = ns,
              column(2,
                 fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("COLA_1"),
                              label = "",
                              step = 0.5,
                              value = COLA_new*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & input.Output_1 == 'DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & (input.Output_2 == 'DB' | input.Output_2 == 'Hybrid')", ns = ns,
              column(2,
                     fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("COLA_2"),
                              label = "",
                              step = 0.5,
                              value = COLA_new*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & input.Output_2 == 'DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & (input.Output_3 == 'DB' | input.Output_3 == 'Hybrid')", ns = ns,
              column(2,
                  fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("COLA_3"),
                              label = "",
                              step = 0.5,
                              value = COLA_new*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & input.Output_3 == 'DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & (input.Output_4 == 'DB' | input.Output_4 == 'Hybrid')", ns = ns,
              column(2,
                 fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("COLA_4"),
                              label = "",
                              step = 0.5,
                              value = COLA_new*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & input.Output_4 == 'DC'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DC"
              )
            )
          )
        ),
          
          conditionalPanel(
            condition = "input.ui_inputs == 'Defined Contribution'", ns = ns,
          
          fluidRow(
            style = dynamic_input_style_top,
            
            column(1),
            
            column(2,
              hover_text(
                      title = "DC Return:",
                      text = "The rate of return adopted by the board as its assumption\nof what the DB plan will return on average in the long run.",
                      position = "right"),
              style = "margin-top: 30px;"),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & (input.Output_1 == 'DC' | input.Output_1 == 'Hybrid')", ns = ns,
              column(2,
                     fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_Return_1"),
                              label = "",
                              step = 0.25,
                              value = DC_return*100
                              )
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & input.Output_1 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & (input.Output_2 == 'DC' | input.Output_2 == 'Hybrid')", ns = ns,
              column(2,
                     fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_Return_2"),
                              label = "",
                              step = 0.25,
                              value = DC_return*100
                              )
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & input.Output_2 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & (input.Output_3 == 'DC' | input.Output_3 == 'Hybrid')", ns = ns,
              column(2,
                     fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_Return_3"),
                              label = "",
                              step = 0.25,
                              value = DC_return*100
                              )
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & input.Output_3 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & (input.Output_4 == 'DC' | input.Output_4 == 'Hybrid')", ns = ns,
              column(2,
                  fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_Return_4"),
                              label = "",
                              step = 0.25,
                              value = DC_return*100
                              )
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & input.Output_4 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            ),
            
          ),
          
          fluidRow(
            style = dynamic_input_style_middle,
            
            column(1),
            
            column(2, 
            hover_text(
                      title = "DC Employee Contribution:",
                      text = "The rate of employee contribution into the DB plan.",
                      position = "right"),
            style = "margin-top: 25px; font-size: 14px; font-family: 'Open Sans', sans-serif;"),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & (input.Output_1 == 'DC' | input.Output_1 == 'Hybrid')", ns = ns,
              column(2,
                     fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_EE_cont_1"),
                              label = "",
                              step = 0.25,
                              value = DC_EE_cont*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >=1 & input.Output_1 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & (input.Output_2 == 'DC' | input.Output_2 == 'Hybrid')", ns = ns,
              column(2,
                     fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_EE_cont_2"),
                              label = "",
                              step = 0.25,
                              value = DC_EE_cont*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >=2 & input.Output_2 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & (input.Output_3 == 'DC' | input.Output_3 == 'Hybrid')", ns = ns,
              column(2,
                     fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_EE_cont_3"),
                              label = "",
                              step = 0.25,
                              value = DC_EE_cont*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & input.Output_3 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & (input.Output_4 == 'DC' | input.Output_4 == 'Hybrid')", ns = ns,
              column(2,
                  fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_EE_cont_4"),
                              label = "",
                              step = 0.25,
                              value = DC_EE_cont*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & input.Output_4 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            )
            
          ),
          
          fluidRow(
            style = dynamic_input_style_bottom,
            
            column(1),
            
            column(2,
              hover_text(
                      title = "DC Employer Contribution:",
                      text = "A multiplier used, typically with final average salary and\nyears of service to determine retiree annual benefit.",
                      position = "right"),
              style = margin_top_25),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & (input.Output_1 == 'DC' | input.Output_1 == 'Hybrid')", ns = ns,
              column(2,
                  fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_ER_cont_1"),
                              label = "",
                              step = 0.25,
                              value = DC_ER_cont*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 1 & input.Output_1 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & (input.Output_2 == 'DC' | input.Output_2 == 'Hybrid')", ns = ns,
              column(2,
                    fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_ER_cont_2"),
                              label = "",
                              step = 0.25,
                              value = DC_ER_cont*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 2 & input.Output_2 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & (input.Output_3 == 'DC' | input.Output_3 == 'Hybrid')", ns = ns,
              column(2,
                  fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_ER_cont_3"),
                              label = "",
                              step = 0.25,
                              value = DC_ER_cont*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 3 & input.Output_3 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            ),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & (input.Output_4 == 'DC' | input.Output_4 == 'Hybrid')", ns = ns,
              column(2,
                fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("DC_ER_cont_4"),
                              label = "",
                              step = 0.25,
                              value = DC_ER_cont*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
              )),
            
            conditionalPanel(
              condition = "input.nclass >= 4 & input.Output_4 == 'DB'", ns = ns,
              column(2, style = margin_top_30b,
                     "Not an input for DB"
              )
            ) 
          )
          
          ),
          
          div(style = "height: 30px")
          
          
        )
        
        ),
    
    
    div(align = "center",
        style = "margin: 0",
    fluidRow(
      style = "margin: 0; height: 7%",
      column(
        width = 4,
        align = "left",
        style = top_button_style,
          actionButton(ns("controlbutton"),
                       label = " Control Panel",
                       icon = icon("keyboard"))
      ),
      column(
        width = 4,
        align = "center",
        style = top_button_style,
        ),
      column(
        width = 4,
        align = "right",
        style = top_button_style,
          downloadButton(
            ns("download_excel"),
            "Download Data"
          )
      )
    )),
    
    div(align = "center",
        style = "margin: -50px 0px 0 px 0px",
      fluidRow(
        style = top_charts_fluidRow,
        column(2),
        column(
          width = 8,
          style = top_charts_column,
          data_card(
            shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart1"), height = "37vh", width = "63vw"))
          )
        ),
        column(2)
      )),
    
    div(align = "center",
        style = "margin: -20px 0px 0 px 0px;",
    fluidRow(
      style = bottom_charts_fluidRow,
      column(2),
      column(
        width = 8,
        style = bottom_charts_column,
        data_card(
          shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart2"), height = "37vh", width = "63vw"))
        )
      ),
      column(2)
    ))
  )
  
  
}


# Server

mod_benefits_server <- function(id, state) {
  server <- function(input, output, session) {
    ns <- session$ns

    
    #---------------------------CONTROL PANEL ON/OFF----------------------------
    control_panel <- reactive({
      
      value <- (input$controlbutton - input$controlbutton_close)
      
      return(value)
    })
    
    
    observeEvent(input$controlbutton | input$controlbutton_close, {
      if(control_panel() %% 2 == 0){
        shinyjs::hide(id = "control-panel")
      }else{
        shinyjs::show(id = "control-panel")
      }
    },
    ignoreNULL = F,
    ignoreInit = F)
    
    

    
    scenario_1 <- reactive({
      scenario_1 <- benefit_cal(output = input$Output_1,
                                # EE_tier = "New Hire",
                                DB_ARR = input$ARR_1/100,
                                DB_EE = input$DB_EE_cont_1/100,
                                DC_EE = input$DC_EE_cont_1/100,
                                DC_ER = input$DC_ER_cont_1/100,
                                DB_mult10 = input$BenMult1_1/100,
                                DB_mult10_30 = input$BenMult2_1/100,
                                DB_mult30 = input$BenMult3_1/100,
                                DB_COLA = input$COLA_1/100,
                                ea = input$HiringAge_1,
                                DCreturn = input$DC_Return_1/100) |> as.data.frame()
      names(scenario_1) <- c("Age", "Scenario 1", "Scenario 1.")
      return(scenario_1)
    })

    scenario_2 <- reactive({
      scenario_2 <- benefit_cal(output = input$Output_2,
                                # EE_tier = "New Hire",
                                DB_ARR = input$ARR_2/100,
                                DB_EE = input$DB_EE_cont_2/100,
                                DC_EE = input$DC_EE_cont_2/100,
                                DC_ER = input$DC_ER_cont_2/100,
                                DB_mult10 = input$BenMult1_2/100,
                                DB_mult10_30 = input$BenMult2_2/100,
                                DB_mult30 = input$BenMult3_2/100,
                                DB_COLA = input$COLA_2/100,
                                ea = input$HiringAge_2,
                                DCreturn = input$DC_Return_2/100) |> as.data.frame()
      names(scenario_2) <- c("Age", "Scenario 2", "Scenario 2.")
      return(scenario_2)
    })

    scenario_3 <- reactive({
      scenario_3 <- benefit_cal(output = input$Output_3,
                                # EE_tier = "New Hire",
                                DB_ARR = input$ARR_3/100,
                                DB_EE = input$DB_EE_cont_3/100,
                                DC_EE = input$DC_EE_cont_3/100,
                                DC_ER = input$DC_ER_cont_3/100,
                                DB_mult10 = input$BenMult1_2/100,
                                DB_mult10_30 = input$BenMult2_2/100,
                                DB_mult30 = input$BenMult3_2/100,
                                DB_COLA = input$COLA_3/100,
                                ea = input$HiringAge_3,
                                DCreturn = input$DC_Return_3/100) |> as.data.frame()
      names(scenario_3) <- c("Age", "Scenario 3", "Scenario 3.")
      return(scenario_3)
    })

    scenario_4 <- reactive({
      scenario_4 <- benefit_cal(output = input$Output_4,
                                # EE_tier = "New Hire",
                                DB_ARR = input$ARR_4/100,
                                DB_EE = input$DB_EE_cont_4/100,
                                DC_EE = input$DC_EE_cont_4/100,
                                DC_ER = input$DC_ER_cont_4/100,
                                DB_mult10 = input$BenMult1_4/100,
                                DB_mult10_30 = input$BenMult2_4/100,
                                DB_mult30 = input$BenMult3_4/100,
                                DB_COLA = input$COLA_4/100,
                                ea = input$HiringAge_4,
                                DCreturn = input$DC_Return_4/100) |> as.data.frame()
      names(scenario_4) <- c("Age", "Scenario 4", "Scenario 4.")
      return(scenario_4)
    })


    df <- reactive({
      
      df_1 <- scenario_1() |> select(1:2)
      df_2 <- scenario_2() |> select(1:2)
      df_3 <- scenario_3() |> select(1:2)
      df_4 <- scenario_4() |> select(1:2)

      df <- scenario_1() |>
        full_join(scenario_2(), by = "Age") |>
        full_join(scenario_3(), by = "Age") |>
        full_join(scenario_4(), by = "Age")

    })

    ret_df <- reactive({
      
      df_1 <- scenario_1() |> select(1, 3)
      df_2 <- scenario_2() |> select(1, 3)
      df_3 <- scenario_3() |> select(1, 3)
      df_4 <- scenario_4() |> select(1, 3)
      
      names(df_1)[2] <- "Scenario 1"
      names(df_2)[2] <- "Scenario 2"
      names(df_3)[2] <- "Scenario 3"
      names(df_4)[2] <- "Scenario 4"
      
      df <- df_1 |>
        full_join(df_2, by = "Age") |>
        full_join(df_3, by = "Age") |>
        full_join(df_4, by = "Age")
      
    })

    #-----------------------------PLOTS-----------------------------------------

    output$chart1 <- echarts4r::renderEcharts4r({

      df <- df()
      
      
      varname_a <- input$scen_name_1
      varname_b <- input$scen_name_2
      varname_c <- input$scen_name_3
      varname_d <- input$scen_name_4

      e1 <- df %>%
        echarts4r::e_charts(Age) %>%
        echarts4r::e_line(`Scenario 1`,
                          name = varname_a,
                          symbol = 'none',
                          lineStyle = list(width = 3)) %>%
        echarts4r::e_x_axis(axisLabel = list(fontSize = 13),
                            name = "Age",
                            axisLabel = list(fontSize = 14),
                            nameLocation = "middle",
                            nameTextStyle = list(
                              fontSize = 14,
                              fontStyle = 'bold',
                              padding = c(10, 0, 0, 0)),
                            min = 20) %>%
        echarts4r::e_y_axis(name = "Wealth",
                            axisLabel = list(fontSize = 14),
                            nameLocation = "middle",
                            nameTextStyle = list(
                              fontSize = 14,
                              fontStyle = 'bold',
                              padding = c(0, 0, 70, 0)),
                            formatter = echarts4r::e_axis_formatter(style = "currency",
                                                                    digits = 0)) %>%
        echarts4r::e_tooltip(trigger = "axis",
                             formatter = echarts4r::e_tooltip_pointer_formatter(style = "currency",
                                                                                digits = 0),
                             textStyle = list(fontSize = 14),
                             confine = T) %>%
        echarts4r::e_axis_pointer(label = list(show = F)) %>%
        echarts4r::e_grid(left = "17%") %>%
        echarts4r::e_title("Total Benefits (Inf. Adjusted)") %>%
        echarts4r::e_theme_custom("echarts_theme.json") %>%
        # echarts4r::e_color(background = "#fff") %>%
        echarts4r::e_toolbox_feature(feature = "dataZoom") |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Save")

      if (input$nclass == 1) {

        e1 %>%
          echarts4r::e_legend(type = "scroll",
                              top = "9%",
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect'))

      } else if (input$nclass == 2) {

        e1 %>%
          echarts4r::e_line(`Scenario 2`,
                            name = varname_b,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) %>%
          echarts4r::e_legend(type = "scroll",
                              top = "9%",
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect'))

      } else if (input$nclass == 3) {

        e1 %>%
          echarts4r::e_line(`Scenario 2`,
                            name = varname_b,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))  %>%
          echarts4r::e_line(`Scenario 3`,
                            name = varname_c,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) %>%
          echarts4r::e_legend(type = "scroll",
                              top = "9%",
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect'))

      } else {

        e1 %>%
          echarts4r::e_line(`Scenario 2`,
                            name = varname_b,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) %>%
          echarts4r::e_line(`Scenario 3`,
                            name = varname_c,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) %>%
          echarts4r::e_line(`Scenario 4`,
                            name = varname_d,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) %>%
          echarts4r::e_legend(type = "scroll",
                              top = "9%",
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect', 'roundRect'))

      }


    })
    
    
    output$chart2 <- echarts4r::renderEcharts4r({
      
      df <- ret_df()
      
      
      varname_a <- input$scen_name_1
      varname_b <- input$scen_name_2
      varname_c <- input$scen_name_3
      varname_d <- input$scen_name_4
      
      e1 <- df %>%
        echarts4r::e_charts(Age) %>%
        echarts4r::e_line(`Scenario 1`,
                          name = varname_a,
                          symbol = 'none',
                          lineStyle = list(width = 3)) %>%
        echarts4r::e_x_axis(axisLabel = list(fontSize = 13),
                            name = "Age",
                            axisLabel = list(fontSize = 14),
                            nameLocation = "middle",
                            nameTextStyle = list(
                              fontSize = 14,
                              fontStyle = 'bold',
                              padding = c(10, 0, 0, 0)),
                            min = 20) %>%
        echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "percent",
                                                                    digits = 0),
                            name = "Attrition",
                            axisLabel = list(fontSize = 14),
                            nameLocation = "middle",
                            nameTextStyle = list(
                              fontSize = 14,
                              fontStyle = 'bold',
                              padding = c(0, 0, 30, 0))) %>%
        echarts4r::e_tooltip(trigger = "axis",
                             formatter = echarts4r::e_tooltip_pointer_formatter(style = "percent",
                                                                                digits = 1),
                             textStyle = list(fontSize = 14),
                             confine = T) %>%
        echarts4r::e_axis_pointer(label = list(show = F)) %>%
        echarts4r::e_grid(left = "15%") %>%
        echarts4r::e_title("Percent of Members Remaining") %>%
        echarts4r::e_theme_custom("echarts_theme.json") %>%
        # echarts4r::e_color(background = "#fff") %>%
        echarts4r::e_toolbox_feature(feature = "dataZoom") |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Save")
      
      if (input$nclass == 1) {
        
        e1 %>%
          echarts4r::e_legend(type = "scroll",
                              top = "9%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect'))
        
      } else if (input$nclass == 2) {
        
        e1 %>%
          echarts4r::e_line(`Scenario 2`,
                            name = varname_b,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) %>%
          echarts4r::e_legend(type = "scroll",
                              top = "9%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect'))
        
      } else if (input$nclass == 3) {
        
        e1 %>%
          echarts4r::e_line(`Scenario 2`,
                            name = varname_b,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))  %>%
          echarts4r::e_line(`Scenario 3`,
                            name = varname_c,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) %>%
          echarts4r::e_legend(type = "scroll",
                              top = "9%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect'))
        
      } else {
        
        e1 %>%
          echarts4r::e_line(`Scenario 2`,
                            name = varname_b,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) %>%
          echarts4r::e_line(`Scenario 3`,
                            name = varname_c,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) %>%
          echarts4r::e_line(`Scenario 4`,
                            name = varname_d,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) %>%
          echarts4r::e_legend(type = "scroll",
                              top = "9%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect', 'roundRect'))
        
      }
      
      
    })
    
    
    output$download_excel <- downloadHandler(
      
      filename = function() {
        "benefits_data.xlsx"
      },
      content = function(file) {
        
        xl_rows <- 63
        
        
        my_workbook <- openxlsx::createWorkbook()
        
        openxlsx::addWorksheet(wb = my_workbook,sheetName = "Benefits")
        openxlsx::addWorksheet(wb = my_workbook, sheetName = "Attrition")
        
        openxlsx::setColWidths(my_workbook, sheet = 1, cols = 1:7, widths = c(35, 25, 25, 25, 25, 25, 25))
        openxlsx::setColWidths(my_workbook, sheet = 2, cols = 1:7, widths = c(35, 25, 25, 25, 25, 25, 25))
        
        openxlsx::writeData(my_workbook, sheet = 1, c("PERS | Reason Foundation's Pension Integrity Project", " ",
                                                      "Retirement Plan", "Hiring Age", "Assumed Rate of Return", 
                                                      "DB Employee Contribution",
                                                      "DB Benefit Multiplier (< 10 YOS)", "DB Benefit Multiplier (10-30 YOS)", "DB Benefit Multiplier (>30 YOS)",
                                                      "COLA", "DC Return", "DC Employee Contribution",
                                                      "DC Employer Contribution"), startRow = 1, startCol = 1)
        
        openxlsx::writeData(my_workbook, sheet = 2, c("PERS | Reason Foundation's Pension Integrity Project", " ",
                                                      "Retirement Plan", "Hiring Age", "Assumed Rate of Return", 
                                                      "DB Employee Contribution", 
                                                      "DB Benefit Multiplier (< 10 YOS)", "DB Benefit Multiplier (10-30 YOS)", "DB Benefit Multiplier (>30 YOS)",
                                                      "COLA", "DC Return", "DC Employee Contribution",
                                                      "DC Employer Contribution"), startRow = 1, startCol = 1)
        
        openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fontSize = 16, textDecoration = "bold"), rows = 1, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(textDecoration = "bold"), rows = 3:xl_rows, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fontSize = 16, textDecoration = "bold"), rows = 1, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fontSize = 14, textDecoration = "bold"), rows = 2, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(textDecoration = "bold"), rows = 3:xl_rows, cols = 1)
        
        output_1 <- print(input$Output_1)
        output_2 <- print(input$Output_2)
        output_3 <- print(input$Output_3)
        output_4 <- print(input$Output_4)
        
        hiring_age_1 <- print(input$HiringAge_1)
        hiring_age_2 <- print(input$HiringAge_2)
        hiring_age_3 <- print(input$HiringAge_3)
        hiring_age_4 <- print(input$HiringAge_4)
        
        arr_1 <- paste0(input$ARR_1, "%")
        arr_2 <- paste0(input$ARR_2, "%")
        arr_3 <- paste0(input$ARR_3, "%")
        arr_4 <- paste0(input$ARR_4, "%")
        
        db_ee_cont_1 <- paste0(input$DB_EE_cont_1, "%")
        db_ee_cont_2 <- paste0(input$DB_EE_cont_2, "%")
        db_ee_cont_3 <- paste0(input$DB_EE_cont_3, "%")
        db_ee_cont_4 <- paste0(input$DB_EE_cont_4, "%")
        
        ben_mult1_1 <- paste0(input$BenMult1_1, "%")
        ben_mult1_2 <- paste0(input$BenMult1_2, "%")
        ben_mult1_3 <- paste0(input$BenMult1_3, "%")
        ben_mult1_4 <- paste0(input$BenMult1_4, "%")
        
        ben_mult2_1 <- paste0(input$BenMult2_1, "%")
        ben_mult2_2 <- paste0(input$BenMult2_2, "%")
        ben_mult2_3 <- paste0(input$BenMult2_3, "%")
        ben_mult2_4 <- paste0(input$BenMult2_4, "%")
        
        ben_mult3_1 <- paste0(input$BenMult3_1, "%")
        ben_mult3_2 <- paste0(input$BenMult3_2, "%")
        ben_mult3_3 <- paste0(input$BenMult3_3, "%")
        ben_mult3_4 <- paste0(input$BenMult3_4, "%")
        
        cola_1 <- paste0(input$COLA_1, "%")
        cola_2 <- paste0(input$COLA_2, "%")
        cola_3 <- paste0(input$COLA_3, "%")
        cola_4 <- paste0(input$COLA_4, "%")
        
        dc_return_1 <- paste0(input$DC_Return_1, "%")
        dc_return_2 <- paste0(input$DC_Return_2, "%")
        dc_return_3 <- paste0(input$DC_Return_3, "%")
        dc_return_4 <- paste0(input$DC_Return_4, "%")
        
        dc_ee_cont_1 <- paste0(input$DC_EE_cont_1, "%")
        dc_ee_cont_2 <- paste0(input$DC_EE_cont_2, "%")
        dc_ee_cont_3 <- paste0(input$DC_EE_cont_3, "%")
        dc_ee_cont_4 <- paste0(input$DC_EE_cont_4, "%")
        
        dc_er_cont_1 <- paste0(input$DC_ER_cont_1, "%")
        dc_er_cont_2 <- paste0(input$DC_ER_cont_2, "%")
        dc_er_cont_3 <- paste0(input$DC_ER_cont_3, "%")
        dc_er_cont_4 <- paste0(input$DC_ER_cont_4, "%")
        
        if (input$nclass == 1) {
          
          openxlsx::writeData(my_workbook, sheet = 1, c(output_1, hiring_age_1, arr_1, db_ee_cont_1, 
                                                        ben_mult1_1, ben_mult2_1, ben_mult3_1,
                                                        cola_1, dc_return_1, dc_ee_cont_1, dc_er_cont_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 2, c(output_1, hiring_age_1, arr_1, db_ee_cont_1,
                                                        ben_mult1_1, ben_mult2_1, ben_mult3_1,
                                                        cola_1, dc_return_1, dc_ee_cont_1, dc_er_cont_1), startRow = 3, startCol = 2)
          
          # Sheet 1
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:13, cols = 1:2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "$#,##0"), rows = 15:xl_rows, cols = 2, gridExpand = TRUE)
          
          # Sheet 2
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:13, cols = 1:2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 15:xl_rows, cols = 2, gridExpand = TRUE)
          
          
        } else if (input$nclass == 2) {
          
          openxlsx::writeData(my_workbook, sheet = 1, c(output_1, hiring_age_1, arr_1, db_ee_cont_1, 
                                                        ben_mult1_1, ben_mult2_1, ben_mult3_1,
                                                        cola_1, dc_return_1, dc_ee_cont_1, dc_er_cont_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 2, c(output_1, hiring_age_1, arr_1, db_ee_cont_1, ben_mult1_1,
                                                        cola_1, dc_return_1, dc_ee_cont_1, dc_er_cont_1), startRow = 3, startCol = 2)
          
          openxlsx::writeData(my_workbook, sheet = 1, c(output_2, hiring_age_2, arr_2, db_ee_cont_2, 
                                                        ben_mult1_2, ben_mult2_2, ben_mult3_2,
                                                        cola_2, dc_return_2, dc_ee_cont_2, dc_er_cont_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 2, c(output_2, hiring_age_2, arr_2, db_ee_cont_2, 
                                                        ben_mult1_2, ben_mult2_2, ben_mult3_2,
                                                        cola_2, dc_return_2, dc_ee_cont_2, dc_er_cont_2), startRow = 3, startCol = 3)
          
          
          # sheet 1
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:13, cols = 1:3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "$#,##0"), rows = 15:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "$#,##0"), rows = 15:xl_rows, cols = 3, gridExpand = TRUE)
          
          # sheet 2
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:13, cols = 1:3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 15:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "percentage"), rows = 15:xl_rows, cols = 3, gridExpand = TRUE)
          
          
        } else if (input$nclass == 3) {
          
          openxlsx::writeData(my_workbook, sheet = 1, c(output_1, hiring_age_1, arr_1, db_ee_cont_1, 
                                                        ben_mult1_1, ben_mult2_1, ben_mult3_1,
                                                        cola_1, dc_return_1, dc_ee_cont_1, dc_er_cont_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 2, c(output_1, hiring_age_1, arr_1, db_ee_cont_1, ben_mult1_1,
                                                        cola_1, dc_return_1, dc_ee_cont_1, dc_er_cont_1), startRow = 3, startCol = 2)
          
          openxlsx::writeData(my_workbook, sheet = 1, c(output_2, hiring_age_2, arr_2, db_ee_cont_2, 
                                                        ben_mult1_2, ben_mult2_2, ben_mult3_2,
                                                        cola_2, dc_return_2, dc_ee_cont_2, dc_er_cont_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 2, c(output_2, hiring_age_2, arr_2, db_ee_cont_2, 
                                                        ben_mult1_2, ben_mult2_2, ben_mult3_2,
                                                        cola_2, dc_return_2, dc_ee_cont_2, dc_er_cont_2), startRow = 3, startCol = 3)
          
          openxlsx::writeData(my_workbook, sheet = 1, c(output_3, hiring_age_3, arr_3, db_ee_cont_3, 
                                                        ben_mult1_3, ben_mult2_3, ben_mult3_3,
                                                        cola_3, dc_return_3, dc_ee_cont_3, dc_er_cont_3), startRow = 3, startCol = 4)
          openxlsx::writeData(my_workbook, sheet = 2, c(output_3, hiring_age_3, arr_3, db_ee_cont_3, 
                                                        ben_mult1_3, ben_mult2_3, ben_mult3_3,
                                                        cola_3, dc_return_3, dc_ee_cont_3, dc_er_cont_3), startRow = 3, startCol = 4)
          
          # sheet 1
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:13, cols = 1:4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "$#,##0"), rows = 15:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "$#,##0"), rows = 15:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "$#,##0"), rows = 15:xl_rows, cols = 4, gridExpand = TRUE)
          
          # sheet 2
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:13, cols = 1:4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 15:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "percentage"), rows = 15:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "percentage"), rows = 15:xl_rows, cols = 4, gridExpand = TRUE)
          
        } else {
          
          openxlsx::writeData(my_workbook, sheet = 1, c(output_1, hiring_age_1, arr_1, db_ee_cont_1, 
                                                        ben_mult1_1, ben_mult2_1, ben_mult3_1,
                                                        cola_1, dc_return_1, dc_ee_cont_1, dc_er_cont_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 2, c(output_1, hiring_age_1, arr_1, db_ee_cont_1,
                                                        ben_mult1_1, ben_mult2_1, ben_mult3_1,
                                                        cola_1, dc_return_1, dc_ee_cont_1, dc_er_cont_1), startRow = 3, startCol = 2)
          
          openxlsx::writeData(my_workbook, sheet = 1, c(output_2, hiring_age_2, arr_2, db_ee_cont_2, 
                                                        ben_mult1_2, ben_mult2_2, ben_mult3_2,
                                                        cola_2, dc_return_2, dc_ee_cont_2, dc_er_cont_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 2, c(output_2, hiring_age_2, arr_2, db_ee_cont_2, 
                                                        ben_mult1_2, ben_mult2_2, ben_mult3_2,
                                                        cola_2, dc_return_2, dc_ee_cont_2, dc_er_cont_2), startRow = 3, startCol = 3)
          
          openxlsx::writeData(my_workbook, sheet = 1, c(output_3, hiring_age_3, arr_3, db_ee_cont_3, 
                                                        ben_mult1_3, ben_mult2_3, ben_mult3_3,
                                                        cola_3, dc_return_3, dc_ee_cont_3, dc_er_cont_3), startRow = 3, startCol = 4)
          openxlsx::writeData(my_workbook, sheet = 2, c(output_3, hiring_age_3, arr_3, db_ee_cont_3, 
                                                        ben_mult1_3, ben_mult2_3, ben_mult3_3,
                                                        cola_3, dc_return_3, dc_ee_cont_3, dc_er_cont_3), startRow = 3, startCol = 4)
          
          openxlsx::writeData(my_workbook, sheet = 1, c(output_4, hiring_age_4, arr_4, db_ee_cont_4, 
                                                        ben_mult1_4, ben_mult2_4, ben_mult3_4,
                                                        cola_4, dc_return_4, dc_ee_cont_4, dc_er_cont_4), startRow = 3, startCol = 5)
          openxlsx::writeData(my_workbook, sheet = 2, c(output_4, hiring_age_4, arr_4, db_ee_cont_4, 
                                                        ben_mult1_4, ben_mult2_4, ben_mult3_4,
                                                        cola_4, dc_return_4, dc_ee_cont_4, dc_er_cont_4), startRow = 3, startCol = 5)
          
          
          # sheet 1
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:13, cols = 1:5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "$#,##0"), rows = 15:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "$#,##0"), rows = 15:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "$#,##0"), rows = 15:xl_rows, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f6b941", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f9d48a", numFmt = "$#,##0"), rows = 15:xl_rows, cols = 5, gridExpand = TRUE)
          
          # sheet 2
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:13, cols = 1:5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 15:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "percentage"), rows = 15:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "percentage"), rows = 15:xl_rows, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f6b941", halign = "center", fontColour = "#ffffff"), rows = 14, cols = 5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f9d48a", numFmt = "percentage"), rows = 15:xl_rows, cols = 5, gridExpand = TRUE)
          
        }
        
        
        df <- df()
        ret_df <- ret_df()
        
        if (input$nclass == 1) {
          
          df <- df |> select(Age, `Scenario 1`)
          names(df) <- c("Age", input$scen_name_1)
          ret_df <- ret_df |> select(Age, `Scenario 1`)
          names(ret_df) <- c("Age", input$scen_name_1)
          
          
        } else if (input$nclass == 2) {
          
          df <- df |> select(Age, `Scenario 1`, `Scenario 2`)
          names(df) <- c("Age", input$scen_name_1, input$scen_name_2)
          ret_df <- ret_df |> select(Age, `Scenario 1`, `Scenario 2`)
          names(ret_df) <- c("Age", input$scen_name_1, input$scen_name_2)
          
          
        } else if (input$nclass == 3) {
          
          df <- df |> select(Age, `Scenario 1`, `Scenario 2`, `Scenario 3`)
          names(df) <- c("Age", input$scen_name_1, input$scen_name_2, input$scen_name_3)
          ret_df <- ret_df |> select(Age, `Scenario 1`, `Scenario 2`, `Scenario 3`)
          names(ret_df) <- c("Age", input$scen_name_1, input$scen_name_2, input$scen_name_3)
          
        } else {
          
          df <- df |> select(Age, `Scenario 1`, `Scenario 2`, `Scenario 3`, `Scenario 4`)
          names(df) <- c("Age", input$scen_name_1, input$scen_name_2, input$scen_name_3, input$scen_name_4)
          ret_df <- ret_df |> select(Age, `Scenario 1`, `Scenario 2`, `Scenario 3`, `Scenario 4`)
          names(ret_df) <- c("Age", input$scen_name_1, input$scen_name_2, input$scen_name_3, input$scen_name_4)
          
          
        }
        
        openxlsx::writeData(my_workbook, sheet = 1, df, startRow = 14, startCol = 1)
        openxlsx::writeData(my_workbook, sheet = 2, ret_df, startRow = 14, startCol = 1)

        openxlsx::saveWorkbook(my_workbook, file)
      }
    )
 
    
    
  }
  moduleServer(id, server)
}

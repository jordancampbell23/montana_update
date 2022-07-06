#--------------------------------Funding Model---------------------------------#


# UI

mod_deterministic_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    use_prompt(),

    #---Control Panel----
    div(id = ns("control-panel"),    
        div(style = control_div_style,

        actionButton(ns("controlbutton_close"),
                     label = NULL,
                     icon = icon("close")),

        p("*Beta Version is limited to DB and DC only.",
          style = beta_version_label_style),
        
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
                     size = "normal")
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
                       choices = c("New Plan Design",
                                   "Discount Rates",
                                   "Return Scenarios",
                                   "Funding Policy",
                                   "Amortization Policy"),
                       selected = c("New Plan Design")
                     ))
            )
          ),
          #-----Scenario Inputs-----
          fluidRow(
            column(1),
            column(2,
            hover_text(
                      title = "Preloaded Inputs:",
                      text = "...",
                      position = "right"),
                   style = preload_inputs_style),
            
            conditionalPanel(
              condition = "input.nclass >= 1", ns = ns,
              column(2,
                     fluidRow(
                       column(1),
                       column(9,
                              style = "margin-left: -9px;",
                              pickerInput(
                                ns("preload_scen_1"),
                                label = "",
                                choices = c("Baseline", 
                                            "ADEC",
                                            "Cash Infusion ($100M)",
                                            "No DC Rollback",
                                            "ADEC + Cash Infusion + No DC Rollback"),
                                selected = "Baseline"
                              )
                              ),
                       column(2,
                              style = "margin-top: 24px; margin-left: -3px; font-weight: 800; font-color: #000;",
                              actionButton(ns("refresh_scen_1"),
                                           label = NULL,
                                           icon = icon("refresh"),
                                           style = refresh_button_style)
                              )
                     )
                     )
              ),
            
            conditionalPanel(
              condition = 'input.nclass >= 2', ns = ns,
              column(2,
                     fluidRow(
                       column(1),
                       column(9,
                              style = "margin-left: -5px;",
                              pickerInput(
                                ns("preload_scen_2"),
                                label = "",
                                choices = c("Baseline", 
                                            "ADEC",
                                            "Cash Infusion ($100M)",
                                            "No DC Rollback",
                                            "ADEC + Cash Infusion + No DC Rollback"),
                                selected = "ADEC"
                              )
                       ),
                       column(2,
                              style = "margin-top: 24px; margin-left: -3px; font-weight: 800; font-color: #000;",
                              actionButton(ns("refresh_scen_2"),
                                           label = NULL,
                                           icon = icon("refresh"),
                                           style = refresh_button_style)
                       )
                     )
              )),
            
            conditionalPanel(
              condition = 'input.nclass >= 3', ns = ns,
              column(2,
                     fluidRow(
                       column(1),
                       column(9,
                              style = "margin-left: -5px;",
                              pickerInput(
                                ns("preload_scen_3"),
                                label = "",
                                choices = c("Baseline", 
                                            "ADEC",
                                            "Cash Infusion ($100M)",
                                            "No DC Rollback",
                                            "ADEC + Cash Infusion + No DC Rollback"),
                                selected = "Cash Infusion ($100M)"
                              )
                       ),
                       column(2,
                              style = "margin-top: 24px; margin-left: -3px; font-weight: 800; font-color: #000;",
                              actionButton(ns("refresh_scen_3"),
                                           label = NULL,
                                           icon = icon("refresh"),
                                           style = refresh_button_style)
                            )
                          )
              )),
            
            conditionalPanel(
              condition = 'input.nclass == 4', ns = ns,
              column(2,
                     fluidRow(
                       column(1),
                       column(9,
                              style = "margin-left: -5px;",
                              pickerInput(
                                ns("preload_scen_4"),
                                label = "",
                                choices = c("Baseline", 
                                            "ADEC",
                                            "Cash Infusion ($100M)",
                                            "No DC Rollback",
                                            "ADEC + Cash Infusion + No DC Rollback"),
                                selected = "No DC Rollback"
                              )
                       ),
                       column(2,
                              style = "margin-top: 24px; margin-left: -3px; font-weight: 800; font-color: #000;",
                              actionButton(ns("refresh_scen_4"),
                                           label = NULL,
                                           icon = icon("refresh"),
                                           style = refresh_button_style)
                       )
                     )
              ))
            
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
          #----New Plan Design---- 
          conditionalPanel(
            condition = "input.ui_inputs == 'New Plan Design'", ns = ns,
            
            fluidRow(
              style = dynamic_input_style_top,
              column(1),
              column(2,
                  hover_text(
                      title = "% of New Hires in DC:",
                      text = "The percent of new hires opting for\nthe Defined Contribution (DC) plan.",
                      position = "right"),
                     style = percent_newhires_style),
              conditionalPanel(
                condition = "input.nclass >= 1", ns = ns,
                column(2,
                  fluidRow(
                    column(1),
                    column(8,
                       style = "margin-left: -5px;",
                       numericInput(
                         ns("NewHireDCPct_1"),
                         label = "",
                         step = 1,
                         min = 0,
                         max = 100,
                         value = DC_NewHires*100)
                    ),
                    column(2,
                       right_icon("%")
                       )))),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2,
                       fluidRow(
                         column(1),
                         column(8,
                            style = "margin-left: -5px;",
                            numericInput(
                              ns("NewHireDCPct_2"),
                              label = "",
                              step = 1,
                              min = 0,
                              max = 100,
                              value = DC_NewHires*100
                                )
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
              
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,
                       fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NewHireDCPct_3"),
                                  label = "",
                                  step = 1,
                                  min = 0,
                                  max = 100,
                                  value = DC_NewHires*100
                                )
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
              
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2,
                       fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NewHireDCPct_4"),
                                  label = "",
                                  step = 1,
                                  min = 0,
                                  max = 100,
                                  value = DC_NewHires*100
                                )
                         ),
                         column(2,
                                right_icon("%")
                         ))
                ))
            ),

            fluidRow(
              style = dynamic_input_style_middle,
              column(1),
              column(2,
                     hover_text(
                      title = "DC Contribution Policy:",
                      text = "...",
                      position = "right"), 
                     style = stress_test_style),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2, style = "margin-top: 10px;",
                       pickerInput(ns("DCPolicy_1"), 
                                   label = "",
                                   choices = c("Current Policy",
                                               "No DC Rollback",
                                               "Constant Rate"),
                                   selected = "Current Policy"))
                ),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2, style = "margin-top: 10px;",
                       pickerInput(ns("DCPolicy_2"), 
                                   label = "",
                                   choices = c("Current Policy",
                                               "No DC Rollback",
                                               "Constant Rate"),
                                   selected = "Current Policy"))
                ),
              
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2, style = "margin-top: 10px;",
                       pickerInput(ns("DCPolicy_3"), 
                                   label = "",
                                   choices = c("Current Policy",
                                               "No DC Rollback",
                                               "Constant Rate"),
                                   selected = "Current Policy"))
                ),
              
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2, style = "margin-top: 10px;",
                       pickerInput(ns("DCPolicy_4"), 
                                   label = "",
                                   choices = c("Current Policy",
                                               "No DC Rollback",
                                               "Constant Rate"),
                                   selected = "Current Policy"))
                )
            ),
            
            fluidRow(
              style = dynamic_input_style_bottom,
              column(1),
              column(2,
              hover_text(
                      title = "Employer DC Contribution Rate (new hires):",
                      text = "Employer contribution rate for new hires\nin the Defined Contribution (DC) plan.",
                      position = "right"),
                     style = emp_dc_cont_style),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2,
                       fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("DC_Contrib_1"),
                                  label = "",
                                  step = 1,
                                  min = 0,
                                  max = 100,
                                  value = DC_Contrib*100
                                )
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2, 
                    fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("DC_Contrib_2"),
                                  label = "",
                                  step = 1,
                                  min = 0,
                                  max = 100,
                                  value = DC_Contrib*100
                                )
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("DC_Contrib_3"),
                                  label = "",
                                  step = 1,
                                  min = 0,
                                  max = 100,
                                  value = DC_Contrib*100
                                )
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
              conditionalPanel(
                condition = 'input.nclass >= 4', ns = ns,
                column(2, 
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("DC_Contrib_4"),
                                  label = "",
                                  step = 1,
                                  min = 0,
                                  max = 100,
                                  value = DC_Contrib*100
                                )
                         ),
                         column(2,
                                right_icon("%")
                         ))
                ))
            )
          ),
          #----Discount Rates----- 
          conditionalPanel(
            condition = "input.ui_inputs == 'Discount Rates'", ns = ns,
            fluidRow(
              style = dynamic_input_style_top,
              column(1),
              column(2,
              hover_text(
                      title = "Discount Rate (current members):",
                      text = "The rate used to determine the net\npresent value of pension benefits\nfor current members.",
                      position = "right"),
                     style = dr_curr_mem_style),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2, 
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("dis_r_current_1"),
                                  label = "",
                                  step = 0.25,
                                  min = 4,
                                  max = 9,
                                  value = dis_r_current*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2, 
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("dis_r_current_2"),
                                  label = "",
                                  step = 0.25,
                                  min = 4,
                                  max = 9,
                                  value = dis_r_current*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,
                fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("dis_r_current_3"),
                                  label = "",
                                  step = 0.25,
                                  min = 4,
                                  max = 9,
                                  value = dis_r_current*100
                         )),
                         column(2, right_icon("%"))
                ))),
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2,
                fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("dis_r_current_4"),
                                  label = "",
                                  step = 0.25,
                                  min = 4,
                                  max = 9,
                                  value = dis_r_current*100)

                         ),
                         column(2,
                                right_icon("%")
                         ))
                ))
            ),
            
            fluidRow(
              style = dynamic_input_style_bottom,
              column(1),
              column(2, 
              hover_text(
                      title = "Discount Rate (new hires):",
                      text = "The rate used to determine the net\npresent value of pension benefits\nfor new hires.",
                      position = "right"),
                     style = dr_new_mem_style),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2,
                       fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("dis_r_new_1"),
                                  label = "",
                                  step = 0.25,
                                  min = 4,
                                  max = 9,
                                  value = dis_r_new*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2, 
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("dis_r_new_2"),
                                  label = "",
                                  step = 0.25,
                                  min = 4,
                                  max = 9,
                                  value = dis_r_new*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,  
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("dis_r_new_3"),
                                  label = "",
                                  step = 0.25,
                                  min = 4,
                                  max = 9,
                                  value = dis_r_new*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2,
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("dis_r_new_4"),
                                  label = "",
                                  step = 0.25,
                                  min = 4,
                                  max = 9,
                                  value = dis_r_new*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
                ))
            )
          ),
          #----Return Scenarios---- 
          conditionalPanel(
            condition = "input.ui_inputs == 'Return Scenarios'", ns = ns,
            fluidRow(
              style = dynamic_input_style_top,
              column(1),
              column(2,
                     hover_text(
                      title = "Stress Test:",
                      text = "Baseline (matches discount rate): Matches asset returns with the\ncurrent member discount rate input, default value is 7.65%\nwhich is consistent with plan's assumed rate of return.\n\n6% Fixed Return: Puts asset returns at a consistent annual\nrate of 6%.\n\n2022-25 Crisis + 6% Fixed Return: Adds a single recession \nscenario from 2022 to 2025, which assumes -24% returns in the \nfirst year, followed by three years of 11% average returns. \nOtherwise, asset returns are at a consistent annual rate of 6%.\n\n2022-25 Crisis + 2037-40 Crisis + 6% Fixed Return: Adds two\nrecession scenarios one from 2022 to 2025 and another\nfrom 2036 to 2039. Both recessions assume -24% returns\nin the first year, followed by three years of 11% average\nreturns. Otherwise, asset returns are at a consistent annual\nrate of 6%.",
                      position = "right"), 
                     style = stress_test_style),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2, style = "margin-top: 10px;",
                div(class = custom_dropdown_input,
                       pickerInput(ns("ScenType_1"), 
                                   label = "",
                                   choices = c("Baseline (matches discount rate)" = "Assumption",
                                               "6% Fixed Return" = "6% Constant",
                                               "2022-25 Crisis + 6% Fixed Return" = "Recession",
                                               "2022-25 Crisis + 2037-40 Crisis + 6% Fixed Return" = "Recurring Recession",
                                               "Custom" = "Model"),
                                   selected = "Assumption"))
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2, style = "margin-top: 10px;",
                       pickerInput(ns("ScenType_2"), 
                                   label = "",
                                   choices = c("Baseline (matches discount rate)" = "Assumption",
                                               "6% Fixed Return" = "6% Constant",
                                               "2022-25 Crisis + 6% Fixed Return" = "Recession",
                                               "2022-25 Crisis + 2037-40 Crisis + 6% Fixed Return" = "Recurring Recession",
                                               "Custom" = "Model"),
                                   selected = "Assumption")
                )),
              
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2, style = "margin-top: 10px;",
                       pickerInput(ns("ScenType_3"), 
                                   label = "",
                                   choices = c("Baseline (matches discount rate)" = "Assumption",
                                               "6% Fixed Return" = "6% Constant",
                                               "2022-25 Crisis + 6% Fixed Return" = "Recession",
                                               "2022-25 Crisis + 2037-40 Crisis + 6% Fixed Return" = "Recurring Recession",
                                               "Custom" = "Model"),
                                   selected = "Assumption")
                )),
              
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2, style = "margin-top: 10px;",
                       pickerInput(ns("ScenType_4"), 
                                   label = "",
                                   choices = c("Baseline (matches discount rate)" = "Assumption",
                                               "6% Fixed Return" = "6% Constant",
                                               "2022-25 Crisis + 6% Fixed Return" = "Recession",
                                               "2022-25 Crisis + 2037-40 Crisis + 6% Fixed Return" = "Recurring Recession",
                                               "Custom" = "Model"),
                                   selected = "Assumption")
                )) 
            ),

             fluidRow(
              style = dynamic_input_style_bottom,
              column(1),
              column(2, 
                conditionalPanel(
                condition = "(input.ScenType_1 == 'Model' | input.ScenType_2 == 'Model' | input.ScenType_3 == 'Model' | input.ScenType_4 == 'Model')", ns = ns,
              hover_text(
                      title = "Custom Return",
                      text = "If 'model' is selected then asset returns reflect this input.",
                      position = "right"),
                     style = dr_new_mem_style)),
              conditionalPanel(
                condition = "input.nclass >= 1 & input.ScenType_1 == 'Model'", ns = ns,
                column(2,
                       fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("model_return_1"),
                                  label = "",
                                  step = 0.25,
                                  min = -20,
                                  max = 50,
                                  value = model_return*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),

                conditionalPanel(
                condition = "input.nclass >= 1 & input.ScenType_1 != 'Model'", ns = ns,
                column(2)),

              conditionalPanel(
                condition = "input.nclass >= 2 & input.ScenType_2 == 'Model'", ns = ns,
                column(2, 
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("model_return_2"),
                                  label = "",
                                  step = 0.25,
                                  min = -20,
                                  max = 50,
                                  value = model_return*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),

                conditionalPanel(
                condition = "input.nclass >= 2 & input.ScenType_2 != 'Model'", ns = ns,
                column(2)),

              conditionalPanel(
                condition = "input.nclass >= 3 & input.ScenType_3 == 'Model'", ns = ns,
                column(2,  
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("model_return_3"),
                                  label = "",
                                  step = 0.25,
                                  min = -20,
                                  max = 50,
                                  value = model_return*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
                conditionalPanel(
                condition = "input.nclass >= 3 & input.ScenType_3 != 'Model'", ns = ns,
                column(2)),

              conditionalPanel(
                condition = "input.nclass >= 4 & input.ScenType_4 == 'Model'", ns = ns,
                column(2,
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("model_return_4"),
                                  label = "",
                                  step = 0.25,
                                  min = -20,
                                  max = 50,
                                  value = model_return*100)
                         ),
                         column(2,
                                right_icon("%")
                         ))
                )),
                conditionalPanel(
                condition = "input.nclass >= 4 & input.ScenType_4 != 'Model'", ns = ns,
                column(2))
            )
          ),
          #---Funding Policy---- 
          conditionalPanel(
            condition = "input.ui_inputs == 'Funding Policy'", ns = ns,
            
            fluidRow(
              style = dynamic_input_style_top,
              column(1),
              column(2,
                hover_text(
                      title = "Statutory (variable/fixed) or ADEC:",
                      text = "Statutory Rate: The plan's existing contribution,\nas established by state law or local ordinance.\nA variable statutory responds to a plan's\nfunded status. A fixed statutory rate\nremains fixed regardless of funding status.\n\nActuarial Determined Employer Contribution (ADEC):\nThe actuarially calculated contribution to\nproperly fund the plan.",
                      position = "right"),
                     style = funding_policy_style),
              conditionalPanel(
                condition = "input.nclass >= 1", ns = ns,
                column(2, style = control_panel_input_style_1,
                       pickerInput(ns("ER_Policy_1"),
                                   label = "",
                                   choices = c("Variable Statutory",
                                               "Fixed Statutory",
                                               "ADEC" = "ADC"),
                                   selected = "Variable Statutory")
                )),
              conditionalPanel(
                condition = "input.nclass >= 2", ns = ns,
                column(2, style = control_panel_input_style_1,
                       pickerInput(ns("ER_Policy_2"),
                                   label = "",
                                   choices = c("Variable Statutory",
                                               "Fixed Statutory",
                                               "ADEC" = "ADC"),
                                   selected = "Variable Statutory")
                )),
              conditionalPanel(
                condition = "input.nclass >= 3", ns = ns,
                column(2,  style = control_panel_input_style_1,
                       pickerInput(ns("ER_Policy_3"),
                                   label = "",
                                   choices = c("Variable Statutory",
                                               "Fixed Statutory",
                                               "ADEC" = "ADC"),
                                   selected = "Variable Statutory")
                )),
              conditionalPanel(
                condition = "input.nclass == 4", ns = ns,
                column(2,  style = control_panel_input_style_1,
                       pickerInput(ns("ER_Policy_4"),
                                   label = "",
                                   choices = c("Variable Statutory",
                                               "Fixed Statutory",
                                               "ADEC" = "ADC"),
                                   selected = "Variable Statutory")
                ))
            ),

          #-----Normal Cost Sharing (new hires)-----
            fluidRow(
              style = dynamic_input_style_middle,
              column(1),
              column(2,
                     hover_text(
                      title = "Normal Cost Sharing (new hires):",
                      text = "Yes: New hires share normal cost.\n\nNo: New hires do not share normal cost.\n\nNormal cost represetns a single valuation\nyear's portion of the value of actuarial\nliabilities.",
                      position = "right"),
                     style = norm_cs_new_hire_style),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2, style = control_panel_input_style_1,
                       pickerInput(ns("CostSharingNC_1"), 
                                   label = "",
                                   choices = c("Yes", "No"),
                                   selected = CostSharingNC)
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2, style = control_panel_input_style_1,
                       pickerInput(ns("CostSharingNC_2"), 
                                   label = "",
                                   choices = c("Yes", "No"),
                                   selected = CostSharingNC)
                )),
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,  style = control_panel_input_style_1,
                       pickerInput(ns("CostSharingNC_3"), 
                                   label = "",
                                   choices = c("Yes", "No"),
                                   selected = CostSharingNC)
                )),
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2,  style = control_panel_input_style_1,
                       pickerInput(ns("CostSharingNC_4"), 
                                   label = "",
                                   choices = c("Yes", "No"),
                                   selected = CostSharingNC)
                ))
            ),
            
            fluidRow(
              style = dynamic_input_style_middle,
              column(1),
              column(2,
              hover_text(
                title = "Amortization Cost Sharing (new hires):",
                text = "Yes: New hires share amortizaiton payments\n\nNo: New hires do not share amortiztion payments.\n\nAmortization payments are regular contributions\nto reduce the unfunded liability and are on a\nset schedule.",
                position = "right"),
              style = amo_cs_new_hire_style),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2, style = control_panel_input_style_1,
                       pickerInput(ns("CostSharingAmo_1"), 
                                   label = "",
                                   choices = c("Yes", "No"),
                                   selected = CostSharingAmo)
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2, style = control_panel_input_style_1,
                       pickerInput(ns("CostSharingAmo_2"), 
                                   label = "",
                                   choices = c("Yes", "No"),
                                   selected = CostSharingAmo)
                )),
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,  style = control_panel_input_style_1,
                       pickerInput(ns("CostSharingAmo_3"), 
                                   label = "",
                                   choices = c("Yes", "No"),
                                   selected = CostSharingAmo)
                )),
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2,  style = control_panel_input_style_1,
                       pickerInput(ns("CostSharingAmo_4"), 
                                   label = "",
                                   choices = c("Yes", "No"),
                                   selected = CostSharingAmo)
                ))
            ),
            
            fluidRow(
              style = dynamic_input_style_bottom,
              column(1),
              column(2,
              hover_text(
                title = "Cash Infusion in 2022 (millions):",
                text = "One-time cash infusion in 2022.",
                position = "right"),
               style = cash_inf_style),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2,
                       fluidRow(
                         column(1),
                         column(2,
                                right_icon("$")
                         ),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("CashInfusion_1"),
                                  label = "",
                                  step = 1,
                                  min = 0,
                                  value = CashInfusion
                                ))
                         )
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2, 
                  fluidRow(
                         column(1),
                         column(2,
                                right_icon("$")
                         ),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("CashInfusion_2"),
                                  label = "",
                                  step = 1,
                                  min = 0,
                                  value = CashInfusion
                                ))
                         )
                )),
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,  
                  fluidRow(
                         column(1),
                         column(2,
                                right_icon("$")
                         ),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("CashInfusion_3"),
                                  label = "",
                                  step = 1,
                                  min = 0,
                                  value = CashInfusion
                                ))
                         )
                )),
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2,  
                  fluidRow(
                         column(1),
                         column(2,
                                right_icon("$")
                         ),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("CashInfusion_4"),
                                  label = "",
                                  step = 1,
                                  min = 0,
                                  value = CashInfusion
                                ))
                         )
                ))
            )
          ),
          #-----Amortization Policy-----
          conditionalPanel(
            condition = "input.ui_inputs == 'Amortization Policy'", ns = ns,
            fluidRow(
              style = dynamic_input_style_top,
              column(1),
              column(2, 
              hover_text(
                title = "Amortization Period for Current Unfunded Liability:",
                text = "Schedule (in years) to pay off current unfunded liability.", 
                position = "right"),
                style = amo_per_curr_ual),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2, 
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_CurrentDebt_1"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_CurrentDebt
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         ),
                         )
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2,
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_CurrentDebt_2"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_CurrentDebt
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         ),
                         )
                )),
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_CurrentDebt_3"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_CurrentDebt
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         ),
                         ))
              ),
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2,  
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_CurrentDebt_4"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_CurrentDebt
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         ),
                         )
                ))
            ),
            fluidRow(
              style = dynamic_input_style_middle,
              column(1),
              column(2,
              hover_text(
                      title = "Amortization Period for New Unfunded Liability (current hires):",
                      text = "Schedule (in years) to pay off new unfunded liability (current members).",
                      position = "right"),
                style = amo_per_new_ual),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2, 
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_NewDebtCurrentHire_1"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_NewDebtCurrentHire
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         ),
                         )
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2,
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_NewDebtCurrentHire_2"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_NewDebtCurrentHire
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         ),
                         )
                )),
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,  
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_NewDebtCurrentHire_3"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_NewDebtCurrentHire
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         ),
                         )
                )),
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2,
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_NewDebtCurrentHire_4"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_NewDebtCurrentHire
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         )
                         )
                ))
            ),
            
            fluidRow(
              style = dynamic_input_style_middle,
              column(1),
              column(2,
              hover_text(
                      title = "Amortization Period for New Unfunded Liability (new hires)",
                      text = "Schedule (in years) to pay off new unfunded liability (new hires).",
                      position = "right"),
              style = amo_per_newnew_ual),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2, 
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_NewDebtNewHire_1"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_NewDebtNewHire
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         )
                         )
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2, 
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_NewDebtNewHire_2"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_NewDebtNewHire
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         ),
                         )
                )),
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,  
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_NewDebtNewHire_3"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_NewDebtNewHire
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         )
                         )
                )),
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2,  
                  fluidRow(
                         column(1),
                         column(8,
                                style = "margin-left: -5px;",
                                numericInput(
                                  ns("NoYearsADC_NewDebtNewHire_4"),
                                  label = "",
                                  step = 1,
                                  min = 1,
                                  max = 50,
                                  value = NoYearsADC_NewDebtNewHire
                       )),
                        column(2,
                        style = "text-align: center;",
                                emoji_icon("📆")
                         )
                         )
                ))
            ),
            
            fluidRow(
              style = dynamic_input_style_middle,
              column(1),
              column(2,
              hover_text(
                      title = "Amortization Method for Unfunded Liability Created by Current Plan:",
                      text = "Current Plan\n\nLevel $: Unfunded liabilities are armortized\nsuch that the plan expects to pay the same\ndollar amount each year of the schedule\n\nLevel %: Unfunded liabilites are amortized\nsuch that the plan expects to pay the same\npercentage of payroll each year\nof the schedule.",
                      position = "right"),
                style = amo_method_ual_currp),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2, style = control_panel_input_style_1,
                       pickerInput(ns("AmoMethod_CurrentHire_1"), 
                                   label = "",
                                   choices = c("Level %", "Level $" = "Level dollar"),
                                   selected = AmoMethod_CurrentHire)
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2, style = control_panel_input_style_1,
                       pickerInput(ns("AmoMethod_CurrentHire_2"), 
                                   label = "",
                                   choices = c("Level %", "Level $" = "Level dollar"),
                                   selected = AmoMethod_CurrentHire)
                )),
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,  style = control_panel_input_style_1,
                       pickerInput(ns("AmoMethod_CurrentHire_3"), 
                                   label = "",
                                   choices = c("Level %", "Level $" = "Level dollar"),
                                   selected = AmoMethod_CurrentHire)
                )),
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2,  style = control_panel_input_style_1,
                       pickerInput(ns("AmoMethod_CurrentHire_4"), 
                                   label = "",
                                   choices = c("Level %", "Level $" = "Level dollar"),
                                   selected = AmoMethod_CurrentHire)
                ))
            ),
            
            fluidRow(
              style = dynamic_input_style_bottom,
              column(1),
              column(2, 
              hover_text(
                      title = "Amortization Method for Unfunded Liability Created by New Plan:",
                      text = "New Plan\n\nLevel $: Unfunded liabilities are armortized\nsuch that the plan expects to pay the same\ndollar amount each year of the schedule\n\nLevel %: Unfunded liabilites are amortized\nsuch that the plan expects to pay the same\npercentage of payroll each year\nof the schedule.",
                      position = "right"),
               style = amo_method_ual_newp),
              conditionalPanel(
                condition = 'input.nclass >= 1', ns = ns,
                column(2, style = control_panel_input_style_1,
                       pickerInput(ns("AmoMethod_NewHire_1"), 
                                   label = "",
                                   choices = c("Level %", "Level $" = "Level dollar"),
                                   selected = AmoMethod_NewHire)
                )),
              conditionalPanel(
                condition = 'input.nclass >= 2', ns = ns,
                column(2, style = control_panel_input_style_1,
                       pickerInput(ns("AmoMethod_NewHire_2"), 
                                   label = "",
                                   choices = c("Level %", "Level $" = "Level dollar"),
                                   selected = AmoMethod_NewHire)
                )),
              conditionalPanel(
                condition = 'input.nclass >= 3', ns = ns,
                column(2,  style = control_panel_input_style_1,
                       pickerInput(ns("AmoMethod_NewHire_3"), 
                                   label = "",
                                   choices = c("Level %", "Level $" = "Level dollar"),
                                   selected = AmoMethod_NewHire)
                )),
              conditionalPanel(
                condition = 'input.nclass == 4', ns = ns,
                column(2,  style = control_panel_input_style_1,
                       pickerInput(ns("AmoMethod_NewHire_4"), 
                                   label = "",
                                   choices = c("Level %", "Level $" = "Level dollar"),
                                   selected = AmoMethod_NewHire)
                ))
            )
          ),
          #-----End of Control Panel-----
          div(style = "height: 20px")
        ))
    ),
    
    div(id = ns("all-in-emp"),
        style = control_div_style,
        
        actionButton(ns("empbutton_close"),
                     label = NULL,
                     icon = icon("close"),
                     style = "position: absolute;
                   font-size: 14px;
                   right: 1%;
                   top: 1%;
                   z-index: 9999;"
        ),
        
        fluidRow(
          h4(
            hover_text(
                      title = "All-In Employer Costs by 2052",
                      text = "The true cost of a pension is not only\nin the annual contributions, but also in whatever\nunfunded liabilities remain. The ”All-in Employer Cost”\ncombines the total amount paid in employer contributions\nand adds what unfunded liabilities remain at the end of\nthe forecasting window.",
                      position = "bottom"),
                      style = "font-size: 22px",
                      align = "center")),
        br(),
        fluidRow(
          column(2),
          column(8,
                 shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart5"), height = "30vh", width = "65vw"))),
          column(2)
        ),
        br(),
        fluidRow(
          column(1),
          column(10,
                 reactable::reactableOutput(ns("table3"))),
          column(1))
    ),
    
    fluidRow(
      style = "margin: 0; height: 7%",
      column(
        width = 4,
        align = "left",
        style = top_button_style,
        div(
          actionButton(ns("controlbutton"),
                       label = " Control Panel",
                       icon = icon("keyboard"))
        )
      ),
      column(
        width = 4,
        align = "center",
        style = top_button_style,
        div(
          actionButton(ns("empbutton"),
                       label = " All-In Employer Costs",
                       icon = icon("dollar-sign"))
        )
      ),
      column(
        width = 4,
        align = "right",
        style = top_button_style,
        div(
          downloadButton(
            ns("download_excel"),
            "Download Data"
          )
        )
      )
    ),
    
    fluidRow(
      style = top_charts_fluidRow,
      column(
        width = 6,
        style = top_charts_column,
        data_card(
          shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart1"), height = "38vh", width = "45vw"))
        )
      ),
      column(
        width = 6,
        style = top_charts_column,
        data_card(
          shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart2"), height = "38vh", width = "45vw"))
        )
      )
    ),
    
    fluidRow(
      style = bottom_charts_fluidRow,
      column(
        width = 6,
        style = bottom_charts_column,
        data_card(
          shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart3"), height = "38vh", width = "46vw"))
        )
      ),
      column(
        width = 6,
        style = bottom_charts_column,
        data_card(
          shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("chart4"), height = "38vh", width = "46vw"))
        )
      )
    )
  )
  
}


# Server

mod_deterministic_server <- function(id, app_data) {
  server <- function(input, output, session) {
    ns <- session$ns
    
    preload_inputs <- readr::read_csv("backend/Montana PERS preloaded options.csv")
    first_name <- names(preload_inputs)[2]
    second_name <- names(preload_inputs)[3]
    third_name <- names(preload_inputs)[4]
    fourth_name <- names(preload_inputs)[5]
    fifth_name <- names(preload_inputs)[6]
    
    observe({
      if (input$preload_scen_1 == first_name | (input$refresh_scen_1 & input$preload_scen_1 == first_name)) {
        
        preload_NewHireDCPct_1 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", first_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_1 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", first_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_1<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", first_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_1 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", first_name][[1]])  |> as.numeric() * 100
        preload_model_return_1 <- (preload_inputs[preload_inputs$Input == "ModelReturn", first_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_1 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_1 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_1 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_1 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", first_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_1", value = first_name)
        updateNumericInput(session, "NewHireDCPct_1", value = preload_NewHireDCPct_1)
        updatePickerInput(session, "DCPolicy_1", selected = preload_inputs[preload_inputs$Input == "DC_Policy", first_name][[1]])
        updateNumericInput(session, "DC_Contrib_1", value = preload_DC_Contrib_1)
        updateNumericInput(session, "dis_r_current_1", value = preload_dis_r_current_1)
        updateNumericInput(session, "dis_r_new_1", value = preload_dis_r_new_1)
        updatePickerInput(session, "ScenType_1", selected = preload_inputs[preload_inputs$Input == "DeSimType", first_name][[1]])
        updateNumericInput(session, "model_return_1", value = preload_model_return_1)
        updatePickerInput(session, "ER_Policy_1", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", first_name][[1]])
        updatePickerInput(session, "CostSharingNC_1", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", first_name][[1]])
        updatePickerInput(session, "CostSharingAmo_1", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", first_name][[1]])
        updateNumericInput(session, "CashInfusion_1", value = preload_CashInfusion_1)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_1", value = preload_NoYearsADC_CurrentDebt_1)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_1", value = preload_NoYearsADC_NewDebtCurrentHire_1)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_1", value = preload_NoYearsADC_NewDebtNewHire_1)
        updatePickerInput(session, "AmoMethod_CurrentHire_1", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", first_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_1", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", first_name][[1]])
        
      } else if (input$preload_scen_1 == second_name | (input$refresh_scen_1 & input$preload_scen_1 == second_name)) {
        
        preload_NewHireDCPct_1 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", second_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_1 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", second_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_1<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", second_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_1 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", second_name][[1]])  |> as.numeric() * 100
        preload_model_return_1 <- (preload_inputs[preload_inputs$Input == "ModelReturn", second_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_1 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_1 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_1 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_1 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", second_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_1", value = second_name)
        updateNumericInput(session, "NewHireDCPct_1", value = preload_NewHireDCPct_1)
        updatePickerInput(session, "DCPolicy_1", selected = preload_inputs[preload_inputs$Input == "DC_Policy", second_name][[1]])
        updateNumericInput(session, "DC_Contrib_1", value = preload_DC_Contrib_1)
        updateNumericInput(session, "dis_r_current_1", value = preload_dis_r_current_1)
        updateNumericInput(session, "dis_r_new_1", value = preload_dis_r_new_1)
        updatePickerInput(session, "ScenType_1", selected = preload_inputs[preload_inputs$Input == "DeSimType", second_name][[1]])
        updateNumericInput(session, "model_return_1", value = preload_model_return_1)
        updatePickerInput(session, "ER_Policy_1", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", second_name][[1]])
        updatePickerInput(session, "CostSharingNC_1", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", second_name][[1]])
        updatePickerInput(session, "CostSharingAmo_1", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", second_name][[1]])
        updateNumericInput(session, "CashInfusion_1", value = preload_CashInfusion_1)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_1", value = preload_NoYearsADC_CurrentDebt_1)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_1", value = preload_NoYearsADC_NewDebtCurrentHire_1)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_1", value = preload_NoYearsADC_NewDebtNewHire_1)
        updatePickerInput(session, "AmoMethod_CurrentHire_1", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", second_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_1", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", second_name][[1]])
        
        
      } else if (input$preload_scen_1 == third_name | (input$refresh_scen_1 & input$preload_scen_1 == third_name)) {
        
        preload_NewHireDCPct_1 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", third_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_1 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", third_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_1<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", third_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_1 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", third_name][[1]])  |> as.numeric() * 100
        preload_model_return_1 <- (preload_inputs[preload_inputs$Input == "ModelReturn", third_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_1 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_1 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_1 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_1 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", third_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_1", value = third_name)
        updateNumericInput(session, "NewHireDCPct_1", value = preload_NewHireDCPct_1)
        updatePickerInput(session, "DCPolicy_1", selected = preload_inputs[preload_inputs$Input == "DC_Policy", third_name][[1]])
        updateNumericInput(session, "DC_Contrib_1", value = preload_DC_Contrib_1)
        updateNumericInput(session, "dis_r_current_1", value = preload_dis_r_current_1)
        updateNumericInput(session, "dis_r_new_1", value = preload_dis_r_new_1)
        updatePickerInput(session, "ScenType_1", selected = preload_inputs[preload_inputs$Input == "DeSimType", third_name][[1]])
        updateNumericInput(session, "model_return_1", value = preload_model_return_1)
        updatePickerInput(session, "ER_Policy_1", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", third_name][[1]])
        updatePickerInput(session, "CostSharingNC_1", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", third_name][[1]])
        updatePickerInput(session, "CostSharingAmo_1", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", third_name][[1]])
        updateNumericInput(session, "CashInfusion_1", value = preload_CashInfusion_1)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_1", value = preload_NoYearsADC_CurrentDebt_1)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_1", value = preload_NoYearsADC_NewDebtCurrentHire_1)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_1", value = preload_NoYearsADC_NewDebtNewHire_1)
        updatePickerInput(session, "AmoMethod_CurrentHire_1", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", third_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_1", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", third_name][[1]])
        
        
      } else if (input$preload_scen_1 == fourth_name | (input$refresh_scen_1 & input$preload_scen_1 == fourth_name)) {
        
        preload_NewHireDCPct_1 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", fourth_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_1 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", fourth_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_1<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", fourth_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_1 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", fourth_name][[1]])  |> as.numeric() * 100
        preload_model_return_1 <- (preload_inputs[preload_inputs$Input == "ModelReturn", fourth_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_1 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_1 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_1 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_1 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", fourth_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_1", value = fourth_name)
        updateNumericInput(session, "NewHireDCPct_1", value = preload_NewHireDCPct_1)
        updatePickerInput(session, "DCPolicy_1", selected = preload_inputs[preload_inputs$Input == "DC_Policy", fourth_name][[1]])
        updateNumericInput(session, "DC_Contrib_1", value = preload_DC_Contrib_1)
        updateNumericInput(session, "dis_r_current_1", value = preload_dis_r_current_1)
        updateNumericInput(session, "dis_r_new_1", value = preload_dis_r_new_1)
        updatePickerInput(session, "ScenType_1", selected = preload_inputs[preload_inputs$Input == "DeSimType", fourth_name][[1]])
        updateNumericInput(session, "model_return_1", value = preload_model_return_1)
        updatePickerInput(session, "ER_Policy_1", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", fourth_name][[1]])
        updatePickerInput(session, "CostSharingNC_1", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", fourth_name][[1]])
        updatePickerInput(session, "CostSharingAmo_1", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", fourth_name][[1]])
        updateNumericInput(session, "CashInfusion_1", value = preload_CashInfusion_1)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_1", value = preload_NoYearsADC_CurrentDebt_1)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_1", value = preload_NoYearsADC_NewDebtCurrentHire_1)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_1", value = preload_NoYearsADC_NewDebtNewHire_1)
        updatePickerInput(session, "AmoMethod_CurrentHire_1", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", fourth_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_1", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", fourth_name][[1]])

        } else if (input$preload_scen_1 == fifth_name | (input$refresh_scen_1 & input$preload_scen_1 == fifth_name)) {
        
        preload_NewHireDCPct_1 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", fifth_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_1 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", fifth_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_1<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", fifth_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_1 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", fifth_name][[1]])  |> as.numeric() * 100
        preload_model_return_1 <- (preload_inputs[preload_inputs$Input == "ModelReturn", fifth_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_1 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_1 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_1 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_1 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", fifth_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_1", value = fifth_name)
        updateNumericInput(session, "NewHireDCPct_1", value = preload_NewHireDCPct_1)
        updatePickerInput(session, "DCPolicy_1", selected = preload_inputs[preload_inputs$Input == "DC_Policy", fifth_name][[1]])
        updateNumericInput(session, "DC_Contrib_1", value = preload_DC_Contrib_1)
        updateNumericInput(session, "dis_r_current_1", value = preload_dis_r_current_1)
        updateNumericInput(session, "dis_r_new_1", value = preload_dis_r_new_1)
        updatePickerInput(session, "ScenType_1", selected = preload_inputs[preload_inputs$Input == "DeSimType", fifth_name][[1]])
        updateNumericInput(session, "model_return_1", value = preload_model_return_1)
        updatePickerInput(session, "ER_Policy_1", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", fifth_name][[1]])
        updatePickerInput(session, "CostSharingNC_1", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", fifth_name][[1]])
        updatePickerInput(session, "CostSharingAmo_1", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", fifth_name][[1]])
        updateNumericInput(session, "CashInfusion_1", value = preload_CashInfusion_1)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_1", value = preload_NoYearsADC_CurrentDebt_1)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_1", value = preload_NoYearsADC_NewDebtCurrentHire_1)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_1", value = preload_NoYearsADC_NewDebtNewHire_1)
        updatePickerInput(session, "AmoMethod_CurrentHire_1", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", fifth_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_1", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", fifth_name][[1]])
        
      } else {}
      
    })
    
    
    observe({
      if (input$preload_scen_2 == first_name | (input$refresh_scen_2 & input$preload_scen_2 == first_name)) {
        
        preload_NewHireDCPct_2 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", first_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_2 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", first_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_2<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", first_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_2 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", first_name][[1]])  |> as.numeric() * 100
        preload_model_return_2 <- (preload_inputs[preload_inputs$Input == "ModelReturn", first_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_2 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_2 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_2 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_2 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", first_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_2", value = first_name)
        updateNumericInput(session, "NewHireDCPct_2", value = preload_NewHireDCPct_2)
        updatePickerInput(session, "DCPolicy_2", selected = preload_inputs[preload_inputs$Input == "DC_Policy", first_name][[1]])
        updateNumericInput(session, "DC_Contrib_2", value = preload_DC_Contrib_2)
        updateNumericInput(session, "dis_r_current_2", value = preload_dis_r_current_2)
        updateNumericInput(session, "dis_r_new_2", value = preload_dis_r_new_2)
        updatePickerInput(session, "ScenType_2", selected = preload_inputs[preload_inputs$Input == "DeSimType", first_name][[1]])
        updateNumericInput(session, "model_return_2", value = preload_model_return_2)
        updatePickerInput(session, "ER_Policy_2", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", first_name][[1]])
        updatePickerInput(session, "CostSharingNC_2", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", first_name][[1]])
        updatePickerInput(session, "CostSharingAmo_2", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", first_name][[1]])
        updateNumericInput(session, "CashInfusion_2", value = preload_CashInfusion_2)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_2", value = preload_NoYearsADC_CurrentDebt_2)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_2", value = preload_NoYearsADC_NewDebtCurrentHire_2)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_2", value = preload_NoYearsADC_NewDebtNewHire_2)
        updatePickerInput(session, "AmoMethod_CurrentHire_2", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", first_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_2", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", first_name][[1]])
        
      } else if (input$preload_scen_2 == second_name | (input$refresh_scen_2 & input$preload_scen_2 == second_name)) {
        
        preload_NewHireDCPct_2 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", second_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_2 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", second_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_2<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", second_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_2 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", second_name][[1]])  |> as.numeric() * 100
        preload_model_return_2 <- (preload_inputs[preload_inputs$Input == "ModelReturn", second_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_2 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_2 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_2 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_2 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", second_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_2", value = second_name)
        updateNumericInput(session, "NewHireDCPct_2", value = preload_NewHireDCPct_2)
        updatePickerInput(session, "DCPolicy_2", selected = preload_inputs[preload_inputs$Input == "DC_Policy", second_name][[1]])
        updateNumericInput(session, "DC_Contrib_2", value = preload_DC_Contrib_2)
        updateNumericInput(session, "dis_r_current_2", value = preload_dis_r_current_2)
        updateNumericInput(session, "dis_r_new_2", value = preload_dis_r_new_2)
        updatePickerInput(session, "ScenType_2", selected = preload_inputs[preload_inputs$Input == "DeSimType", second_name][[1]])
        updateNumericInput(session, "model_return_2", value = preload_model_return_2)
        updatePickerInput(session, "ER_Policy_2", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", second_name][[1]])
        updatePickerInput(session, "CostSharingNC_2", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", second_name][[1]])
        updatePickerInput(session, "CostSharingAmo_2", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", second_name][[1]])
        updateNumericInput(session, "CashInfusion_2", value = preload_CashInfusion_2)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_2", value = preload_NoYearsADC_CurrentDebt_2)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_2", value = preload_NoYearsADC_NewDebtCurrentHire_2)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_2", value = preload_NoYearsADC_NewDebtNewHire_2)
        updatePickerInput(session, "AmoMethod_CurrentHire_2", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", second_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_2", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", second_name][[1]])
        
        
      } else if (input$preload_scen_2 == third_name | (input$refresh_scen_2 & input$preload_scen_2 == third_name)) {
        
        preload_NewHireDCPct_2 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", third_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_2 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", third_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_2<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", third_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_2 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", third_name][[1]])  |> as.numeric() * 100
        preload_model_return_2 <- (preload_inputs[preload_inputs$Input == "ModelReturn", third_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_2 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_2 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_2 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_2 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", third_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_2", value = third_name)
        updateNumericInput(session, "NewHireDCPct_2", value = preload_NewHireDCPct_2)
        updatePickerInput(session, "DCPolicy_2", selected = preload_inputs[preload_inputs$Input == "DC_Policy", third_name][[1]])
        updateNumericInput(session, "DC_Contrib_2", value = preload_DC_Contrib_2)
        updateNumericInput(session, "dis_r_current_2", value = preload_dis_r_current_2)
        updateNumericInput(session, "dis_r_new_2", value = preload_dis_r_new_2)
        updatePickerInput(session, "ScenType_2", selected = preload_inputs[preload_inputs$Input == "DeSimType", third_name][[1]])
        updateNumericInput(session, "model_return_2", value = preload_model_return_2)
        updatePickerInput(session, "ER_Policy_2", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", third_name][[1]])
        updatePickerInput(session, "CostSharingNC_2", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", third_name][[1]])
        updatePickerInput(session, "CostSharingAmo_2", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", third_name][[1]])
        updateNumericInput(session, "CashInfusion_2", value = preload_CashInfusion_2)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_2", value = preload_NoYearsADC_CurrentDebt_2)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_2", value = preload_NoYearsADC_NewDebtCurrentHire_2)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_2", value = preload_NoYearsADC_NewDebtNewHire_2)
        updatePickerInput(session, "AmoMethod_CurrentHire_2", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", third_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_2", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", third_name][[1]])
        
        
      } else if (input$preload_scen_2 == fourth_name | (input$refresh_scen_2 & input$preload_scen_2 == fourth_name)) {
        
        preload_NewHireDCPct_2 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", fourth_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_2 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", fourth_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_2<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", fourth_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_2 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", fourth_name][[1]])  |> as.numeric() * 100
        preload_model_return_2 <- (preload_inputs[preload_inputs$Input == "ModelReturn", fourth_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_2 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_2 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_2 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_2 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", fourth_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_2", value = fourth_name)
        updateNumericInput(session, "NewHireDCPct_2", value = preload_NewHireDCPct_2)
        updatePickerInput(session, "DCPolicy_2", selected = preload_inputs[preload_inputs$Input == "DC_Policy", fourth_name][[1]])
        updateNumericInput(session, "DC_Contrib_2", value = preload_DC_Contrib_2)
        updateNumericInput(session, "dis_r_current_2", value = preload_dis_r_current_2)
        updateNumericInput(session, "dis_r_new_2", value = preload_dis_r_new_2)
        updatePickerInput(session, "ScenType_2", selected = preload_inputs[preload_inputs$Input == "DeSimType", fourth_name][[1]])
        updateNumericInput(session, "model_return_2", value = preload_model_return_2)
        updatePickerInput(session, "ER_Policy_2", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", fourth_name][[1]])
        updatePickerInput(session, "CostSharingNC_2", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", fourth_name][[1]])
        updatePickerInput(session, "CostSharingAmo_2", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", fourth_name][[1]])
        updateNumericInput(session, "CashInfusion_2", value = preload_CashInfusion_2)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_2", value = preload_NoYearsADC_CurrentDebt_2)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_2", value = preload_NoYearsADC_NewDebtCurrentHire_2)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_2", value = preload_NoYearsADC_NewDebtNewHire_2)
        updatePickerInput(session, "AmoMethod_CurrentHire_2", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", fourth_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_2", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", fourth_name][[1]])

        } else if (input$preload_scen_2 == fifth_name | (input$refresh_scen_2 & input$preload_scen_2 == fifth_name)) {
        
        preload_NewHireDCPct_2 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", fifth_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_2 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", fifth_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_2<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", fifth_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_2 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", fifth_name][[1]])  |> as.numeric() * 100
        preload_model_return_2 <- (preload_inputs[preload_inputs$Input == "ModelReturn", fifth_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_2 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_2 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_2 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_2 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", fifth_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_2", value = fifth_name)
        updateNumericInput(session, "NewHireDCPct_2", value = preload_NewHireDCPct_2)
        updatePickerInput(session, "DCPolicy_2", selected = preload_inputs[preload_inputs$Input == "DC_Policy", fifth_name][[1]])
        updateNumericInput(session, "DC_Contrib_2", value = preload_DC_Contrib_2)
        updateNumericInput(session, "dis_r_current_2", value = preload_dis_r_current_2)
        updateNumericInput(session, "dis_r_new_2", value = preload_dis_r_new_2)
        updatePickerInput(session, "ScenType_2", selected = preload_inputs[preload_inputs$Input == "DeSimType", fifth_name][[1]])
        updateNumericInput(session, "model_return_2", value = preload_model_return_2)
        updatePickerInput(session, "ER_Policy_2", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", fifth_name][[1]])
        updatePickerInput(session, "CostSharingNC_2", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", fifth_name][[1]])
        updatePickerInput(session, "CostSharingAmo_2", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", fifth_name][[1]])
        updateNumericInput(session, "CashInfusion_2", value = preload_CashInfusion_2)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_2", value = preload_NoYearsADC_CurrentDebt_2)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_2", value = preload_NoYearsADC_NewDebtCurrentHire_2)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_2", value = preload_NoYearsADC_NewDebtNewHire_2)
        updatePickerInput(session, "AmoMethod_CurrentHire_2", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", fifth_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_2", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", fifth_name][[1]])
        
      } else {}
      
    })
    
    
    observe({
      if (input$preload_scen_3 == first_name | (input$refresh_scen_3 & input$preload_scen_3 == first_name)) {
        
        preload_NewHireDCPct_3 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", first_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_3 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", first_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_3<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", first_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_3 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", first_name][[1]])  |> as.numeric() * 100
        preload_model_return_3 <- (preload_inputs[preload_inputs$Input == "ModelReturn", first_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_3 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_3 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_3 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_3 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", first_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_3", value = first_name)
        updateNumericInput(session, "NewHireDCPct_3", value = preload_NewHireDCPct_3)
        updatePickerInput(session, "DCPolicy_3", selected = preload_inputs[preload_inputs$Input == "DC_Policy", first_name][[1]])
        updateNumericInput(session, "DC_Contrib_3", value = preload_DC_Contrib_3)
        updateNumericInput(session, "dis_r_current_3", value = preload_dis_r_current_3)
        updateNumericInput(session, "dis_r_new_3", value = preload_dis_r_new_3)
        updatePickerInput(session, "ScenType_3", selected = preload_inputs[preload_inputs$Input == "DeSimType", first_name][[1]])
        updateNumericInput(session, "model_return_3", value = preload_model_return_3)
        updatePickerInput(session, "ER_Policy_3", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", first_name][[1]])
        updatePickerInput(session, "CostSharingNC_3", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", first_name][[1]])
        updatePickerInput(session, "CostSharingAmo_3", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", first_name][[1]])
        updateNumericInput(session, "CashInfusion_3", value = preload_CashInfusion_3)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_3", value = preload_NoYearsADC_CurrentDebt_3)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_3", value = preload_NoYearsADC_NewDebtCurrentHire_3)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_3", value = preload_NoYearsADC_NewDebtNewHire_3)
        updatePickerInput(session, "AmoMethod_CurrentHire_3", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", first_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_3", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", first_name][[1]])
        
      } else if (input$preload_scen_3 == second_name | (input$refresh_scen_3 & input$preload_scen_3 == second_name)) {
        
        preload_NewHireDCPct_3 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", second_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_3 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", second_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_3<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", second_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_3 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", second_name][[1]])  |> as.numeric() * 100
        preload_model_return_3 <- (preload_inputs[preload_inputs$Input == "ModelReturn", second_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_3 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_3 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_3 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_3 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", second_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_3", value = second_name)
        updateNumericInput(session, "NewHireDCPct_3", value = preload_NewHireDCPct_3)
        updatePickerInput(session, "DCPolicy_3", selected = preload_inputs[preload_inputs$Input == "DC_Policy", second_name][[1]])
        updateNumericInput(session, "DC_Contrib_3", value = preload_DC_Contrib_3)
        updateNumericInput(session, "dis_r_current_3", value = preload_dis_r_current_3)
        updateNumericInput(session, "dis_r_new_3", value = preload_dis_r_new_3)
        updatePickerInput(session, "ScenType_3", selected = preload_inputs[preload_inputs$Input == "DeSimType", second_name][[1]])
        updateNumericInput(session, "model_return_3", value = preload_model_return_3)
        updatePickerInput(session, "ER_Policy_3", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", second_name][[1]])
        updatePickerInput(session, "CostSharingNC_3", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", second_name][[1]])
        updatePickerInput(session, "CostSharingAmo_3", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", second_name][[1]])
        updateNumericInput(session, "CashInfusion_3", value = preload_CashInfusion_3)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_3", value = preload_NoYearsADC_CurrentDebt_3)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_3", value = preload_NoYearsADC_NewDebtCurrentHire_3)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_3", value = preload_NoYearsADC_NewDebtNewHire_3)
        updatePickerInput(session, "AmoMethod_CurrentHire_3", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", second_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_3", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", second_name][[1]])
        
        
      } else if (input$preload_scen_3 == third_name | (input$refresh_scen_3 & input$preload_scen_3 == third_name)) {
        
        preload_NewHireDCPct_3 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", third_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_3 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", third_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_3<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", third_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_3 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", third_name][[1]])  |> as.numeric() * 100
        preload_model_return_3 <- (preload_inputs[preload_inputs$Input == "ModelReturn", third_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_3 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_3 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_3 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_3 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", third_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_3", value = third_name)
        updateNumericInput(session, "NewHireDCPct_3", value = preload_NewHireDCPct_3)
        updatePickerInput(session, "DCPolicy_3", selected = preload_inputs[preload_inputs$Input == "DC_Policy", third_name][[1]])
        updateNumericInput(session, "DC_Contrib_3", value = preload_DC_Contrib_3)
        updateNumericInput(session, "dis_r_current_3", value = preload_dis_r_current_3)
        updateNumericInput(session, "dis_r_new_3", value = preload_dis_r_new_3)
        updatePickerInput(session, "ScenType_3", selected = preload_inputs[preload_inputs$Input == "DeSimType", third_name][[1]])
        updateNumericInput(session, "model_return_3", value = preload_model_return_3)
        updatePickerInput(session, "ER_Policy_3", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", third_name][[1]])
        updatePickerInput(session, "CostSharingNC_3", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", third_name][[1]])
        updatePickerInput(session, "CostSharingAmo_3", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", third_name][[1]])
        updateNumericInput(session, "CashInfusion_3", value = preload_CashInfusion_3)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_3", value = preload_NoYearsADC_CurrentDebt_3)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_3", value = preload_NoYearsADC_NewDebtCurrentHire_3)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_3", value = preload_NoYearsADC_NewDebtNewHire_3)
        updatePickerInput(session, "AmoMethod_CurrentHire_3", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", third_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_3", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", third_name][[1]])
        
        
      } else if (input$preload_scen_3 == fourth_name | (input$refresh_scen_3 & input$preload_scen_3 == fourth_name)) {
        
        preload_NewHireDCPct_3 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", fourth_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_3 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", fourth_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_3<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", fourth_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_3 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", fourth_name][[1]])  |> as.numeric() * 100
        preload_model_return_3 <- (preload_inputs[preload_inputs$Input == "ModelReturn", fourth_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_3 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_3 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_3 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_3 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", fourth_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_3", value = fourth_name)
        updateNumericInput(session, "NewHireDCPct_3", value = preload_NewHireDCPct_3)
        updatePickerInput(session, "DCPolicy_3", selected = preload_inputs[preload_inputs$Input == "DC_Policy", fourth_name][[1]])
        updateNumericInput(session, "DC_Contrib_3", value = preload_DC_Contrib_3)
        updateNumericInput(session, "dis_r_current_3", value = preload_dis_r_current_3)
        updateNumericInput(session, "dis_r_new_3", value = preload_dis_r_new_3)
        updatePickerInput(session, "ScenType_3", selected = preload_inputs[preload_inputs$Input == "DeSimType", fourth_name][[1]])
        updateNumericInput(session, "model_return_3", value = preload_model_return_3)
        updatePickerInput(session, "ER_Policy_3", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", fourth_name][[1]])
        updatePickerInput(session, "CostSharingNC_3", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", fourth_name][[1]])
        updatePickerInput(session, "CostSharingAmo_3", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", fourth_name][[1]])
        updateNumericInput(session, "CashInfusion_3", value = preload_CashInfusion_3)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_3", value = preload_NoYearsADC_CurrentDebt_3)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_3", value = preload_NoYearsADC_NewDebtCurrentHire_3)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_3", value = preload_NoYearsADC_NewDebtNewHire_3)
        updatePickerInput(session, "AmoMethod_CurrentHire_3", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", fourth_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_3", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", fourth_name][[1]])
      
        } else if (input$preload_scen_3 == fifth_name | (input$refresh_scen_3 & input$preload_scen_3 == fifth_name)) {
        
        preload_NewHireDCPct_3 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", fifth_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_3 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", fifth_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_3<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", fifth_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_3 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", fifth_name][[1]])  |> as.numeric() * 100
        preload_model_return_3 <- (preload_inputs[preload_inputs$Input == "ModelReturn", fifth_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_3 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_3 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_3 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_3 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", fifth_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_3", value = fifth_name)
        updateNumericInput(session, "NewHireDCPct_3", value = preload_NewHireDCPct_3)
        updatePickerInput(session, "DCPolicy_3", selected = preload_inputs[preload_inputs$Input == "DC_Policy", fifth_name][[1]])
        updateNumericInput(session, "DC_Contrib_3", value = preload_DC_Contrib_3)
        updateNumericInput(session, "dis_r_current_3", value = preload_dis_r_current_3)
        updateNumericInput(session, "dis_r_new_3", value = preload_dis_r_new_3)
        updatePickerInput(session, "ScenType_3", selected = preload_inputs[preload_inputs$Input == "DeSimType", fifth_name][[1]])
        updateNumericInput(session, "model_return_3", value = preload_model_return_3)
        updatePickerInput(session, "ER_Policy_3", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", fifth_name][[1]])
        updatePickerInput(session, "CostSharingNC_3", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", fifth_name][[1]])
        updatePickerInput(session, "CostSharingAmo_3", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", fifth_name][[1]])
        updateNumericInput(session, "CashInfusion_3", value = preload_CashInfusion_3)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_3", value = preload_NoYearsADC_CurrentDebt_3)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_3", value = preload_NoYearsADC_NewDebtCurrentHire_3)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_3", value = preload_NoYearsADC_NewDebtNewHire_3)
        updatePickerInput(session, "AmoMethod_CurrentHire_3", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", fifth_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_3", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", fifth_name][[1]])

      } else {}
      
    })
    
    
    observe({
      if (input$preload_scen_4 == first_name | (input$refresh_scen_4 & input$preload_scen_4 == first_name)) {
        
        preload_NewHireDCPct_4 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", first_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_4 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", first_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_4<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", first_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_4 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", first_name][[1]])  |> as.numeric() * 100
        preload_model_return_4 <- (preload_inputs[preload_inputs$Input == "ModelReturn", first_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_4 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_4 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_4 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", first_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_4 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", first_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_4", value = first_name)
        updateNumericInput(session, "NewHireDCPct_4", value = preload_NewHireDCPct_4)
        updatePickerInput(session, "DCPolicy_4", selected = preload_inputs[preload_inputs$Input == "DC_Policy", first_name][[1]])
        updateNumericInput(session, "DC_Contrib_4", value = preload_DC_Contrib_4)
        updateNumericInput(session, "dis_r_current_4", value = preload_dis_r_current_4)
        updateNumericInput(session, "dis_r_new_4", value = preload_dis_r_new_4)
        updatePickerInput(session, "ScenType_4", selected = preload_inputs[preload_inputs$Input == "DeSimType", first_name][[1]])
        updateNumericInput(session, "model_return_4", value = preload_model_return_4)
        updatePickerInput(session, "ER_Policy_4", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", first_name][[1]])
        updatePickerInput(session, "CostSharingNC_4", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", first_name][[1]])
        updatePickerInput(session, "CostSharingAmo_4", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", first_name][[1]])
        updateNumericInput(session, "CashInfusion_4", value = preload_CashInfusion_4)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_4", value = preload_NoYearsADC_CurrentDebt_4)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_4", value = preload_NoYearsADC_NewDebtCurrentHire_4)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_4", value = preload_NoYearsADC_NewDebtNewHire_4)
        updatePickerInput(session, "AmoMethod_CurrentHire_4", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", first_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_4", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", first_name][[1]])
        
      } else if (input$preload_scen_4 == second_name | (input$refresh_scen_4 & input$preload_scen_4 == second_name)) {
        
        preload_NewHireDCPct_4 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", second_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_4 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", second_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_4<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", second_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_4 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", second_name][[1]])  |> as.numeric() * 100
        preload_model_return_4 <- (preload_inputs[preload_inputs$Input == "ModelReturn", second_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_4 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_4 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_4 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", second_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_4 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", second_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_4", value = second_name)
        updateNumericInput(session, "NewHireDCPct_4", value = preload_NewHireDCPct_4)
        updatePickerInput(session, "DCPolicy_4", selected = preload_inputs[preload_inputs$Input == "DC_Policy", second_name][[1]])
        updateNumericInput(session, "DC_Contrib_4", value = preload_DC_Contrib_4)
        updateNumericInput(session, "dis_r_current_4", value = preload_dis_r_current_4)
        updateNumericInput(session, "dis_r_new_4", value = preload_dis_r_new_4)
        updatePickerInput(session, "ScenType_4", selected = preload_inputs[preload_inputs$Input == "DeSimType", second_name][[1]])
        updateNumericInput(session, "model_return_4", value = preload_model_return_4)
        updatePickerInput(session, "ER_Policy_4", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", second_name][[1]])
        updatePickerInput(session, "CostSharingNC_4", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", second_name][[1]])
        updatePickerInput(session, "CostSharingAmo_4", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", second_name][[1]])
        updateNumericInput(session, "CashInfusion_4", value = preload_CashInfusion_4)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_4", value = preload_NoYearsADC_CurrentDebt_4)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_4", value = preload_NoYearsADC_NewDebtCurrentHire_4)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_4", value = preload_NoYearsADC_NewDebtNewHire_4)
        updatePickerInput(session, "AmoMethod_CurrentHire_4", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", second_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_4", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", second_name][[1]])
        
        
      } else if (input$preload_scen_4 == third_name | (input$refresh_scen_4 & input$preload_scen_4 == third_name)) {
        
        preload_NewHireDCPct_4 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", third_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_4 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", third_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_4<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", third_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_4 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", third_name][[1]])  |> as.numeric() * 100
        preload_model_return_4 <- (preload_inputs[preload_inputs$Input == "ModelReturn", third_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_4 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_4 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_4 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", third_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_4 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", third_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_4", value = third_name)
        updateNumericInput(session, "NewHireDCPct_4", value = preload_NewHireDCPct_4)
        updatePickerInput(session, "DCPolicy_4", selected = preload_inputs[preload_inputs$Input == "DC_Policy", third_name][[1]])
        updateNumericInput(session, "DC_Contrib_4", value = preload_DC_Contrib_4)
        updateNumericInput(session, "dis_r_current_4", value = preload_dis_r_current_4)
        updateNumericInput(session, "dis_r_new_4", value = preload_dis_r_new_4)
        updatePickerInput(session, "ScenType_4", selected = preload_inputs[preload_inputs$Input == "DeSimType", third_name][[1]])
        updateNumericInput(session, "model_return_4", value = preload_model_return_4)
        updatePickerInput(session, "ER_Policy_4", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", third_name][[1]])
        updatePickerInput(session, "CostSharingNC_4", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", third_name][[1]])
        updatePickerInput(session, "CostSharingAmo_4", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", third_name][[1]])
        updateNumericInput(session, "CashInfusion_4", value = preload_CashInfusion_4)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_4", value = preload_NoYearsADC_CurrentDebt_4)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_4", value = preload_NoYearsADC_NewDebtCurrentHire_4)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_4", value = preload_NoYearsADC_NewDebtNewHire_4)
        updatePickerInput(session, "AmoMethod_CurrentHire_4", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", third_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_4", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", third_name][[1]])
        
        
      } else if (input$preload_scen_4 == fourth_name | (input$refresh_scen_4 & input$preload_scen_4 == fourth_name)) {
        
        preload_NewHireDCPct_4 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", fourth_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_4 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", fourth_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_4<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", fourth_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_4 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", fourth_name][[1]])  |> as.numeric() * 100
        preload_model_return_4 <- (preload_inputs[preload_inputs$Input == "ModelReturn", fourth_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_4 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_4 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_4 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", fourth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_4 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", fourth_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_4", value = fourth_name)
        updateNumericInput(session, "NewHireDCPct_4", value = preload_NewHireDCPct_4)
        updatePickerInput(session, "DCPolicy_4", selected = preload_inputs[preload_inputs$Input == "DC_Policy", fourth_name][[1]])
        updateNumericInput(session, "DC_Contrib_4", value = preload_DC_Contrib_4)
        updateNumericInput(session, "dis_r_current_4", value = preload_dis_r_current_4)
        updateNumericInput(session, "dis_r_new_4", value = preload_dis_r_new_4)
        updatePickerInput(session, "ScenType_4", selected = preload_inputs[preload_inputs$Input == "DeSimType", fourth_name][[1]])
        updateNumericInput(session, "model_return_4", value = preload_model_return_4)
        updatePickerInput(session, "ER_Policy_4", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", fourth_name][[1]])
        updatePickerInput(session, "CostSharingNC_4", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", fourth_name][[1]])
        updatePickerInput(session, "CostSharingAmo_4", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", fourth_name][[1]])
        updateNumericInput(session, "CashInfusion_4", value = preload_CashInfusion_4)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_4", value = preload_NoYearsADC_CurrentDebt_4)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_4", value = preload_NoYearsADC_NewDebtCurrentHire_4)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_4", value = preload_NoYearsADC_NewDebtNewHire_4)
        updatePickerInput(session, "AmoMethod_CurrentHire_4", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", fourth_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_4", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", fourth_name][[1]])
        
      } else if (input$preload_scen_4 == fifth_name | (input$refresh_scen_4 & input$preload_scen_4 == fifth_name)) {
        
        preload_NewHireDCPct_4 <- (preload_inputs[preload_inputs$Input == "NewHireDC_choice", fifth_name][[1]]) |> as.numeric() * 100
        preload_DC_Contrib_4 <- (preload_inputs[preload_inputs$Input == "DC_ContRate", fifth_name][[1]])  |> as.numeric() * 100
        preload_dis_r_current_4<- (preload_inputs[preload_inputs$Input == "DR_CurrentHires", fifth_name][[1]]) |> as.numeric() * 100
        preload_dis_r_new_4 <- (preload_inputs[preload_inputs$Input == "DR_NewHires", fifth_name][[1]])  |> as.numeric() * 100
        preload_model_return_4 <- (preload_inputs[preload_inputs$Input == "ModelReturn", fifth_name][[1]])  |> as.numeric() * 100
        preload_CashInfusion_4 <- (preload_inputs[preload_inputs$Input == "OneTimeInfusion", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_CurrentDebt_4 <- (preload_inputs[preload_inputs$Input == "CurrentDebt_period", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtCurrentHire_4 <- (preload_inputs[preload_inputs$Input == "NewDebtCurrentHire_period", fifth_name][[1]])  |> as.numeric()
        preload_NoYearsADC_NewDebtNewHire_4 <- (preload_inputs[preload_inputs$Input == "NewDebtNewHire_period", fifth_name][[1]])  |> as.numeric()
        
        updateTextInput(session, "scen_name_4", value = fifth_name)
        updateNumericInput(session, "NewHireDCPct_4", value = preload_NewHireDCPct_4)
        updatePickerInput(session, "DCPolicy_4", selected = preload_inputs[preload_inputs$Input == "DC_Policy", fifth_name][[1]])
        updateNumericInput(session, "DC_Contrib_4", value = preload_DC_Contrib_4)
        updateNumericInput(session, "dis_r_current_4", value = preload_dis_r_current_4)
        updateNumericInput(session, "dis_r_new_4", value = preload_dis_r_new_4)
        updatePickerInput(session, "ScenType_4", selected = preload_inputs[preload_inputs$Input == "DeSimType", fifth_name][[1]])
        updateNumericInput(session, "model_return_4", value = preload_model_return_4)
        updatePickerInput(session, "ER_Policy_4", selected = preload_inputs[preload_inputs$Input == "FundingPolicy", fifth_name][[1]])
        updatePickerInput(session, "CostSharingNC_4", selected = preload_inputs[preload_inputs$Input == "CostShare_NCNew", fifth_name][[1]])
        updatePickerInput(session, "CostSharingAmo_4", selected = preload_inputs[preload_inputs$Input == "CostShare_AmoNew", fifth_name][[1]])
        updateNumericInput(session, "CashInfusion_4", value = preload_CashInfusion_4)
        updateNumericInput(session, "NoYearsADC_CurrentDebt_4", value = preload_NoYearsADC_CurrentDebt_4)
        updateNumericInput(session, "NoYearsADC_NewDebtCurrentHire_4", value = preload_NoYearsADC_NewDebtCurrentHire_4)
        updateNumericInput(session, "NoYearsADC_NewDebtNewHire_4", value = preload_NoYearsADC_NewDebtNewHire_4)
        updatePickerInput(session, "AmoMethod_CurrentHire_4", selected = preload_inputs[preload_inputs$Input == "AmoMethod_current", fifth_name][[1]])
        updatePickerInput(session, "AmoMethod_NewHire_4", selected = preload_inputs[preload_inputs$Input == "AmoMethod_new", fifth_name][[1]])

      } else {}
      
    })
    
    
    #----------------------------PANEL ON/OFF----------------------------------
    control_panel <- reactive({
      value <- (input$controlbutton - input$controlbutton_close)
      return(value)
    })
    
    allin_panel <- reactive({
      value <- (input$empbutton - input$empbutton_close)
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
    
    observeEvent(input$empbutton | input$empbutton_close, {
      if(allin_panel() %% 2 == 0){
        shinyjs::hide(id = "all-in-emp")
      }else{
        shinyjs::show(id = "all-in-emp")
      }
    },
    ignoreNULL = F,
    ignoreInit = F)
    
    scen_1 <- reactive({
      
      scen_1 <- RunModel(
        DR_CurrentHires = input$dis_r_current_1/100,
        DC_Policy = input$DCPolicy_1,
        DR_NewHires = input$dis_r_new_1/100,
        # ReturnType = input$AnalysisType_1,
        DeSimType = input$ScenType_1,
        ModelReturn = input$model_return_1/100,
        # StoSimType = input$SimType_1,
        FundingPolicy = input$ER_Policy_1,
        CostShare_AmoNew = input$CostSharingAmo_1,
        CostShare_NCNew = input$CostSharingNC_1,
        CurrentDebt_period = input$NoYearsADC_CurrentDebt_1,
        NewDebtCurrentHire_period = input$NoYearsADC_NewDebtCurrentHire_1,
        NewDebtNewHire_period = input$NoYearsADC_NewDebtNewHire_1,
        AmoMethod_current = input$AmoMethod_CurrentHire_1,
        AmoMethod_new = input$AmoMethod_NewHire_1,
        OneTimeInfusion = input$CashInfusion_1,
        NewHireDC_choice = input$NewHireDCPct_1/100,
        DC_ContRate = input$DC_Contrib_1/100
      ) |>
        as.data.frame() |>
        mutate(FYE = Output)
      
      return(scen_1)
    })
    
    scen_2 <- reactive({
      
      scen_2 <- RunModel(
        DR_CurrentHires = input$dis_r_current_2/100,
        DC_Policy = input$DCPolicy_2,
        DR_NewHires = input$dis_r_new_2/100,
        # ReturnType = input$AnalysisType_2,
        DeSimType = input$ScenType_2,
        ModelReturn = input$model_return_2/100,
        # StoSimType = input$SimType_2,
        FundingPolicy = input$ER_Policy_2,
        CostShare_AmoNew = input$CostSharingAmo_2,
        CostShare_NCNew = input$CostSharingNC_2,
        CurrentDebt_period = input$NoYearsADC_CurrentDebt_2,
        NewDebtCurrentHire_period = input$NoYearsADC_NewDebtCurrentHire_2,
        NewDebtNewHire_period = input$NoYearsADC_NewDebtNewHire_2,
        AmoMethod_current = input$AmoMethod_CurrentHire_2,
        AmoMethod_new = input$AmoMethod_NewHire_2,
        OneTimeInfusion = input$CashInfusion_2,
        NewHireDC_choice = input$NewHireDCPct_2/100,
        DC_ContRate = input$DC_Contrib_2/100
      ) |>
        as.data.frame() |>
        mutate(FYE = Output)
      
      return(scen_2)
    })
    
    scen_3 <- reactive({
      
      scen_3 <- RunModel(
        DR_CurrentHires = input$dis_r_current_3/100,
        DC_Policy = input$DCPolicy_3,
        DR_NewHires = input$dis_r_new_3/100,
        # ReturnType = input$AnalysisType_3,
        DeSimType = input$ScenType_3,
        ModelReturn = input$model_return_3/100,
        # StoSimType = input$SimType_3,
        FundingPolicy = input$ER_Policy_3,
        CostShare_AmoNew = input$CostSharingAmo_3,
        CostShare_NCNew = input$CostSharingNC_3,
        CurrentDebt_period = input$NoYearsADC_CurrentDebt_3,
        NewDebtCurrentHire_period = input$NoYearsADC_NewDebtCurrentHire_3,
        NewDebtNewHire_period = input$NoYearsADC_NewDebtNewHire_3,
        AmoMethod_current = input$AmoMethod_CurrentHire_3,
        AmoMethod_new = input$AmoMethod_NewHire_3,
        OneTimeInfusion = input$CashInfusion_3,
        NewHireDC_choice = input$NewHireDCPct_3/100,
        DC_ContRate = input$DC_Contrib_3/100
      ) |>
        as.data.frame() |>
        mutate(FYE = Output)
      
      return(scen_3)
    })
    
    
    scen_4 <- reactive({
      
      scen_4 <- RunModel(
        DR_CurrentHires = input$dis_r_current_4/100,
        DC_Policy = input$DCPolicy_4,
        DR_NewHires = input$dis_r_new_4/100,
        # ReturnType = input$AnalysisType_4,
        DeSimType = input$ScenType_4,
        ModelReturn = input$model_return_4/100,
        # StoSimType = input$SimType_4,
        FundingPolicy = input$ER_Policy_4,
        CostShare_AmoNew = input$CostSharingAmo_4,
        CostShare_NCNew = input$CostSharingNC_4,
        CurrentDebt_period = input$NoYearsADC_CurrentDebt_4,
        NewDebtCurrentHire_period = input$NoYearsADC_NewDebtCurrentHire_4,
        NewDebtNewHire_period = input$NoYearsADC_NewDebtNewHire_4,
        AmoMethod_current = input$AmoMethod_CurrentHire_4,
        AmoMethod_new = input$AmoMethod_NewHire_4,
        OneTimeInfusion = input$CashInfusion_4,
        NewHireDC_choice = input$NewHireDCPct_4/100,
        DC_ContRate = input$DC_Contrib_4/100
      ) |>
        as.data.frame() |>
        mutate(FYE = Output)
      
      return(scen_4)
    })
    
    
    df_allint <- reactive({
      
      varname_a <- input$scen_name_1
      varname_b <- input$scen_name_2
      varname_c <- input$scen_name_3
      varname_d <- input$scen_name_4
      
      # Scenario 1
      df_1 <- scen_1()
      EndingUAL_1 <- tail(df_1$UAL_MVA_InflAdj, n = 1) |> as.numeric() / 1e3
      TotalERScenario_1 <- tail(df_1$Total_ER, n = 1) |> as.numeric() / 1e3
      AllInER_1 <- tail(df_1$AllInCost, n = 1) |> as.numeric() / 1e3
      TotalERScenario_1 <- paste0("$", round(TotalERScenario_1, 3), " B")
      EndingUAL_1 <- paste0("$", round(EndingUAL_1, 3), " B")
      AllInER_1 <- paste0("$", round(AllInER_1, 3), " B")
      
      # Scenario 2
      df_2 <- scen_2()
      EndingUAL_2 <- tail(df_2$UAL_MVA_InflAdj, n = 1) |> as.numeric() / 1e3
      TotalERScenario_2 <- tail(df_2$Total_ER, n = 1) |> as.numeric() / 1e3
      AllInER_2 <- tail(df_2$AllInCost, n = 1) |> as.numeric() / 1e3
      TotalERScenario_2 <- paste0("$", round(TotalERScenario_2, 3), " B")
      EndingUAL_2 <- paste0("$", round(EndingUAL_2, 3), " B")
      AllInER_2 <- paste0("$", round(AllInER_2, 3), " B")

      # Scenario 3
      df_3 <- scen_3()
      EndingUAL_3 <- tail(df_3$UAL_MVA_InflAdj, n = 1) |> as.numeric() / 1e3
      TotalERScenario_3 <- tail(df_3$Total_ER, n = 1) |> as.numeric() / 1e3
      AllInER_3 <- tail(df_3$AllInCost, n = 1) |> as.numeric() / 1e3
      TotalERScenario_3 <- paste0("$", round(TotalERScenario_3, 3), " B")
      EndingUAL_3 <- paste0("$", round(EndingUAL_3, 3), " B")
      AllInER_3 <- paste0("$", round(AllInER_3, 3), " B")

      # Scenario 4
      df_4 <- scen_4()
      EndingUAL_4 <- tail(df_4$UAL_MVA_InflAdj, n = 1) |> as.numeric()  / 1e3
      TotalERScenario_4 <- tail(df_4$Total_ER, n = 1) |> as.numeric()  / 1e3
      AllInER_4 <- tail(df_4$AllInCost, n = 1)  |> as.numeric() / 1e3
      TotalERScenario_4 <- paste0("$", round(TotalERScenario_4, 3), " B")
      EndingUAL_4 <- paste0("$", round(EndingUAL_4, 3), " B")
      AllInER_4 <- paste0("$", round(AllInER_4, 3), " B")
      
      if (input$nclass == 1) {
        df <- data.frame(Scenarios = c(varname_a),
                         col_4 = c(TotalERScenario_1),
                         col_5 = c(EndingUAL_1),
                         col_6 = c(AllInER_1))
      } else if (input$nclass == 2) {
        df <- data.frame(Scenarios = c(varname_a, varname_b),
                         col_4 = c(TotalERScenario_1, TotalERScenario_2),
                         col_5 = c(EndingUAL_1, EndingUAL_2),
                         col_6 = c(AllInER_1, AllInER_2))
      } else if (input$nclass == 3) {
        df <- data.frame(Scenarios = c(varname_a, varname_b, varname_c),
                         col_4 = c(TotalERScenario_1, TotalERScenario_2, TotalERScenario_3),
                         col_5 = c(EndingUAL_1, EndingUAL_2, EndingUAL_3),
                         col_6 = c(AllInER_1, AllInER_2, AllInER_3))
      } else {
        df <- data.frame(Scenarios = c(varname_a, varname_b, varname_c, varname_d),
                         col_4 = c(TotalERScenario_1, TotalERScenario_2, TotalERScenario_3, TotalERScenario_4),
                         col_5 = c(EndingUAL_1, EndingUAL_2, EndingUAL_3, EndingUAL_4),
                         col_6 = c(AllInER_1, AllInER_2, AllInER_3, AllInER_4))
      }
      
    })
    
    
    df_allin <- reactive({
      
      varname_a <- input$scen_name_1
      varname_b <- input$scen_name_2
      varname_c <- input$scen_name_3
      varname_d <- input$scen_name_4
      
      # Scenario 1
      df_1 <- scen_1()
      EndingUAL_1 <- tail(df_1$UAL_MVA_InflAdj, n = 1) |> as.numeric() / 1e3
      TotalERScenario_1 <- tail(df_1$Total_ER, n = 1) |> as.numeric() / 1e3
      AllInER_1 <- tail(df_1$AllInCost, n = 1) |> as.numeric() / 1e3
      
      # Scenario 2
      df_2 <- scen_2()
      EndingUAL_2 <- tail(df_2$UAL_MVA_InflAdj, n = 1) |> as.numeric() / 1e3
      TotalERScenario_2 <- tail(df_2$Total_ER, n = 1) |> as.numeric() / 1e3
      AllInER_2 <- tail(df_2$AllInCost, n = 1) |> as.numeric() / 1e3
      
      # Scenario 3
      df_3 <- scen_3()
      EndingUAL_3 <- tail(df_3$UAL_MVA_InflAdj, n = 1) |> as.numeric() / 1e3
      TotalERScenario_3 <- tail(df_3$Total_ER, n = 1) |> as.numeric() / 1e3
      AllInER_3 <- tail(df_3$AllInCost, n = 1) |> as.numeric() / 1e3
      
      # Scenario 4
      df_4 <- scen_4()
      EndingUAL_4 <- tail(df_4$UAL_MVA_InflAdj, n = 1) |> as.numeric() / 1e3
      TotalERScenario_4 <- tail(df_4$Total_ER, n = 1) |> as.numeric() / 1e3
      AllInER_4 <- tail(df_4$AllInCost, n = 1) |> as.numeric() / 1e3
      
      
      if (input$nclass == 1) {
        df <- data.frame(Scenarios = c(varname_a),
                         col_4 = c(TotalERScenario_1),
                         col_5 = c(EndingUAL_1),
                         col_6 = c(AllInER_1))
      } else if (input$nclass == 2) {
        df <- data.frame(Scenarios = c(varname_a, varname_b),
                         col_4 = c(TotalERScenario_1, TotalERScenario_2),
                         col_5 = c(EndingUAL_1, EndingUAL_2),
                         col_6 = c(AllInER_1, AllInER_2))
      } else if (input$nclass == 3) {
        df <- data.frame(Scenarios = c(varname_a, varname_b, varname_c),
                         col_4 = c(TotalERScenario_1, TotalERScenario_2, TotalERScenario_3),
                         col_5 = c(EndingUAL_1, EndingUAL_2, EndingUAL_3),
                         col_6 = c(AllInER_1, AllInER_2, AllInER_3))
      } else {
        
        df <- data.frame(Scenarios = c(varname_a, varname_b, varname_c, varname_d),
                         col_4 = c(TotalERScenario_1, TotalERScenario_2, TotalERScenario_3, TotalERScenario_4),
                         col_5 = c(EndingUAL_1, EndingUAL_2, EndingUAL_3, EndingUAL_4),
                         col_6 = c(AllInER_1, AllInER_2, AllInER_3, AllInER_4))
        
      }
      
    })
    
    
    
    
    output$chart1 <- echarts4r::renderEcharts4r({
      
      df <- scen_1() |> select(FYE)
      
      df$year <- df$FYE|> as.character()
      
      df$scen_1 <- scen_1()$FR_MVA
      df$scen_2 <- scen_2()$FR_MVA
      df$scen_3 <- scen_3()$FR_MVA
      df$scen_4 <- scen_4()$FR_MVA
      
      varname_a <- input$scen_name_1
      varname_b <- input$scen_name_2
      varname_c <- input$scen_name_3
      varname_d <- input$scen_name_4
      
      e1 <- df|>
        filter(FYE >= 2021) |>
        echarts4r::e_charts(year)|>
        echarts4r::e_line(scen_1,
                          name = varname_a,
                          symbol = 'none',
                          lineStyle = list(width = 3))|>
        echarts4r::e_x_axis(#category = "time",
          axisLabel = list(fontSize = 13),
          axisTick = list(alignWithLabel = T))|>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 13),
                            # min = 0,
                            # max = 1,
                            formatter = echarts4r::e_axis_formatter(style = "percent",
                                                                    digits = 0))|>
        echarts4r::e_tooltip(trigger = "axis",
                             formatter = echarts4r::e_tooltip_pointer_formatter(style = "percent",
                                                                                digits = 0),
                             textStyle = list(fontSize = 14),
                             confine = T)|>
        echarts4r::e_axis_pointer(label = list(show = F)) |>
        echarts4r::e_title(paste("Funded Ratio (MVA Basis)"),
                           link = "https://reason.org/backgrounder/glossary-of-pension-terminology/#fundedratio")|>
        echarts4r::e_theme_custom("echarts_theme.json")|>
        # echarts4r::e_color(background = "#fff")|> 
        echarts4r::e_toolbox_feature(feature = "dataZoom") |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Save")
      
      if (input$nclass == 1) {
        
        e1|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect'))
        
      } else if (input$nclass == 2) {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect'))
        
      } else if (input$nclass == 3) {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) |>
          echarts4r::e_line(name = varname_c,
                            scen_3,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect'))
        
      } else {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_line(name = varname_c,
                            scen_3,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_line(name = varname_d,
                            scen_4,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect', 'roundRect'))
      }
    })
    
    output$chart2 <- echarts4r::renderEcharts4r({
      
      df <- scen_1() |> select(FYE)

      df$year <- df$FYE|> as.character()

      df$scen_1 <- scen_1()$UAL_MVA_InflAdj
      df$scen_2 <- scen_2()$UAL_MVA_InflAdj
      df$scen_3 <- scen_3()$UAL_MVA_InflAdj
      df$scen_4 <- scen_4()$UAL_MVA_InflAdj

      varname_a <- input$scen_name_1
      varname_b <- input$scen_name_2
      varname_c <- input$scen_name_3
      varname_d <- input$scen_name_4
      
      e1 <- df |>
        mutate(year = as.character(FYE)) |>
        filter(FYE >= 2021) |>
        mutate(scalar = 1e3) |>
        mutate(scen_1 = as.numeric(scen_1),
               scen_2 = as.numeric(scen_2),
               scen_3 = as.numeric(scen_3),
               scen_4 = as.numeric(scen_4)) |>
        mutate(scen_1 = scen_1 / scalar,
               scen_2 = scen_2 / scalar,
               scen_3 = scen_3 / scalar,
               scen_4 = scen_4 / scalar) |>
        echarts4r::e_charts(year)|>
        echarts4r::e_line(scen_1,
                          name = varname_a,
                          symbol = 'none',
                          lineStyle = list(width = 3))|>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 13),
                            axisTick = list(alignWithLabel = T))|>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 13),
                            # min = 0,
                            # max = 10,
                            name = "Billions",
                            nameLocation = "middle",
                            nameTextStyle = list(
                              fontSize = 15,
                              fontStyle = 'bold',
                              padding = c(2, 2, 30, 2)
                            ),
                            formatter = echarts4r::e_axis_formatter(style = "currency",
                                                                    digits = 1))|>
        echarts4r::e_tooltip(trigger = "axis",
                             formatter = echarts4r::e_tooltip_pointer_formatter(style = "currency",
                                                                                digits = 3),
                             textStyle = list(fontSize = 14),
                             confine = T)|>
        echarts4r::e_axis_pointer(label = list(show = F)) |>
        echarts4r::e_title(paste("Unfunded Market Liability (Inf. Adjusted)"),
                           link = "https://reason.org/backgrounder/glossary-of-pension-terminology/#unfundedliability")|>
        echarts4r::e_grid(left = "15%")|>
        echarts4r::e_theme_custom("echarts_theme.json")|>
        # echarts4r::e_color(background = "#fff")|>
        echarts4r::e_toolbox_feature(feature = "dataZoom") |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Save")
      
      if (input$nclass == 1) {
        
        e1|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect'))
        
      } else if (input$nclass == 2) {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect'))
        
      } else if (input$nclass == 3) {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) |>
          echarts4r::e_line(name = varname_c,
                            scen_3,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect'))
        
      } else if (input$nclass == 4) {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_line(name = varname_c,
                            scen_3,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_line(name = varname_d,
                            scen_4,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect', 'roundRect'))
      }
    })
    
    output$chart3 <- echarts4r::renderEcharts4r({
      
      df <- scen_1() |> select(FYE)
      
      df$year <- df$FYE|> as.character()
      
      df$scen_1 <- scen_1()$ER_Percentage
      df$scen_2 <- scen_2()$ER_Percentage
      df$scen_3 <- scen_3()$ER_Percentage
      df$scen_4 <- scen_4()$ER_Percentage
      
      varname_a <- input$scen_name_1
      varname_b <- input$scen_name_2
      varname_c <- input$scen_name_3
      varname_d <- input$scen_name_4
      
      e1 <- df|>
        filter(FYE >= 2021) |>
        echarts4r::e_charts(year)|>
        echarts4r::e_line(scen_1,
                          name = varname_a,
                          symbol = 'none',
                          lineStyle = list(width = 3))|>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 13),
                            axisTick = list(alignWithLabel = T))|>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 13),
                            # min = 0,
                            # max = 0.7,
                            formatter = echarts4r::e_axis_formatter(style = "percent",
                                                                    digits = 0))|>
        echarts4r::e_tooltip(trigger = "axis",
                             formatter = echarts4r::e_tooltip_pointer_formatter(style = "percent",
                                                                                digits = 1),
                             textStyle = list(fontSize = 14),
                             confine = T)|>
        echarts4r::e_axis_pointer(label = list(show = F)) |>
        echarts4r::e_title(paste("Employer Contribution (% of Payroll)"),
                           link = "https://reason.org/backgrounder/glossary-of-pension-terminology/#payroll")|>
        echarts4r::e_theme_custom("echarts_theme.json")|>
        # echarts4r::e_color(background = "#fff")|>
        echarts4r::e_toolbox_feature(feature = "dataZoom") |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Save")
      
      if (input$nclass == 1) {
        
        e1|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect'))
        
      } else if (input$nclass == 2) {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect'))
        
      } else if (input$nclass == 3) {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) |>
          echarts4r::e_line(name = varname_c,
                            scen_3,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect'))
        
      } else {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_line(name = varname_c,
                            scen_3,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_line(name = varname_d,
                            scen_4,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect', 'roundRect')) 
      }
    })
    
    
    output$chart4 <- echarts4r::renderEcharts4r({
      
      df <- scen_1() |> select(FYE)
      
      df$year <- df$FYE|> as.character()
      
      df$scen_1 <- scen_1()$ER_InflAdj
      df$scen_2 <- scen_2()$ER_InflAdj
      df$scen_3 <- scen_3()$ER_InflAdj
      df$scen_4 <- scen_4()$ER_InflAdj
      
      varname_a <- input$scen_name_1
      varname_b <- input$scen_name_2
      varname_c <- input$scen_name_3
      varname_d <- input$scen_name_4
      
      e1 <- df|>
        filter(FYE >= 2021) |>
        echarts4r::e_charts(year)|>
        echarts4r::e_line(scen_1,
                          name = varname_a,
                          symbol = 'none',
                          lineStyle = list(width = 3))|>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 13),
                            axisTick = list(alignWithLabel = T))|>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 13),
                            # min = 0,
                            # max = 350,
                            name = "Millions",
                            nameLocation = "middle",
                            nameTextStyle = list(
                              fontSize = 15,
                              fontStyle = 'bold',
                              padding = c(2, 2, 30, 2)
                            ),
                            formatter = echarts4r::e_axis_formatter(style = "currency",
                                                                    digits = 0))|>
        echarts4r::e_tooltip(trigger = "axis",
                             formatter = echarts4r::e_tooltip_pointer_formatter(style = "currency",
                                                                                digits = 3),
                             textStyle = list(fontSize = 14),
                             confine = T)|>
        echarts4r::e_axis_pointer(label = list(show = F)) |>
        echarts4r::e_title(paste("Employer Contribution (Inf. Adjusted)"),
                           link = "https://reason.org/backgrounder/glossary-of-pension-terminology/#adec")|>
        echarts4r::e_grid(left = "12%")|>
        echarts4r::e_theme_custom("echarts_theme.json")|>
        # echarts4r::e_color(background = "#fff")|>
        echarts4r::e_toolbox_feature(feature = "dataZoom") |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Save")
      
      if (input$nclass == 1) {
        
        e1|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect'))
        
      } else if (input$nclass == 2) {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect'))
        
      } else if (input$nclass == 3) {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3)) |>
          echarts4r::e_line(name = varname_c,
                            scen_3,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect'))
        
      } else {
        
        e1|>
          echarts4r::e_line(name = varname_b,
                            scen_2,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_line(name = varname_c,
                            scen_3,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_line(name = varname_d,
                            scen_4,
                            symbol = "none",
                            lineStyle = list(type = "solid", width = 3))|>
          echarts4r::e_legend(type = "scroll",
                              bottom = "5%",
                              itemGap = 25,
                              textStyle = list(fontSize = 12),
                              icons = c('roundRect', 'roundRect', 'roundRect', 'roundRect'))
      }
    })
    
    
    output$table3 <- reactable::renderReactable({

      reactable::reactable(
        df_allint(),
        striped = T,
        highlight = T,
        sortable = F,
        columnGroups = list(
          reactable::colGroup(name = "All-in Employer Costs", columns = c("col_4", "col_5", "col_6"),
                              headerStyle = list(background = "#333f50", color = "#FFF")),
          reactable::colGroup(name = "", columns = c("Scenarios"),
                              headerStyle = list(background = "#333f50", color = "#FFF"))
        ),
        defaultColDef = reactable::colDef(
          header = function(value) gsub(".", " ", value, fixed = T),
          headerStyle = list(background = "#333f50", color = "#FFF"),
          align = "center",
          format = reactable::colFormat(percent = F, digits = 1),
          style = list(color = "#333")),
        columns = list(
          Scenarios = reactable::colDef(
            name = "Scenarios"
          ),
          col_4 = reactable::colDef(
            name = "Total ER Contribution (infl. adj)"
          ),
          col_5 = reactable::colDef(
            name = "Ending UAL (infl. adj)"
          ),
          col_6 = reactable::colDef(
            name = "All-in ER Cost (infl. adj)"
          )
        ),
        theme = reactable::reactableTheme(
          stripedColor = "#adb9ca",
          highlightColor = "#f4b183",
          background = "#d6dce5",
          cellPadding = "8px 10px",
          style = list(fontFamily = "'Open Sans', sans-serif"),
          searchInputStyle = list(width = "100%")
        )
      )
      
    })
    
    output$chart5 <- echarts4r::renderEcharts4r({
      
      df_allin()|>
        dplyr::mutate(Scenarios = stringr::str_wrap(Scenarios, 15))|>
        echarts4r::e_charts(Scenarios, stack = "grp")|>
        echarts4r::e_bar(col_4, name = "Total ER Contribution (infl adj)", symbol = 'roundRect', barGap = "0%")|>
        echarts4r::e_bar(col_5, name = "Ending UAL (infl adj)", symbol = 'roundRect', barGap = "0%")|>
        echarts4r::e_color(color = c("#FF6633", "#2879CB"))|>
        echarts4r::e_legend(textStyle = list(fontSize = 14),
                            top = "0%",
                            itemGap = 30)|>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 14))|>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 14),
                            name = "Billions",
                            nameLocation = "middle",
                            nameTextStyle = list(
                              fontSize = 14,
                              fontStyle = "bold",
                              padding = c(2, 2, 20, 2)),
                            formatter = echarts4r::e_axis_formatter(style = "currency",
                                                                    digits = 1))|>
        echarts4r::e_tooltip(trigger = "item",
                             formatter = echarts4r::e_tooltip_item_formatter(style = "currency",
                                                                             digits = 3),
                             textStyle = list(fontSize = 14),
                             confine = T)|>
        echarts4r::e_grid(bottom = '20%', top = "7%", right = "10%")|>
        echarts4r::e_theme_custom("echarts_theme.json")|>
        echarts4r::e_toolbox_feature(feature = c("saveAsImage"), title = "Save")
      
    })
    
    
    
    
    data_1 <- reactive({
      
      df <- scen_1()
      
      if (input$nclass == 1) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$FR_MVA |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1)
        
      } else if (input$nclass == 2) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$FR_MVA |> as.numeric()
        df$scen_2 <- scen_2()$FR_MVA |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1, scen_2) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2)
        
      } else if (input$nclass == 3) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$FR_MVA |> as.numeric()
        df$scen_2 <- scen_2()$FR_MVA |> as.numeric()
        df$scen_3 <- scen_3()$FR_MVA |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1, scen_2, scen_3) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2, input$scen_name_3)
        
      } else {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$FR_MVA |> as.numeric()
        df$scen_2 <- scen_2()$FR_MVA |> as.numeric()
        df$scen_3 <- scen_3()$FR_MVA |> as.numeric()
        df$scen_4 <- scen_4()$FR_MVA |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1, scen_2, scen_3, scen_4) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2, input$scen_name_3, input$scen_name_4)
        
      }
      
      return(df)
      
    })
    
    data_2 <- reactive({
      
      df <- scen_1()
      
      if (input$nclass == 1) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$UAL_MVA_InflAdj |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1)
        
      } else if (input$nclass == 2) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$UAL_MVA_InflAdj |> as.numeric()
        df$scen_2 <- scen_2()$UAL_MVA_InflAdj |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1, scen_2) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2)
        
      } else if (input$nclass == 3) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$UAL_MVA_InflAdj |> as.numeric()
        df$scen_2 <- scen_2()$UAL_MVA_InflAdj |> as.numeric()
        df$scen_3 <- scen_3()$UAL_MVA_InflAdj |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1, scen_2, scen_3) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2, input$scen_name_3)
        
      } else {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$UAL_MVA_InflAdj |> as.numeric()
        df$scen_2 <- scen_2()$UAL_MVA_InflAdj |> as.numeric()
        df$scen_3 <- scen_3()$UAL_MVA_InflAdj |> as.numeric()
        df$scen_4 <- scen_4()$UAL_MVA_InflAdj |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1, scen_2, scen_3, scen_4) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2, input$scen_name_3, input$scen_name_4)
        
      }
      
      return(df)
      
    })
    
    
    data_3 <- reactive({
      
      df <- scen_1()
      
      if (input$nclass == 1) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$ER_Percentage |> as.numeric()
        
        df <- df %>%
          dplyr::select(year, scen_1) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1)
        
      } else if (input$nclass == 2) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$ER_Percentage |> as.numeric()
        df$scen_2 <- scen_2()$ER_Percentage |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1, scen_2) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2)
        
      } else if (input$nclass == 3) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$ER_Percentage |> as.numeric()
        df$scen_2 <- scen_2()$ER_Percentage |> as.numeric()
        df$scen_3 <- scen_3()$ER_Percentage |> as.numeric()
        
        df <- df %>%
          dplyr::select(year, scen_1, scen_2, scen_3) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2, input$scen_name_3)
        
      } else {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$ER_Percentage |> as.numeric()
        df$scen_2 <- scen_2()$ER_Percentage |> as.numeric()
        df$scen_3 <- scen_3()$ER_Percentage |> as.numeric()
        df$scen_4 <- scen_4()$ER_Percentage |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1, scen_2, scen_3, scen_4) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2, input$scen_name_3, input$scen_name_4)
        
      }
      return(df)
    })
    
    data_4 <- reactive({
      
      df <- scen_1()
      
      if (input$nclass == 1) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$ER_InflAdj |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1)
        
      } else if (input$nclass == 2) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$ER_InflAdj |> as.numeric()
        df$scen_2 <- scen_2()$ER_InflAdj |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1, scen_2) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2)
        
      } else if (input$nclass == 3) {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$ER_InflAdj |> as.numeric()
        df$scen_2 <- scen_2()$ER_InflAdj |> as.numeric()
        df$scen_3 <- scen_3()$ER_InflAdj |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1, scen_2, scen_3) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2, input$scen_name_3)
        
      } else {
        
        df$year <- df$FYE |> as.character() 
        df$scen_1 <- scen_1()$ER_InflAdj |> as.numeric()
        df$scen_2 <- scen_2()$ER_InflAdj |> as.numeric()
        df$scen_3 <- scen_3()$ER_InflAdj |> as.numeric()
        df$scen_4 <- scen_4()$ER_InflAdj |> as.numeric()
        
        df <- df |>
          dplyr::select(year, scen_1, scen_2, scen_3, scen_4) |>
          dplyr::filter(year >= 2021)
        
        names(df) <- c("Year", input$scen_name_1, input$scen_name_2, input$scen_name_3, input$scen_name_4)
        
      }
      return(df)
    })
    
    
    output$download_excel <- downloadHandler(
      
      filename = function() {
        "funding_data.xlsx"
      },
      content = function(file) {
        
        xl_rows <- 50
        
        my_workbook <- openxlsx::createWorkbook()
        
        openxlsx::addWorksheet(wb = my_workbook,sheetName = "Funded Ratio (MVA Basis)")
        openxlsx::addWorksheet(wb = my_workbook, sheetName = "UAL (Inf. Adjusted)")
        openxlsx::addWorksheet(wb = my_workbook, sheetName = "Employer Cont (% of Payroll)")
        openxlsx::addWorksheet(wb = my_workbook, sheetName = "Employer Cont (Inf. Adjusted)")
        openxlsx::addWorksheet(wb = my_workbook, sheetName = "All-In Employer Costs")
        
        openxlsx::setColWidths(my_workbook, sheet = 1, cols = 1:7, widths = c(35, 25, 25, 25, 25, 25, 25))
        openxlsx::setColWidths(my_workbook, sheet = 2, cols = 1:7, widths = c(35, 25, 25, 25, 25, 25, 25))
        openxlsx::setColWidths(my_workbook, sheet = 3, cols = 1:7, widths = c(35, 25, 25, 25, 25, 25, 25))
        openxlsx::setColWidths(my_workbook, sheet = 4, cols = 1:7, widths = c(35, 25, 25, 25, 25, 25, 25))
        openxlsx::setColWidths(my_workbook, sheet = 5, cols = 1:7, widths = c(35, 25, 25, 25, 25, 25, 25))
        
        openxlsx::writeData(my_workbook, sheet = 1, c("PERS | Reason Foundation's Pension Integrity Project", " ",
                                                      "Asset Returns", "Funding Policy", "Discount Rate (current members)", "Discount Rate (new hires)",
                                                      "Normal Cost Sharing (new hires)", "Amo Cost Sharing (new hires)",
                                                      "Amortization Period for Current Unfunded Liability",
                                                      "Amortization Period for New Unfunded Liability (current hires)",  "Amortization Period for New Unfunded Liability (new hires)",
                                                      "Amortization Method for Unfunded Liability Created by Current Plan", "Amortization Method for Unfunded Liability Created by New Plan",
                                                      "Cash Infusion in 2022 (millions)", "Percent of New Hires Electing DC Plan", "Employer DC Contribution Rate (new hires)",
                                                      "DC Contribution Policy"), startRow = 1, startCol = 1)
        openxlsx::writeData(my_workbook, sheet = 2, c("PERS | Reason Foundation's Pension Integrity Project", "$ Millions",
                                                      "Asset Returns", "Funding Policy", "Discount Rate (current members)", "Discount Rate (new hires)",
                                                      "Normal Cost Sharing (new hires)", "Amo Cost Sharing (new hires)",
                                                      "Amortization Period for Current Unfunded Liability",
                                                      "Amortization Period for New Unfunded Liability (current hires)",  "Amortization Period for New Unfunded Liability (new hires)",
                                                      "Amortization Method for Unfunded Liability Created by Current Plan", "Amortization Method for Unfunded Liability Created by New Plan",
                                                      "Cash Infusion in 2022 (millions)", "Percent of New Hires Electing DC Plan", "Employer DC Contribution Rate (new hires)",
                                                      "DC Contribution Policy"), startRow = 1, startCol = 1)
        openxlsx::writeData(my_workbook, sheet = 3, c("PERS | Reason Foundation's Pension Integrity Project", "",
                                                      "Asset Returns", "Funding Policy", "Discount Rate (current members)", "Discount Rate (new hires)",
                                                      "Normal Cost Sharing (new hires)", "Amo Cost Sharing (new hires)",
                                                      "Amortization Period for Current Unfunded Liability",
                                                      "Amortization Period for New Unfunded Liability (current hires)",  "Amortization Period for New Unfunded Liability (new hires)",
                                                      "Amortization Method for Unfunded Liability Created by Current Plan", "Amortization Method for Unfunded Liability Created by New Plan",
                                                      "Cash Infusion in 2022 (millions)", "Percent of New Hires Electing DC Plan", "Employer DC Contribution Rate (new hires)",
                                                      "DC Contribution Policy"), startRow = 1, startCol = 1)
        openxlsx::writeData(my_workbook, sheet = 4, c("PERS | Reason Foundation's Pension Integrity Project", "$ Millions",
                                                      "Asset Returns", "Funding Policy", "Discount Rate (current members)", "Discount Rate (new hires)",
                                                      "Normal Cost Sharing (new hires)", "Amo Cost Sharing (new hires)",
                                                      "Amortization Period for Current Unfunded Liability",
                                                      "Amortization Period for New Unfunded Liability (current hires)",  "Amortization Period for New Unfunded Liability (new hires)",
                                                      "Amortization Method for Unfunded Liability Created by Current Plan", "Amortization Method for Unfunded Liability Created by New Plan",
                                                      "Cash Infusion in 2022 (millions)", "Percent of New Hires Electing DC Plan", "Employer DC Contribution Rate (new hires)",
                                                      "DC Contribution Policy"), startRow = 1, startCol = 1)
        openxlsx::writeData(my_workbook, sheet = 5, c("PERS | Reason Foundation's Pension Integrity Project", "",
                                                      "Asset Returns", "Funding Policy", "Discount Rate (current members)", "Discount Rate (new hires)",
                                                      "Normal Cost Sharing (new hires)", "Amo Cost Sharing (new hires)",
                                                      "Amortization Period for Current Unfunded Liability",
                                                      "Amortization Period for New Unfunded Liability (current hires)",  "Amortization Period for New Unfunded Liability (new hires)",
                                                      "Amortization Method for Unfunded Liability Created by Current Plan", "Amortization Method for Unfunded Liability Created by New Plan",
                                                      "Cash Infusion in 2022 (millions)", "Percent of New Hires Electing DC Plan", "Employer DC Contribution Rate (new hires)",
                                                      "DC Contribution Policy",
                                                      "", "Total ER Contribution (infl adj)", "Ending UAL (infl adju)", "All-in ER Cost (infl adj)"), startRow = 1, startCol = 1)
        
        openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fontSize = 16, textDecoration = "bold"), rows = 1, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(textDecoration = "bold"), rows = 3:xl_rows, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fontSize = 16, textDecoration = "bold"), rows = 1, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fontSize = 14, textDecoration = "bold"), rows = 2, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(textDecoration = "bold"), rows = 3:xl_rows, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fontSize = 16, textDecoration = "bold"), rows = 1, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(textDecoration = "bold"), rows = 3:xl_rows, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fontSize = 16, textDecoration = "bold"), rows = 1, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fontSize = 14, textDecoration = "bold"), rows = 2, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(textDecoration = "bold"), rows = 3:xl_rows, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fontSize = 16, textDecoration = "bold"), rows = 1, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fontSize = 14, textDecoration = "bold"), rows = 2, cols = 1)
        openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(textDecoration = "bold"), rows = 3:11, cols = 1)
        
        if (input$ScenType_1 == "Assumption") {scen_type_1 <- "Baseline (matches discount rate)"} else if (input$ScenType_1 == "6% Constant") {scen_type_1 <- "6% Fixed Return"} else if (input$ScenType_1 == "Recession") {scen_type_1 <- "2022-25 Crisis & 6% Fixed Return"} else if (input$ScenType_1 == "Recurring Recession") {scen_type_1 <- "2022-25 Crisis + 2037-40 Crisis & 6% Fixed Return"} else {scen_type_1 <- paste0("Custom (", input$model_return_1,"%)")}
        if (input$ScenType_2 == "Assumption") {scen_type_2 <- "Baseline (matches discount rate)"} else if (input$ScenType_2 == "6% Constant") {scen_type_2 <- "6% Fixed Return"} else if (input$ScenType_2 == "Recession") {scen_type_2 <- "2022-25 Crisis & 6% Fixed Return"} else if (input$ScenType_2 == "Recurring Recession") {scen_type_2 <- "2022-25 Crisis + 2037-40 Crisis & 6% Fixed Return"} else {scen_type_2 <- paste0("Custom (", input$model_return_2,"%)")}
        if (input$ScenType_3 == "Assumption") {scen_type_3 <- "Baseline (matches discount rate)"} else if (input$ScenType_3 == "6% Constant") {scen_type_3 <- "6% Fixed Return"} else if (input$ScenType_3 == "Recession") {scen_type_3 <- "2022-25 Crisis & 6% Fixed Return"} else if (input$ScenType_3 == "Recurring Recession") {scen_type_3 <- "2022-25 Crisis + 2037-40 Crisis & 6% Fixed Return"} else {scen_type_3 <- paste0("Custom (", input$model_return_3,"%)")}
        if (input$ScenType_4 == "Assumption") {scen_type_4 <- "Baseline (matches discount rate)"} else if (input$ScenType_4 == "6% Constant") {scen_type_4 <- "6% Fixed Return"} else if (input$ScenType_4 == "Recession") {scen_type_4 <- "2022-25 Crisis & 6% Fixed Return"} else if (input$ScenType_4 == "Recurring Recession") {scen_type_4 <- "2022-25 Crisis + 2037-40 Crisis & 6% Fixed Return"} else {scen_type_4 <- paste0("Custom (", input$model_return_4,"%)")}
        
        if (input$ER_Policy_1 == "ADC") {er_policy_1 <- "ADEC"} else {er_policy_1 <- print(input$ER_Policy_1)}
        if (input$ER_Policy_2 == "ADC") {er_policy_2 <- "ADEC"} else {er_policy_2 <- print(input$ER_Policy_2)}
        if (input$ER_Policy_3 == "ADC") {er_policy_3 <- "ADEC"} else {er_policy_3 <- print(input$ER_Policy_3)}
        if (input$ER_Policy_4 == "ADC") {er_policy_4 <- "ADEC"} else {er_policy_4 <- print(input$ER_Policy_4)}
        
        dr_current_1 <- paste0(input$dis_r_current_1, "%")
        dr_current_2 <- paste0(input$dis_r_current_2, "%")
        dr_current_3 <- paste0(input$dis_r_current_3, "%")
        dr_current_4 <- paste0(input$dis_r_current_4, "%")
        
        dr_new_hire_1 <- paste0(input$dis_r_new_1, "%")
        dr_new_hire_2 <- paste0(input$dis_r_new_2, "%")
        dr_new_hire_3 <- paste0(input$dis_r_new_3, "%")
        dr_new_hire_4 <- paste0(input$dis_r_new_4, "%")
        
        nc_new_hire_1 <- print(input$CostSharingNC_1)
        nc_new_hire_2 <- print(input$CostSharingNC_2)
        nc_new_hire_3 <- print(input$CostSharingNC_3)
        nc_new_hire_4 <- print(input$CostSharingNC_4)
        
        amo_new_hire_1 <- print(input$CostSharingAmo_1)
        amo_new_hire_2 <- print(input$CostSharingAmo_2)
        amo_new_hire_3 <- print(input$CostSharingAmo_3)
        amo_new_hire_4 <- print(input$CostSharingAmo_4)
        
        adc_curr_1 <- paste(input$NoYearsADC_CurrentDebt_1, "Years")
        adc_curr_2 <- paste(input$NoYearsADC_CurrentDebt_2, "Years")
        adc_curr_3 <- paste(input$NoYearsADC_CurrentDebt_3, "Years")
        adc_curr_4 <- paste(input$NoYearsADC_CurrentDebt_4, "Years")
        
        adc_new_curr_1 <- paste(input$NoYearsADC_NewDebtCurrentHire_1, "Years")
        adc_new_curr_2 <- paste(input$NoYearsADC_NewDebtCurrentHire_2, "Years")
        adc_new_curr_3 <- paste(input$NoYearsADC_NewDebtCurrentHire_3, "Years")
        adc_new_curr_4 <- paste(input$NoYearsADC_NewDebtCurrentHire_4, "Years")
        
        adc_new_nhire_1 <- paste(input$NoYearsADC_NewDebtNewHire_1, "Years")
        adc_new_nhire_2 <- paste(input$NoYearsADC_NewDebtNewHire_2, "Years")
        adc_new_nhire_3 <- paste(input$NoYearsADC_NewDebtNewHire_3, "Years")
        adc_new_nhire_4 <- paste(input$NoYearsADC_NewDebtNewHire_4, "Years")
        
        amo_method_curr_1 <- print(input$AmoMethod_CurrentHire_1)
        amo_method_curr_2 <- print(input$AmoMethod_CurrentHire_2)
        amo_method_curr_3 <- print(input$AmoMethod_CurrentHire_3)
        amo_method_curr_4 <- print(input$AmoMethod_CurrentHire_4)
        
        amo_method_new_1 <- print(input$AmoMethod_NewHire_1)
        amo_method_new_2 <- print(input$AmoMethod_NewHire_2)
        amo_method_new_3 <- print(input$AmoMethod_NewHire_3)
        amo_method_new_4 <- print(input$AmoMethod_NewHire_4)
        
        cash_infusion_1 <- paste0("$", input$CashInfusion_1)
        cash_infusion_2 <- paste0("$", input$CashInfusion_2)
        cash_infusion_3 <- paste0("$", input$CashInfusion_3)
        cash_infusion_4 <- paste0("$", input$CashInfusion_4)
        
        nhire_dc_pct_1 <- paste0(input$NewHireDCPct_1, "%")
        nhire_dc_pct_2 <- paste0(input$NewHireDCPct_2, "%")
        nhire_dc_pct_3 <- paste0(input$NewHireDCPct_3, "%")
        nhire_dc_pct_4 <- paste0(input$NewHireDCPct_4, "%")
        
        nhire_dc_cont_1 <- paste0(input$DC_Contrib_1, "%")
        nhire_dc_cont_2 <- paste0(input$DC_Contrib_2, "%")
        nhire_dc_cont_3 <- paste0(input$DC_Contrib_3, "%")
        nhire_dc_cont_4 <- paste0(input$DC_Contrib_4, "%")
        
        dc_cont_policy_1 <- print(input$DCPolicy_1)
        dc_cont_policy_2 <- print(input$DCPolicy_2)
        dc_cont_policy_3 <- print(input$DCPolicy_3)
        dc_cont_policy_4 <- print(input$DCPolicy_4)
        
        if (input$nclass == 1) {
          
          openxlsx::writeData(my_workbook, sheet = 1, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 2, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 3, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 4, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 5, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          
          # Sheet 1
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          
          # Sheet 2
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          
          # Sheet 3
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          
          # Sheet 4
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          
          # Sheet 5
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:21, cols = 2, gridExpand = TRUE)
          
          
        } else if (input$nclass == 2) {
          
          openxlsx::writeData(my_workbook, sheet = 1, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 2, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 3, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 4, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 5, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          
          
          openxlsx::writeData(my_workbook, sheet = 1, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 2, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 3, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 4, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 5, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          
          
          # sheet 1
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "percentage"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          
          # sheet 2
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "currency"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          
          # sheet 3
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "percentage"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          
          # sheet 4
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "currency"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          
          # sheet 5
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:21, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "currency"), rows = 19:21, cols = 3, gridExpand = TRUE)
          
          
        } else if (input$nclass == 3) {
          
          openxlsx::writeData(my_workbook, sheet = 1, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 2, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 3, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 4, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 5, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          
          
          openxlsx::writeData(my_workbook, sheet = 1, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 2, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 3, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 4, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 5, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          
          openxlsx::writeData(my_workbook, sheet = 1, c(scen_type_3, er_policy_3, dr_current_3, dr_new_hire_3, nc_new_hire_3,
                                                        amo_new_hire_3, adc_curr_3, adc_new_curr_3, adc_new_nhire_3, amo_method_curr_3,
                                                        amo_method_new_3, cash_infusion_3, nhire_dc_pct_3, nhire_dc_cont_3,
                                                        dc_cont_policy_3), startRow = 3, startCol = 4)
          openxlsx::writeData(my_workbook, sheet = 2, c(scen_type_3, er_policy_3, dr_current_3, dr_new_hire_3, nc_new_hire_3,
                                                        amo_new_hire_3, adc_curr_3, adc_new_curr_3, adc_new_nhire_3, amo_method_curr_3,
                                                        amo_method_new_3, cash_infusion_3, nhire_dc_pct_3, nhire_dc_cont_3,
                                                        dc_cont_policy_3), startRow = 3, startCol = 4)
          openxlsx::writeData(my_workbook, sheet = 3, c(scen_type_3, er_policy_3, dr_current_3, dr_new_hire_3, nc_new_hire_3,
                                                        amo_new_hire_3, adc_curr_3, adc_new_curr_3, adc_new_nhire_3, amo_method_curr_3,
                                                        amo_method_new_3, cash_infusion_3, nhire_dc_pct_3, nhire_dc_cont_3,
                                                        dc_cont_policy_3), startRow = 3, startCol = 4)
          openxlsx::writeData(my_workbook, sheet = 4, c(scen_type_3, er_policy_3, dr_current_3, dr_new_hire_3, nc_new_hire_3,
                                                        amo_new_hire_3, adc_curr_3, adc_new_curr_3, adc_new_nhire_3, amo_method_curr_3,
                                                        amo_method_new_3, cash_infusion_3, nhire_dc_pct_3, nhire_dc_cont_3,
                                                        dc_cont_policy_3), startRow = 3, startCol = 4)
          openxlsx::writeData(my_workbook, sheet = 5, c(scen_type_3, er_policy_3, dr_current_3, dr_new_hire_3, nc_new_hire_3,
                                                        amo_new_hire_3, adc_curr_3, adc_new_curr_3, adc_new_nhire_3, amo_method_curr_3,
                                                        amo_method_new_3, cash_infusion_3, nhire_dc_pct_3, nhire_dc_cont_3,
                                                        dc_cont_policy_3), startRow = 3, startCol = 4)
          
          # sheet 1
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "percentage"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "percentage"), rows = 19:xl_rows, cols = 4, gridExpand = TRUE)
          
          # sheet 2
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "currency"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "currency"), rows = 19:xl_rows, cols = 4, gridExpand = TRUE)
          
          # sheet 3
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "percentage"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "percentage"), rows = 19:xl_rows, cols = 4, gridExpand = TRUE)
          
          # sheet 4
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "currency"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "currency"), rows = 19:xl_rows, cols = 4, gridExpand = TRUE)
          
          # sheet 5
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:21, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "currency"), rows = 19:21, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "currency"), rows = 19:21, cols = 4, gridExpand = TRUE)
          
        } else {
          
          openxlsx::writeData(my_workbook, sheet = 1, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 2, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 3, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 4, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          openxlsx::writeData(my_workbook, sheet = 5, c(scen_type_1, er_policy_1, dr_current_1, dr_new_hire_1, nc_new_hire_1,
                                                        amo_new_hire_1, adc_curr_1, adc_new_curr_1, adc_new_nhire_1, amo_method_curr_1,
                                                        amo_method_new_1, cash_infusion_1, nhire_dc_pct_1, nhire_dc_cont_1,
                                                        dc_cont_policy_1), startRow = 3, startCol = 2)
          
          
          openxlsx::writeData(my_workbook, sheet = 1, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 2, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 3, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 4, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          openxlsx::writeData(my_workbook, sheet = 5, c(scen_type_2, er_policy_2, dr_current_2, dr_new_hire_2, nc_new_hire_2,
                                                        amo_new_hire_2, adc_curr_2, adc_new_curr_2, adc_new_nhire_2, amo_method_curr_2,
                                                        amo_method_new_2, cash_infusion_2, nhire_dc_pct_2, nhire_dc_cont_2,
                                                        dc_cont_policy_2), startRow = 3, startCol = 3)
          
          openxlsx::writeData(my_workbook, sheet = 1, c(scen_type_3, er_policy_3, dr_current_3, dr_new_hire_3, nc_new_hire_3,
                                                        amo_new_hire_3, adc_curr_3, adc_new_curr_3, adc_new_nhire_3, amo_method_curr_3,
                                                        amo_method_new_3, cash_infusion_3, nhire_dc_pct_3, nhire_dc_cont_3,
                                                        dc_cont_policy_3), startRow = 3, startCol = 4)
          openxlsx::writeData(my_workbook, sheet = 2, c(scen_type_3, er_policy_3, dr_current_3, dr_new_hire_3, nc_new_hire_3,
                                                        amo_new_hire_3, adc_curr_3, adc_new_curr_3, adc_new_nhire_3, amo_method_curr_3,
                                                        amo_method_new_3, cash_infusion_3, nhire_dc_pct_3, nhire_dc_cont_3,
                                                        dc_cont_policy_3), startRow = 3, startCol = 4)
          openxlsx::writeData(my_workbook, sheet = 3, c(scen_type_3, er_policy_3, dr_current_3, dr_new_hire_3, nc_new_hire_3,
                                                        amo_new_hire_3, adc_curr_3, adc_new_curr_3, adc_new_nhire_3, amo_method_curr_3,
                                                        amo_method_new_3, cash_infusion_3, nhire_dc_pct_3, nhire_dc_cont_3,
                                                        dc_cont_policy_3), startRow = 3, startCol = 4)
          openxlsx::writeData(my_workbook, sheet = 4, c(scen_type_3, er_policy_3, dr_current_3, dr_new_hire_3, nc_new_hire_3,
                                                        amo_new_hire_3, adc_curr_3, adc_new_curr_3, adc_new_nhire_3, amo_method_curr_3,
                                                        amo_method_new_3, cash_infusion_3, nhire_dc_pct_3, nhire_dc_cont_3,
                                                        dc_cont_policy_3), startRow = 3, startCol = 4)
          openxlsx::writeData(my_workbook, sheet = 5, c(scen_type_3, er_policy_3, dr_current_3, dr_new_hire_3, nc_new_hire_3,
                                                        amo_new_hire_3, adc_curr_3, adc_new_curr_3, adc_new_nhire_3, amo_method_curr_3,
                                                        amo_method_new_3, cash_infusion_3, nhire_dc_pct_3, nhire_dc_cont_3,
                                                        dc_cont_policy_3), startRow = 3, startCol = 4)
          
          openxlsx::writeData(my_workbook, sheet = 1, c(scen_type_4, er_policy_4, dr_current_4, dr_new_hire_4, nc_new_hire_4,
                                                        amo_new_hire_4, adc_curr_4, adc_new_curr_4, adc_new_nhire_4, amo_method_curr_4,
                                                        amo_method_new_4, cash_infusion_4, nhire_dc_pct_4, nhire_dc_cont_4,
                                                        dc_cont_policy_4), startRow = 3, startCol = 5)
          openxlsx::writeData(my_workbook, sheet = 2, c(scen_type_4, er_policy_4, dr_current_4, dr_new_hire_4, nc_new_hire_4,
                                                        amo_new_hire_4, adc_curr_4, adc_new_curr_4, adc_new_nhire_4, amo_method_curr_4,
                                                        amo_method_new_4, cash_infusion_4, nhire_dc_pct_4, nhire_dc_cont_4,
                                                        dc_cont_policy_4), startRow = 3, startCol = 5)
          openxlsx::writeData(my_workbook, sheet = 3, c(scen_type_4, er_policy_4, dr_current_4, dr_new_hire_4, nc_new_hire_4,
                                                        amo_new_hire_4, adc_curr_4, adc_new_curr_4, adc_new_nhire_4, amo_method_curr_4,
                                                        amo_method_new_4, cash_infusion_4, nhire_dc_pct_4, nhire_dc_cont_4,
                                                        dc_cont_policy_4), startRow = 3, startCol = 5)
          openxlsx::writeData(my_workbook, sheet = 4, c(scen_type_4, er_policy_4, dr_current_4, dr_new_hire_4, nc_new_hire_4,
                                                        amo_new_hire_4, adc_curr_4, adc_new_curr_4, adc_new_nhire_4, amo_method_curr_4,
                                                        amo_method_new_4, cash_infusion_4, nhire_dc_pct_4, nhire_dc_cont_4,
                                                        dc_cont_policy_4), startRow = 3, startCol = 5)
          openxlsx::writeData(my_workbook, sheet = 5, c(scen_type_4, er_policy_4, dr_current_4, dr_new_hire_4, nc_new_hire_4,
                                                        amo_new_hire_4, adc_curr_4, adc_new_curr_4, adc_new_nhire_4, amo_method_curr_4,
                                                        amo_method_new_4, cash_infusion_4, nhire_dc_pct_4, nhire_dc_cont_4,
                                                        dc_cont_policy_4), startRow = 3, startCol = 5)
          
          
          
          # sheet 1
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "percentage"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "percentage"), rows = 19:xl_rows, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f6b941", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 1, style = openxlsx::createStyle(fgFill = "#f9d48a", numFmt = "percentage"), rows = 19:xl_rows, cols = 5, gridExpand = TRUE)
          
          # sheet 2
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "currency"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "currency"), rows = 19:xl_rows, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f6b941", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 2, style = openxlsx::createStyle(fgFill = "#f9d48a", numFmt = "currency"), rows = 19:xl_rows, cols = 5, gridExpand = TRUE)
          
          # sheet 3
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "percentage"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "percentage"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "percentage"), rows = 19:xl_rows, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#f6b941", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 3, style = openxlsx::createStyle(fgFill = "#f9d48a", numFmt = "percentage"), rows = 19:xl_rows, cols = 5, gridExpand = TRUE)
          
          # sheet 4
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:xl_rows, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "currency"), rows = 19:xl_rows, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "currency"), rows = 19:xl_rows, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#f6b941", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 4, style = openxlsx::createStyle(fgFill = "#f9d48a", numFmt = "currency"), rows = 19:xl_rows, cols = 5, gridExpand = TRUE)
          
          # sheet 5
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#f5f5f5"), rows = 3:17, cols = 1:5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#ff6633", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#ff9f80", numFmt = "currency"), rows = 19:21, cols = 2, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#2879cb", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#609fe0", numFmt = "currency"), rows = 19:21, cols = 3, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#2e3745", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#6e819e", numFmt = "currency"), rows = 19:21, cols = 4, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#f6b941", halign = "center", fontColour = "#ffffff"), rows = 18, cols = 5, gridExpand = TRUE)
          openxlsx::addStyle(my_workbook, sheet = 5, style = openxlsx::createStyle(fgFill = "#f9d48a", numFmt = "currency"), rows = 19:21, cols = 5, gridExpand = TRUE)
          
        }
        
        
        openxlsx::writeData(my_workbook, sheet = 1, data_1(), startRow = 18, startCol = 1)
        openxlsx::writeData(my_workbook, sheet = 2, data_2(), startRow = 18, startCol = 1)
        openxlsx::writeData(my_workbook, sheet = 3, data_3(), startRow = 18, startCol = 1)
        openxlsx::writeData(my_workbook, sheet = 4, data_4(), startRow = 18, startCol = 1)
        openxlsx::writeData(my_workbook, sheet = 5, t(df_allint()), colNames = F, startRow = 18, startCol = 2)
        
        openxlsx::saveWorkbook(my_workbook, file)
      }
    )
    
    
    
    
  }
  moduleServer(id, server)
}

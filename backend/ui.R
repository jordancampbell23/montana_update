# UI Utilities

top_button_style <- "height: 100%; padding-top: 60px; margin-bottom: 0;"

top_charts_fluidRow <- "margin: 0;" 
top_charts_column <- "height: 100%; padding-top: 0;"
bottom_charts_fluidRow <- "margin: 0;"
bottom_charts_column <- "height: 100%; margin-top: -5px; padding-top: 0; margin-bottom: -50px;"

control_div_style <- "z-index: 99999; 
                overflow: auto;
                background-color: #f5f5f5;
                border: 8px solid #f5f5f5;
                border-radius: 20px;
                position: absolute;
                width: 95%;
                height: 82%;
                top: 11%;
                left: 50%;
                transform: translate(-50%, 0);
                box-shadow: 10px 10px 1000px #333;"

beta_version_label_style <- "position: fixed; top: 2%; left: 1%; font-size: 16px; font-weight: 700; color: #818589; font-family: 'Open Sans', sans-serif;"

refresh_button_style <- "color: #fff; background-color: #2e3745; border-color: #2e3745; height: 33px; font-size: 14px; z-index: 9999;"

margin_top_25 <- "margin-top: 25px;"
margin_top_30b <- "margin-top: 35px; font-weight: 700;"
preload_inputs_style <- "margin-top: 35px;"
name_scenario_style <- "margin-top: 17px; font-weight: 700;"
percent_newhires_style <- "margin-top: 30px;"
emp_dc_cont_style <- "margin-top: 25px;"
dr_curr_mem_style <- "margin-top: 25px;"
dr_new_mem_style <- "margin-top: 25px;"
stress_test_style <- "margin-top: 35px;"
funding_policy_style <- "margin-top: 25px;"
amo_cs_new_hire_style <- "margin-top: 25px;"
cash_inf_style <- "margin-top: 25px;"
amo_per_curr_ual <- "margin-top: 25px"
norm_cs_new_hire_style <- "margin-top: 25px;"
amo_per_new_ual <- "margin-top: 25px;"
amo_per_newnew_ual <- "margin-top: 25px;"
amo_method_ual_currp <- "margin-top: 25px;"
amo_method_ual_newp <- "margin-top: 25px;"

dynamic_input_style_top <- "background-color: #d3d3d3;
                        border-radius: 10px 10px 0 0;
                        border-top: 1px solid #b6b6b6;
                        border-right: 1px solid #b6b6b6;
                        border-left: 1px solid #b6b6b6;"

dynamic_input_style_middle <- "background-color: #d3d3d3;
                        border-radius: 0;
                        border-right: 1px solid #b6b6b6;
                        border-left: 1px solid #b6b6b6;"

dynamic_input_style_bottom <- "background-color: #d3d3d3;
                        border-radius: 0 0 10px 10px;
                        border-bottom: 1px solid #b6b6b6;
                        border-right: 1px solid #b6b6b6;
                        border-left: 1px solid #b6b6b6;"

dynamic_input_style_all <- "background-color: #d3d3d3;
                        border-radius: 10px 10px 10px 10px;
                        border-top: 1px solid #b6b6b6;
                        border-bottom: 1px solid #b6b6b6;
                        border-right: 1px solid #b6b6b6;
                        border-left: 1px solid #b6b6b6;"

control_panel_input_style_1 <- "margin-top: 10px; font-weight: 800; font-color: #000;"


custom_dropdown_input <- ".dropdown,
.bootstrap-select,
.form-control,
.bs3,
.shinyjs-resettable,
.shiny-bound-input {
  font-family: 'Open Sans', sans-serif;
  text-transform: uppercase;
  font-size: 14px;
  display: inline-block;
  margin-bottom: 0;
  font-weight: 700;
  text-align: center;
  white-space: nowrap;
  vertical-align: middle;
  background-image: none;
  background-color: #d3d3d3;
  color: #333;
  border: 1px solid;
  border-radius: 10px;
}"



emoji_icon <- function(icon) {
  div(
    div(icon, style = "position: absolute; 
                       top: 50%;
                       left: 25%;"),
    style = "color: #333;
                           text-align: center;
                           font-weight: 800;
                           margin-top: 20px;
                           margin-left: -3px;
                           height: 31px;
                           width: 30px;
                           background-color: #d3d3d3;
                           font-size: 16px;")
}


right_icon <- function(icon) {
  div(
    div(icon, style = "position: absolute; 
                       top: 50%;
                       left: 25%;"),
    style = "color: #333;
                           text-align: center;
                           font-weight: 800;
                           margin-top: 20px;
                           margin-left: -3px;
                           height: 31px;
                           width: 30px;
                           background-color: #d3d3d3;
                           font-size: 16px;")
}

left_icon <- function(icon) {
  div(
    div(icon, style = "position: absolute; 
                       top: 50%;
                       left: 25%;"),
    style = "color: #333;
                           text-align: center;
                           font-weight: 800;
                           margin-top: 20px;
                           margin-left: -3px;
                           height: 31px;
                           width: 30px;
                           background-color: #d3d3d3;
                           font-size: 16px;")
}


hover_text <- function(title, text, position) {
  strong(
               title, 
               tags$span(
                 icon(
                   name = "question-circle",
                   style= "color: #808080"
                 ) 
               ) |>
                 add_prompt(message = text,
                            position = position,
                            bounce = F)
             )
}


text_card <- function(..., header = NULL) {
  div(
    class = "card",
    style = "margin: 0px; border-radius: 10px;",
    header, 
    div(class = "card-content", ..., style = " border-radius: 10px; color: #333; background-color: #fff; margin-top: 20px;", align = "center")
  )
}

data_card <- function(..., header = NULL) {
  div(
    class = "card",
    style = "margin: 0px;  border-radius: 10px;",
    header, 
    div(class = "card-content", ..., style = " border-radius: 10px; color: #333; background-color: #fff; margin-top: 20px;", 
        align = "left")
  )
}

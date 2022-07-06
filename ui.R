
options(spinner.color = "#ff6633", spinner.size = 1, spinner.type = 8)


div(class = "navbar", navbarPage(title = div(img(src="", height = "25px"), "TESTING"),
                                  position = "fixed-top",
                                  theme = "css/style.css",
                                  # fluid = F,
                                  collapsible = TRUE,
                                 
                                  
                                  # ----------------------------------
                                  # tab panel 1 - Home
                                  tabPanel(strong("Home"),
                                           # mod_login_ui("login"),
                                           includeHTML("home.html"),
                                           # tags$script(src = "plugins/scripts.js"),
                                           tags$head(
                                             tags$link(rel = "stylesheet", 
                                                       type = "text/css", 
                                                       href = "css/static.css"),
                                             tags$link(rel = "stylesheet", 
                                                       type = "text/css", 
                                                       href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css")
                                           )),
                                  
                                  # ----------------------------------
                                  # tab panel 2 - Neighborhood Browser
                                  tabPanel("PERS at a Glance",
                                           mod_historical_ui("historical")
                                           ),
                                 
                                  
                                  # ----------------------------------
                                  # tab panel 3 - Location Comparison
                                  tabPanel("Funding Model",
                                          mod_deterministic_ui("deterministic"),
                                           tags$head(
                                             tags$link(rel = "stylesheet", 
                                                       type = "text/css", 
                                                       href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css")
                                           )
                                           ),
                                  
                                  # ----------------------------------
                                  # tab panel 4 - About
                                  tabPanel("Benefit Model",
                                           mod_benefits_ui("benefits"),
                                           tags$head(
                                             tags$link(rel = "stylesheet", 
                                                       type = "text/css", 
                                                       href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css")
                                           ))
                                  
))
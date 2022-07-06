#--------------------------------Login for App---------------------------------#


# UI

mod_login_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    
    div(
      id = ns("protect"),
      style = "z-index: 99999999;
      background-color: #f5f5f5;
      position: fixed;
      width: 100%;
      height: 100%;
      left: 50%;
      transform: translate(-50%, 0%);
      box-shadow: 10px 10px 400px 200px rgba(0,0,0,0.1);
-webkit-box-shadow: 10px 10px 400px 200px rgba(0,0,0,0.1);
-moz-box-shadow: 10px 10px 400px 200px rgba(0,0,0,0.1);",

div(
  h2(
    "MPERA-PERS - Login",
    style = "font-family: 'Open Sans', sans-serif; font-weight: 800; color: #333",
    align = "center"
  ),
  br(),
  
  fluidRow(column(4),
           
           column(
             4,
             
             div(
               class = "well",
               # h4(class = "text-center", "Login"),
               
               textInput(
                 inputId     = ns("user_name"),
                 label       = tagList("Username"),
                 placeholder = "Enter username"
               ),
               
               passwordInput(
                 inputId     = ns("password"),
                 label       = tagList("Password"),
                 placeholder = "Enter password"
               ),
               
               div(
                 class = "text-center",
                 actionButton(
                   inputId = ns("login_button"),
                   label = "Log In",
                   class = "btn-primary"
                 )
               )
             )
           ),
           
           column(4),),
  
  br(),
  
  div(
    id = "logo-bottom",
    align = "center",
    tags$a(
      img(
        src = 'https://raw.githubusercontent.com/jordancampbell23/Texas/master/reason_logo_V.PNG',
        style = "display: block; margin-left:auto; margin-right: auto;",
        height = 95,
        width = 237
      ),
      href = "https://reason.org/topics/pension-reform/"
    ),
    p(
      tags$a(href = "https://reason.org/topics/pension-reform/",
             "Reason Foundation",
             style = "color: #FF6633; font-size: 16px;font-family: 'Open Sans', sans-serif; font-family: 'Roboto', sans-serif;"),
      " | Pension Integrity Project",
      style = "font-size: 16px"
    )
  )
  
)
    )

  )
  
  
}


# Server

mod_login <- function(id, state) {
  server <- function(input, output, session) {
    ns <- session$ns
    
    
    ## observe the button being pressed
    observeEvent(input$login_button, {
      if (input$user_name == "MPERA-PERS" & input$password == ">H5roZ") {
        shinyjs::hide(id = "protect")
      } else{
        shinyjs::show(id = "protect")
      }
    },
    ignoreNULL = F)
    
  }
  moduleServer(id, server)
}

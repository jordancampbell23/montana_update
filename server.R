server <- function(input, output, session) {
  
  # mod_login("login")
  mod_historical_server("historical")
  mod_deterministic_server("deterministic")
  mod_benefits_server("benefits")
  
}
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(purrr)
library(echarts4r)
library(reactable)
library(zoo)
library(openxlsx)
library(readxl)
library(prompter)
library(stringr)

#---------------------------SOURCE & SETTINGS--------------------------------
source("backend/ui.R")
source("backend/benefit_model.R")
source("backend/funding_model.R")
options(spinner.color = "#ff6633", spinner.size = 1, spinner.type = 8)


#--------------------------------MODULES----------------------------------------
# source("modules/mod_login.R")
source("modules/mod_historical.R")
source("modules/mod_deterministic.R")
source("modules/mod_benefits.R")
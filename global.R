library(DT)
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(bslib)
library(shinyFiles)

theme <- bs_theme(
  bootswatch = "cerulean",
  base_font = font_google("Roboto"),
  heading_font = font_google("Poppins"),
  primary = "#2c3e50",
  secondary = "#adb5bd",
  bg = "#f8f9fa",
  fg = "#2c3e50"
)

vogais <- c("a", "e", "eh", "i", "o", "oh", "u")

options(shiny.maxRequestSize = 500*1024^2)
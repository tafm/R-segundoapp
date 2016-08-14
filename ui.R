library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "PRO-SPB"),
  
  dashboardSidebar(
    numericInput("nvars", "Número de variáveis:", value = 1, min = 1, max = 2),
    uiOutput("seletorvariavelx"),
    conditionalPanel(
      cond = "input.nvars == 2", 
      uiOutput("seletorvariavely"),
      uiOutput("seletorgrafico")
    )
  ),
  
  dashboardBody(fluidRow(tabBox(
    width=12,
    selected = "Gráficos",
    tabPanel("Gráficos", "Tab content 1"),
    tabPanel("Sumário", "Tab content 2"),
    tabPanel("Tabela", "Note that when side=right, the tab order is reversed.")
  )))
)

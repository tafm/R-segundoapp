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
      uiOutput("seletorvariavely")
    ),
    uiOutput("seletorgrafico")
  ),
  
  dashboardBody(fluidRow(tabBox(
    width=12,
    selected = "Gráfico",
    tabPanel("Gráfico", "Tab content 1"),
    tabPanel("Sumário", tableOutput('sumario')),
    tabPanel("Tabela", dataTableOutput('tabela'))
  )))
)

library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "PRO-SPB"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráficos", tabName = "tabGraficos", icon = icon("bar-chart")),
      menuItem("Sumário", tabName = "tabSumario", icon = icon("file-text-o")),
      menuItem("Tabela", tabName = "tabTabela", icon = icon("table"))
    ),
    numericInput("nvars", "Número de variáveis:", value = 1, min = 1, max = 2),
    uiOutput("seletorvariavelx"),
    conditionalPanel(
      cond = "input.nvars == 2", 
      uiOutput("seletorvariavely")
    )
  ),
  
  dashboardBody()
)

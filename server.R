#Carregando dados
dados <- data.frame(read.csv2("dados.csv", header = TRUE, sep = ";", dec = ","))
nomevariaveis <- colnames(dados)

shinyServer(function(input, output) { 
  #Variável X
  output$seletorvariavelx <- renderUI({
    if(input$nvars == "1") {
      variaveisA <- nomevariaveis
    } else {
      if(input$varx != input$vary) {
        variaveisA <- nomevariaveis[nomevariaveis != input$vary]
      } else {
        variaveisA <- nomevariaveis
      }
    }
    
    selectInput("varx", "Variável x:", variaveisA, selected = input$varx)
  })
  
  #Variável Y
  output$seletorvariavely <- renderUI({
    variaveisB <- nomevariaveis[nomevariaveis != input$varx]
    if(!is.null(input$vary)) {
      if(input$varx == input$vary) {
        selecionadaB <- variaveisB[1]
      } else {
        selecionadaB <- input$vary
      }
    } else {
      selecionadaB <- input$vary
    }
    selectInput("vary", "Variável y:", variaveisB, selected = selecionadaB)
  })
  
  #Seletor gráfico
  
  output$seletorgrafico<- renderUI({
    if(input$nvars == "1") {
      tipograficos <- c("Histograma" = 1, "Pizza" = 2)
    } else {
      tipograficos <- c("Barras" = 3)
    }
    selectInput("tipografico", "Escolha o gráfico:", tipograficos)
  })
  
  #Sumário
  
  
  output$sumario <- renderTable({
    matrizsumario <- matrix(NA, nrow=input$nvars, ncol=6)
    matrizsumario[1,] <- c(min(dados[,input$varx]), quantile(dados[, input$varx], 0.25), median(dados[, input$varx]), mean(dados[, input$varx]), quantile(dados[, input$varx], 0.75), max(dados[, input$varx]))
    if(input$nvars == "1") {
      nomessum <- c(input$varx)
      summary(dados[,c(input$varx)])
      dimnames(matrizsumario) = (list(nomessum, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")))
      matrizsumario
    } else {
      if(input$vary != "") {
        nomessum <- c(input$varx, input$vary)
        matrizsumario[2,] <- c(min(dados[,input$vary]), quantile(dados[, input$vary], 0.25), median(dados[, input$vary]), mean(dados[, input$vary]), quantile(dados[, input$vary], 0.75), max(dados[, input$vary]))
        summary(dados[,c(input$varx, input$vary)])
        dimnames(matrizsumario) = (list(nomessum, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")))
        matrizsumario
      }
    }
  })
  
  #Tabela
  output$tabela <- renderDataTable(dados,options = list(scrollX = TRUE))
})
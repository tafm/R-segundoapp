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
})
#Carregando dados
dados <- data.frame(read.csv2("dados.csv", header = TRUE, sep = ";", dec = ","))
nomevariaveis <- colnames(dados)

#Separa dados em intervalos
legendaRanges <- function(intervalos) {
  #legendas <- c(paste(intervalos[1], intervalos[2], sep = "-"))
  legendas <- c(paste(paste(paste("de", intervalos[1], sep = " "), "a", sep = " "), intervalos[2], sep = " "))
  if(length(intervalos) > 4) {
    for(i in 2:(length(intervalos) - 2)) {
      #legendas <- c(legendas, paste(intervalos[i], intervalos[i + 1], sep = "-"))
      legendas <- c(legendas, paste(paste(paste("de", intervalos[i], sep = " "), "a", sep = " "), intervalos[i + 1], sep = " "))
    }
  }
  #legendas <- c(legendas, paste(">=", intervalos[length(intervalos) - 1]))
  legendas <- c(legendas, paste("maior que", intervalos[length(intervalos) - 1], sep = " "))
  return (legendas)
}

quebraRanges <- function(vetor, min, max, nfatias) {
  if((max - min) > nfatias) {
    tam <- floor((max - min) / nfatias)
    intervalos <- c(min)
    for(i in 1:(nfatias - 1)) {
      intervalos <- c(intervalos, min + (tam * i))
    }
    intervalos <- c(intervalos, max)
  } else {
    
    intervalos <- c(min:(min + ceiling(max - min)))
  }
  return (intervalos)
}

quebraDados <- function(variavel, intervalos) {
  contagem <- c(sum(variavel >= intervalos[1] & variavel < intervalos[2]))
  if(length(intervalos) > 4) {
    for(i in 2:(length(intervalos) - 2)) {
      contagem <- c(contagem, sum(variavel >= intervalos[i] & variavel < intervalos[i + 1]))
    }
  }
  contagem <- c(contagem, sum(variavel >= intervalos[length(intervalos) - 1]))
  return(contagem)
}

porcentagem <- function(contagem, total) {
  return(contagem / total * 100)
}

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
  
  #Gráfico
  output$grafico <- renderPlot({
    if(!is.null(input$varx)) {
      if(input$tipografico == 1) {
        valor <- dados[,input$varx]
        m <- ggplot(data = dados, aes(x = valor))
        m + geom_histogram(binwidth = .5, fill = "darkblue") +
        ggtitle(paste("Histograma de", input$varx, sep = " "))
      } else if(input$tipografico == 2) {
        variavel1 <- dados[,which(colnames(dados)==input$varx)]
        intervalos <- quebraRanges(variavel1, min(variavel1), max(variavel1), 4)
        dadospizza <- data.frame(legendaRanges(intervalos), quebraDados(variavel1, intervalos))
        colnames(dadospizza) <- c("intervalos", "frequencia")
        
        ggplot(dadospizza, aes(x="", y=frequencia, fill=intervalos))+
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y", start=0) +
          ggtitle("Gráfico Pizza")
      } else if(input$tipografico == 3) {
        if(input$vary != "") {
          variavel1 <- dados[,which(colnames(dados)==input$varx)]
          variavel2 <- dados[,which(colnames(dados)==input$vary)]
          intervalos <- quebraRanges(variavel1, min(min(variavel1), min(variavel2)), max(max(variavel1), max(variavel2)), 4)
          dadosbarras <- data.frame(legendaRanges(intervalos), quebraDados(variavel1, intervalos), quebraDados(variavel2, intervalos))
          colnames(dadosbarras) <- c("intervalo", input$varx, input$vary)
          #print(dadosbarras)
          df.long <- melt(dadosbarras, id.var = "intervalo")
          colnames(df.long) <- c("Intervalos", "Variavel", "Frequencia")
          #print(df.long)
          ggplot(df.long,aes(Intervalos, Frequencia,fill=Variavel)) +
            geom_bar(stat="identity",position="dodge") +
            ggtitle("Gráfico de barras")
        }
      }
    }
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
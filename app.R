library(shiny)
library(GA)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Otimização de Transporte de Cargas"),
    
    tabsetPanel(
        # Para importar os dados
        tabPanel("Dados",
            fluidRow(
                column(6, fileInput("arquivo", "Selecione o arquivo:",multiple = F, accept = c(".csv"))),
                column(6, actionButton("Totais", "Calcular Totais"))
            ),
            fluidRow(
                column(3,h3(textOutput("TQuantidade"))),
                column(3,h3(textOutput("TPesototal"))),
                column(3,h3(textOutput("TVolumetotal"))),
                column(3,h3(textOutput("TValor")))
            ),
            fluidRow(
                column(12, tableOutput("Dados"))
            )
            
        ),
        # Para procurar a melhor solução
        tabPanel("Processamento",
             fluidRow(
                 column(3,numericInput("sobrapeso", "Informe a sobra de peso", value = 10)),
                 column(3,numericInput("sobravolume", "Informe a sobra de volume", value = 6800)),
                 column(3,numericInput("sobrapeso2", "Informe a sobra de peso2", value = 16)),
                 column(3,numericInput("sobravolume2", "Informe a sobra de volume2", value = 8700)),
                 column(3,numericInput("sobrapeso3", "Informe a sobra de peso3", value = 8)),
                 column(3,numericInput("sobravolume3", "Informe a sobra de volume3", value = 5300)),
                 column(3,numericInput("iteracoes", "Informe quantidade de iterações", value = 10)),
                     column(3,actionButton("Processar","Processar"))
             ),
             fluidRow(
                 column(3,h3(textOutput("RQuantidade"))),
                 column(3,h3(textOutput("RPesototal"))),
                 column(3,h3(textOutput("RVolumetotal"))),
                 column(3,h3(textOutput("RValor")))
             ),
             fluidRow(
                 column(12, tableOutput("Rfinal"))
             )
        )
    )
)  

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$Totais, {
        file1 = input$arquivo
        #Lê o arquivo
        itens <<- read.csv(file1$datapath, sep = ";")
        #Para saber quantas linhas/cargas tem o arquivo
        z <<- nrow(itens)
        
        #Outputs iniciais
        output$Dados = renderTable({itens})
        output$TQuantidade = renderText({paste0("Quantidade de itens: ",z)})
        output$TPesototal = renderText({paste0("Peso Total: ", sum(itens$PESO))})
        output$TVolumetotal = renderText({paste0("Volume Total: ", sum(itens$VOLUME))})
        output$TValor = renderText({paste0("Valor Total: ", sum(itens$VALOR))})
    })
    
    #Ler os dados que vamos passar ao sistema
    observeEvent(input$Processar,{
        maxvolume = input$sobravolume
        maxpeso = input$sobrapeso
        maxvolume2 = input$sobravolume2
        maxpeso2 = input$sobrapeso2
        maxvolume3 = input$sobravolume3
        maxpeso3 = input$sobrapeso3
        
        #função que vai validar minha opção de solução
        f = function(x)
        {
            valor = 0
            peso = 0
            volume = 0
            
            #Percorre todos os itens
            for (i in 1:z) {
                #Verifica se o item foi selecionado
                if (x[i] != 0) {
                    valor = valor + itens[i,3]
                    peso = peso + itens[i,2]
                    volume = volume + itens[i,4]
                }
            }
            #Se ultrapassar, retorna 0, para ser invalida
            if ((volume > maxvolume | peso > maxpeso) & (volume > maxvolume2 | peso > maxpeso2) & (volume > maxvolume3 | peso > maxpeso3)){
                valor = 0
            }
            return(valor)
        }
    
    
    #Chama a função ga
    resultado = ga("binary", fitness = f, nBits = z, popSize = 100, maxiter = input$iteracoes, pmutation = 0.05)
    
    #Transformar em dataframe a função summary do resultado do elemento solution
    result = t(as.data.frame(summary(resultado)$solution))
    
    #Pega apenas os itens que o AG escolheu como solução
    result = itens[result[,1]==1,]
    
    #Resultado Final
    output$Rfinal = renderTable({result})
    
    #Itens da escolha final
    output$RQuantidade = renderText({paste0("Quantidade final: ", nrow(result))})
    output$RPesototal = renderText({paste0("Peso Final: ", sum(result$PESO))})
    output$RVolumetotal = renderText({paste0("Volume Final: ", sum(result$VOLUME))})
    output$RValor = renderText({paste0("Valor Total: ", sum(result$VALOR))})
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

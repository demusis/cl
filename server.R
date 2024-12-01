server <- function(input, output, session) {
  options(shiny.maxRequestSize = -1)  # Desativa limite de upload

  options(shiny.launch.browser = TRUE)
  
  options(shiny.upload.handler = function(file, ...) {
    # Apenas retorna o caminho do arquivo sem fazer upload
    list(
      name = file$name,
      datapath = file$datapath,
      size = file$size,
      type = file$type
    )
  })
  
  dados <- reactiveValues(
    arquivos = list(
      padrao = list(
        formant = NULL,
        textgrid = NULL,
        pitchtier = NULL
      ),
      questionado = list(
        formant = NULL,
        textgrid = NULL,
        pitchtier = NULL
      )
    )
  )

    # Armazena resultados do processamento
    resultados_processamento <- reactiveValues(
      dados_calculados = NULL,
      status = NULL,
      tabela = NULL,
      grafico = NULL
    )
  
    # Observa o clique do botão processar
    observeEvent(input$processar_btn, {
      # Inicia processamento
      withProgress(message = 'Processando dados...', {
        resultados_processamento$status <- "Processamento iniciado..."
        
        # Simula cálculo pesado
        resultados <- isolate({
          # Seu código de processamento aqui
          Sys.sleep(2) # Simula processamento
          list(
            dados = runif(100),
            timestamp = Sys.time()
          )
        })
        
        # Armazena resultados
        resultados_processamento$dados_calculados <- resultados
        resultados_processamento$status <- "Processamento concluído"
      })
    })
    
    # Renderiza outputs reativos
    output$status_processamento <- renderText({
      req(resultados_processamento$status)
      resultados_processamento$status
    })
    
    output$resultados_tabela <- renderDT({
      req(resultados_processamento$dados_calculados)
      # Processa dados para tabela
      datatable(data.frame(
        valores = resultados_processamento$dados_calculados$dados
      ))
    })
    
    output$grafico_resultados <- renderPlot({
      req(resultados_processamento$dados_calculados)
      # Processa dados para gráfico
      plot(resultados_processamento$dados_calculados$dados)
    })

  observe({
    # Store standard file paths and names
    if (!is.null(input$padrao_formant)) {
      dados$arquivos$padrao$formant <- list(
        nome = input$padrao_formant$name,
        caminho = input$padrao_formant$datapath
      )
    }
    
    if (!is.null(input$padrao_textgrid)) {
      dados$arquivos$padrao$textgrid <- list(
        nome = input$padrao_textgrid$name,
        caminho = input$padrao_textgrid$datapath
      )
    }
    
    if (!is.null(input$padrao_pitchtier)) {
      dados$arquivos$padrao$pitchtier <- list(
        nome = input$padrao_pitchtier$name,
        caminho = input$padrao_pitchtier$datapath
      )
    }
    
    # Store questioned file paths and names
    if (!is.null(input$quest_formant)) {
      dados$arquivos$questionado$formant <- list(
        nome = input$quest_formant$name,
        caminho = input$quest_formant$datapath
      )
    }
    
    if (!is.null(input$quest_textgrid)) {
      dados$arquivos$questionado$textgrid <- list(
        nome = input$quest_textgrid$name,
        caminho = input$quest_textgrid$datapath
      )
    }
    
    if (!is.null(input$quest_pitchtier)) {
      dados$arquivos$questionado$pitchtier <- list(
        nome = input$quest_pitchtier$name,
        caminho = input$quest_pitchtier$datapath
      )
    }
    
    # Enable/disable process button based on file selection
    todos_arquivos_selecionados <- !is.null(dados$arquivos$padrao$formant) &&
      !is.null(dados$arquivos$padrao$textgrid) &&
      !is.null(dados$arquivos$padrao$pitchtier) &&
      !is.null(dados$arquivos$questionado$formant) &&
      !is.null(dados$arquivos$questionado$textgrid) &&
      !is.null(dados$arquivos$questionado$pitchtier) &&
      length(input$vogais_selecionadas) > 0
    
    if(todos_arquivos_selecionados) {
      enable("processar_btn")
    } else {
      disable("processar_btn")
    }
  })
}
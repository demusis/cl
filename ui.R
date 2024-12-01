ui <- fluidPage(
  useShinyjs(),
  theme = theme,
  titlePanel("Análise de Áudio"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Seção Padrão
      wellPanel(
        h4("Padrão"),
        fileInput("padrao_formant", "Selecione o arquivo .FORMANT",
                  buttonLabel = "Procurar",
                  placeholder = "Sem arquivo selecionado",
                  accept = ".FORMANT",
                  multiple = FALSE
        ),
        fileInput("padrao_textgrid", "Selecione o arquivo .TEXTGRID",
                  buttonLabel = "Procurar",
                  placeholder = "Sem arquivo selecionado",
                  accept = ".TEXTGRID",
                  multiple = FALSE
        ),
        fileInput("padrao_pitchtier", "Selecione o arquivo .PITCHTIER",
                  buttonLabel = "Procurar",
                  placeholder = "Sem arquivo selecionado",
                  accept = ".PITCHTIER",
                  multiple = FALSE
        )
      ),
      
      # Seção Questionado
      wellPanel(
        h4("Questionado"),
        fileInput("quest_formant", "Selecione o arquivo .FORMANT",
                  buttonLabel = "Procurar",
                  placeholder = "Sem arquivo selecionado",
                  accept = ".FORMANT",
                  multiple = FALSE
        ),
        fileInput("quest_textgrid", "Selecione o arquivo .TEXTGRID",
                  buttonLabel = "Procurar",
                  placeholder = "Sem arquivo selecionado",
                  accept = ".TEXTGRID",
                  multiple = FALSE
        ),
        fileInput("quest_pitchtier", "Selecione o arquivo .PITCHTIER",
                  buttonLabel = "Procurar",
                  placeholder = "Sem arquivo selecionado",
                  accept = ".PITCHTIER",
                  multiple = FALSE
        )
      ),
      
      # Seção Processamento
      wellPanel(
        h4("Processamento"),
        sliderInput("num_repeticoes",
                    "Número de repetições:",
                    min = 10,
                    max = 10000,
                    value = 100,
                    step = 10
        ),
        checkboxGroupInput("vogais_selecionadas",
                           "Selecione as vogais:",
                           choices = vogais,
                           selected = c("a", "e", "eh"),
                           inline = TRUE
        ),
        tags$div(
          style = "color: red; font-size: 0.8em;",
          textOutput("erro_vogais")
        ),
        br(),
        actionButton(
          "processar_btn",
          "Processar Dados",
          class = "btn btn-primary btn-block"
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Status",
          withSpinner(verbatimTextOutput("status_processamento"))
        ),
        tabPanel(
          "Resultados",
          withSpinner(DTOutput("resultados_tabela"))
        ),
        tabPanel(
          "Gráficos",
          withSpinner(plotOutput("grafico_resultados"))
        )
      )
    )
  )
)
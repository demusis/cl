# Análise estatístisca de arquivos do Praat para comparação de locutor 
# v. 0.03
# Autor: Carlo Ralph De Musis

ui <- fluidPage(
  useShinyjs(),
  theme = theme,
  titlePanel("Análise estatístisca de arquivos do Praat para comparação de locutor v. 0.03"),
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
        textInput(
          inputId = "id",
          label = "Identificador:",
          value = "VM1.1",
          placeholder = "Digite um identificador"
        ),
        sliderInput("num_formantes",
                    "Número de formantes:",
                    min = 2,
                    max = 5,
                    value = 2,
                    step = 1
        ),
        radioButtons("remover_outliers",
                     "Remover outliers?",
                     choices = c("Sim" = "sim", "Não" = "nao"),
                     selected = "nao",
                     inline = TRUE
        ),
        sliderInput("num_permutacoes",
                    "Número de permutações:",
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
          "Descritivas",
          withSpinner(verbatimTextOutput("descritivas")),
          h5("Fala exclusiva"),
          h6("Padrão"),
          withSpinner(verbatimTextOutput("fe_padrao")),
          h6("Questionado"),
          withSpinner(verbatimTextOutput("fe_questionado")),
          h5("Processos"),
          withSpinner(DTOutput("tabela_socio"))
        ),
        tabPanel(
          "Formantes",
          h5("PERMANOVA: (f1, f2, ... fn) ~ Grupo + Vogal + Grupo * Vogal"),
          withSpinner(DTOutput("anova_tabela")),
          h5("PERMDISP"),
          withSpinner(plotOutput("permdisp")),
          withSpinner(verbatimTextOutput("permdisp_res")),
          h5("Gráfico de dispersão por formantes (f1 e f2), vogais selecionadas e grupo"),
          withSpinner(plotOutput("hexvogais")),
          h5("Box-plots dos formantes por vogais selecionadas e grupos"),
          withSpinner(plotOutput("boxplotvogaisf1")),
          withSpinner(plotOutput("boxplotvogaisf2")),
          withSpinner(plotOutput("boxplotvogaisf3")),
          withSpinner(plotOutput("boxplotvogaisf4")),
          withSpinner(plotOutput("boxplotvogaisf5")),
        ),
        tabPanel(
          "Frequência fundamental (f0)",
          # h5("ANOVA"),
          # withSpinner(DTOutput("anova_pitch")),
          #h5("Comparações de médias (não-paramétrica)"),
          #withSpinner(DTOutput("anova_posthoc")),
          h5("Histogramas por grupo"),
          withSpinner(plotOutput("histf0")),
          # h5("Teste de Kolmogorov-Smirnov para duas amostras"),
          withSpinner(verbatimTextOutput("f0_ks"))
        )
      )
    )
  )
)
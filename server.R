# Análise estatístisca de arquivos do Praat para comparação de locutor 
# v. 0.037
# Autor: Carlo Ralph De Musis

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
      withProgress(message = 'Organizando os dados...', value = 0, {
        resultados_processamento$status <- "Processamento iniciado..."
        
        # Simula cálculo pesado
        resultados <- isolate({
          n <- 11
          
          # Ler arquivo TextGrid e arquivo de formantes do Padrao e Questionado
          textgrid_padrao <<- tg.read(input$padrao_textgrid$datapath, 
                                     encoding = "auto")
          incProgress(1/n)
          formantes_padrao <<- formant.read(input$padrao_formant$datapath, 
                                           encoding = "auto")
          incProgress(1/n)
          formantes_padrao <<- formant.toArray(formantes_padrao)
          incProgress(1/n)
          
          textgrid  <<- tg.read(input$quest_textgrid$datapath, 
                               encoding = "auto")
          incProgress(1/n)
          formantes <<- formant.read(input$quest_formant$datapath, 
                                    encoding = "auto")
          incProgress(1/n)
          formantes <<- formant.toArray(formantes)
          incProgress(1/n)
          
          # Ler e processar os arquivos de Pitch
          pitch_padrao <<- pitchtier_dataframe(textgrid_padrao, 
                                              input$padrao_pitchtier$datapath, 
                                              "Padrão", 
                                              "Vozes",
                                              outlier = (input$remover_outliers == 'sim'))
          incProgress(1/n)
          
          pitch_questionado <<- pitchtier_dataframe(textgrid,
                                                   input$quest_pitchtier$datapath, 
                                                   "Questionado", 
                                                   "Vozes",
                                                   outlier = (input$remover_outliers == 'sim'))
                                                   
          pitch_combinado <<- rbind(pitch_padrao, pitch_questionado)
          pitch_combinado$Origem <<- as.factor(pitch_combinado$Origem)
          
          incProgress(1/n)
          
          resultado_padrao <- processar_formantes(input$remover_outliers, input$num_formantes, formantes_padrao, textgrid_padrao)
          dados_vogais_sem_outliers_padrao <- resultado_padrao$dados_vogais_sem_outliers
          
          resultado <- processar_formantes(input$remover_outliers, input$num_formantes, formantes, textgrid)
          dados_vogais_sem_outliers <- resultado$dados_vogais_sem_outliers
          incProgress(1/n)
          
          # Adicionar uma coluna de grupo para diferenciar os dataframes e combiná-los
          dados_vogais_sem_outliers_padrao <- dados_vogais_sem_outliers_padrao %>% mutate(grupo = "padrão")
          dados_vogais_sem_outliers <- dados_vogais_sem_outliers %>% mutate(grupo = "questionado")
          
          medias_vogais_sem_outliers <- resultado$medias_vogais_sem_outliers
          medias_vogais_sem_outliers_padrao <- resultado_padrao$medias_vogais_sem_outliers
          
          medias_vogais_sem_outliers_padrao <- medias_vogais_sem_outliers_padrao %>% mutate(grupo = "padrão")
          medias_vogais_sem_outliers <- medias_vogais_sem_outliers %>% mutate(grupo = "questionado")
          
          df_combinado <- rbind(medias_vogais_sem_outliers_padrao, medias_vogais_sem_outliers)
          incProgress(1/n)
          
          # Filtrar as linhas que contêm apenas as vogais selecionadas
          df_filtrado <<- df_combinado[df_combinado$vogal %in% input$vogais_selecionadas, ]
        })
        
        # Armazena resultados
        # resultados_processamento$dados_calculados <- resultados
        resultados_processamento$status <- "Processamento concluído."
      })
    })
    
    # Renderiza outputs reativos
    output$descritivas <- renderText({
      req(resultados_processamento$status)
      resultados_processamento$status
    })
    
    output$fe_padrao <- renderText({
      req(resultados_processamento$status)
      fala_liquida_padrao <- calcular_tempos(textgrid_padrao, 1, input$id)
      
      # Garantir que os valores são numéricos e arredondar para duas casas decimais
      valores <- round(as.numeric(unlist(fala_liquida_padrao)), 2)
      
      # Formatar os resultados
      texto_formatado <- sprintf(
        "Total: %.2f s, com fala exclusiva: %.2f s, correspondendo a %.2f%%.",
        valores[2], valores[1], valores[3]
      )
      
      texto_formatado
    })
    
    
    output$fe_questionado <- renderText({
      req(resultados_processamento$status)
      fala_liquida_questionado <- calcular_tempos(textgrid, 1, input$id)
      
      # Garantir que os valores são numéricos e arredondar para duas casas decimais
      valores <- round(as.numeric(unlist(fala_liquida_questionado)), 2)
      
      # Formatar os resultados
      texto_formatado <- sprintf(
        "Total: %.2f s, com fala exclusiva: %.2f s, correspondendo a %.2f%%.",
        valores[2], valores[1], valores[3]
      )
      
      texto_formatado
    })
    
    output$tabela_socio <- renderDT({
      req(resultados_processamento$status)
      
      fl_questionado <- gerar_dataframe_textgrid(textgrid)
      fl_padrao <- gerar_dataframe_textgrid(textgrid_padrao)
      
      tabela_fl <- combine_fl(fl_questionado, fl_padrao)
      
      datatable(
        tabela_fl,
        extensions = c("Buttons"),
        options = list(
          dom = 'Bfrtip',
          searching = FALSE,
          paging = FALSE, # Desabilita paginação para mostrar todas as linhas
          buttons = list(
            list(
              extend = "csv",
              text = "Salvar como CSV",
              exportOptions = list(
                modifier = list(page = "all") # Exporta todas as páginas
              ),
              customize = JS(
                "function(csv) {",
                "  var data = csv.replace(/\"/g, '');", # Remove aspas dos textos
                "  var blob = new Blob([data], { type: 'text/csv;charset=utf-8;' });",
                "  var url = URL.createObjectURL(blob);",
                "  var a = document.createElement('a');",
                "  a.href = url;",
                "  a.download = 'dados.csv';", # Nome do arquivo de saída
                "  document.body.appendChild(a);",
                "  a.click();",
                "  document.body.removeChild(a);",
                "  return false;",
                "}"
              )
            ),
            list(
              extend = "excel",
              text = "Salvar como XLSX",
              exportOptions = list(
                modifier = list(page = "all") # Exporta todas as páginas
              )
            )
          ),
          language = list(
            url = 'https://cdn.datatables.net/plug-ins/1.13.5/i18n/pt-BR.json'
          ),
          scrollX = TRUE, # Habilita rolagem horizontal
          autoWidth = TRUE, # Ajusta automaticamente a largura das colunas
          columnDefs = list(
            list(
              targets = "_all",
              render = JS(
                "function(data, type, row, meta) {",
                "  return type === 'export' ? data : '<div style=\"white-space: normal;\">' + data + '</div>';",
                "}"
              )
            )
          )
        ),
        rownames = FALSE, # Remove números das linhas
        class = "display compact"
      )
      
    })
    
    
    
    
    output$anova_tabela <- renderDT({
      req(resultados_processamento$status)

      # Filtrar os nomes das colunas que começam com "f"
      num_formantes <- input$num_formantes  # Número de formantes selecionados pelo usuário
      
      # Criar uma sequência de nomes de colunas do tipo f1, f2, ..., fn
      colunas_formantes <- paste0("f", 1:num_formantes)
      
      # Identificar colunas que não começam com "f"
      colunas_preservadas <- names(df_filtrado)[!grepl("^f", names(df_filtrado))]
      
      # Combinar as colunas preservadas com as colunas formantes
      colunas_selecionadas <- c(colunas_preservadas, colunas_formantes)
      
      # Filtrar o dataframe, mantendo apenas as colunas selecionadas que existem no dataframe
      colunas_existentes <- colunas_selecionadas[colunas_selecionadas %in% names(df_filtrado)]
      df_selecionado <- df_filtrado[, colunas_existentes]
      
      colunas_modelo <- c(colunas_formantes, "grupo", "vogal")
      df_modelo <- df_selecionado[complete.cases(df_selecionado[, colunas_modelo]), ]
      
      permanova_resultado <- tryCatch({
        adonis2(
          df_modelo[, colunas_formantes] ~ grupo + vogal + grupo * vogal,
          data = df_modelo,
          method = "euclidean",
          by = "terms",
          parallel = parallel::detectCores() - 1,
          permutations = input$num_permutacoes
        )
      }, error = function(e) {
        # Exibe notificação visível no caso de erro
        showNotification("Erro ao calcular PERMANOVA: verifique os dados e parâmetros.", type = "error")
        NULL
      })
      
      # Caso a execução falhe, exibir uma tabela vazia
      if (is.null(permanova_resultado)) {
        return(DT::datatable(data.frame(Mensagem = "Erro ao calcular PERMANOVA. 
                                                    Verifique se para todos os grupos 
                                                    tem repetições ou se as variâncias 
                                                    são diferentes de zero."), 
                             options = list(dom = "t"), 
                             class = "display compact"))
      }
      
      # Renderizar os resultados da PERMANOVA
      
      resultado_formatado <- as.data.frame(permanova_resultado)
      resultado_formatado[] <- lapply(resultado_formatado, function(coluna) {
        if (is.numeric(coluna)) {
          coluna <- formatC(coluna, width = 5, digits = 3, format = "fg", drop0trailing = FALSE)
        }
      })
      resultado_formatado[5,5] <- ""
      resultado_formatado[4,4] <- ""
      resultado_formatado[5,4] <- ""
      resultado_formatado[4,5] <- ""
      
      datatable(
        resultado_formatado,
        extensions = c("Buttons"),
        options = list(
          dom = 'Bfrtip',
          searching = FALSE,
          buttons = list(
            list(
              extend = "csv",
              text = "Salvar como CSV"
            ),
            list(
              extend = "excel",
              text = "Salvar como XLSX"
            )
          ),
          language = list(
            url = 'https://cdn.datatables.net/plug-ins/1.13.5/i18n/pt-BR.json'
          )
        ),
        class = "display nowrap compact"
      )
      
    })
    
    output$permdisp <- renderPlot({
      req(resultados_processamento$status)
      
      df_filtrado$grupo <- as.factor(df_filtrado$grupo)
      df_filtrado$vogal <- as.factor(df_filtrado$vogal)
      
      # Filtrar os nomes das colunas que começam com "f"
      num_formantes <- input$num_formantes  # Número de formantes selecionados pelo usuário
      
      # Criar uma sequência de nomes de colunas do tipo f1, f2, ..., fn
      colunas_formantes <- paste0("f", 1:num_formantes)
      
      # Identificar colunas que não começam com "f"
      colunas_preservadas <- names(df_filtrado)[!grepl("^f", names(df_filtrado))]
      
      # Combinar as colunas preservadas com as colunas formantes
      colunas_selecionadas <- c(colunas_preservadas, colunas_formantes)
      
      # Filtrar o dataframe, mantendo apenas as colunas selecionadas que existem no dataframe
      colunas_existentes <- colunas_selecionadas[colunas_selecionadas %in% names(df_filtrado)]
      df_selecionado <- df_filtrado[, colunas_existentes]
      
      colunas_modelo <- c(colunas_formantes, "grupo", "vogal")
      df_modelo <- df_selecionado[complete.cases(df_selecionado[, colunas_modelo]), ]
      
      # Colunas acústicas
      colunas_com_f <- names(df_modelo)[startsWith(names(df_modelo), "f")]
      distancias <- dist(df_modelo[, colunas_com_f], method = "euclidean")
      
      # Interação
      df_modelo$grupo_vogais <- interaction(df_modelo$grupo, df_modelo$vogal, sep = " - ")
      disp_interacao <- betadisper(distancias, df_modelo$grupo_vogais)
      
      grupos <- levels(disp_interacao$group)
      n_grupos <- length(grupos)
      
      # Cores alternadas: "cyan" e "tomato"
      cores <- rep(c("cyan", "tomato"), length.out = n_grupos)
      
      n_symbols <- ceiling(n_grupos / 2)
      base_symbols <- 15 + seq_len(n_symbols)
      
      simbolos <- sapply(
        seq_len(n_grupos),
        function(i) base_symbols[floor((i - 1)/2) + 1]
      )
      
      # Plot com cores diferentes para cada grupo
      plot(disp_interacao,
        label   = FALSE,
        main    = "",
        # ellipse = FALSE,
        hull    = TRUE,
        conf    = 0.95,
        col     = cores
      )
      
      # Adiciona a legenda dos grupos
      legend(
        "topleft",
        legend = grupos,
        col    = cores,
        pch    = simbolos,
        ncol = 2
      )
    })
    
    output$permdisp_res <- renderText({
      req(resultados_processamento$status)
      df_filtrado$grupo <- as.factor(df_filtrado$grupo)
      df_filtrado$vogal <- as.factor(df_filtrado$vogal)
      
      # Filtrar os nomes das colunas que começam com "f"
      colunas_com_f <- names(df_filtrado)[startsWith(names(df_filtrado), "f")]
      
      # Realiza a análise de dispersão multivariada (PERMDISP)
      distancias <- dist(df_filtrado[, colunas_com_f], method = "euclidean") # Calcula a matriz de distâncias
      
      df_filtrado$grupo_vogais <- interaction(df_filtrado$grupo, df_filtrado$vogal)
      disp_interacao <- betadisper(distancias, df_filtrado$grupo_vogais)
      resultado_interacao <- permutest(disp_interacao, permutations = input$num_permutacoes)
      
      linhas_saida <- capture.output(resultado_interacao)
      saida_formatada <- paste(linhas_saida, collapse = "\n")
      
      saida_formatada
    })
    
    output$hexvogais <- renderPlot({
      req(resultados_processamento$status)
      
      # Calcular as médias de f1 e f2 agrupadas por vogal e grupo
      df_medias <- df_filtrado %>%
        group_by(vogal, grupo) %>%
        summarise(f1_media = mean(f1, na.rm = TRUE),
                  f2_media = mean(f2, na.rm = TRUE))
      
      ggplot() +
        # Adicionar o hexbin plot, com ajuste do número de bins
        geom_hex(data = df_filtrado, aes(x = f2, y = f1), bins = 20, alpha = 0.4) +
        
        # Alterar a escala de cor do hexbin de branco para amarelo
        scale_fill_gradient(low = "white", high = "black") +  # Degradê
        
        # Adicionar os pontos das médias
        geom_point(data = df_medias, aes(x = f2_media, y = f1_media, color = grupo, shape = vogal), size = 4) +
        
        # Adicionar as linhas conectando os pontos dentro de cada grupo
        # geom_line(data = df_medias, aes(x = f2_media, y = f1_media, color = grupo, group = grupo, linetype = grupo), size = 1) +
        
        # Inverter os eixos
        scale_x_reverse() +
        scale_y_reverse() +
        
        # Adicionar rótulos e legendas
        labs(title = "", 
             x = "f2", 
             y = "f1", 
             color = "Grupo", 
             shape = "Vogal",
             linetype = "Grupo",
             fill = "Densidade") +  # Legenda de densidade para o hexbin plot
        
        # Estilo minimalista do gráfico
        theme_minimal() +
        
        # Ajustes de tema
        theme(
          plot.title = element_text(hjust = 0.5),  # Centralizar o título
          legend.position = "right"  # Colocar a legenda à direita
        )
    })
    
    output$boxplotvogaisf1 <- renderPlot({
      req(resultados_processamento$status)
      
      ggplot(df_filtrado, aes(x = vogal, y = f1, fill = grupo)) +
        geom_boxplot(alpha = 0.6, position = position_dodge(0.8)) +  # Boxplot para F1 com grupos lado a lado
        labs(title = "", x = "Vogal", y = "f1", fill = "Grupo") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))  # Centralizar o título
    })
    
    output$boxplotvogaisf2 <- renderPlot({
      req(resultados_processamento$status)
      
      ggplot(df_filtrado, aes(x = vogal, y = f2, fill = grupo)) +
        geom_boxplot(alpha = 0.6, position = position_dodge(0.8)) +  
        labs(title = "", x = "Vogal", y = "f2", fill = "Grupo") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))  # Centralizar o título
    })
    
    output$boxplotvogaisf3 <- renderPlot({
      req(resultados_processamento$status)
      req(input$num_formantes>=3)
      
      ggplot(df_filtrado, aes(x = vogal, y = f3, fill = grupo)) +
        geom_boxplot(alpha = 0.6, position = position_dodge(0.8)) +  
        labs(title = "", x = "Vogal", y = "f3", fill = "Grupo") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))  # Centralizar o título
    })
    
    output$boxplotvogaisf4 <- renderPlot({
      req(resultados_processamento$status)
      req(input$num_formantes>=4)
      
      ggplot(df_filtrado, aes(x = vogal, y = f4, fill = grupo)) +
        geom_boxplot(alpha = 0.6, position = position_dodge(0.8)) +  
        labs(title = "", x = "Vogal", y = "f4", fill = "Grupo") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))  # Centralizar o título
    })
    
    output$boxplotvogaisf5 <- renderPlot({
      req(resultados_processamento$status)
      req(input$num_formantes==5)
      
      ggplot(df_filtrado, aes(x = vogal, y = f5, fill = grupo)) +
        geom_boxplot(alpha = 0.6, position = position_dodge(0.8)) +  
        labs(title = "", x = "Vogal", y = "f5", fill = "Grupo") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))  # Centralizar o título
    })
    

    
    output$histf0 <- renderPlot({
      req(resultados_processamento$status)
      
      # Criando tibbles separados para cada grupo
      grupo1 <- tibble(value = pitch_combinado$Frequency[pitch_combinado$Origem == unique(pitch_combinado$Origem)[1]])
      grupo2 <- tibble(value = pitch_combinado$Frequency[pitch_combinado$Origem == unique(pitch_combinado$Origem)[2]])
      
      # Calculando bins ótimos
      bins_grupo1 <- opt_bin(grupo1, value)
      bins_grupo2 <- opt_bin(grupo2, value)
      
      # Usando a média do número de bins dos dois grupos
      n_bins_otimo <- round(mean(c(nrow(bins_grupo1), nrow(bins_grupo2))))
      
      # Calculando as médias
      media_grupo1 <- mean(grupo1$value, na.rm = TRUE)
      media_grupo2 <- mean(grupo2$value, na.rm = TRUE)
      
      # Criando densidades para calcular a área de interseção
      densidade_grupo1 <- density(grupo1$value, n = 512)
      densidade_grupo2 <- density(grupo2$value, n = 512)
      
      # Interpolação linear para calcular a sobreposição
      x_comum <- seq(min(densidade_grupo1$x, densidade_grupo2$x), 
                     max(densidade_grupo1$x, densidade_grupo2$x), length.out = 512)
      densidade1_interp <- approx(densidade_grupo1$x, densidade_grupo1$y, x_comum)$y
      densidade2_interp <- approx(densidade_grupo2$x, densidade_grupo2$y, x_comum)$y
      
      # Substituindo valores NA por 0 nas densidades interpoladas
      densidade1_interp[is.na(densidade1_interp)] <- 0
      densidade2_interp[is.na(densidade2_interp)] <- 0
      
      # Calculando a área de interseção
      area_intersecao <- sum(pmin(densidade1_interp, densidade2_interp)) * diff(x_comum[1:2])
      area_intersecao_percent <- area_intersecao * 100  # Convertendo para porcentagem
      
      
      # Criando o histograma com linhas de média e anotações
      ggplot(pitch_combinado, aes(x = Frequency, fill = Origem)) +
        geom_histogram(aes(y = ..density..), 
                       position = "identity",  # Manter sobreposição transparente
                       alpha = 0.5,            # Transparência para distinguir os grupos
                       bins = n_bins_otimo,
                       colour = "gray",    
                       linewidth = 0.2) +   
        scale_fill_manual(values = c("coral", "lightblue")) +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "",
             x = "Frequência",
             y = "Densidade") +
        # Adicionando as linhas pontilhadas para as médias
        geom_vline(xintercept = media_grupo1, linetype = "dotted", color = "coral", linewidth = 0.8) +
        geom_vline(xintercept = media_grupo2, linetype = "dotted", color = "lightblue", linewidth = 0.8) +
        # Adicionando os valores das médias como texto abaixo do eixo x
        annotate("text", x = media_grupo1, y = -0.0015, label = sprintf("%.1f", media_grupo1),
                 color = "coral", vjust = 1, hjust = 0.5, size = 5) +
        annotate("text", x = media_grupo2, y = -0.0005, label = sprintf("%.1f", media_grupo2),
                 color = "lightblue", vjust = 1, hjust = 0.5, size = 5) +
        # Adicionando o valor da área de interseção no gráfico
        annotate("text", x = mean(c(media_grupo1, media_grupo2)), y = 0.013, 
                 label = sprintf("Área de interseção: %.1f%%", area_intersecao_percent),
                 color = "black", vjust = 1, hjust = 0.5, size = 5, fontface = "bold") +
        theme_classic() +
        theme(panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
              panel.grid.minor = element_line(color = "gray95", linewidth = 0.1),
              panel.border = element_rect(color = "black", fill = NA),
              legend.position = c(0.95, 0.95),          # Posição da legenda (x,y)
              legend.justification = c(1, 1),           # Alinhamento da legenda
              legend.background = element_rect(fill = "white", color = "black"),  # Fundo e borda da legenda
              legend.margin = margin(5, 5, 5, 5))       # Margem interna da legenda
    })
    
    
    output$f0_ks <- renderText({
      # Capturar mensagens de erro ou sucesso
      resultado <- tryCatch({
        # Separando os dados por grupo
        grupo1 <- pitch_combinado$Frequency[pitch_combinado$Origem == levels(pitch_combinado$Origem)[1]]
        grupo2 <- pitch_combinado$Frequency[pitch_combinado$Origem == levels(pitch_combinado$Origem)[2]]
        
        # Realizando o teste KS
        resultado_ks <- ks.test(grupo1, grupo2)
        
        # Retorna a mensagem formatada
        paste("Teste de Kolmogorov-Smirnov para duas amostras:",
              "\nEstatística D =", round(resultado_ks$statistic, 4),
              "\nValor-p =", round(resultado_ks$p.value, digits = 4))
      }, error = function(e) {
        # Captura o erro e mostra uma notificação
        showNotification(paste("Erro ao executar o teste KS:", e$message), type = "error")
        
        # Mensagem de erro que será exibida no renderText
        "Erro ao executar o teste KS. Verifique os dados."
      })
      
      # Retorna o resultado, seja ele sucesso ou mensagem de erro
      resultado
    })
    
    # output$anova_pitch <- renderDT({
    #   req(resultados_processamento$status)
    #   
    #   # Realizar a PERMANOVA com maior número de permutações
    #   permanova_resultado <- adonis2(
    #     pitch_combinado$Frequency ~ pitch_combinado$Origem,
    #     data = pitch_combinado,
    #     method = "euclidean",
    #     by = "terms",
    #     parallel = 10,
    #     permutations = input$num_permutacoes
    #   )
    # 
    #   datatable(
    #     as.data.frame(permanova_resultado),
    #     extensions = c("Buttons"),
    #     options = list(
    #       dom = 'Bfrtip',
    #       searching = FALSE, # Desativa a funcionalidade de busca
    #       buttons = list(
    #         list(
    #           extend = "csv",
    #           text = "Salvar como CSV"
    #         ),
    #         list(
    #           extend = "excel",
    #           text = "Salvar como XLSX"
    #         )
    #       ),
    #       language = list(
    #         url = 'https://cdn.datatables.net/plug-ins/1.13.5/i18n/pt-BR.json' # Tradução para português
    #       )
    #     ),
    #     class = "display nowrap compact" # Mantém a tabela ajustada
    #   )
    # })
    
    output$anova_posthoc <- renderDT({
      req(resultados_processamento$status)

      # ANOVA não-paramétrica
      art_pitch <- art(Frequency ~ Origem, data = pitch_combinado)
      # anova(art_pitch)
      posthoc_art_pitch <- art.con(art_pitch, "Origem")
      # summary(posthoc_art_pitch)
      
      # Extraindo informações do teste post-hoc
      comparacoes <- data.frame(summary(posthoc_art_pitch))
      
      # Calculando as médias originais por grupo
      medias_originais <- aggregate(Frequency ~ Origem, data = pitch_combinado, mean)
      
      # Criando dataframe com as diferenças nas unidades originais
      df_comparacoes <- data.frame(
        Comparacao = paste(comparacoes$contrast),
        Diferenca = NA,
        p_valor = comparacoes$p.value
      )
      
      # Preenchendo as diferenças nas unidades originais
      for(i in 1:nrow(df_comparacoes)) {
        grupos <- strsplit(as.character(df_comparacoes$Comparacao[i]), " - ")[[1]]
        media1 <- medias_originais$Frequency[medias_originais$Origem == grupos[1]]
        media2 <- medias_originais$Frequency[medias_originais$Origem == grupos[2]]
        df_comparacoes$Diferenca[i] <- media1 - media2
      }
      
      # Arredondando valores e formatando p-valores
      df_comparacoes$Diferenca <- round(df_comparacoes$Diferenca, 2)
      df_comparacoes$p_valor <- format.pval(df_comparacoes$p_valor, digits = 3)

      datatable(
        df_comparacoes,
        extensions = c("Buttons"),
        options = list(
          dom = 'Bfrtip',
          searching = FALSE, # Desativa a funcionalidade de busca
          buttons = list(
            list(
              extend = "csv",
              text = "Salvar como CSV"
            ),
            list(
              extend = "excel",
              text = "Salvar como XLSX"
            )
          ),
          language = list(
            url = 'https://cdn.datatables.net/plug-ins/1.13.5/i18n/pt-BR.json' # Tradução para português
          )
        ),
        class = "display nowrap compact" # Mantém a tabela ajustada
      )
    })

  #
  # Verificar se necessário  
  #  
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
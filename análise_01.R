library(rPraat)
library(dplyr)
library(dygraphs)
library(ggplot2)
library(multimode)
library(biotools) 
library(vegan)
library(factoextra)
library(MVN)  # Pacote para testes de normalidade multivariada
library(tuneR)
library(seewave)
library(mvoutlier)
library(openxlsx)
library(robustbase)
library(boot)
library(ggplot2)
library(ARTool)
library(healthyR)

# ----------------------------------------------------------------------------

vogal_formantes <- function(vogal, textgrid_palavra_df, formantes_df) {
  # Obter o número de formantes detectados
  num_formantes <- dim(formantes_df)[2] - 1 # Excluindo a coluna de tempo
  
  # Inicializar o data frame para armazenar os formantes da vogal
  dados_formantes_vogal <- data.frame(tempo_absoluto = numeric(), instancia_vogal = integer())
  
  # Adicionar dinamicamente as colunas de formantes ao data frame de formantes da vogal
  for (i in 1:num_formantes) {
    col_name <- paste0("f", i)
    dados_formantes_vogal[[col_name]] <- numeric()
  }
  
  # Selecionar apenas os segmentos da vogal especificada
  textgrid_vogal <- textgrid_palavra_df[textgrid_palavra_df$label %in% vogal,]
  
  # Iterar sobre cada instância da vogal especificada e coletar os formantes
  for (i in 1:nrow(textgrid_vogal)) {
    # Obter os tempos de início e fim da vogal
    tempo_inicio <- textgrid_vogal$t1[i]
    tempo_fim <- textgrid_vogal$t2[i]
    
    # Selecionar os dados do data frame de formantes que estão entre tempo_inicio e tempo_fim
    formantes_selecionados <- formantes_df %>% filter(tempo >= tempo_inicio & tempo <= tempo_fim)
    
    # Ajustar o tempo absoluto e adicionar a instância da vogal
    formantes_selecionados <- formantes_selecionados %>%
      mutate(tempo_absoluto = tempo, instancia_vogal = i)
    
    # Acumular os valores no data frame final
    dados_formantes_vogal <- rbind(dados_formantes_vogal, formantes_selecionados)
  }
  
  # Retornar o dataframe final com os formantes da vogal especificada
  return(dados_formantes_vogal)
}

# ----------------------------------------------------------------------------

filtrar_outliers <- function(max_formantes, dados_formantes_total, nivel_confianca = 0.975) {
  # print(dados_formantes_total)
  
  # Selecionar apenas as colunas dos formantes para calcular a distância de Mahalanobis
  # formantes_data <- dados_formantes_total[, grep("^f", names(dados_formantes_total))]
  
  # Selecionar apenas as colunas dos formantes até o limite definido por max_formantes
  formantes_data <- dados_formantes_total[, grep(paste0("^f[1-", max_formantes, "]$"), 
                                                 names(dados_formantes_total))]
  
  # Detectar outliers multivariados usando a distância de Mahalanobis robusta
  result <- aq.plot(formantes_data)
  
  # Adicionar a coluna de outliers ao dataframe original
  dados_formantes_total$outlier <- result$outliers
  
  dados_sem_outliers <- dados_formantes_total[dados_formantes_total$outlier == FALSE, ]
  
  return(dados_sem_outliers)
}

# ----------------------------------------------------------------------------

calcular_medias_por_instancia <- function(dados_formantes_vogal) {
  print(dados_formantes_vogal)
  
  # Calcular a média dos formantes e do tempo para cada instância da vogal
  medias_por_instancia <- dados_formantes_vogal %>%
    group_by(instancia_vogal) %>%
    summarise(
      Media_Tempo = mean(tempo_absoluto, na.rm = TRUE),  # Média dos tempos absolutos para localização da vogal
      across(starts_with("f"), \(x) mean(x, na.rm = TRUE))  # Média dos formantes usando função anônima
    )
  
  # Retornar o dataframe final com as médias
  return(medias_por_instancia)
}

# ----------------------------------------------------------------------------

# Definir a função para gerar histogramas de formantes para cada vogal
gerar_histogramas_vogais <- function(dados_vogais_sem_outliers) {
  # Obter as vogais únicas, excluindo a string vazia
  vogais_unicas <- setdiff(unique(dados_vogais_sem_outliers$vogal), "")
  
  # Calcular o valor mínimo e máximo dos formantes para definir os limites do eixo X
  min_x <- min(c(dados_vogais_sem_outliers$f1, 
                 dados_vogais_sem_outliers$f2), na.rm = TRUE)
  
  max_x <- max(c(dados_vogais_sem_outliers$f1, 
                 dados_vogais_sem_outliers$f2), na.rm = TRUE)
  
  # Iterar sobre cada vogal e gerar um gráfico separado
  for (vogal in vogais_unicas) {
    # Filtrar os dados para a vogal atual
    dados_vogal <- dados_vogais_sem_outliers %>% filter(vogal == !!vogal)
    
    # Gerar o gráfico para a vogal atual
    p <- ggplot(dados_vogal) +
      geom_histogram(aes(x = f1), binwidth = 50, fill = "blue", color = "black", alpha = 0.5) +
      geom_histogram(aes(x = f2), binwidth = 50, fill = "red", color = "black", alpha = 0.5) +
      labs(title = paste("Histogramas dos Formantes para a Vogal", vogal), 
           x = "Valor do Formante", y = "Frequência") +
      theme_minimal() +
      xlim(min_x, max_x)  # Aplicar os mesmos limites no eixo X
    
    # Exibir o gráfico para a vogal atual
    print(p)
  }
}

# ----------------------------------------------------------------------------

# Função para realizar a PCA com base no dataframe e vogal fornecidos
realizar_pca_vogal <- function(df, vogal) {
  # Selecionar apenas as observações onde a vogal é a fornecida e as variáveis f1, f2, f3, e grupo
  dados_pca <- df[df$vogal == vogal, c("f1", "f2", "f3", "grupo")]
  
  # Verificar se há NA (dados faltantes) e removê-los
  dados_pca <- na.omit(dados_pca)
  
  # Separar os dados para PCA (apenas f1, f2, f3)
  dados_pca_valores <- dados_pca[, c("f1", "f2", "f3")]
  
  # Executar a PCA - escalando os dados (padronizando)
  pca_resultado <- prcomp(dados_pca_valores, scale. = TRUE)
  
  # Scree plot (gráfico das variâncias explicadas por cada componente)
  scree_plot <- fviz_eig(pca_resultado, addlabels = TRUE, ylim = c(0, 100))
  
  # Plotar variáveis (PCA biplot) para ver a contribuição das variáveis para os componentes
  var_plot <- fviz_pca_var(pca_resultado, col.var = "contrib", 
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                           repel = TRUE)
  
  # Garantir que o vetor grupo tenha o mesmo tamanho que os dados da PCA
  grupos <- dados_pca$grupo
  
  # Plotar indivíduos (observações) para ver a dispersão em relação aos componentes,
  # Colorir conforme o grupo
  ind_plot <- fviz_pca_ind(pca_resultado, 
                           geom.ind = "point", 
                           col.ind = grupos, # Colorir os indivíduos conforme o grupo
                           palette = "jco", 
                           addEllipses = TRUE, # Adicionar elipses de confiança ao redor dos grupos
                           legend.title = "Grupo")
  
  # Retornar os gráficos como uma lista
  return(list(
    scree_plot = scree_plot,
    var_plot = var_plot,
    ind_plot = ind_plot,
    pca_summary = summary(pca_resultado)
  ))
}

# ----------------------------------------------------------------------------

# Função para concatenar segmentos de áudio com base no DataFrame, áudio e filtro de label
concatenar_audio_com_filtro <- function(textgrid_palavra_df, audio, filtro_label) {
  
  # Filtrar o dataframe com base no texto da coluna 'label'
  textgrid_palavra_df_filtrado <- textgrid_palavra_df %>%
    filter(label == filtro_label)
  
  # Criar uma função para extrair os segmentos de áudio
  extrair_segmento_audio <- function(inicio, fim, audio) {
    # Converter o tempo de início e fim em amostras
    taxa_amostragem <- audio@samp.rate
    inicio_amostra <- as.integer(inicio * taxa_amostragem)
    fim_amostra <- as.integer(fim * taxa_amostragem)
    
    # Extrair o segmento do arquivo de áudio
    segmento <- extractWave(audio, from = inicio_amostra, to = fim_amostra, xunit = "samples")
    
    # Normalizar para 16-bit PCM, independentemente do formato original
    segmento <- normalize(segmento, unit = "16")
    
    return(segmento)
  }
  
  # Inicializar o áudio concatenado como 16 bits PCM com as mesmas características do áudio original
  if (audio@stereo) {
    # Se o áudio é estéreo
    audio_concatenado <- Wave(left = integer(0), right = integer(0), 
                              samp.rate = audio@samp.rate, bit = 16)
  } else {
    # Se o áudio é mono
    audio_concatenado <- Wave(left = integer(0), 
                              samp.rate = audio@samp.rate, bit = 16)
  }
  
  # Loop para concatenar os segmentos de áudio
  for (i in 1:nrow(textgrid_palavra_df_filtrado)) {
    # Extrair início e fim
    inicio <- textgrid_palavra_df_filtrado[i, "t1"]
    fim <- textgrid_palavra_df_filtrado[i, "t2"]
    
    # Extrair o segmento correspondente
    segmento <- extrair_segmento_audio(inicio, fim, audio)
    
    # Concatenar ao áudio final
    audio_concatenado <- bind(audio_concatenado, segmento)
  }
  
  # Retornar o áudio concatenado
  return(audio_concatenado)
}

# ----------------------------------------------------------------------------

processar_formantes <- function(max_formantes, formantes, textgrid) {
  # Criar dinamicamente um data frame a partir dos dados de formantes
  dados_formantes <- data.frame(tempo = formantes$t)
  
  # Adicionar colunas dinamicamente para cada formante disponível
  for (i in 1:formantes$maxnFormants) {
    col_name <- paste0("f", i)
    dados_formantes[[col_name]] <- formantes$frequencyArray[i, ]
  }
  
  dados_formantes[, c("f1", "f2", "f3", "f4", "f5")] <- t(apply(dados_formantes[, c("f1", "f2", "f3", "f4", "f5")], 1, function(x) {
    # Ordena os valores ignorando NAs
    sorted <- sort(x, na.last = TRUE)
    return(sorted)
  }))
  
  # Identifica dinamicamente as colunas a serem utilizadas e exclui as demais
  formantes_cols <- paste0("f", 1:max_formantes)
  dados_formantes <- dados_formantes[,c("tempo", formantes_cols)]
  
  # Converter a tier Palavra do TextGrid em um data frame
  textgrid_palavra_df <- as.data.frame(textgrid$Palavra)
  
  # Obter as vogais únicas do textgrid_palavra_df
  vogais_unicas <- unique(textgrid_palavra_df$label)
  vogais_unicas <- setdiff(vogais_unicas, "")  # Exclui a string vazia
  
  # Inicializar dataframes para armazenar os resultados
  dados_vogais_sem_outliers <- data.frame()
  medias_vogais_sem_outliers <- data.frame()
  
  # Loop para processar cada vogal
  for (vogal in vogais_unicas) {
    # Recorta formantes para a vogal atual
    dados_formantes_vogal <- vogal_formantes(vogal, textgrid_palavra_df, dados_formantes)
    
    # Remove outliers
    dados_sem_outliers <- filtrar_outliers(max_formantes,
                                           dados_formantes_vogal, 
                                           nivel_confianca = 0.975)
    
    # Calcula as médias dos formantes para cada instância da vogal
    medias_vogal <- calcular_medias_por_instancia(dados_sem_outliers)
    
    # Adicionar a vogal como uma nova coluna para identificar nos dataframes
    dados_sem_outliers <- dados_sem_outliers %>% mutate(vogal = vogal)
    medias_vogal <- medias_vogal %>% mutate(vogal = vogal)
    
    # Acumular os resultados nos dataframes finais
    dados_vogais_sem_outliers <- rbind(dados_vogais_sem_outliers, dados_sem_outliers)
    medias_vogais_sem_outliers <- rbind(medias_vogais_sem_outliers, medias_vogal)
  }
  
  # Retornar os dois data frames resultantes
  return(list(
    dados_vogais_sem_outliers = dados_vogais_sem_outliers,
    medias_vogais_sem_outliers = medias_vogais_sem_outliers
  ))
}

# ----------------------------------------------------------------------------

analisar_vogal <- function(textgrid, arquivo_audio, vogal, arquivo_saida = "Audio_concatenado.wav") {
  # Limpar TextGrid e carregar áudio
  textgrid_palavra_df <- as.data.frame(textgrid$Palavra) %>% filter(label != "")
  audio <- readWave(arquivo_audio)
  
  # Concatenar segmentos de áudio para a vogal especificada
  audio_concatenado <- concatenar_audio_com_filtro(textgrid_palavra_df, audio, vogal)
  
  # Salvar o áudio concatenado no arquivo especificado
  writeWave(audio_concatenado, arquivo_saida)
  message(paste("Áudio concatenado salvo em:", arquivo_saida))
  
  # Definir uma paleta de cores com mais níveis
  paleta_cores <- colorRampPalette(c("white", "black"))
  
  # Plotar o espectrograma com configurações otimizadas
  spectro(audio_concatenado, 
          f = audio_concatenado@samp.rate, 
          flim = c(0, 5), 
          ovlp = 90,       # Aumentando a sobreposição para 90%
          wl = 1024,       # Janela de análise maior para maior resolução
          scale = TRUE, 
          osc = TRUE, 
          tlab = "Tempo (s)",
          flab = "Frequência (kHz)",
          palette = paleta_cores,
          collevels = seq(-40, 0, length.out = 100),  # 100 níveis de cor
          grid = TRUE)     # Linhas de grade no gráfico
  
  # Retornar o áudio concatenado para uso posterior, se necessário
  return(audio_concatenado)
}

# ----------------------------------------------------------------------------

calcular_tempos <- function(textgrid, tier_num, texto) {
  # Verifica se o número do tier é válido
  if (tier_num < 1 || tier_num > tg.getNumberOfTiers(textgrid)) {
    stop("Número de tier inválido.")
  }
  
  # Verifica se o tier é do tipo intervalo
  if (!tg.isIntervalTier(textgrid, tier_num)) {
    stop("O tier especificado não é do tipo intervalo.")
  }
  
  # Inicializa o tempo total dos segmentos encontrados
  tempo_total_segmentos <- 0
  
  # Obtém o número de intervalos no tier
  num_intervals <- tg.getNumberOfIntervals(textgrid, tier_num)
  
  # Itera pelos intervalos do tier para buscar o texto
  for (i in seq_len(num_intervals)) {
    label <- tg.getLabel(textgrid, tier_num, i)
    if (label == texto) {
      inicio <- tg.getIntervalStartTime(textgrid, tier_num, i)
      fim <- tg.getIntervalEndTime(textgrid, tier_num, i)
      tempo_total_segmentos <- tempo_total_segmentos + (fim - inicio)
    }
  }
  
  # Obtém o tempo total do TextGrid
  tmin <- tg.getStartTime(textgrid)
  tmax <- tg.getEndTime(textgrid)
  tempo_total <- tmax - tmin
  
  # Calcula o percentual do tempo
  percentual <- (tempo_total_segmentos / tempo_total) * 100
  
  # Retorna os resultados
  list(
    tempo_total_segmentos = tempo_total_segmentos,
    tempo_total = tempo_total,
    percentual = percentual
  )
}

# ----------------------------------------------------------------------------

gerar_dataframe_textgrid <- function(textgrid) {
  
  # Obtém os nomes dos tiers de interesse
  tier_names <- sapply(1:tg.getNumberOfTiers(textgrid), function(i) tg.getTierName(textgrid, i))
  tier_anotacoes <- which(tier_names == "Anotações")
  tier_idios <- which(tier_names == "Idios")
  
  # Verifica se os tiers foram encontrados corretamente
  if (length(tier_anotacoes) != 1 || length(tier_idios) != 1) {
    stop("Erro: Um ou mais tiers não foram encontrados ou há múltiplos tiers com o mesmo nome.")
  }
  
  # Função para obter a descrição do tipo de anotação
  obter_anotacao_texto <- function(anotacao) {
    switch(anotacao,
           '1' = 'Abaixamento',
           '2' = 'Alçamento',
           '3' = 'Ditongação',
           '4' = 'Monotongação',
           '5' = 'Palatalização da oclusiva alveolar /t/',
           '6' = 'Palatalização da oclusiva alveolar /d/',
           '7' = 'Palatalização da fricativa alveolar /s/ em coda',
           '8' = 'Alongamento',
           '9' = 'Aspectos Prosódicos',
           '10' = 'Aférese',
           '11' = 'Apócope',
           '12' = 'Síncope',
           '13' = 'Sândi',
           '14' = 'Haplologia (inter ou intrapalavras)',
           '15' = 'Elisão silábica',
           '16' = 'Concordância',
           '17' = 'Marcador Discursivo',
           '18' = 'Pausa Preenchida',
           '19' = 'Desnasalização',
           '20' = 'Disfluência',
           '21' = 'Ênfase',
           '22' = 'Epêntese',
           '23' = 'Iotização',
           '24' = 'Metátese',
           '25' = 'Hipértese',
           '26' = 'Nome Próprio',
           '27' = 'Rotacismo',
           '28' = 'Lambdacismo',
           '29' = 'Velarização Fricativa',
           '30' = 'Rótico em ataque inicial',
           '31' = 'Rótico em ataque medial',
           '32' = 'Rótico em coda medial',
           '33' = 'Rótico em coda final',
           '34' = 'Variação Livre',
           '35' = 'Comentário',
           anotacao
    )
  }
  
  # Cria uma lista de dataframes para cada intervalo do tier "Anotações"
  dados <- lapply(1:tg.getNumberOfIntervals(textgrid, tier_anotacoes), function(i) {
    anotacao <- tg.getLabel(textgrid, tier_anotacoes, i)
    t_inicio <- tg.getIntervalStartTime(textgrid, tier_anotacoes, i)
    t_fim <- tg.getIntervalEndTime(textgrid, tier_anotacoes, i)
    
    # Obtém os valores correspondentes dos tiers "Idios" no mesmo intervalo de tempo
    idios <- tg.getLabel(textgrid, tier_idios, tg.getIntervalIndexAtTime(textgrid, tier_idios, (t_inicio + t_fim) / 2))
    
    # Separa anotações por vírgula, se houver mais de uma
    anotacoes_split <- strsplit(anotacao, ",")[[1]]
    anotacoes_split <- trimws(anotacoes_split) # Remove espaços em branco
    
    # Cria um dataframe para cada anotação
    lapply(anotacoes_split, function(anotacao_individual) {
      data.frame(Anotacao = obter_anotacao_texto(anotacao_individual), TempoInicio = t_inicio, TempoFim = t_fim, Idios = idios, stringsAsFactors = FALSE)
    })
  })
  
  # Converte a lista em um dataframe e realiza as operações de filtragem e ordenação
  df <- do.call(rbind, unlist(dados, recursive = FALSE))
  df <- df[df$Anotacao != "", ]
  df <- df[order(df$Anotacao), ]
  
  return(df)
}

# ----------------------------------------------------------------------------

# Combinar os dois dataframes de fl
combine_fl <- function(df1, df2) {
  # Agrupar e concatenar os valores únicos da coluna Idios no primeiro dataframe
  grouped_df1 <- df1 %>%
    group_by(Anotacao) %>%
    summarise(Idios1 = paste(unique(Idios), collapse = "; "))
  
  # Agrupar e concatenar os valores únicos da coluna Idios no segundo dataframe
  grouped_df2 <- df2 %>%
    group_by(Anotacao) %>%
    summarise(Idios2 = paste(unique(Idios), collapse = "; "))
  
  # Combinar os dois agrupamentos
  combinado <- full_join(grouped_df1, grouped_df2, by = "Anotacao")
  colnames(combinado) <- c("Variáveis linguísticas", "Questionado", "Padrão")
  
  # Ordenar a tabela pela coluna "Variáveis linguísticas"
  combinado <- combinado[order(combinado$`Variáveis linguísticas`), ]
  combinado[is.na(combinado)] <- ""
  
  return(combinado)
}

# ----------------------------------------------------------------------------

pitchtier_dataframe <- function(textgrid, arquivo, origem) {
  # Ler o arquivo de PitchTier
  pitchTier <- pt.read(arquivo, encoding = "auto")
  
  # Extrair tempos e frequências do PitchTier
  tempos <- pitchTier$t  # Vetor de tempos
  frequencias <- pitchTier$f  # Frequências correspondentes aos tempos
  
  # Criar DataFrame
  data <- data.frame(
    Tempo = tempos,
    Frequency = frequencias
  )
  
  # Adicionar a coluna de origem
  data$Origem <- origem
  
  # Converter a tier Vozes do TextGrid em um DataFrame
  textgrid_Vozes_df <- as.data.frame(textgrid$Vozes)
  textgrid_Vozes_df <- textgrid_Vozes_df[textgrid_Vozes_df$label != "", ] # Exclui as strings vazias
  
  # Filtrar pelo intervalo de tempo definido no TextGrid
  condicao <- sapply(data$Tempo, function(tempo) {
    any(tempo >= textgrid_Vozes_df$t1 & tempo <= textgrid_Vozes_df$t2)
  })
  data_filtrado <- data[condicao, ]
  
  # Detectar e remover outliers univariados na coluna Frequency
  # Utiliza a função adjboxStats (adjusted boxplot) do pacote robustbase
  boxplot_stats <- adjboxStats(data_filtrado$Frequency)
  outlier_limites <- boxplot_stats$fence # Limites inferior e superior
  data_sem_outliers <- data_filtrado[
    data_filtrado$Frequency >= outlier_limites[1] & 
      data_filtrado$Frequency <= outlier_limites[2], 
  ]
  
  return(data_sem_outliers)
}

# ----------------------------------------------------------------------------

# Função para converter PitchTier em DataFrame e filtrar por presença ou ausência de um valor específico em uma tier
pitchtier_dataframe <- function(textgrid, arquivo, origem, tier_nome, valor_tier = "", filtrar_presenca = FALSE) {
  # Ler o arquivo de PitchTier
  pitchTier <- pt.read(arquivo, encoding = "auto")
  
  # Extrair tempos e frequências do PitchTier
  tempos <- pitchTier$t  # Vetor de tempos
  frequencias <- pitchTier$f  # Frequências correspondentes aos tempos
  
  # Criar DataFrame inicial
  data <- data.frame(
    Tempo = tempos,
    Frequency = frequencias,
    Origem = origem
  )
  
  # Verificar se a tier especificada existe no TextGrid
  tier_index <- tryCatch({
    tg.checkTierInd(textgrid, tier_nome)
  }, error = function(e) {
    stop(paste("A tier", tier_nome, "não existe no TextGrid fornecido."))
  })
  
  # Extrair a tier especificada
  tier <- textgrid[[tier_index]]
  
  # Converter a tier em um data frame
  if (tier$type == "interval") {
    tier_df <- data.frame(
      t1 = tier$t1,
      t2 = tier$t2,
      label = tier$label
    )
  } else if (tier$type == "point") {
    tier_df <- data.frame(
      t = tier$t,
      label = tier$label
    )
  } else {
    stop("Tipo de tier não suportado.")
  }
  
  # Filtrar o data frame da tier pelo valor especificado
  tier_df_filtrado <- subset(tier_df, label == valor_tier)
  
  # Filtrar os dados principais com base nos tempos da tier filtrada
  if (tier$type == "interval") {
    condicao <- sapply(data$Tempo, function(tempo) {
      any(tempo >= tier_df_filtrado$t1 & tempo <= tier_df_filtrado$t2)
    })
  } else if (tier$type == "point") {
    condicao <- data$Tempo %in% tier_df_filtrado$t
  }
  
  # Ajustar a condição com base no parâmetro filtrar_presenca
  if (!filtrar_presenca) {
    condicao <- !condicao
  }
  
  data_filtrado <- data[condicao, ]
  
  # Detectar e remover outliers univariados na coluna Frequency
  # Utiliza a função adjboxStats (adjusted boxplot) do pacote robustbase
  boxplot_stats <- adjboxStats(data_filtrado$Frequency)
  outlier_limites <- boxplot_stats$fence # Limites inferior e superior
  data_sem_outliers <- data_filtrado[
    data_filtrado$Frequency >= outlier_limites[1] & 
      data_filtrado$Frequency <= outlier_limites[2], 
  ]
  
  return(data_sem_outliers)
}

# ----------------------------------------------------------------------------
# Principal
# ----------------------------------------------------------------------------

# Diretório de trabalho e leitura dos arquivos
setwd("D:/Meu Drive/Em processamento/Protocolo 044962.2024 Requisição Digital/Em processamento")

# Ler arquivo TextGrid e arquivo de formantes do Questionado
textgrid <- tg.read("Audio_da_confissão.TextGrid", encoding = "auto")

formantes <- formant.read("Audio_da_confissão.Formant", encoding = "auto")
formantes <- formant.toArray(formantes)

# Formantes
num_formantes <- formantes$maxnFormants
resultado <- processar_formantes(2, formantes, textgrid)

dados_vogais_sem_outliers <- resultado$dados_vogais_sem_outliers
medias_vogais_sem_outliers <- resultado$medias_vogais_sem_outliers

gerar_histogramas_vogais(dados_vogais_sem_outliers)
gerar_histogramas_vogais(medias_vogais_sem_outliers)

# Ler arquivo TextGrid e arquivo de formantes do Padrão
textgrid_padrao <- tg.read("padrao.TextGrid", encoding = "auto")
formantes_padrao <- formant.read("padrao.Formant", encoding = "auto")
formantes_padrao <- formant.toArray(formantes_padrao)

num_formantes_padrao <- formantes_padrao$maxnFormants
resultado_padrao <- processar_formantes(2, formantes_padrao, textgrid_padrao)

dados_vogais_sem_outliers_padrao <- resultado_padrao$dados_vogais_sem_outliers
medias_vogais_sem_outliers_padrao <- resultado_padrao$medias_vogais_sem_outliers

gerar_histogramas_vogais(dados_vogais_sem_outliers_padrao)
gerar_histogramas_vogais(medias_vogais_sem_outliers_padrao)

# Fala exclusiva
fala_liquida_questionado <- calcular_tempos(textgrid, 1, "VM1.1")
print(fala_liquida_questionado)

fala_liquida_padrao <- calcular_tempos(textgrid_padrao, 1, "VM.1")
print(fala_liquida_padrao)

# Cria planilhas para as ocorrência de fenômenos linguisticos
fl_questionado <- gerar_dataframe_textgrid(textgrid)
write.xlsx(fl_questionado, "fl_questionado.xlsx", rownames = TRUE)
write.csv(fl_questionado, "fl_questionado.csv", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")

fl_padrao <- gerar_dataframe_textgrid(textgrid_padrao)
write.xlsx(fl_padrao, "fl_padrao.xlsx", rownames = TRUE)
write.csv(fl_padrao, "fl_padrao.csv", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")

tabela_fl <- combine_fl(fl_questionado, fl_padrao)
write.xlsx(tabela_fl, "tabela_fl.xlsx", rownames = TRUE)
write.csv(tabela_fl, "tabela_fl.csv", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")

# Adicionar uma coluna de grupo para diferenciar os dataframes e combiná-los
dados_vogais_sem_outliers_padrao <- dados_vogais_sem_outliers_padrao %>% mutate(grupo = "padrão")
dados_vogais_sem_outliers <- dados_vogais_sem_outliers %>% mutate(grupo = "questionado")

medias_vogais_sem_outliers_padrao <- medias_vogais_sem_outliers_padrao %>% mutate(grupo = "padrão")
medias_vogais_sem_outliers <- medias_vogais_sem_outliers %>% mutate(grupo = "questionado")

#
# Combina os arquivos
#

df_combinado <- rbind(medias_vogais_sem_outliers_padrao, medias_vogais_sem_outliers)
# df_combinado <- rbind(dados_vogais_sem_outliers_padrao, dados_vogais_sem_outliers)

# Filtrar as linhas que contêm apenas as vogais "a" ou "e"
df_filtrado <- df_combinado[df_combinado$vogal %in% c("a", "e", "eh"), ]
df_filtrado <- df_combinado

# # Ajustar o modelo MANOVA
# modelo_manova <- manova(cbind(f1, f2) ~ grupo * vogal, data = df_filtrado)
# 
# # Resumo do modelo
# summary(modelo_manova)

# Realizar a PERMANOVA com maior número de permutações
permanova_resultado <- adonis2(
  df_filtrado[, c("f1", "f2")] ~ grupo + vogal + grupo*vogal,
  data = df_filtrado,
  method = "euclidean",
  by = "terms",
  parallel = 10,
  permutations = 999 # Número de permutações
)

# Calcular as médias de f1 e f2 agrupadas por vogal e grupo
df_medias <- df_filtrado %>%
  group_by(vogal, grupo) %>%
  summarise(f1_media = mean(f1, na.rm = TRUE),
            f2_media = mean(f2, na.rm = TRUE))

# Criar o gráfico de dispersão com as médias, o eixo Y invertido e as linhas conectando os grupos
ggplot(df_medias, aes(x = f2_media, y = f1_media, color = grupo, shape = vogal, group = grupo)) +
  geom_point(size = 4) +  # Tamanho dos pontos
  geom_line(aes(linetype = grupo), size = 1) +  # Adicionar as linhas conectando os pontos dentro de cada grupo
  scale_y_reverse() +  # Inverter o eixo Y
  labs(title = "Dispersão de médias de F1 vs F2 por grupo e vogal", 
       x = "F2", 
       y = "F1", 
       color = "Grupo", 
       shape = "Vogal",
       linetype = "Grupo") +  # Legenda das linhas também de acordo com o grupo
  theme_minimal() +  # Estilo minimalista do gráfico
  theme(
    plot.title = element_text(hjust = 0.5),  # Centralizar o título
    legend.position = "right"  # Colocar a legenda à direita
  )

# Calcular as médias de f1 e f2 agrupadas por vogal e grupo
df_medias <- df_filtrado %>%
  group_by(vogal, grupo) %>%
  summarise(f1_media = mean(f1, na.rm = TRUE),
            f2_media = mean(f2, na.rm = TRUE))

# Criar o gráfico combinando hexbin, pontos e linhas
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
  labs(title = "Dispersão de médias de F1 vs F2 com densidade de pontos", 
       x = "F2", 
       y = "F1", 
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

# Boxplot para F1 e F2 lado a lado para cada vogal
ggplot(df_filtrado, aes(x = vogal, y = f1, fill = grupo)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(0.8)) +  # Boxplot para F1 com grupos lado a lado
  labs(title = "Boxplots de F1 por vogal e grupo", x = "Vogal", y = "F1", fill = "Grupo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centralizar o título

# Boxplot de F2 com grupos lado a lado para cada vogal
ggplot(df_filtrado, aes(x = vogal, y = f2, fill = grupo)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(0.8)) +  # Boxplot para F2 com grupos lado a lado
  labs(title = "Boxplots de F2 por vogal e grupo", x = "Vogal", y = "F2", fill = "Grupo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centralizar o título

# # Análise de seleção concatenada
# # Limpar TextGrid e carregar áudio
# textgrid_palavra_df <- as.data.frame(textgrid$Palavra) %>% filter(label != "")
# audio <- readWave("Audio_da_confissão_mono.wav")
# 
# # Chamar a função com filtro para a vogal "a"
# audio_concatenado <- concatenar_audio_com_filtro(textgrid_palavra_df, audio, "a")
# 
# # Tocar o áudio concatenado (opcional)
# # play(audio_concatenado)
# 
# # Salvar o áudio concatenado (opcional)
# writeWave(audio_concatenado, "Audio_concatenado.wav")
# 
# # Plotar o oscilograma (forma de onda)
# oscillo(audio_concatenado, f = audio_concatenado@samp.rate)
# 
# 
# # Definindo uma paleta de cores com mais níveis de cores
# paleta_cores <- colorRampPalette(c("white",
#                                     "black"))
# 
# # Aumentando a janela de análise (wl) e sobreposição (ovlp) para melhorar a resolução
# spectro(audio_concatenado,
#         f = audio_concatenado@samp.rate,
#         flim = c(0, 5),
#         ovlp = 90,  # Aumentando a sobreposição para 90%
#         wl = 1024,  # Aumentando a janela de análise para mais resolução
#         scale = TRUE,
#         osc = TRUE,
#         tlab = "Tempo (s)",
#         flab = "Frequência (kHz)",
#         palette = paleta_cores,
#         collevels = seq(-40, 0, length.out = 100),  # 100 níveis de cor
#         grid = TRUE)  # Adiciona linhas de grade ao gráfico
# 
# 
# # Chamar a função para a vogal "a"
# audio_concatenado <- analisar_vogal(
#    textgrid = textgrid,
#    arquivo_audio = "Audio_da_confissão_mono.wav",
#    vogal = "a",
#    arquivo_saida = "Audio_concatenado_a.wav"
# )

# ----------------------------------------------------------------------------
# Carregar e combinar arquivos de Pitch usando pitch.toFrame
# ----------------------------------------------------------------------------

# Função para converter PitchTier para DataFrame convencional
# Instalar o pacote robustbase (caso não esteja instalado)
if (!requireNamespace("robustbase", quietly = TRUE)) {
  install.packages("robustbase")
}

# Função atualizada para converter PitchTier e filtrar outliers
options(mc_doScale_quiet = TRUE)

# Processar os arquivos de Pitch
pitch_padrao <- pitchtier_dataframe(textgrid_padrao, "padrao.PitchTier", "Padrão", "Vozes")
pitch_questionado <- pitchtier_dataframe(textgrid,"Audio_da_confissão.PitchTier", "Questionado", "Vozes")

hist(pitch_questionado$Frequency)
hist(pitch_padrao$Frequency)

pitch_combinado <- rbind(pitch_padrao, pitch_questionado)

# Criando tibbles separados para cada grupo
grupo1 <- tibble(value = pitch_combinado$Frequency[pitch_combinado$Origem == unique(pitch_combinado$Origem)[1]])
grupo2 <- tibble(value = pitch_combinado$Frequency[pitch_combinado$Origem == unique(pitch_combinado$Origem)[2]])

# Calculando bins ótimos
bins_grupo1 <- opt_bin(grupo1, value)
bins_grupo2 <- opt_bin(grupo2, value)

# Usando a média do número de bins dos dois grupos
n_bins_otimo <- mean(c(nrow(bins_grupo1), nrow(bins_grupo2)))

# Criando o histograma com legenda interna
ggplot(pitch_combinado, aes(x = Frequency, fill = Origem)) +
  geom_histogram(aes(y = ..density..), 
                 position = "identity", 
                 alpha = 0.5,
                 bins = n_bins_otimo,
                 colour = "gray",    
                 linewidth = 0.2) +   
  scale_fill_manual(values = c("coral", "lightblue")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Histograma da Frequência por Origem",
       x = "Frequência",
       y = "Densidade") +
  theme_classic() +
  theme(panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
        panel.grid.minor = element_line(color = "gray95", linewidth = 0.1),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = c(0.95, 0.95),          # Posição da legenda (x,y)
        legend.justification = c(1, 1),           # Alinhamento da legenda
        legend.background = element_rect(fill = "white", color = "black"),  # Fundo e borda da legenda
        legend.margin = margin(5, 5, 5, 5))       # Margem interna da legenda

# Realizar a PERMANOVA com maior número de permutações
permanova_resultado <- adonis2(
  pitch_combinado$Frequency ~ pitch_combinado$Origem,
  data = pitch_combinado,
  method = "euclidean",
  by = "terms",
  parallel = 10,
  permutations = 999 # Número de permutações
)

# Exibir o resultado detalhado
print(permanova_resultado)

# Convertendo os fatores para 'factor'
pitch_combinado$Origem <- as.factor(pitch_combinado$Origem)

# ANOVA não-paramétrica
art_pitch <- art(Frequency ~ Origem, data = pitch_combinado)
anova(art_pitch)
posthoc_art_pitch <- art.con(art_pitch, "Origem")
summary(posthoc_art_pitch)

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

# Ordenando por p-valor
df_comparacoes <- df_comparacoes[order(df_comparacoes$p_valor), ]
print(df_comparacoes)

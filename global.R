library(ARTool)
library(bslib)
library(dplyr)
library(DT)
library(ggplot2)
library(healthyR)
library(mvoutlier)
library(robustbase)
library(rPraat)
library(shiny)
library(shinycssloaders)
library(shinyFiles)
library(shinyjs)
library(vegan)


theme <- bs_theme(
  bootswatch = "cerulean",
  base_font = font_google("Roboto"),
  heading_font = font_google("Poppins"),
  primary = "#2c3e50",
  secondary = "#adb5bd",
  bg = "#f8f9fa",
  fg = "#2c3e50"
)

vogais <- c("a", "e", "eh", "i", "o", "oh", "u")

options(shiny.maxRequestSize = 500*1024^2)
options(mc_doScale_quiet = TRUE)

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

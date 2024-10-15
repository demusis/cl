library(rPraat)
library(dplyr)
library(dygraphs)
library(ggplot2)
library(multimode)
library(biotools) 
library(vegan)
library(factoextra)
library(MVN)  # Pacote para testes de normalidade multivariada

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

filtrar_outliers_mahalanobis <- function(dados_formantes_total, nivel_confianca = 0.975) {
  print(dados_formantes_total)
  
  # Selecionar apenas as colunas dos formantes para calcular a distância de Mahalanobis
  formantes_data <- dados_formantes_total[, grep("^f", names(dados_formantes_total))]
  
  # Calcular a matriz de covariância dos formantes
  cov_matrix <- cov(formantes_data, use = "complete.obs")
  
  # Calcular a distância de Mahalanobis para cada linha do data frame
  mahalanobis_dist <- mahalanobis(formantes_data, colMeans(formantes_data, na.rm = TRUE), cov_matrix)
  
  # Definir o limite para a distância de Mahalanobis com base no nível de confiança e graus de liberdade
  limite <- qchisq(nivel_confianca, df = num_formantes)
  
  # Adicionar uma coluna para indicar se a linha é um outlier
  dados_formantes_total$outlier <- mahalanobis_dist > limite
  
  # Filtrar os dados removendo os outliers
  dados_sem_outliers_total <- dados_formantes_total %>% filter(outlier == FALSE)
  
  # Retornar o dataframe sem outliers
  return(dados_sem_outliers_total)
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
                 dados_vogais_sem_outliers$f2, 
                 dados_vogais_sem_outliers$f3, 
                 dados_vogais_sem_outliers$f4, 
                 dados_vogais_sem_outliers$f5), na.rm = TRUE)
  
  max_x <- max(c(dados_vogais_sem_outliers$f1, 
                 dados_vogais_sem_outliers$f2, 
                 dados_vogais_sem_outliers$f3, 
                 dados_vogais_sem_outliers$f4, 
                 dados_vogais_sem_outliers$f5), na.rm = TRUE)
  
  # Iterar sobre cada vogal e gerar um gráfico separado
  for (vogal in vogais_unicas) {
    # Filtrar os dados para a vogal atual
    dados_vogal <- dados_vogais_sem_outliers %>% filter(vogal == !!vogal)
    
    # Gerar o gráfico para a vogal atual
    p <- ggplot(dados_vogal) +
      geom_histogram(aes(x = f1), binwidth = 50, fill = "blue", color = "black", alpha = 0.5) +
      geom_histogram(aes(x = f2), binwidth = 50, fill = "red", color = "black", alpha = 0.5) +
      geom_histogram(aes(x = f3), binwidth = 50, fill = "green", color = "black", alpha = 0.5) +
      geom_histogram(aes(x = f4), binwidth = 50, fill = "gray", color = "black", alpha = 0.5) +
      geom_histogram(aes(x = f5), binwidth = 50, fill = "orange", color = "black", alpha = 0.5) +
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
# Principal
# ----------------------------------------------------------------------------

# Diretório de trabalho e leitura dos arquivos
setwd("~/Parsel")

# Ler arquivo TextGrid e arquivo de formantes
textgrid <- tg.read("Audio_da_confissão.TextGrid")
formantes <- formant.read("Audio_da_confissão.Formant")
formantes <- formant.toArray(formantes)

# Obter o número de formantes detectados
num_formantes <- dim(formantes$frequencyArray)[1]

# Criar dinamicamente um data frame a partir dos dados de formantes
dados_formantes <- data.frame(tempo = formantes$t)

# Adicionar colunas dinamicamente para cada formante disponível
for (i in 1:num_formantes) {
  col_name <- paste0("f", i)
  dados_formantes[[col_name]] <- formantes$frequencyArray[i, ]
}

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
  
  # Remove outliers usando a distância de Mahalanobis
  dados_sem_outliers <- filtrar_outliers_mahalanobis(dados_formantes_vogal, 
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

gerar_histogramas_vogais(dados_vogais_sem_outliers)

# Definir a fração (metade aproximadamente)
frac <- 0.5

# Gerar o primeiro dataframe com metade dos dados de forma aleatória, sem reposição
df1 <- dados_vogais_sem_outliers %>% sample_frac(frac)
df1_mv <- medias_vogais_sem_outliers %>% sample_frac(frac)

# Gerar o segundo dataframe com os dados restantes (aqueles que não foram selecionados no primeiro)
df2 <- dados_vogais_sem_outliers %>% anti_join(df1)
df2_mv <- medias_vogais_sem_outliers %>% anti_join(df1_mv)

# Verificar o tamanho dos dataframes resultantes
nrow(df1)  # Tamanho do primeiro dataframe
nrow(df2)  # Tamanho do segundo dataframe


# Adicionar uma coluna de grupo para diferenciar os dataframes
df1 <- df1 %>% mutate(grupo = "padrão")
df2 <- df2 %>% mutate(grupo = "questionado")

df1_mv <- df1_mv %>% mutate(grupo = "padrão")
df2_mv <- df2_mv %>% mutate(grupo = "questionado")

df_combinado <- rbind(df1, df2)
df_combinado_mv <- rbind(df1_mv, df2_mv)

# Realizar a MANOVA usando as variáveis Media_F1, Media_F2 e Media_F3
manova_resultado <- manova(cbind(f1, f2, f3) ~ grupo + vogal + grupo*vogal, data = df_combinado)

# Mostrar a tabela de testes estatísticos, como o teste de Wilks
summary(manova_resultado, test = "Wilks")

# Teste de Homogeneidade de Variâncias - Box's M
# Preparar os dados para o teste de Box
dados_box <- df_combinado[, c("f1", "f2", "f3", "grupo")]

# Teste de Box para homogeneidade de covariâncias
box_test <- boxM(df_combinado[, c("f1", "f2", "f3")], df_combinado$grupo)

# Exibir o resultado do teste de Box
print("Teste de Homogeneidade de Variâncias - Box's M:")
print(box_test)

# Extração dos resíduos da MANOVA
residuos <- residuals(manova_resultado)

# Teste de Aderência à Normalidade Multivariada
# Aplicar o teste de normalidade multivariada (Henze-Zirkler)
normalidade_teste <- mvn(residuos, multivariatePlot = "qq", mvnTest = "hz")

# Exibir o resultado do teste de Henze-Zirkler
print("Teste de Normalidade Multivariada - Henze-Zirkler:")
print(normalidade_teste)

# Realizar a MANOVA não paramétrica (PERMANOVA)
# Utilizando a distância Euclidiana para calcular a dissimilaridade
permanova_resultado <- adonis2(cbind(df_combinado$f1, df_combinado$f2, df_combinado$f3) ~ grupo, 
                               data = df_combinado, method = "euclidean", permutations = 999)
# Exibir o resultado da PERMANOVA
print("Resultado da MANOVA não paramétrica (PERMANOVA):")
print(permanova_resultado)

# PCA
resultado_pca <- realizar_pca_vogal(df_combinado, "a")

resultado_pca$scree_plot
resultado_pca$var_plot  
resultado_pca$ind_plot  

# Calcular as médias de f1 e f2 agrupadas por vogal e grupo
df_medias <- df_combinado %>%
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
df_medias <- df_combinado %>%
  group_by(vogal, grupo) %>%
  summarise(f1_media = mean(f1, na.rm = TRUE),
            f2_media = mean(f2, na.rm = TRUE))

# Criar o gráfico combinando hexbin, pontos e linhas
ggplot() +
  # Adicionar o hexbin plot, com ajuste do número de bins
  geom_hex(data = df_combinado, aes(x = f2, y = f1), bins = 20, alpha = 0.4) +
  
  # Alterar a escala de cor do hexbin de branco para amarelo
  scale_fill_gradient(low = "white", high = "black") +  # Degradê
  
  # Adicionar os pontos das médias
  geom_point(data = df_medias, aes(x = f2_media, y = f1_media, color = grupo, shape = vogal), size = 4) +
  
  # Adicionar as linhas conectando os pontos dentro de cada grupo
  geom_line(data = df_medias, aes(x = f2_media, y = f1_media, color = grupo, group = grupo, linetype = grupo), size = 1) +
  
  # Inverter o eixo Y
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


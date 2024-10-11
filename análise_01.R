library(rPraat)
library(dplyr)
library(dygraphs)
library(ggplot2)
library(multimode)

# Diretório de trabalho e leitura dos arquivos
setwd("~/Parsel")

# Ler arquivo TextGrid e arquivo de formantes
textgrid <- tg.read("ac.TextGrid")
formantes <- formant.read("ac.Formant")
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

# Selecionar apenas os segmentos da vogal "a"
vogais <- c("a")
textgrid_a <- textgrid_palavra_df[textgrid_palavra_df$label %in% vogais,]

# Inicializar o data frame para armazenar os formantes da vogal "a"
dados_formantes_vogal_a <- data.frame(tempo_absoluto = numeric(), instancia_vogal = integer())

# Adicionar dinamicamente as colunas de formantes ao data frame de formantes da vogal "a"
for (i in 1:num_formantes) {
  col_name <- paste0("f", i)
  dados_formantes_vogal_a[[col_name]] <- numeric()
}

# Iterar sobre cada instância da vogal "a" e coletar os formantes
for (i in 1:nrow(textgrid_a)) {
  # Obter os tempos de início e fim da vogal "a"
  tempo_inicio <- textgrid_a$t1[i]
  tempo_fim <- textgrid_a$t2[i]
  
  # Selecionar os dados do data frame de formantes que estão entre tempo_inicio e tempo_fim
  formantes_selecionados <- dados_formantes %>% filter(tempo >= tempo_inicio & tempo <= tempo_fim)
  
  # Ajustar o tempo absoluto e adicionar a instância da vogal
  formantes_selecionados <- formantes_selecionados %>%
    mutate(tempo_absoluto = tempo, instancia_vogal = i)
  
  # Acumular os valores no data frame final
  dados_formantes_vogal_a <- rbind(dados_formantes_vogal_a, formantes_selecionados)
}

# Calcular a média dos formantes e do tempo para cada instância da vogal "a"
medias_por_instancia <- dados_formantes_vogal_a %>%
  group_by(instancia_vogal) %>%
  summarise(
    Media_Tempo = mean(tempo_absoluto, na.rm = TRUE),  # Média dos tempos absolutos para localização da vogal
    across(starts_with("f"), \(x) mean(x, na.rm = TRUE))  # Média dos formantes usando função anônima
  )

# Renomear colunas para tornar a tabela mais compreensível
colnames(medias_por_instancia) <- c("Instancia_Vogal", "Media_Tempo", paste0("Media_F", 1:num_formantes))

# Mostrar a tabela de médias por instância
print(medias_por_instancia)

# Detecção de outliers usando a Distância de Mahalanobis
# Selecionar apenas as colunas dos formantes para calcular a distância
formantes_data <- dados_formantes_vogal_a %>% select(starts_with("f"))

# Calcular a matriz de covariância dos formantes
cov_matrix <- cov(formantes_data, use = "complete.obs")

# Calcular a distância de Mahalanobis para cada linha do data frame
mahalanobis_dist <- mahalanobis(formantes_data, colMeans(formantes_data, na.rm = TRUE), cov_matrix)

# Definir um limite para a distância de Mahalanobis para determinar outliers
limite <- qchisq(0.975, df = num_formantes)  # 97.5% da distribuição qui-quadrado com 'num_formantes' graus de liberdade

# Adicionar uma coluna para indicar se a linha é um outlier
dados_formantes_vogal_a$outlier <- mahalanobis_dist > limite

# Filtrar os dados removendo os outliers
dados_sem_outliers <- dados_formantes_vogal_a %>% filter(outlier == FALSE)

# Agrupar por 'instancia_vogal' e calcular a média dos formantes para cada instância da vogal
medias_vogais <- dados_sem_outliers %>%
  group_by(instancia_vogal) %>%
  summarise(
    Media_Tempo = mean(tempo_absoluto, na.rm = TRUE),  # Média do tempo absoluto
    across(starts_with("f"), mean, na.rm = TRUE)        # Calcular a média de todos os formantes
  )

# Renomear colunas para indicar que são médias
colnames(medias_vogais) <- c("Instancia_Vogal", "Media_Tempo", paste0("Media_F", 1:num_formantes))

# Gerar histogramas para cada formante (geral)
for (i in 1:num_formantes) {
  col_name <- paste0("f", i)
  
  # Criar o histograma para o formante atual
  p <- ggplot(dados_sem_outliers, aes_string(x = col_name)) +
    geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histograma do Formante", i), x = paste("Formante", i), y = "Frequência") +
    theme_minimal()
  
  # Mostrar o histograma
  print(p)
}

# Teste de multimodalidade para cada formante
for (i in 1:num_formantes) {
  col_name <- paste0("f", i)
  formante_values <- dados_sem_outliers[[col_name]]
  
  # Realizar o teste de multimodalidade usando a função "modetest" do pacote multimode
  mode_test <- modetest(formante_values, mod0 = 1) # Teste para verificar se há mais de uma moda
  
  # Imprimir o resultado do teste
  print(paste("Teste de multimodalidade para o Formante", i, ":"))
  print(mode_test)
}

# Gerar histogramas compartilhando o mesmo eixo X
p <- ggplot(dados_sem_outliers) +
  geom_histogram(aes(x = f1), binwidth = 50, fill = "blue", color = "black", alpha = 0.5) +
  geom_histogram(aes(x = f2), binwidth = 50, fill = "red", color = "black", alpha = 0.5) +
  geom_histogram(aes(x = f3), binwidth = 50, fill = "green", color = "black", alpha = 0.5) +
  geom_histogram(aes(x = f4), binwidth = 50, fill = "gray", color = "black", alpha = 0.5) +
  geom_histogram(aes(x = f5), binwidth = 50, fill = "pink", color = "black", alpha = 0.5) +
  labs(title = "Histogramas dos Formantes Compartilhando o Mesmo Eixo X", x = "Valor do Formante", y = "Frequência") +
  theme_minimal()

# Mostrar o histograma combinado
print(p)

# Gerar histogramas compartilhando o mesmo eixo X
q <- ggplot(medias_vogais) +
  geom_histogram(aes(x = Media_F1), binwidth = 50, fill = "blue", color = "black", alpha = 0.5) +
  geom_histogram(aes(x = Media_F2), binwidth = 50, fill = "red", color = "black", alpha = 0.5) +
  geom_histogram(aes(x = Media_F3), binwidth = 50, fill = "green", color = "black", alpha = 0.5) +
  geom_histogram(aes(x = Media_F4), binwidth = 50, fill = "gray", color = "black", alpha = 0.5) +
  geom_histogram(aes(x = Media_F5), binwidth = 50, fill = "pink", color = "black", alpha = 0.5) +
  labs(title = "Histogramas dos Formantes Compartilhando o Mesmo Eixo X", x = "Valor do Formante", y = "Frequência") +
  theme_minimal()

# Mostrar o histograma combinado
print(q)

# Definir a fração (metade aproximadamente)
frac <- 0.5

# Gerar o primeiro dataframe com metade dos dados de forma aleatória, sem reposição
df1 <- medias_vogais %>% sample_frac(frac)

# Gerar o segundo dataframe com os dados restantes (aqueles que não foram selecionados no primeiro)
df2 <- medias_vogais %>% anti_join(df1)

# Verificar o tamanho dos dataframes resultantes
nrow(df1)  # Tamanho do primeiro dataframe
nrow(df2)  # Tamanho do segundo dataframe


# Adicionar uma coluna de grupo para diferenciar os dataframes
df1 <- df1 %>% mutate(grupo = "df1")
df2 <- df2 %>% mutate(grupo = "df2")

df_combinado <- rbind(df1, df2)

# Realizar a MANOVA usando as variáveis Media_F1, Media_F2 e Media_F3
manova_resultado <- manova(cbind(Media_F1, Media_F2, Media_F3) ~ grupo, data = df_combinado)

# Mostrar o resumo da MANOVA
summary(manova_resultado)

# Mostrar a tabela de testes estatísticos, como o teste de Wilks
summary(manova_resultado, test = "Wilks")

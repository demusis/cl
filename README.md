# Análise Estatística de Arquivos do Praat

## Descrição
Esta aplicação Shiny permite realizar análises estatísticas de arquivos gerados pelo software **Praat**, com foco em **comparações de locutores**. Entre as análises disponíveis estão:
- Cálculo de estatísticas descritivas sobre os dados de fala;
- Gráficos de dispersão, boxplots e histogramas baseados em formantes (f1, f2) e frequência fundamental (f0);
- Testes estatísticos como PERMANOVA e Kolmogorov-Smirnov;
- Geração de tabelas para análise.

## Funcionalidades
- **Uploads de arquivos:** Aceita arquivos `.FORMANT`, `.TEXTGRID` e `.PITCHTIER` do Praat para dois grupos: *Padrão* e *Questionado*.
- **Seleção de vogais:** Escolha de vogais específicas para análise.
- **Gráficos interativos:** Visualização de dados através de hexplots, boxplots e histogramas.
- **Resultados exportáveis:** Salve os resultados em formatos `.csv` ou `.xlsx`.

## Requisitos
Antes de usar a aplicação, certifique-se de ter:
- **R** instalado em sua máquina;
- Os pacotes listados abaixo:
  - `shiny`
  - `shinyjs`
  - `ggplot2`
  - `dplyr`
  - `DT`
  - `vegan`
  - `shinyWidgets`
  - `shinycssloaders`
  - `ggthemes`

Você pode instalar todos os pacotes necessários com o seguinte comando no R:

```R
install.packages(c("shiny", "shinyjs", "ggplot2", "dplyr", "DT", "vegan", "shinyWidgets", "shinycssloaders", "ggthemes"))
```

## Alertas para interpretação dos resultados:

1. Sobre a Não Detecção de Diferenças Significativas para o fator Origem:
- A não detecção de diferenças significativas não confirma automaticamente que as amostras são do mesmo indivíduo.
- Indica apenas que não se pode rejeitar a hipótese de origem na mesma população.
- É fundamental complementar com análise qualitativa para uma interpretação adequada.

2. Sobre a Detecção de Diferenças Significativas para o fator Origem:
- A identificação de diferenças significativas sugere origem em populações distintas.
- Contudo, é necessário considerar que efeitos do locutor podem se confundir com variáveis do ambiente e equipamento de gravação.
- Portanto, não se pode concluir definitivamente que são locutores diferentes.

3. Sobre Interações entre os fatores Origem e Vogais:
- A ausência de interações significativas pode corroborar a hipótese de se um mesmo locutor.
- O paralelismo observado pode indicar similaridade fisiológica entre indivíduos.

4. Considerações Metodológicas:
- A análise qualitativa tem precedência sobre a quantitativa.
- Recomenda-se abordagem multiparamétrica antes de conclusões definitivas.
- É necessário estabelecer hierarquia de parâmetros, priorizando aqueles com menor variação intrapessoal e maior variação interpessoal.

## Guia de Uso
Para facilitar o uso da aplicação, foram criados dois vídeos explicativos que demonstram como operar as principais funcionalidades:
- **Parte 1:** [Introdução e upload de arquivos](https://youtu.be/ykZdsxOYN3Y)
- **Parte 2:** [Configuração de análises e exportação de resultados](https://youtu.be/sbpgxEptWVU)

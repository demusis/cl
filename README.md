# Aplicação Shiny: Análise Estatística de Arquivos do Praat

## Descrição
Esta aplicação Shiny permite realizar análises estatísticas de arquivos gerados pelo software **Praat**, com foco em **comparações de locutores**. Entre as análises disponíveis estão:
- Cálculo de estatísticas descritivas sobre os dados de fala;
- Gráficos de dispersão, boxplots e histogramas baseados em formantes (f1, f2) e pitch (f0);
- Testes estatísticos como PERMANOVA e Kolmogorov-Smirnov;
- Geração de tabelas detalhadas para análise.

## Funcionalidades
- **Uploads de arquivos:** Aceita arquivos `.FORMANT`, `.TEXTGRID` e `.PITCHTIER` do Praat para dois grupos: *Padrão* e *Questionado*.
- **Seleção de vogais:** Escolha de vogais específicas para análise.
- **Gráficos interativos:** Visualização de dados através de hexplots, boxplots e histogramas.
- **Resultados exportáveis:** Salve os resultados em formatos `.csv` ou `.xlsx`.

## Requisitos
Antes de usar a aplicação, certifique-se de ter:
- **R** instalado em sua máquina;
- O pacote **Shiny** e outros pacotes adicionais listados abaixo:
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

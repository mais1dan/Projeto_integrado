# Projeto Integrado: Análise de Acidentes em Rodovias Nacionais 📊

Este repositório contém um projeto de análise exploratória de dados desenvolvido na **linguagem R**. O objetivo principal é processar, limpar e extrair insights estatísticos a partir de uma base de dados estruturada (`.csv`) contendo registros detalhados sobre acidentes de trânsito ocorridos nas rodovias federais do Brasil.

O projeto demonstra a aplicação prática de técnicas de *Data Analytics*, desde a manipulação de grandes volumes de dados até a geração de visualizações gráficas para suporte à tomada de decisão.

---

## 🚀 Funcionalidades do Código

O script principal (`base.R`) executa um pipeline completo de dados:
* **Carga de Dados:** Importação eficiente do arquivo volumoso `datatran2024.csv`.
* **Processamento e Filttragem:** Limpeza e tratamento de variáveis estatísticas dos acidentes.
* **Análise Estatística:** Extração de métricas de recorrência, severidade e principais fatores dos acidentes.
* **Visualização de Dados:** Geração de múltiplos gráficos de diagnóstico (`Rplot.png`) para evidenciar padrões e tendências geográficas ou temporais.

---

## 🛠️ Tecnologias e Ferramentas

* **[R Language](https://r-project.org):** Linguagem estatística central utilizada para toda a manipulação e modelagem de dados.
* **[RStudio](https://posit.co):** Ambiente de desenvolvimento integrado (IDE) utilizado no projeto (`project.Rproj`).
* **Formatos de Dados:** Base em formato CSV (`datatran2024.csv`) e outputs visuais em PNG.

---

## 📦 Como Executar o Projeto

Para reproduzir a análise localmente em sua máquina, siga os passos abaixo:

1. **Clone o repositório:**
   ```bash
   git clone https://github.com
   cd Projeto_integrado
   ```

2. **Pré-requisitos:**
   * Certifique-se de ter o **R** e o **RStudio** instalados na sua máquina.
   * Certifique-se de que o arquivo `datatran2024.csv` está descompactado e na raiz da pasta do projeto.

3. **Execução:**
   * Abra o arquivo `project.Rproj` no RStudio.
   * Abra o script `base.R` e execute as linhas de código para processar a base e gerar os gráficos analíticos.

---

## 📈 Resultados Visuais (Insights)

O script gera automaticamente gráficos explicativos que ajudam a entender o comportamento dos acidentes rodoviários. Os resultados gerados nesta análise estão salvos na raiz do repositório como:
![Rplot](https://github.com/user-attachments/assets/c48d4cbc-5d05-4764-b30b-0d219bbb6127)

![Rplot01](https://github.com/user-attachments/assets/b3998656-cecd-4341-8ab7-0110b1bc31ab)

![Rplot02](https://github.com/user-attachments/assets/909ab648-e07b-430a-bd92-bde5a3d8a859)

---

## 💡 Próximos Passos & Melhorias
* [ ] Implementar uma interface visual interativa utilizando **R Shiny**.
* [ ] Adicionar mapas de calor utilizando pacotes geoespaciais (como `leaflet` ou `ggmap`).
* [ ] Integração com ferramentas de BI (Power BI/Tableau) para criação de dashboards executivos dinâmicos.


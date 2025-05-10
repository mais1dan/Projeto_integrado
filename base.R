# 0. Carregar bibliotecas necessárias
library(tidyverse)
library(stringr)

# 1. Leitura dos dados
dados <- read.csv("datatran2024.csv", sep = ";",
                  fill = TRUE, check.names = FALSE,
                  encoding = "UTF-8")

# 2. LIMPEZA DE TEXTOS E CODIFICAÇÃO --------------------------------------
cols_texto <- c("condicao_metereologica",
                "tipo_acidente",
                "causa_acidente",
                "tipo_pista",
                "uso_solo")

clean_text <- function(x) {
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = "") %>%
    str_replace_all("[^[:alnum:] ]", "") %>%
    str_squish()
}

for(col in cols_texto) {
  dados[[col]] <- clean_text(dados[[col]])
}

# 3. CONVERSÃO DE FATORES -----------------------------------------------
dados <- dados %>%
  mutate(
    condicao_metereologica = factor(condicao_metereologica),
    tipo_acidente          = factor(tipo_acidente),
    causa_acidente         = factor(causa_acidente),
    tipo_pista             = factor(tipo_pista),
    uso_solo               = factor(uso_solo),
    classificacao_acidente = factor(classificacao_acidente),
    fase_dia               = factor(fase_dia),
    sentido_via            = factor(sentido_via)
  )

# 4. ANÁLISE DESCRITIVA --------------------------------------------------
descricao_acidentes <- dados %>%
  summarise(
    total_acidentes       = n(),
    proporcao_fatais      = mean(mortos > 0, na.rm = TRUE),
    proporcao_com_feridos = mean(feridos > 0, na.rm = TRUE),
    proporcao_sem_vitimas = mean(mortos == 0 & feridos == 0, na.rm = TRUE),
    media_veiculos        = mean(veiculos, na.rm = TRUE),
    media_vitimas         = mean(pessoas, na.rm = TRUE)
  )
print(descricao_acidentes)

# 5. GRAVIDADE POR CONDIÇÃO METEOROLÓGICA ---------------------------------
gravidade_metereo <- dados %>%
  group_by(condicao_metereologica) %>%
  summarise(
    total       = n(),
    perc_fatais = mean(mortos > 0, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(perc_fatais))

ggplot(gravidade_metereo,
       aes(x = reorder(condicao_metereologica, -perc_fatais),
           y = perc_fatais)) +
  geom_col(fill = "firebrick") +
  labs(title = "Acidentes Fatais por Condição Meteorológica",
       x = "Condição", y = "% Fatais") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. GRAVIDADE POR TIPO DE PISTA -----------------------------------------
gravidade_pista <- dados %>%
  group_by(tipo_pista) %>%
  summarise(
    total       = n(),
    perc_fatais = mean(mortos > 0, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(perc_fatais))

ggplot(gravidade_pista,
       aes(x = reorder(tipo_pista, -perc_fatais), y = perc_fatais)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Acidentes Fatais por Tipo de Pista",
       x = "Tipo de Pista", y = "% Fatais") +
  theme_minimal()

# 7. TOP 10 TIPOS DE ACIDENTE --------------------------------------------
tipos_acidente <- dados %>%
  filter(!is.na(tipo_acidente)) %>%
  count(tipo_acidente, name = "total") %>%
  left_join(
    dados %>%
      group_by(tipo_acidente) %>%
      summarise(perc_fatais = mean(mortos > 0, na.rm = TRUE) * 100),
    by = "tipo_acidente"
  ) %>%
  arrange(desc(total))

ggplot(head(tipos_acidente, 10),
       aes(x = reorder(tipo_acidente, -total), y = total)) +
  geom_col(fill = "steelblue") +
  labs(title = "Top 10 Tipos de Acidente", x = NULL, y = "Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8. CAUSAS FREQUENTES ----------------------------------------------------
causas_frequentes <- dados %>%
  count(causa_acidente, name = "n") %>%
  filter(n > 100)
print(head(causas_frequentes, 10))

# 9. RECOMENDAÇÕES --------------------------------------------------------
cat("\nRecomendações baseadas na análise:\n")
cat("- Direcionar campanhas aos tipos de acidente mais recorrentes.\n")
cat("- Intensificar fiscalização em pistas simples, onde a fatalidade é maior.\n")
cat("- Implementar alertas meteorológicos em condições críticas.\n")
cat("- Focar educação nas causas de acidente mais frequentes.\n")

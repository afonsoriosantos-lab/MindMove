
library(tidyverse)
library(here)
library(janitor)
library(gtsummary)
library(gt)


dados <- read_csv(here("data", "raw", "mindmove.csv")) %>% clean_names()


glimpse(dados)
summary(dados)


dados |> summarise(across(everything(), ~ sum(is.na(.x))))



dados <- dados |> 
  mutate(reducao_phq9 = phq9_baseline - phq9_semana8)


tabela_basal_fmt <- dados |>
  select(grupo, idade, sexo, phq9_baseline, gad7_baseline) |> # Variáveis do teu CSV
  tbl_summary(
    by = grupo,
    label = list(
      idade         ~ "Idade (anos)",
      sexo          ~ "Sexo",
      phq9_baseline ~ "Depressão Basal (PHQ-9)",
      gad7_baseline ~ "Ansiedade Basal (GAD-7)"
    ),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) |>
  add_overall() |>
  modify_header(label ~ "**Variável**")

tabela_basal_fmt


tabela_basal_fmt |>
  as_gt() |>
  gt::gtsave(here("outputs", "tabelas", "tabela_basal.html"))


fig_outcome <- ggplot(
  dados |> filter(!is.na(phq9_semana8)), 
  aes(x = grupo, y = phq9_semana8, colour = grupo)
) +
  geom_boxplot(outlier.shape = NA, width = 0.4, linewidth = 0.7) +
  geom_jitter(width = 0.12, alpha = 0.7, size = 2.5) +
  scale_colour_manual(
    values = c("intervencao" = "#2C3E50", "controlo" = "#7A9B9E")
  ) +
  labs(
    x     = "Grupo",
    y     = "Score PHQ-9 às 8 semanas",
    title = "MindMove: Outcome Primário"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

fig_outcome


ggsave(
  here("outputs", "figuras", "figura_outcome.png"),
  plot   = fig_outcome,
  width  = 6,
  height = 4,
  dpi    = 150
)
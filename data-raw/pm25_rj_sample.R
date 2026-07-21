# data-raw/pm25_rj_sample.R
# Script to generate the sample PM2.5 dataset for Rio de Janeiro
#
# This creates a small sample dataset (100 rows from 5 municipalities)
# for use in examples, tests, and vignettes without requiring an
# internet connection or Power BI session.

# 5 representative municipalities from different RJ macro-regions
sample_municipios <- c(
  "330455"  # Rio de Janeiro (Metropolitana I)
)

# Years: 2020-2022
sample_anos <- 2020:2022

# Generate synthetic (but realistic) sample data
set.seed(2026)

pm25_rj_sample <- expand.grid(
  cod_municipio = c(
    330455,  # Rio de Janeiro - Metropolitana I
    330170,  # Duque de Caxias - Metropolitana I
    330330,  # Niterói - Metropolitana II
    330350,  # Nova Iguaçu - Metropolitana I
    330490   # São Gonçalo - Metropolitana II
  ),
  ano = sample_anos,
  stringsAsFactors = FALSE
)

# Add realistic PM2.5 values (mean ~11, sd ~3)
pm25_rj_sample$pm25_media_anual <- round(rnorm(
  nrow(pm25_rj_sample),
  mean = 11.5,
  sd = 2.8
), 1)

# Ensure all values are positive
pm25_rj_sample$pm25_media_anual <- abs(pm25_rj_sample$pm25_media_anual)

# Add sigla_uf
pm25_rj_sample$sigla_uf <- "RJ"

# Add category based on OMS guidelines
pm25_rj_sample$pm25_categoria <- cut(
  pm25_rj_sample$pm25_media_anual,
  breaks = c(0, 5, 10, 15, 25, Inf),
  labels = c(
    "OMS-2021 (< 5)",
    "OMS-2021 (5-10)",
    "OMS-interim (10-15)",
    "OMS-2005 (15-25)",
    "Acima OMS-2005 (> 25)"
  )
)

# Add latitude/longitude (approximate)
coords <- data.frame(
  cod_municipio = c(330455, 330170, 330330, 330350, 330490),
  latitude  = c(-22.91, -22.79, -22.88, -22.76, -22.83),
  longitude = c(-43.20, -43.31, -43.10, -43.45, -43.05)
)

pm25_rj_sample <- merge(pm25_rj_sample, coords, by = "cod_municipio")

# Set as tibble
pm25_rj_sample <- tibble::as_tibble(pm25_rj_sample)

# Add vigiar class and metadata
class(pm25_rj_sample) <- c("vigiar_pm25", "vigiar_tbl", class(pm25_rj_sample))
attr(pm25_rj_sample, "vigiar_tabela") <- "df_anual"
attr(pm25_rj_sample, "vigiar_metadados") <- list(
  origem = "Dados sinteticos para exemplos e testes",
  municipios = 5,
  anos = "2020-2022"
)

# Save
usethis::use_data(pm25_rj_sample, overwrite = TRUE, compress = "xz")

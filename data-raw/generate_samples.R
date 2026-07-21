# Generate sample data for the vigiar package
set.seed(2026)

pm25_rj_sample <- expand.grid(
  cod_municipio = c(
    330455,  # Rio de Janeiro - Metropolitana I
    330170,  # Duque de Caxias - Metropolitana I
    330330,  # Niteroi - Metropolitana II
    330350,  # Nova Iguacu - Metropolitana I
    330490   # Sao Goncalo - Metropolitana II
  ),
  ano = 2020:2022,
  stringsAsFactors = FALSE
)

pm25_rj_sample$pm25_media_anual <- round(abs(rnorm(
  nrow(pm25_rj_sample), mean = 11.5, sd = 2.8
)), 1)

pm25_rj_sample$sigla_uf <- "RJ"

pm25_rj_sample$pm25_categoria <- cut(
  pm25_rj_sample$pm25_media_anual,
  breaks = c(0, 5, 10, 15, 25, Inf),
  labels = c("OMS-2021 (< 5)", "OMS-2021 (5-10)", "OMS-interim (10-15)", "OMS-2005 (15-25)", "Acima OMS-2005 (> 25)")
)

coords <- data.frame(
  cod_municipio = c(330455, 330170, 330330, 330350, 330490),
  latitude  = c(-22.91, -22.79, -22.88, -22.76, -22.83),
  longitude = c(-43.20, -43.31, -43.10, -43.45, -43.05)
)

pm25_rj_sample <- merge(pm25_rj_sample, coords, by = "cod_municipio")

# Ensure tibble is loaded
library(tibble)
pm25_rj_sample <- as_tibble(pm25_rj_sample)

class(pm25_rj_sample) <- c("vigiar_pm25", "vigiar_tbl", class(pm25_rj_sample))
attr(pm25_rj_sample, "vigiar_tabela") <- "df_anual"
attr(pm25_rj_sample, "vigiar_metadados") <- list(
  origem = "Dados sinteticos para exemplos e testes",
  municipios = 5,
  anos = "2020-2022"
)

save(pm25_rj_sample, file = "data/pm25_rj_sample.rda", compress = "xz")
cat("Sample data saved: data/pm25_rj_sample.rda\n")
cat("Rows:", nrow(pm25_rj_sample), "Cols:", ncol(pm25_rj_sample), "\n")
print(head(pm25_rj_sample))

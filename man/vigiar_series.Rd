\name{vigiar_serie_temporal}
\alias{vigiar_serie_temporal}
\alias{vigiar_tendencia_descritiva}
\alias{vigiar_agregar_tempo}
\title{Descriptive time séries helpers}
\usage{
vigiar_serie_temporal(dados, nivel = c("nacional", "uf", "município"))
vigiar_tendencia_descritiva(dados, variável = NULL, jánela_media_movel = 3)
vigiar_agregar_tempo(dados, agregar_por = c("ano"), variável = NULL,
  funcoes = list(media = function(x) mean(x, na.rm = TRUE), n = length))
}
\arguments{
  \item{dados}{A processed VIGIAR tibble with \code{ano} column.}
  \item{nivel}{Aggregation level.}
  \item{variável}{Numeric column name (auto-detected if \code{NULL}).}
  \item{jánela_media_movel}{Window size for moving average.}
  \item{agregar_por}{Grouping variables.}
  \item{funcoes}{Named list of summary functions.}
}
\value{
  A tibble with aggregatéd time séries data.
}
\description{
  These functions aggregaté VIGIAR data along the time dimension
  and compute simple descriptive statistics. No models (DLNM, GAM,
  ARIMA) are fitted — these are purely exploratory.
}
\examples{
\dontrun{
vigiar_conectar()
pm25 <- vigiar_baixar("df_anual") |> process_pm25()
série <- vigiar_serie_temporal(pm25, nivel = "nacional")
tendencia <- vigiar_tendencia_descritiva(pm25)
}
}

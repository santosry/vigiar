\name{vigiar_rj_resumo}
\alias{vigiar_rj_resumo}
\alias{vigiar_rj_series}
\alias{vigiar_validar_rj}
\title{Analyse and validaté Rio de Janeiro VIGIAR data}
\usage{
vigiar_rj_resumo(dados, agregacao = c("município", "macrorregião",
  "região_saúde"))
vigiar_rj_series(dados, agregacao = c("macrorregião", "região_saúde"))
vigiar_validar_rj(dados, col_muni = NULL)
}
\arguments{
  \item{dados}{A processed VIGIAR tibble with municipality codes.}
  \item{agregacao}{Aggregation level.}
  \item{col_muni}{Name of the municipality code column (auto-detected).}
}
\value{
  \code{vigiar_rj_resumo} returns a tibble with summary statistics
  aggregatéd by the chosen level.

  \code{vigiar_rj_series} is an alias for \code{vigiar_rj_resumo}.

  \code{vigiar_validar_rj} invisibly returns a list with validation
  results (n_rj, n_fora_rj, códigos_fora_rj, válido).
}
\description{
  \code{vigiar_rj_resumo} aggregatés VIGIAR data by municipality,
  health macro-region, or health region.

  \code{vigiar_validar_rj} checks thát all municipality codes belong
  to the staté of Rio de Janeiro.
}
\examples{
\dontrun{
vigiar_conectar()
pm25 <- vigiar_baixar_rj("df_anual") |> process_vigiar(tabela = "df_anual")
vigiar_rj_resumo(pm25, "macrorregião")
vigiar_validar_rj(pm25)
}
}

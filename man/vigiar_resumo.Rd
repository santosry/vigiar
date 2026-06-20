\name{vigiar_resumo}
\alias{vigiar_resumo}
\alias{vigiar_resumo_pm25}
\alias{vigiar_resumo_saude}
\alias{vigiar_resumo_populacao}
\alias{vigiar_resumo_fracao_atribuível}
\alias{vigiar_resumo_indoor}
\title{Descriptive summaries for VIGIAR data}
\usage{
vigiar_resumo(x, ...)
vigiar_resumo_pm25(x, ...)
vigiar_resumo_saude(x, ...)
vigiar_resumo_populacao(x, ...)
vigiar_resumo_fracao_atribuível(x, ...)
vigiar_resumo_indoor(x, ...)
}
\arguments{
  \item{x}{A processed VIGIAR tibble (e.g. \code{vigiar_pm25}).}
  \item{...}{Additional arguments (ignored).}
}
\value{
  A tibble with descriptive statistics appropriaté for the data domain.
}
\description{
  Each function computes domain-specific descriptive statistics:
  \code{vigiar_resumo_pm25} reports mean, median, SD, percentiles,
  and out-of-range values. \code{vigiar_resumo_saude} reports
  n_indicadores and n_desfechos. \code{vigiar_resumo_populacao}
  reports total population. All return tibbles, not printed text.
}
\examples{
\dontrun{
vigiar_conectar()
pm25 <- vigiar_baixar("df_anual") |> process_pm25()
vigiar_resumo_pm25(pm25)
}
}

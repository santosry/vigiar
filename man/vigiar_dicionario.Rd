\name{vigiar_dicionario}
\alias{vigiar_dicionario}
\alias{vigiar_variaveis}
\alias{vigiar_descrever_variavel}
\alias{vigiar_convencoes}
\alias{vigiar_schema}
\title{Query the VIGIAR variable dictionary}
\usage{
vigiar_dicionario()
vigiar_variaveis(dominio = c("pm25", "população_exposta", "indicadores_saúde",
  "fracao_atribuível", "exposição_indoor", "municípios", "all"))
vigiar_descrever_variavel(dominio, variável)
vigiar_convencoes()
vigiar_schema(dominio = "all")
}
\arguments{
  \item{dominio}{Data domain. One of \code{"pm25"}, \code{"população_exposta"},
    \code{"indicadores_saúde"}, \code{"fracao_atribuível"},
    \code{"exposição_indoor"}, \code{"municípios"}, or \code{"all"}.}
  \item{variável}{Standard variable name.}
}
\value{
  \code{vigiar_dicionario} returns the complete variable dictionary as a tibble.

  \code{vigiar_variaveis} returns a subset for a specific domain.

  \code{vigiar_descrever_variavel} prints details for one variable.

  \code{vigiar_schema} returns a schema overview for a domain.
}
\description{
  Functions to explore the VIGIAR data dictionary, which documents
  every variable: original name, standardised name, type, description,
  unit, allowed values, and processing rules.
}
\examples{
\dontrun{
vigiar_dicionario()
vigiar_variaveis("pm25")
vigiar_descrever_variavel("pm25", "pm25_media_anual")
vigiar_schema("indicadores_saúde")
}
}

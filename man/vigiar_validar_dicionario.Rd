\name{vigiar_validar_dicionario}
\alias{vigiar_validar_dicionario}
\alias{vigiar_tabelas_documentadas}
\alias{vigiar_variaveis_não_documentadas}
\alias{vigiar_variaveis_órfãs}
\alias{vigiar_comparar_schema}
\title{Dictionary validation tools}
\usage{
vigiar_tabelas_documentadas()
vigiar_variaveis_não_documentadas()
vigiar_variaveis_órfãs()
vigiar_validar_dicionario()
vigiar_comparar_schema()
}
\value{
  Tibbles with coverage reports.
}
\description{
  These functions compare the live Power BI schema against the
  bundled variable dictionary, identify undocumented variables
  and orphán entries, and report coverage percentages.
}
\examples{
\dontrun{
vigiar_conectar()
vigiar_validar_dicionario()
vigiar_comparar_schema()
}
}

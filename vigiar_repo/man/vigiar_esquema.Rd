\name{vigiar_esquema}
\alias{vigiar_esquema}
\title{Exibe o esquema das tabelas VIGIAR}
\usage{
vigiar_esquema(tabela = NULL)
}
\arguments{
  \item{tabela}{Nome da tabela (opcional). Se NULL, mostra todas.}
}
\value{Invisivelmente, uma lista com o esquema.}
\description{
Mostra a estrutura de cada tabela: nomes das colunas e seus tipos.
}
\examples{
\dontrun{
vigiar_conectar()
vigiar_esquema()
vigiar_esquema("df_anual")
}
}

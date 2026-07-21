\name{vigiar_info}
\alias{vigiar_info}
\title{Catalogo de tabelas com descricoes}
\usage{
vigiar_info()
}
\value{Um tibble com colunas: tabela, colunas, descricao, categoria.}
\description{
Retorna um data.frame com todas as tabelas do VIGIAR, numero de colunas,
descricao do conteudo e categoria tematica.
}
\examples{
\dontrun{
vigiar_conectar()
vigiar_info()
}
}

\name{vigiar_baixar_tudo}
\alias{vigiar_baixar_tudo}
\alias{vigiar_baixar_principais}
\title{Baixa multiplas tabelas do VIGIAR}
\usage{
vigiar_baixar_tudo(tabelas = NULL, progresso = TRUE)
vigiar_baixar_principais()
}
\arguments{
  \item{tabelas}{Vetor opcional com os nomes das tabelas. NULL = todas.}
  \item{progresso}{Se TRUE, mostra barra de progresso.}
}
\value{Uma lista nomeada de data.frames (tibbles).}
\description{
\code{vigiar_baixar_tudo} baixa todas as tabelas (ou as especificadas).
\code{vigiar_baixar_principais} e um atalho para as tabelas mais relevantes.
}
\examples{
\dontrun{
vigiar_conectar()
tudo <- vigiar_baixar_tudo()
principais <- vigiar_baixar_principais()
}
}

\name{vigiar_conectar}
\alias{vigiar_conectar}
\title{Conecta ao dashboard VIGIAR do Power BI}
\usage{
vigiar_conectar(atualizar = FALSE, timeout = 30)
}
\arguments{
  \item{atualizar}{Se TRUE, forca uma nova conexao mesmo que ja exista uma sessao ativa.}
  \item{timeout}{Tempo maximo (em segundos) para estabelecer a conexao.}
}
\value{Invisivelmente, uma lista com os dados da sessao.}
\description{
Estabelece uma sessao com o dashboard publico do Power BI,
obtendo cookies e tokens necessarios para as consultas de dados.
}
\examples{
\dontrun{
vigiar_conectar()
vigiar_conectar(atualizar = TRUE)
}
}

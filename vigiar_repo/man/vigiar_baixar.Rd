\name{vigiar_baixar}
\alias{vigiar_baixar}
\title{Baixa dados de uma tabela especifica do VIGIAR}
\usage{
vigiar_baixar(tabela, colunas = NULL, ordenar_por = NULL, limite = NULL)
}
\arguments{
  \item{tabela}{Nome da tabela (use vigiar_tabelas() para listar)}
  \item{colunas}{Vetor opcional com nomes das colunas. NULL = todas.}
  \item{ordenar_por}{Coluna para ordenacao (opcional)}
  \item{limite}{Numero maximo de linhas (opcional)}
}
\value{Um data.frame (tibble) com os dados da tabela.}
\description{
Consulta a API do Power BI e retorna os dados de uma tabela do dashboard VIGIAR.
}
\examples{
\dontrun{
vigiar_conectar()
df_anual <- vigiar_baixar("df_anual")
df_mensal <- vigiar_baixar("df_mensal", colunas = c("ano", "UF", "pm25"))
}
}

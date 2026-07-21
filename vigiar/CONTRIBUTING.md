# Contribuindo com o pacote vigiar

Obrigado por contribuir! Este documento explica como colaborar com o
desenvolvimento do vigiar.

## Reportando problemas

### Mudancas no portal VIGIAR

Se o dashboard do Power BI mudar e as funcoes pararem de funcionar:

1. Verifique se o problema persiste executando `vigiar_conectar(refresh = TRUE)`
2. Rode `vigiar_status()` para comparar o esquema atual com o cache
3. Abra uma issue com o template **Schema Change** incluindo:
   - Data e hora da ultima conexao bem-sucedida
   - Mensagem de erro completa
   - Tabelas que mudaram (use `vigiar_comparar_schema()`)

### Bug reproducible

Para reportar um bug, inclua um exemplo minimo reproduzivel:

```r
library(vigiar)
vigiar_conectar()
# Codigo minimo que demonstra o problema
dados <- vigiar_baixar("df_anual", limite = 10)
# ...
vigiar_desconectar()
```

Use o template **Bug Report** e inclua `sessionInfo()`.

### Erro de municipio ou codigo IBGE

Se encontrar municipios faltando ou codigos IBGE incorretos:

1. Rode `vigiar_rj_municipios()` para ver o registro atual
2. Confirme o codigo no site do IBGE (https://cidades.ibge.gov.br)
3. Abra issue com o template **Municipio/IBGE**

## Ambiente de desenvolvimento

### Pre-requisitos

- R >= 4.1.0
- Git
- Pacotes: `devtools`, `testthat`, `roxygen2`, `pkgdown`, `lintr`, `renv`

### Configuracao inicial

```r
# Clonar o repositorio
git clone https://github.com/santosry/vigiar.git
cd vigiar

# Restaurar ambiente reprodutivel
renv::restore()

# Carregar pacote em modo desenvolvimento
devtools::load_all()
```

### Fluxo de trabalho

```r
# 1. Fazer alteracoes no codigo (R/*.R)

# 2. Atualizar documentacao
devtools::document()

# 3. Rodar testes offline
devtools::test()

# 4. Rodar lint
lintr::lint_package()

# 5. Verificar pacote
devtools::check()

# 6. Construir site
pkgdown::build_site()
```

### Testes online

Testes que requerem internet sao controlados pela variavel de ambiente
`VIGIAR_RUN_ONLINE_TESTS`. Para roda-los:

```r
Sys.setenv(VIGIAR_RUN_ONLINE_TESTS = "true")
devtools::test()
```

Na CI (GitHub Actions), os testes online sao desligados por padrao.

### Atualizando o dicionario de variaveis

Se o schema do VIGIAR mudar:

```r
source("data-raw/dictionary.R")
```

### Adicionando suporte a novo estado

Para expandir alem do RJ:

1. Crie um data frame similar a `RJ_MUNICIPIOS` com os municipios do estado
2. Adicione a constante `XX_MUNICIPIOS` e funcoes de validacao
3. Atualize `vigiar_checar_cobertura_espacial()` com o novo escopo
4. Envie um PR com o template **Feature Request**

## Estrutura do pacote

```
R/
  zzz.R                Constantes, ambiente interno, utilidades
  conexao.R            Gerenciamento de sessao
  client.R             Cliente Power BI (S3)
  api.R                Construcao e execucao de queries
  parse.R              Parser do formato DSR
  download.R           Funcoes de download publicas
  classes.R            Classes S3 para dados tipados
  process.R            Processamento e padronizacao
  validar.R            Validacao de dados
  diagnostic.R         Diagnostico de qualidade
  rj.R                 Registro de municipios do RJ
  series.R             Series temporais descritivas
  resumo.R             Sumarios estatisticos
  dictionary.R         Dicionario de variaveis
  auditar.R            Auditoria e compliance
  benchmark.R          Benchmarks de performance
  log.R                Logging estruturado
  cache.R              Cache e snapshots
  exportar.R           Exportacao (CSV, RDS, Parquet)

tests/testthat/
  test-offline.R       Testes sem internet
  test-online.R        Testes com internet
  test-new-features.R  Testes das features v0.7.0
  test-diagnostic.R    Testes de diagnostico

data-raw/
  dictionary.R         Geracao do dicionario

vignettes/
  vigiar.Rmd           Introducao ao pacote
  fluxo-download-processamento.Rmd  Fluxo principal
  variaveis-vigiar.Rmd Dicionario de variaveis
  convencoes-vigiar.Rmd Convencoes
  uso-responsavel-dados.Rmd  Limitacoes e etica
```

## Convencoes de codigo

- Funcoes exportadas: `vigiar_` prefixo, snake_case, sem acentos
- Funcoes internas: `.vigiar_` prefixo
- Classes S3: `vigiar_` prefixo (ex: `vigiar_tbl`, `vigiar_diagnostic`)
- Mensagens usam `cli::` para formatacao
- Logging usa `.vigiar_log()` para registro estruturado

## Processo de review

1. Fork o repositorio
2. Crie um branch: `git checkout -b feature/nome-da-feature`
3. Faca commits atomicos com mensagens descritivas
4. Rode `devtools::check()` para garantir 0 errors, 0 warnings
5. Envie um PR para `main`
6. Aguarde review

## Licenca

Ao contribuir, voce concorda que seu codigo sera licenciado sob MIT.

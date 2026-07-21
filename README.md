# vigiar

<!-- badges: start -->
[![R-CMD-check](https://github.com/santosry/vigiar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/santosry/vigiar/actions/workflows/R-CMD-check.yaml)
[![lint](https://github.com/santosry/vigiar/actions/workflows/lint.yaml/badge.svg)](https://github.com/santosry/vigiar/actions/workflows/lint.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://www.r-project.org/)
<!-- badges: end -->

Pacote R para download, processamento, validação e diagnóstico dos dados do
[VIGIAR](https://app.powerbi.com/view?r=eyJrIjoiNmRhODQwNzItNThlOS00ZmQ4LWJjZmItZDYxOTNhOTRmYmFhIiwidCI6IjlhNTU0YWQzLWI1MmItNDg2Mi1hMzZmLTg0ZDg5MWU1YzcwNSJ9)
(Vigilância em Saúde Ambiental) do Ministério da Saúde, com foco no estado do
Rio de Janeiro (92 municípios e 9 macrorregiões de saúde SES-RJ).

**vigiar desconfia dos dados antes de seduzir o pesquisador com gráficos.**

Para saber mais sobre o pacote e exemplos de uso, [acesse o e-book](https://santosry.github.io/vigiar/ebook/).

## Instalação

```r
# Instalar do GitHub
devtools::install_github("santosry/vigiar")
```

### Dependências

```r
install.packages(c(
  "httr2", "jsonlite", "tibble", "dplyr",
  "cli", "checkmate", "openssl", "magrittr", "rlang"
))
```

## Exemplo rápido (20 linhas)

```r
library(vigiar)
library(dplyr)

# 1. Conectar
vigiar_conectar()

# 2. Baixar dados do RJ
pm25 <- vigiar_baixar_rj("df_anual")

# 3. Processar e validar
pm25 <- process_pm25(pm25, tipo = "anual")

# 4. Diagnosticar qualidade
diag <- vigiar_diagnosticar_serie(pm25)
vigiar_relatorio_diagnostico(diag)

# 5. Agregar por ano
tendencia <- vigiar_serie_temporal(pm25, nivel = "nacional")
print(tendencia)

# 6. Auditoria completa
audit <- vigiar_auditar(pm25, tabela = "df_anual")
print(audit)

# 7. Snapshot para reprodutibilidade
snap <- vigiar_snapshot(dados = pm25, tabela = "df_anual")
vigiar_salvar_snapshot(snap, "pm25_rj_2026.rds")

vigiar_desconectar()
```

## Funcionalidades principais

### Download & Conexão

```r
# Listar tabelas disponíveis
vigiar_tabelas()

# Catálogo completo com descrições e categorias
vigiar_info()

# Baixar tabela com filtro RJ (estratégia ASC+DESC particionada)
pm25 <- vigiar_baixar_rj("df_anual")

# Download com cache local (reusa por 24h)
vigiar_cache_dir("~/vigiar_cache")
dados <- vigiar_baixar_com_cache("df_anual")
```

### Processamento e Validação

```r
# Padronizar colunas com classes S3 tipadas
pm25 <- process_pm25(dados, tipo = "anual")
# vigiar_pm25 → vigiar_tbl → tibble → data.frame

# Validar códigos IBGE
vigiar_validar_ibge(pm25, col_codigo = "cod_municipio")

# Validar datas, unidades, coerência
vigiar_validar_datas(pm25)
vigiar_validar_unidades(pm25)
```

### Diagnóstico e Auditoria

```r
# Diagnóstico completo de série temporal
diag <- vigiar_diagnosticar_serie(pm25)
# Severidade: ok | aviso | problema | crítico
vigiar_relatorio_diagnostico(diag)

# Auditoria completa com SHA256
audit <- vigiar_auditar(pm25, tabela = "df_anual")

# Compliance multi-perfil: basico, rigoroso, rj, corrupcao
comp <- vigiar_compliance_check(pm25, tabela = "df_anual",
  profiles = c("rigoroso", "rj"))
```

### Reproductibilidade

```r
# Snapshot com checksum SHA256
snap <- vigiar_snapshot(dados = pm25, tabela = "df_anual")
vigiar_salvar_snapshot(snap, "pm25_snapshot.rds")

# Travar schema para detectar mudanças
vigiar_esquema_lock("schema_lock.json")
vigiar_esquema_verificar("schema_lock.json")

# Log estruturado de operações
vigiar_log()
```

### Rio de Janeiro

```r
# 92 municípios com macrorregiões de saúde SES-RJ
rj <- vigiar_rj_municipios()

# Agregar por macrorregião
resumo <- vigiar_rj_resumo(pm25, agregacao = "macrorregiao")

# Séries temporais por macrorregião
series <- vigiar_rj_series(pm25,
  variavel = "pm25_media_anual",
  agrupamento = "macrorregiao")
```

### Exportação

```r
vigiar_exportar_csv(pm25, "pm25_rj.csv")
vigiar_exportar_rds(pm25, "pm25_rj.rds")
vigiar_exportar_parquet(pm25, "pm25_rj.parquet")  # requer arrow
vigiar_exportar_auditoria(audit, "auditoria.json")
```

## Como Funciona

O pacote implementa o protocolo de API do Power BI "Publish to Web":

1. **Sessão**: Obtém cookies (`WFESessionId`, `ARRAffinity`) e o
   `telemetrySessionId` da página do dashboard via scraping.

2. **Esquema**: Consulta o endpoint `/conceptualschema` para obter a
   estrutura de tabelas e colunas.

3. **Query**: Monta queries no formato Semantic Query (JSON) e as envia
   para o endpoint `/querydata`.

4. **Parse**: Decodifica o formato comprimido DSR (Data Shape Response)
   do Power BI para data.frames R.

```
Usuário → vigiar_conectar() → Power BI Page → obtém cookies/session
              ↓
         vigiar_baixar("tabela")  [validado com checkmate]
              ↓
         .vigiar_construir_query()  → monta JSON Semantic Query
              ↓
         .vigiar_executar_query()   → POST /querydata [com retry]
              ↓
         .vigiar_parse_dados()      → decodifica DSR → data.frame
              ↓
         process_pm25() / process_*() → S3 classes tipadas
              ↓
         vigiar_diagnosticar_serie()  → diagnóstico automático
              ↓
         vigiar_auditar()             → auditoria + SHA256
```

## Limitações

- **API Power BI**: limita respostas a ~30K linhas. Use `vigiar_baixar_rj()`
  para tabelas grandes (download ASC + DESC particionado).
- **Schema instável**: o dashboard pode mudar sem aviso. Use
  `vigiar_esquema_lock()` para congelar e `vigiar_status()` para verificar.
- **Dependência externa**: o pacote depende do portal Power BI do Ministério
  da Saúde. Se o portal sair do ar, o download falha.
- **Dados secundários**: o pacote baixa dados públicos, não os gera. Validação
  é obrigatória antes de qualquer análise.

## Citação

Para citar o pacote em trabalhos acadêmicos:

> Santos, R. (2026). vigiar: VIGIAR Environmental Health Data for Rio de
> Janeiro. R package version 0.7.2.
> [https://github.com/santosry/vigiar](https://github.com/santosry/vigiar)

## Licença

MIT. Os dados baixados pertencem ao Ministério da Saúde / VIGIAR.

## Agradecimento

O desenvolvimento deste pacote foi inspirado pelo pacote
[*microdatasus*](https://github.com/rfsaldanha/microdatasus), de Raphael
Saldanha, Ronaldo Bastos e Christovam Barcellos.

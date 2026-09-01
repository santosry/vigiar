# vigiar

<!-- badges: start -->
[![R-CMD-check](https://github.com/santosry/vigiar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/santosry/vigiar/actions/workflows/R-CMD-check.yaml)
[![lint](https://github.com/santosry/vigiar/actions/workflows/lint.yaml/badge.svg)](https://github.com/santosry/vigiar/actions/workflows/lint.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://www.r-project.org/)
<!-- badges: end -->

Pacote R para download, processamento, validação, auditoria e diagnóstico dos
dados do [VIGIAR](https://app.powerbi.com/view?r=eyJrIjoiNmRhODQwNzItNThlOS00ZmQ4LWJjZmItZDYxOTNhOTRmYmFhIiwidCI6IjlhNTU0YWQzLWI1MmItNDg2Mi1hMzZmLTg0ZDg5MWU1YzcwNSJ9)
(Vigilância em Saúde Ambiental) do Ministério da Saúde, com foco no estado do
Rio de Janeiro (92 municípios e 9 macrorregiões de saúde SES-RJ).

> **O vigiar desconfia dos dados antes de seduzir o pesquisador com gráficos.**

## O que o pacote faz

O pacote segue a arquitetura:

```
fonte externa (Power BI)
  → aquisição (vigiar_conectar, vigiar_baixar)
  → validação (vigiar_validar_*, vigiar_checar_*)
  → processamento (process_*)
  → auditoria (vigiar_auditar, vigiar_compliance_check)
  → diagnóstico (vigiar_diagnosticar_serie)
  → reprodutibilidade (snapshots, checksums, schema lock)
```

Ele disponibiliza:

- concentração de PM2.5 por município (séries anuais e mensais);
- dias acima dos limites da OMS e do CONAMA;
- população exposta por faixa de concentração;
- indicadores de saúde (fração atribuível, desfechos);
- exposição indoor a combustíveis sólidos;
- cadastro oficial dos 92 municípios do Rio de Janeiro e das macrorregiões de saúde.

## Origem dos dados

Os dados são públicos e disponibilizados pelo VIGIAR (Ministério da Saúde) por
meio de um dashboard **Power BI "Publish to Web"** (acesso anônimo, sem login).
O pacote **não é oficial do Ministério da Saúde**.

A disponibilidade, a URL e o esquema (tabelas/colunas) da fonte externa podem
mudar independentemente do pacote. O pacote tenta detectar essas mudanças
(`vigiar_status()`, `vigiar_esquema_verificar()`) e falhar com mensagens
claras, mas o acesso programático é engenharia reversa de um endpoint interno
e não contratual do Power BI. Consulte a vinheta de
[uso responsável](https://santosry.github.io/vigiar/articles/uso-responsavel-dados.html).

## Instalação

O pacote é distribuído pelo GitHub (não está no CRAN).

```r
install.packages("remotes")
remotes::install_github("santosry/vigiar")
```

Requer R >= 4.1.0 e acesso à internet para as funções de download. O
processamento, a validação e a auditoria funcionam offline.

## Uso mínimo

```r
library(vigiar)

# 1. Conecta ao dashboard (requer internet)
vigiar_conectar()

# 2. Lista tabelas e baixa dados
vigiar_tabelas()
df_anual <- vigiar_baixar("df_anual")

# 3. Processa e valida
pm25 <- process_pm25(df_anual, tipo = "anual")
vigiar_checar_dados(pm25, tabela = "df_anual")

# 4. Diagnostica a série temporal do RJ
diag <- vigiar_diagnosticar_serie(pm25, escopo = "rj")
vigiar_relatorio_diagnostico(diag)

# 5. Encerra a sessão
vigiar_desconectar()
```

Para testar o pacote sem internet, use o dado de exemplo incluído:

```r
library(vigiar)
data(pm25_rj_sample)
summary(pm25_rj_sample)

diag <- vigiar_diagnosticar_serie(pm25_rj_sample, escopo = "rj")
print(diag)
```

## Documentação completa

Toda a documentação — instalação, guia de uso, exemplos, auditoria,
reprodutibilidade e limitações — está no
[e-book do vigiar](https://santosry.github.io/vigiar/ebook/).
As vinhetas (`vignette(package = "vigiar")`) cobrem convenções de variáveis,
fluxo de download/processamento e uso responsável dos dados.

## Limitações

- PM2.5 é estimado por satélite/modelagem, não medido em superfície.
- População exposta usa projeções do IBGE, não contagens censitárias.
- Indicadores de saúde são estimativas epidemiológicas com intervalo de confiança.
- Os dados são agregados por município/ano — não há dados individuais.
- O dashboard do Power BI pode ficar temporariamente indisponível ou mudar de esquema.

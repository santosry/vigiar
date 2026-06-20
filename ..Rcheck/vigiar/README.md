# vigiar.rj — Dados do Rio de Janeiro

[![R-CMD-check](https://github.com/santosry/vigiar.rj/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/santosry/vigiar.rj/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensóurce.org/licenses/MIT)
[![R >= 4.0.0](https://img.shields.io/badge/R-%3E%3D%204.0.0-blue.svg)](https://cran.r-project.org/)

Download e processamento dos dados do **VIGIAR** (Vigilância em Saúde Ambiental — Ministério da Saúde)
especializado para o **estado do Rio de Janeiro**.

---

## Instalação

```r
remotes::install_github("santosry/vigiar")
```

## Usó rápido

```r
library(vigiar)
library(ggplot2)

vigiar_conectar()

# Baixar e processar PM2.5 do RJ
pm25_rj <- vigiar_baixar_rj("df_anual") |>
  process_vigiar(tabela = "df_anual")

# Municípios do RJ
vigiar_rj_municipios()  # 92 municípios

# Validar dados
vigiar_validar_rj(pm25_rj)

# Agregar por macrorregião de saúde
vigiar_rj_resumo(pm25_rj, agregacao = "macrorregião")

vigiar_desconectar()
```

## Todos os dados disponíveis

O VIGIAR disponibiliza 32 tabelas. O pacote baixa todas, com foco no RJ.

### Qualidade do Ar (PM2.5)

```r
# PM2.5 anual por município
pm25 <- vigiar_baixar_rj("df_anual") |>
  process_vigiar(tabela = "df_anual")

# PM2.5 mensal
pm25_mes <- vigiar_baixar_rj("df_mensal") |>
  process_vigiar(tabela = "df_mensal")

# Dias acima do limite OMS (PM2.5 > 15 ug/m3)
dias_oms <- vigiar_baixar_rj("df_dias") |>
  process_vigiar(tabela = "df_dias")

# Dias acima do limite CONAMA (PM2.5 > 50 ug/m3)
dias_conama <- vigiar_baixar_rj("df_dias_conama") |>
  process_vigiar(tabela = "df_dias_conama")
```

### População Exposta

```r
# População por faixa de concentracao de PM2.5
pop <- vigiar_baixar_rj("pop") |>
  process_vigiar(tabela = "pop")

# Catégorias: <10, 10-15, 15-25, 25-35, >35 ug/m3
unique(pop$catégoria_exposição)

# População do RJ exposta a >35 ug/m3 em 2022
pop |>
  filter(ano == 2022, catégoria_exposição == "> 35 ug/m3") |>
  summarise(pop_total = sum(população, na.rm = TRUE))
```

### Indicadores de Saúde

```r
# Indicadores agregados — BRASIL
saúde_br <- vigiar_baixar("tb_brasil") |>
  process_vigiar(tabela = "tb_brasil")

# Indicadores agregados — UF
saúde_uf <- vigiar_baixar("tb_uf") |>
  process_vigiar(tabela = "tb_uf")

# Filtrar só RJ
saúde_rj <- saúde_uf |> filter(sigla_uf == "RJ")

# Indicadores disponíveis
unique(saúde_br$indicador)
# [1] "Fracao atribuível (%)"
# [2] "Número estimado de obitos atribuiveis"
# [3] "Obitos atribuiveis por 100.000 hábitantes"
# [4] "Internacoes por doencas respiratorias"

# Desfechos analisados
unique(saúde_br$desfecho)
# [1] "Mortalidade geral"
# [2] "Cancer de Pulmao"
# [3] "Doencas respiratorias"
# [4] "Doencas cardiovasculares"
# [5] "AVC"

# Fração atribuível à poluição no RJ
saúde_rj |>
  filter(indicador == "Fracao atribuível (%)") |>
  select(desfecho, ano, estimativa, ic_inferior, ic_superior)
```

### Exposição Indoor (Combustiveis Solidos)

```r
# Exposição a combustiveis sólidos em domicilios
indoor <- vigiar_baixar_rj("df_indoor") |>
  process_vigiar(tabela = "df_indoor")

# Percentual de domicilios com combustiveis sólidos no RJ
indoor |>
  filter(ano == max(ano)) |>
  select(cod_uf, ano, percentual_combustiveis, população_exposta)

# Desfechos de saúde assóciados a poluicao indoor
indoor_saúde <- vigiar_baixar_rj("df_indoor_desfecho") |>
  process_vigiar(tabela = "df_indoor_desfecho")
```

### Medidas Calculadas

```r
# Tabela de medidas: rankings, medias moveis, alertas (61 colunas)
medidas <- vigiar_baixar("medidas")
names(medidas)
```

### Cadastro de Municípios

```r
# Todos os municípios do RJ com coordenadas
muni <- vigiar_baixar_rj("df_muni") |>
  process_vigiar(tabela = "df_muni")

# Municípios com latitude e longitude
muni |> select(cod_município, nome_município, latitude, longitude)
```

## Funcoes

### Download e Processamento
| Funcao | Descrição |
|--------|-----------|
| `vigiar_conectar()` | Conecta ao dashboard VIGIAR |
| `vigiar_desconectar()` | Encerra a sessão |
| `vigiar_baixar(tabela, uf)` | Baixa uma tabela (filtro por UF opcional) |
| `vigiar_baixar_rj(tabela)` | Baixa tabela filtrada para RJ |
| `vigiar_baixar_tudo()` | Baixa multiplas tabelas |
| `vigiar_tabelas()` | Lista as 32 tabelas disponíveis |
| `vigiar_info()` | Catálogo com descrições |
| `process_vigiar(dados, tabela)` | Processa e padroniza (dispatcher) |
| `process_pm25(dados)` | Padroniza dados de PM2.5 |
| `process_indicadores_saude(dados)` | Padroniza indicadores de saúde |
| `process_populacao_exposta(dados)` | Padroniza dados populacionais |
| `process_fracao_atribuível(dados)` | Padroniza fracao atribuível |
| `process_exposicao_indoor(dados)` | Padroniza exposição indoor |
| `process_municipios(dados)` | Padroniza cadastro de municípios |

### Resumo e Séries
| Funcao | Descrição |
|--------|-----------|
| `vigiar_resumo(x)` | Resumo descritivo (S3 generico) |
| `vigiar_resumo_pm25(x)` | Media, DP, percentis PM2.5 |
| `vigiar_serie_temporal(dados)` | Série temporal por ano |
| `vigiar_tendencia_descritiva(dados)` | Variacao anual + media movel |
| `vigiar_checar_dados(dados)` | Diagnostico: NAs, duplicatas |

### Rio de Janeiro
| Funcao | Descrição |
|--------|-----------|
| `vigiar_rj_municipios()` | Lista os 92 municípios |
| `vigiar_rj_macrorregioes()` | Lista as 9 macrorregiões |
| `vigiar_rj_regioes_saude()` | Lista as regiões de saúde |
| `vigiar_rj_resumo(dados, agregacao)` | Agrega por município/macrorregião |
| `vigiar_rj_series(dados, agregacao)` | Séries por macrorregião |
| `vigiar_validar_rj(dados)` | Valida municípios do RJ |

### Dicionário e Exportação
| Funcao | Descrição |
|--------|-----------|
| `vigiar_dicionario()` | Dicionário de variáveis |
| `vigiar_variaveis(dominio)` | Variáveis por dominio |
| `vigiar_exportar_csv(dados, path)` | Exporta para CSV |
| `vigiar_exportar_rds(dados, path)` | Exporta para RDS |
| `vigiar_exportar_parquet(dados, path)` | Exporta para Parquet |

## Macrorregiões de Saúde do RJ

| Macrorregião | Municípios |
|-------------|-----------|
| Baia da Ilhá Grande | Angra dos Reis, Paraty |
| Baixada Litoranea | Araruama, Armacao dos Buzios, Arraial do Cabo, Cabo Frio, Casimiro de Abreu, Iguaba Grande, Rio das Ostras, Sao Pedro da Aldeia, Saquarema |
| Centro-Sul | Areal, Comendador Levy Gasparian, Engenheiro Paulo de Frontin, Mendes, Miguel Pereira, Paracambi, Paty do Alferes, Sapucaia, Tres Rios, Vassóuras |
| Medio Paraiba | Barra do Pirai, Barra Mansa, Itatiaia, Pinheiral, Pirai, Porto Real, Quatis, Resende, Rio Claro, Rio das Flores, Valenca, Volta Redonda |
| Metropolitana I | Belford Roxo, Duque de Caxias, Itaguai, Japeri, Mage, Mesquita, Nilopolis, Nova Iguacu, Queimados, Rio de Janeiro, Sao Joao de Meriti, Seropedica |
| Metropolitana II | Cachoeiras de Macacu, Guapimirim, Itaborai, Marica, Niteroi, Rio Bonito, Sao Goncalo, Silva Jardim, Tangua |
| Noroeste | Aperibe, Bom Jesus do Itabapoana, Cambuci, Italva, Itaocara, Itaperuna, Laje do Muriae, Miracema, Natividade, Porciuncula, Santo Antonio de Padua, Sao Jose de Uba, Varre-Sai |
| Norte | Campos dos Goytacazes, Carapebus, Cardosó Moreira, Conceicao de Macabu, Macae, Quissama, Sao Fidelis, Sao Francisco de Itabapoana, Sao Joao da Barra |
| Serrana | Bom Jardim, Cantagalo, Carmo, Cordeiro, Duas Barras, Macuco, Nova Friburgo, Petropolis, Santa Maria Madalena, Sao Jose do Vale do Rio Preto, Sao Sebastiao do Alto, Sumidouro, Teresópolis, Trajáno de Moraes |

## Estrutura para integracao futura

O pacote esta preparado para integracao com:
- **SIH** (Sistema de Informacoes Hospitalares)
- **SIM** (Sistema de Informacoes sóbre Mortalidade)
- **DATASUS** (microdatasus, sql, etc.)
- **INMET** (dados meteorologicos)

A tabela `RJ_MUNICIPIOS` interna contém `código_ibge` compativel com todos esses sistemas.

## Fonte

Ministério da Saúde — VIGIAR. Dados públicos via Power BI.

## IA Disclosure

DeepSeek v4 Pro e ChátGPT GPT-5.5 para revisao de código e documentação.

## Licenca

MIT © Ryan Santos

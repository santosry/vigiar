# vigiar

<!-- badges: start -->
<!-- badges: end -->

Pacote R para download automatizado de **todos os dados** do dashboard
[VIGIAR](https://app.powerbi.com/view?r=eyJrIjoiNmRhODQwNzItNThlOS00ZmQ4LWJjZmItZDYxOTNhOTRmYmFhIiwidCI6IjlhNTU0YWQzLWI1MmItNDg2Mi1hMzZmLTg0ZDg5MWU1YzcwNSJ9)
(Vigilância em Saúde Ambiental — Ministério da Saúde) publicado no Power BI.

---

## O que é o VIGIAR?

O VIGIAR é o sistema de vigilância em saúde ambiental do Ministério da
Saúde brasileiro. O dashboard público fornece **32 tabelas** com dados sobre:

### Qualidade do Ar (PM2.5)
- Concentração de **PM2.5** por município — séries anuais e mensais
- **Dias críticos** acima dos limites da OMS (15 µg/m³) e CONAMA (50 µg/m³)
- Coordenadas geográficas (LAT/LON) para mapas

### População Exposta
- População residente por município, ano e **categoria de concentração**
- Estimativas de população por faixa: <10, 10-15, 15-25, 25-35, >35 µg/m³

### Indicadores de Saúde
- **Fração atribuível** — percentual de desfechos atribuíveis à poluição
- **Óbitos atribuíveis** por 100.000 habitantes
- **Internações** por doenças respiratórias e cardiovasculares
- Desfechos: mortalidade geral, câncer de pulmão, doenças respiratórias,
  doenças cardiovasculares, AVC
- Agregados por **Brasil**, **UF** e **município**

### Exposição Indoor
- **Combustíveis sólidos** em domicílios — percentual da população exposta
- Desfechos de saúde associados à poluição indoor
- Dados por estado e ano

## Instalação

```r
# install.packages("remotes")
remotes::install_github("santosry/vigiar-download")
```

Dependências: `httr2`, `jsonlite`, `tibble` · Requer **R ≥ 4.0.0**

## Uso Básico

```r
library(vigiar)

# 1. Conectar
vigiar_conectar()
#> Sessão pronta! 32 tabelas disponíveis.

# 2. Ver catálogo completo com descrições
vigiar_info()
#> # A tibble: 32 × 4
#>    tabela         colunas descricao                                  categoria
#>    <chr>            <int> <chr>                                      <chr>
#>  1 df_anual             6 Médias anuais de PM2.5 por município       Qualidade do Ar
#>  2 df_mensal           20 Médias mensais de PM2.5 por município      Qualidade do Ar
#>  3 df_dias              7 Dias acima do limite OMS                   Qualidade do Ar
#>  4 pop                  6 População por município e categoria        População
#>  5 tb_brasil            8 Indicadores agregados — Brasil             Indicadores de Saúde
#>  ...

# 3. Baixar qualquer tabela
pm25 <- vigiar_baixar("df_anual")           # Qualidade do ar
saude <- vigiar_baixar("tb_brasil")          # Indicadores de saúde
indoor <- vigiar_baixar("df_indoor")          # Exposição indoor
fracao <- vigiar_baixar("tb_fracao")          # Fração atribuível

# 4. Baixar todas as tabelas de uma categoria
tudo <- vigiar_baixar_tudo()
```

## Catálogo Completo de Tabelas

### 🌍 Qualidade do Ar (PM2.5)

| Tabela | Cols | Conteúdo |
|--------|------|----------|
| `df_anual` | 6 | Médias anuais PM2.5 por município: muni, UF, ano, Media_pm25, Categoria_pm25, Categoria_pm25_conama |
| `df_mensal` | 20 | Médias mensais PM2.5: muni, pm25, Região, UF, Município, LAT, LON, ano, mês... |
| `df_dias` | 7 | Dias acima do limite OMS (PM2.5 > 15 µg/m³): ID_MUNI, t_dias, mes, ano, n_dias |
| `df_dias_conama` | 5 | Dias acima do limite CONAMA (PM2.5 > 50 µg/m³): ID_MUNI, mes, ano, n_dias_conama |

### 👥 População

| Tabela | Cols | Conteúdo |
|--------|------|----------|
| `pop` | 6 | População por município, ano e categoria de exposição: muni, ano, pop, categoria, UF |

### 🏥 Indicadores de Saúde (Epidemiológicos)

| Tabela | Cols | Conteúdo |
|--------|------|----------|
| `tb_brasil` | 8 | Indicadores agregados — **BRASIL**: Indicador, n (pop exposta), est (estimativa), low/high (IC 95%), desfecho, ano |
| `tb_uf` | 8 | Indicadores agregados — **UF**: mesmas colunas + loc (código UF) |
| `tb_muni` | 12 | Indicadores por **município**: + cod (IBGE), lat, long |
| `tb_fracao` | 9 | **Fração atribuível**: Indicador, n, est, low, high, desfecho, ano, loc |
| `tb_quartis` | 5 | **Quartis** dos indicadores: q1, q2, q3 por indicador e desfecho |

**Indicadores disponíveis** (coluna `Indicador`):
- Fração atribuível (%)
- Número estimado de óbitos atribuíveis
- Óbitos atribuíveis por 100.000 habitantes
- Internações por doenças respiratórias
- Internações por doenças cardiovasculares

**Desfechos** (coluna `desfecho`):
- Mortalidade geral (todas as causas)
- Câncer de pulmão
- Doenças respiratórias
- Doenças cardiovasculares
- AVC (Acidente Vascular Cerebral)

### 🏠 Exposição Indoor (Combustíveis Sólidos)

| Tabela | Cols | Conteúdo |
|--------|------|----------|
| `df_indoor` | 10 | Exposição a combustíveis sólidos: Code (UF), Ano, comb_sol (%), pop_exposta, percent_comb, Quartis |
| `df_indoor_desfecho` | 16 | Desfechos de saúde — indoor: Code, State.x, Ano, parametro, sexo, pop, comb_sol_perc, indicador, est, low, up |

### 📊 Medidas Calculadas

| Tabela | Cols | Conteúdo |
|--------|------|----------|
| `medidas` | 61 | Tabela de medidas: rankings, médias móveis, limites de controle, proporções populacionais, alertas, atualizações |

### 📋 Cadastro

| Tabela | Cols | Conteúdo |
|--------|------|----------|
| `df_muni` | 19 | Cadastro completo de municípios: REGIAO, UF_COD, UF_SIGLA, UF_NOME, MUN_COD, MUN_NOME, LAT, LON, população... |

### 🔧 Tabelas Auxiliares

| Tabela | Cols | Função |
|--------|------|--------|
| `df_mes` | 3 | Meses (número → nome) |
| `df_ano` | 1 | Anos disponíveis na base |
| `aux_uf` | 2 | Código UF → nome |
| `legenda` | 3 | Cores para faixas PM2.5 (OMS) |
| `legenda_conama` | 3 | Cores para faixas PM2.5 (CONAMA) |
| `legenda_quartis` | 3 | Cores para quartis |
| `legenda_indoor` | 3 | Cores para exposição indoor |
| `Ano` | 3 | Seletor de ano |
| `Selecao` | 3 | Seletor de categoria |
| `referencia` | 3 | Valores de referência OMS |
| `referencia_conama` | 3 | Valores de referência CONAMA |
| `seletor_indicador` | 3 | Seletor de indicador |
| `dados_ate` | 1 | Data dos últimos dados |
| `last_update` | 1 | Última atualização |
| `att_em` | 1 | Timestamp de atualização |
| `DateTableTemplate_*` | 7 | Tabelas de data (calendário) |
| `LocalDateTable_*` | 7 | Tabelas de data local |

## Exemplos por Categoria

### Qualidade do Ar

```r
library(vigiar)
library(dplyr)

vigiar_conectar()

# PM2.5 anual
anual <- vigiar_baixar("df_anual")

# Municípios com maior PM2.5 no último ano
anual |>
  filter(ano == max(ano)) |>
  slice_max(Media_pm25, n = 10) |>
  select(UF, Media_pm25)

# Dias críticos (OMS)
dias <- vigiar_baixar("df_dias")
dias |>
  group_by(ano) |>
  summarise(total_dias_criticos = sum(n_dias, na.rm = TRUE))
```

### Indicadores de Saúde

```r
# Indicadores nacionais
brasil <- vigiar_baixar("tb_brasil")

# Todos os indicadores disponíveis
unique(brasil$Indicador)
unique(brasil$desfecho)

# Fração atribuível por desfecho
brasil |>
  filter(Indicador == "Fração atribuível (%)") |>
  select(desfecho, ano, est, low, high)

# Óbitos atribuíveis à poluição
brasil |>
  filter(grepl("Óbito", Indicador)) |>
  select(Indicador, desfecho, est, low, high)

# Indicadores por UF
uf <- vigiar_baixar("tb_uf")
# Indicadores por município  
muni <- vigiar_baixar("tb_muni")

# Fração atribuível detalhada
fracao <- vigiar_baixar("tb_fracao")
quartis <- vigiar_baixar("tb_quartis")
```

### Exposição Indoor

```r
indoor <- vigiar_baixar("df_indoor")

# Estados com maior % de combustíveis sólidos
indoor |>
  filter(Ano == max(Ano)) |>
  slice_max(percent_comb, n = 10)

# Desfechos de saúde indoor
indoor_saude <- vigiar_baixar("df_indoor_desfecho")
```

### População Exposta

```r
pop <- vigiar_baixar("pop")

# População por categoria de exposição
pop |>
  group_by(categoria, ano) |>
  summarise(pop_total = sum(pop, na.rm = TRUE) / 1e6,
            .groups = "drop")
```

## Funções

| Função | Descrição |
|--------|-----------|
| `vigiar_conectar()` | Estabelece sessão com o Power BI |
| `vigiar_info()` | Catálogo com descrições de todas as tabelas |
| `vigiar_tabelas()` | Lista nomes das tabelas |
| `vigiar_esquema(tabela)` | Mostra colunas e tipos |
| `vigiar_baixar(tabela, ...)` | Baixa uma tabela → tibble |
| `vigiar_baixar_tudo(tabelas)` | Baixa múltiplas tabelas |
| `vigiar_baixar_principais()` | Atalho para 14 tabelas principais |
| `vigiar_desconectar()` | Encerra sessão |
| `vigiar_sessao_ativa()` | Verifica se há sessão ativa |

## Como Funciona

```
vigiar_conectar()
  └─ GET app.powerbi.com → cookies + sessionId
  └─ GET /conceptualschema → 32 tabelas + colunas

vigiar_baixar("tabela")
  └─ Monta query SemanticQueryDataShapeCommand (JSON)
  └─ POST /querydata → resposta DSR comprimida (gzip)
  └─ Decodifica: ValueDicts + DM0 array + referências R
  └─ Retorna tibble

vigiar_desconectar()
  └─ Limpa cache
```

O pacote implementa o protocolo DSR (Data Shape Response) do Power BI:
comprime linhas consecutivas que compartilham colunas via referência (`R`),
e usa dicionários (`ValueDicts`) para colunas de texto.

## Licença

MIT · Dados: Ministério da Saúde / Datasus — VIGIAR

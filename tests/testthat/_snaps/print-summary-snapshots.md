# print.vigiar_tbl has a stable format

    Code
      print(construir_tbl())
    Output
      # VIGIAR tibble: df_anual  |  3 linhas x 2 colunas
      # Processado em: 2026-01-01
      
      # A tibble: 3 x 2
        cod_municipio pm25_media_anual
                <int>            <dbl>
      1        330455             12.3
      2        330010             10.1
      3            NA             NA  

# summary.vigiar_tbl has a stable format

    Code
      summary(construir_tbl())
    Output
      Resumo: df_anual
      -------------------------------------------------- 
      Linhas:  3
      Colunas: 2
      Classes: vigiar_pm25, vigiar_tbl, tbl_df, tbl, data.frame
      
      Valores ausentes:
        cod_municipio                  1 (33.3%)
        pm25_media_anual               1 (33.3%)


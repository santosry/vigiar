# vigiar 0.1.0

* Primeira versão do pacote.
* `vigiar_conectar()`: conexão com o dashboard Power BI do VIGIAR.
* `vigiar_baixar()`: download de tabelas individuais.
* `vigiar_baixar_tudo()`: download de todas as 32 tabelas.
* `vigiar_baixar_principais()`: atalho para as 14 tabelas mais usadas.
* `vigiar_info()`: catálogo completo com descrições e categorias.
* `vigiar_esquema()`: visualização da estrutura (colunas e tipos).
* `vigiar_tabelas()`: listagem de tabelas disponíveis.
* Parser DSR completo: ValueDicts, compressão por referência (R), DM0 array.
* Suporte a todos os tipos de dados do Power BI (Text, Double, Integer, DateTime, Boolean).
* Documentação completa das 32 tabelas: qualidade do ar, indicadores de saúde,
  exposição indoor, população, fração atribuível, quartis.
* Vignette com exemplos de análise para cada categoria de dados.
* Testes unitários com testthat.
* Compatibilidade: R >= 4.0.0.

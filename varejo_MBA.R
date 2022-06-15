# Big Data na Prática 3 - Data Science no Varejo com Market Basket Analysis (MBA)
# Busca de combinações/padrões

# Problema de negócio: Buscar padrões de compra
# Laboratório realizado em 21/05/2021
# Utilização do algoritmo apriori
# O algoritmo apriori utiliza 3 métricas:
# a) Support = Fração de transações que contem x e y: Support = Transações de x e y/total de transações
# b) Confidence = probabilidade de compra do item y quando x é comprado: Confidence = transações de x e y/ total de x
# c) Lift = indicador de quanto aumentou a confiança: lift=(transações de x e y/transações de x)/fração de transações em y
# o parametro minlen do apriori indica o comprimento mínimo da regra (número de itens)

# Configurando o diretório de trabalho

setwd("C:/Users/deeww/OneDrive/Área de Trabalho/Ciencia de dados/Curso Formação Cientista de dados/BigData_R_Azure_ML/cap05")
getwd()

# Carrega os pacotes
# https://cran.r-project.org/web/packages/arules/index.html
# https://cran.r-project.org/web/packages/arulesViz/index.html
library(dplyr)    # processar os dados
library(arules)   #pacote para regras de associação. Tem o algoritmo apriori
library(arulesViz)  #visualização das regras de associação
library(htmlwidgets)  #ajustes para os gráficos
library(writexl)   #gerar arquivos excel
options(warn=-1)   #filtra warning

# Carrega e explora o dataset
dados <- read.csv("dataset_bd3.csv")
dim(dados)
View(dados)
summary(dados)
str(dados)

# Uma forma inteligente de resolver o problema no dataset: Existem linhas em branco
# Separamos as linhas pares das linhas ímpares
# As linhas em branco são ímpares
linhas_pares <- seq(2, nrow(dados), 2)
linhas_impares <- seq(1, nrow(dados), 2)

# Separamos os dados e então usaremos o dataset com as linhas pares (linhas de dados válidos)
df1 <- dados[linhas_pares, ]
View(df1)
df2 <- dados[linhas_impares, ] 
View(df2)

# Verifica se temos valores ausentes no primeiro item de compra
sum(is.na(df1$Item01))

# Verifica se temos valores ausentes no segundo item de compra (ATENÇÃO)
sum(is.na(df1$Item02))
View(df1)

# Verifica se temos valores ausentes representados por espaço em branco
#trim remove os espaços
#which é uma pergunta: Existe?
which(nchar(trimws(df1$Item01))==0)
which(nchar(trimws(df1$Item02))==0)

# Verifica se temos valores ausentes representados por espaço em branco (usando expressão regular)
#caracter espaço: \s
grepl("^\\s*$", df1$Item02)

# Número de itens distintos
# Para buscar o padrão de compras só precisamos dos itens com mais de um produto
n_distinct(df1)

# Vamos trabalhar somente com os registros onde o item 2 não for nulo
df1_two <- df1[!grepl("^\\s*$", df1$Item02), ]

# Número de itens distintos (compras de 2 ou mais produtos)
n_distinct(df1_two)

# Prepara o pacote convertendo as variáveis para o tipo fator 
#Precisa usar todas colunas? NÃO... porque estão vazias
# (variáveis que usaremos daqui em diante) - 6 colunas
View(df1_two)
pacote <- df1_two
pacote$Item01 <- as.factor(pacote$Item01)
pacote$Item02 <- as.factor(pacote$Item02)
pacote$Item03 <- as.factor(pacote$Item03)
pacote$Item04 <- as.factor(pacote$Item04)
pacote$Item05 <- as.factor(pacote$Item05)
pacote$Item06 <- as.factor(pacote$Item06)
summary(pacote)
View(pacote)
str(pacote)

?split

pacote_split <- split(pacote$Item01, 
                      pacote$Item02,
                      pacote$Item03, 
                      pacote$Item04,
                      pacote$Item05, 
                      pacote$Item06)

View(pacote_split)

# Transações
?as
transacoes <- as(pacote_split, "transactions")

# Inspeção das regras
?inspect
inspect(head(transacoes, 5))

# Vamos verificar as regras de um produto: Dust-Off Compressed Gas 2 pack
?apriori  #espera receber os dados na classe transaction (utiliza-se a função as)
regras_produto1 <- apriori(transacoes, 
                           parameter = list(conf = 0.5, minlen = 3),
                           appearance = list(rhs = "Dust-Off Compressed Gas 2 pack", default = "lhs")) 

# Inspeção das regras
inspect(head(sort(regras_produto1, by = "confidence"), 5))

# Vamos verificar as regras de um produto: HP 61 ink
regras_produto2 <- apriori(transacoes,
                           parameter = list(minlen = 3, conf = 0.5),
                           appearance = list(rhs = "HP 61 ink",default = "lhs"))

# Inspeção das regras
inspect(head(sort(regras_produto2, by = "confidence"), 5))

# Vamos verificar as regras de um produto: VIVO Dual LCD Monitor Desk mount
regras_produto3 <- apriori(transacoes,
                           parameter = list(minlen = 3, conf = 0.5),
                           appearance = list(rhs = "VIVO Dual LCD Monitor Desk mount", default = "lhs"))

# Inspeção das regras
inspect(head(sort(regras_produto3, by = "confidence"), 5))

# Vamos verificar novamente as regras do produto: Dust-Off Compressed Gas 2 pack, 
# alterando uma das métricas
# lhs = antecedente
# rhs = consequente
regras_produto1 <- apriori(transacoes, 
                           parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = "rules"),
                           appearance = list(rhs = "Dust-Off Compressed Gas 2 pack", default = "lhs")) 

# Inspeção das regras
inspect(head(sort(regras_produto1, by = "confidence"), 5))

# Filtra as regras redundantes
regras_produto1_clean <- regras_produto1[!is.redundant(regras_produto1)]

# Inspeção das regras
inspect(head(sort(regras_produto1_clean, by = "confidence"), 5))

# Sumário
summary(regras_produto1_clean)

# Plot
plot(regras_produto1_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")

# Vamos verificar novamente as regras do produto: HP 61 ink,
# alterando uma das métricas
regras_produto2 <- apriori(transacoes,
                           parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = "rules"),
                           appearance = list(rhs = "HP 61 ink", default = "lhs"))

# Inspeção das regras
inspect(head(sort(regras_produto2, by = "confidence"), 5))

# Filtra as regras redundantes
regras_produto2_clean <- regras_produto2[!is.redundant(regras_produto2)]

# Inspeção das regras
inspect(head(sort(regras_produto2_clean, by = "confidence"), 5))

# Sumário
summary(regras_produto2_clean)

# Plot
plot(regras_produto2_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")

# Vamos verificar novamente as regras do produto: VIVO Dual LCD Monitor Desk mount,
# alterando uma das métricas
regras_produto3 <- apriori(transacoes,
                           parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = "rules"),
                           appearance = list(rhs = "VIVO Dual LCD Monitor Desk mount", default = "lhs"))

# Inspeção das regras
inspect(head(sort(regras_produto3, by = "confidence"), 5))

# Filtra as regras redundantes
regras_produto3_clean <- regras_produto3[!is.redundant(regras_produto3)]

# Inspeção das regras
inspect(head(sort(regras_produto3_clean, by = "confidence"), 5))

# Sumário
summary(regras_produto3_clean)

# Plot
plot(regras_produto3_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")

# Top 3 regras
inspect(head(sort(regras_produto1_clean, by = "support", decreasing = TRUE), 1))
inspect(head(sort(regras_produto2_clean, by = "confidence", decreasing = TRUE), 1))
inspect(head(sort(regras_produto3_clean, by = "confidence", decreasing = TRUE), 1))

# Salvamos o conjunto de regras dos 3 produtos como dataframe  e então salvamos em disco
View(regras_produto1_clean)
# Para converter o conjunto de regras em dataframe utiliza-se a função as
df_produto1 <- as(regras_produto1_clean, "data.frame")
View(df_produto1)
write_xlsx(df_produto1, "df_produto1.xlsx")

df_produto2 <- as(regras_produto2_clean, "data.frame")
View(df_produto2)
write_xlsx(df_produto2, "df_produto2.xlsx")

df_produto3 <- as(regras_produto3_clean, "data.frame")
View(df_produto3)
write_xlsx(df_produto3, "df_produto3.xlsx")





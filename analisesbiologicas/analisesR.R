#####################################################################################################
#####################################  PROJETO SENESCÊNCIA  #########################################
#####################################################################################################
install.packages('esquisse')


#importando pacotes necessários
library('ggplot2')#para gráfico
library('readxl')#para leitura de arquivo excel
library('dplyr')#para tratamento da base de dados
library('plotly')#para gráfico
library('esquisse')

#identificando pasta de trabalaho
setwd("C:/Users/artur/Documents/projeto senescencia")

#importando base de dados de excel
dados = read_xlsx('rosa_MM_turvsao4.emapper.annotations.xlsx')
View(dados)

#tratamento de dados
dados = select(dados, -c(1:5,8:21))#excluindo colunas
dados = slice(dados, -c(1:2))#excluindo linhas
dados = dados %>% filter(...7!='S')#excluir linha da coluna "...7" que tenham S
dados = dados %>% filter(...7!='-')#excluir linha da coluna "...7" que tenham -
#como eu conseguiria separar letras de mesma linha em linhas diferentes?
# Aplicar a função strsplit à coluna
split_chars <- strsplit(as.character(dados$...7), "")

# Transformar a lista de listas em um vetor
char_vector <- unlist(split_chars)

# Criar um novo dataframe
expanded_df <- data.frame(coluna = char_vector)
# Criar uma nova coluna em dados e atribuir os valores de expanded_df
dados$...7_nova <- ifelse(seq_along(dados$...7) <= length(expanded_df$coluna),
                          expanded_df$coluna,
                          NA)
dados = select(dados, -c(2))

#como eu consigo separar os dados 75678|eucariota em colunas distintas tipo 75678|   eucariota?
dados = tidyr::separate(dados,...6,into = c('coluna1','coluna2'),sep = "\\|", remove = FALSE)
dados = select(dados,-c(1:2))
dados = dados %>% filter(coluna2!='-')#exlcuindo linhas com conteúndo igual a '-'
dados = rename(dados, especie = coluna2, COGs=...7_nova)
#gráficos
#com ggplot2
ggplot(dados,aes(x=COGs, fill=especie))+
  geom_bar(stat = 'count')+
  labs(title = 'Número de COGs por reino',x='COGs', y ='contagem',fill='espécie')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
  coord_flip()



#####################################################################################################
#####################################  Análise Banco Census  ########################################
#####################################################################################################

#importando bibliotecas#

library(dplyr)#organização dos dados
library(ggplot2)#pacote para criação de gráficos
library(factoextra)#pacote para PCA
library(FactoMineR)#pacote para PCA

#entrando na pasta que contém o arquivo#

setwd("C:/Users/artur/Desktop/portifólio/Bases de dados")

#importando base de dados#

dados = read.csv('census.csv', sep = ',')

#tratamento dos dados#

summary(dados)#verificando existência de alguma inconsistência nos dados
str(dados)#verificando tipo dos dados
dados = rename(dados, idade = age, classe_de_trabalho=workclass,anos_estudados=education.num,ocupação=occupation,estado_civil=relationship,raça = race, sexo=sex,capital_ganho= capital.gain,
               hora_por_semana_trabalhada=hour.per.week,salario=income, cidade_natal=native.country, nivel_educacional=education)#renomeando colunas
dados = select(dados, -c(3,6,12))#excluindo colunas
sapply(dados, function(x) sum(is.na(x)))#verificando existência de valores nulos

#plotando gráficos#

#avaliar se a interação entre capital ganho, anos estudados e hora trabalhada é positiva ou negativa e qual influencia cada um
dados_numericos<- dados[,c(4,9,10)]
dados.pca <- prcomp(dados_numericos, scale.=TRUE)
fviz_pca_biplot(dados.pca, geom.ind = "point",
                habillage = dados$ocupação,
                addEllipses = TRUE, # Desabilita a adi??o de elipses
                ggtheme = theme_bw()) +
  scale_shape_manual(values = rep(16, length(unique(dados$ocupação)))) +
  scale_color_manual(values = c("#30123BFF", "#4686FBFF", "#1AE4B6FF",
                                "#A2FC3CFF", "#FABA39FF", "#E4460AFF",
                                "#7A0403FF"))
fviz_pca_biplot(dados.pca,
                label = "var", # Mostrar rótulos das variáveis.
                col.ind = dados$raça, 
                col.var = "black",
                ggtheme = theme_bw())

#analisando qual idade mais abrangente na análise
ggplot(dados, aes(x= idade))+
  geom_histogram(bins=25, fill='blue',color='white')+
  theme_classic()+
  labs(x='Idade',y='Contagem')

#analisando se os anos de estudo,hora trabalhada e o sexo tem influência sobre o capital ganho
ggplot(dados, aes(x=idade, y= anos_estudados, color=capital_ganho, size=capital_ganho, shape=sexo, alpha=hora_por_semana_trabalhada))+
  geom_point()+
  scale_color_binned(low='blue', high='yellow')+
  theme_classic()+
  labs(x= 'Idade', y='Anos de estudo', color= 'Capital Ganho', size= 'Capital ganho')

#avaliando a proporção de raça por estado civil
ggplot(dados, aes(x = estado_civil, fill = raça)) +
  geom_bar(position = "stack", color = "white")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(x='Estado civil', y='Contagem',fill='Raça')

#avaliando quanto a idade tem influência sobre o estado civil 
ggplot(dados, aes(x = idade, fill = estado_civil)) +
  geom_density(alpha = 0.5)+
  theme_classic()+
  labs(x='Idade',y='Densidade',fill='Estado civil')

#verificando quanto o pais de vivência com a raça e sexo influencia no capital ganho
ggplot(dados, aes(x=capital_ganho,y=cidade_natal, color=raça, size=idade, shape=sexo))+
  geom_point()+
  theme_classic()

    


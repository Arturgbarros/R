#####################################################################################################
#####################################  PROJETO PAGAMENTO E DÍVIDA  #########################################
#####################################################################################################
#importando bibliotecas
library(ggplot2)
library(dplyr)
library(esquisse)

#importando dados
dados = read.csv('credit_data_regras.csv', sep = ',')

#tratamento dos dados 
summary(dados)#verificando se não encontra nenhum dado fora padrão, encontrado idade mínima menor que 0
dados = rename(dados, salario = income, divida = loan, pagamento = c.default)#mudando nome das colunas
dados = dados %>% filter(age>0)#filtrando coluna 'age' apenas com valores maiores de 0

#####plotagem de gráfico#####
#verificar qual melhor forma de plotar os gráficos e em quais parâmetros utilizar cada amostra
esquisser(dados)
#verificar quantidade de cada idade
ggplot(dados, aes(x=age))+
  geom_histogram(bins = 30, fill = 'blue', color = 'red')+
  theme_classic()+
  labs(x='idade', y= 'contagem')
#verificar onde está a maior concentração do valor das dividas e se existem outliers
ggplot(dados, aes(x=' ', y = divida))+
  geom_boxplot(width = .3, outlier.color = 'purple')+
  theme_classic()+
  labs(x ='', y = 'Divida')
#verificar em qual idade está a maior concentração de dividas pagas e não pagas
ggplot(data = dados, aes(x = age, y = divida, color = factor(pagamento))) +
  geom_col(position = 'identity') +
  scale_color_manual(values = c("1" = "red", "0" = "blue"), labels = c("Pagou", "Não pagou")) +
  theme_classic() +
  labs(x = 'idade', y = 'valor dívida', color='pagamento') +
  guides(color = guide_legend(override.aes = list(fill = alpha("white", 0))))
#verificar se há alguma relação entre salarios mais altos, dividas e idade
ggplot(dados, aes(x = age, y = salario, color = divida)) +
  geom_point() +
  scale_color_gradient(low = "lightcoral", high = "darkred") +
  theme_classic()+
  labs(x='idade', y ='salario', color='valor divida')

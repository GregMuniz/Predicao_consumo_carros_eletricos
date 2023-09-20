library(readxl)
Eletric_Cars <- read_excel('consumo_carros_eletricos/FEV-data-Excel.xlsx')

View(Eletric_Cars)


#Renomear nome das colunas
nome_colunas <- c('Nome_Carro', 'Marca', 'Modelo', 'Preco_Minimo', 'Poder_Motor', 'Torque_Maximo',
                  'Tipo_Freio', 'Tracao', 'Capacidade_Bateria', 'AutonomiaKM', 'Distancia_Eixos',
                  'Comprimento', 'Largura', 'Altura','Peso_Vazio_Minimo', 
                  'Peso_Bruto_Permitido', 'Capacidade_Maxima_Carga', 'Numero_Assentos',
                  'Numero_Portas', 'Tamanho_Pneu', 'Velocidade_Maxima', 'Capacidade_Porta-Malas',
                  '0-100KPH_Segundos', 'Potencia_Maxima_CarregamentoRapido', 'Consumo_Medio_Energia')

colnames(Eletric_Cars) <- nome_colunas

summary(Eletric_Cars)

sum(is.na(Eletric_Cars))

#Além do dataset possuir poucas linhas(53), mais da metade possuem algum valor faltante
#Logo preciso achar uma forma de tratar esses dados sem exclui-los se possivel.

#Irei fazer uma analise bivariada de correlação entre as variáveis preditoras e a variavel alvo
#E depois uma correlação entre as variáveis preditoras para saber quais colunas
#posso/devo excluir do dataset pois o mesmo possui muitas colunas para poucas linhas
#E valores missing.

#Separar as variaveis por categoria e gravar em um novo objeto
categ <- Eletric_Cars[, sapply(Eletric_Cars, is.character)]
numeric <- Eletric_Cars[, sapply(Eletric_Cars, is.numeric)]

#histograma para cada variavel numerica
for (i in colnames(numeric)){
  hist(numeric[[i]], main = paste('Histograma de', i), xlab = i, col = 'green')
}

library(ggplot2)
library(corrgram)

#Correlação entre as variáveis preditoras
matriz_corr <- cor(numeric, use = 'complete.obs')
corrgram(numeric, order = T, upper.panel = panel.pie)


#Além de ter muitas variáveis preditoras, elas também estão com fortes correlações
#entre si, logo terei realmente que excluir varias colunas desse dataset

#Como existem muitas variáveis que estão com uma forte correlação com a variável alvo
#Começarei excluindo as variáveis com baixa correlação com a mesma.
#Depois excluirei as colunas que estão representando informações muito parecidas,
#Deixarei apenas uma delas as que representarem uma correlação maior com a variavel alvo
#e que não possuam valores missing


#Exclusão das variaveis com menores correlações com a variável alvo
excluidas <- c('Altura', 'Numero_Portas', 'Numero_Assentos', 'Capacidade_Maxima_Carga')
Eletric_Cars2 <- Eletric_Cars[, setdiff(names(Eletric_Cars), excluidas)]

#Exclusão das variáveis 0-100segundos por ter valores missing e alta corrrelação com
#quase todas as variáveis preditoras e peso bruto permitido por ter valores missing 
#e possuir uma variável com informação muito parecida, beirando ao 100% de correlação
#E capacidade do porta-malas pois acredito que essa variável não terá muita relevancia
#e possui 1 valor missing que eu poderia facilmente substituir pela media por exemplo,
#esse dataset tem muitas colunas para tão poucas linhas logo quanto menos variáveis melhor
Eletric_Cars2 <- Eletric_Cars2[c(-15, -18, -19)]
summary(Eletric_Cars2)

#visualizar as linhas que ainda possuem valores NA
missing <- Eletric_Cars2[! complete.cases(Eletric_Cars2), ]
View(missing)

#sete dos nove valores faltantes na variável alvo são da marca tesla, o problema é que
#não possui nem um registro dessa marca com informação do consumo para tentar preencher
#os demais com base no que possuissem a informação.
#Logo terei de excluir as linhas com esse modelo pois de nada adianta criar um modelo
#supervisionado sem a informação antecedente a predição, e não posso colocar qualquer
#valor e afetar completamente o resultado do modelo sem saber se está correto
#Só se eu pegasse outras informações das demais colunas dessa marca e comparasse com
#as de outras marcas para tentar chegar em um valor que se aproximasse da realidade.
#Também poderia fazer uma pesquisa na internet para descobrir a media de consumo dos
#modelos em questão, mais irei trabalhar como se essas informações não estivessem disponiveis

#Visualizando a coluna de consumo apenas dos carros da tesla
Eletric_Cars2$Consumo_Medio_Energia[Eletric_Cars2$Marca == 'Tesla']

#Já que essa marca está com todos os valores NA da variavel alvo, irei exclui-lá 
#do dataset e ver como o modelo se sai.

Eletric_Cars_sem_tesla <- subset(Eletric_Cars2, Marca != 'Tesla')

library(dplyr)

#Quantos valores Nas restaram
nulos_restantes <- Eletric_Cars_sem_tesla %>%
  filter_all(any_vars(is.na(.)))
View(nulos_restantes)

#Irei pegar cada um dos 3 valores missing faltantes e puxar todos os modelos da mesma
#marca e comparar as informações e medias para preencher se possível com o valor mais 
#próximo do real.

#Citroen
Citroen <- Eletric_Cars_sem_tesla %>%
  filter(Marca == 'Citroën')
View(Citroen)

#Pegando todas as linhas que possuem o poder de motor e torque iguais a do valor faltante
Comparacao_modelos <- Eletric_Cars_sem_tesla %>%
  filter(Poder_Motor == 136 & Torque_Maximo == 260)
View(Comparacao_modelos) 

#Média do consumo dos modelos que possuem o mesmo poder de motor e mesmo torque,
#Retirando o valor na e o que possui um peso e consumo bem maior
mean(Comparacao_modelos$Consumo_Medio_Energia
     [Comparacao_modelos$Nome_Carro != 'Citroën ë-C4' & 
         Comparacao_modelos$Nome_Carro != 'Citroën ë-Spacetourer (M)'])
  
#16.45 esse é o valor que irei preencher no Citroen  
Eletric_Cars_sem_tesla[Eletric_Cars_sem_tesla$Nome_Carro == 'Citroën ë-C4',
                       "Consumo_Medio_Energia"] <- 16.45

#Poder do motor e o torque são os mesmos para os dois modelos disponiveis da marca
#mas o que está faltando o valor do consumo é mais leve logo deve gastar menos energia
#Poderia colocar um consumo um pouco mais abaixo do outro mais não sei se isso é uma
#boa ideia já que a chance de ser um valor irreal é grande e o modelo estaria aprendendo
#errado

#Peugeot
Peugeot <- Eletric_Cars_sem_tesla %>%
  filter(Marca == 'Peugeot')
View(Peugeot)

#os 2 modelos da peugeot possuem os valores muito mais parecidos sendo o que não possui
#valor na variavel alvo um pouco mais pesado, como o modelo um pouco mais leve possui
#consumo de 16.4 irei atribuir 17 exato a essa linha, acredito que ficará bem proximo,
#do valor real
Eletric_Cars_sem_tesla[Eletric_Cars_sem_tesla$Modelo == 'e-2008', 
                       'Consumo_Medio_Energia'] <- 17.0


#O ultimo valor missing está na coluna tipo freio da qual acredito não ser tão relevante
#Para o nosso objetivo logo irei exclui-la mais para frente.



numeric <- Eletric_Cars_sem_tesla[, sapply(Eletric_Cars_sem_tesla, is.numeric)]
View(numeric)

variaveis_preditoras <- numeric[-13]
variavel_alvo <- numeric[13] 
predictor_names <- colnames(numeric[-13])


#Scatter plots mostrando a relação das variaveis preditoras com a alvo, as que não
#tiverem uma boa relação já irei descarta-lás.
for (predictor in predictor_names) {
  plot <- ggplot(numeric, aes_string(x = predictor, y = "Consumo_Medio_Energia")) +
    geom_point(color = "blue", alpha = 0.7) +
    labs(title = paste("Scatter Plot:", predictor, "vs Consumo Médio de Energia"),
         x = predictor,
         y = "Consumo Médio de energia")
  print(plot)
}

#carregamento_rapido
#Tamanho_Pneu
#Autonomia
#Não possuem nenhuma relação com nossa variável target, portanto irei removê-las
Eletric_Cars_sem_tesla <- Eletric_Cars_sem_tesla %>%
  select(-AutonomiaKM, -Tamanho_Pneu, -Potencia_Maxima_CarregamentoRapido)



#Comprimento e distancia entre eixos são informações bastante parecidas e irei 
#retirar as duas na verdade, porque medidas para o nosso problema só seria relevante
#se não tivessemos o peso do carro que definem as proporções do veiculo que junto
#com outros fatores da o peso total do carro.
Eletric_Cars_sem_tesla <- Eletric_Cars_sem_tesla %>%
  select(-Largura, -Comprimento, -Distancia_Eixos)

#Optei por retirar velocidade maxima por ter uma correlação muito alta com outras 
#variáveis preditoras, e torque maximo que possui uma informação bastante parecida
#com o poder do motor pois o torque faz parte do poder do motor junto com os cavalos
#e preco minimo por ter tbm uma forte correlação com poder motor que é uma variável que 
#quero priorizar na construção e acredito que seja menos relevante.
#e capacidade da bateria, tem uma forte relação com peso vazio e acredito que não
#será tão relevante e nesse dataset tão pequeno menos é mais

Eletric_Cars_sem_tesla2 <- Eletric_Cars_sem_tesla %>%
  select(-Velocidade_Maxima, -Torque_Maximo, -Preco_Minimo, -Capacidade_Bateria)

#Agora uma rápida analise nas variaveis categoricas que acredito não serão muito
#relevantes


#Irei criar um scatterplot para cada variavel categórica relacionado com a alvo
ggplot(Eletric_Cars_sem_tesla2, aes(x = Tracao, y = Consumo_Medio_Energia)) +
  geom_point() +
  labs(title = 'Relação do Consumo para cada tipo de Tração', 
       x = 'Tração', y = 'Consumo Medio de Energia')

#Interessante parece que 4wd que seria tração nas quatro rodas aumenta o consumo
#de energia, pelo menos de acordo com as informações do scaterplot, é claro
#que outras coisas poderiam estar afetando esse resultado como todos os carros
#com o motor mais forte possuem tração nas quatro rodas, mas com uma pesquisa 
#adicional na internet, parece que realmente carros com tração 4x4 gastam
#mais energia, logo essa variável será importante na construção do meu modelo.

ggplot(Eletric_Cars_sem_tesla2, aes(x = Marca, y = Consumo_Medio_Energia)) +
  geom_point() +
  labs(title = 'Relação do Consumo para cada marca de veiculo', 
       x = 'Marca', y = 'Consumo Médio de Energia')
#Observando o gráfico não consegui perceber uma relação, com excessão de uma ou duas
#marcas, mais no geral seguem parecidas.

ggplot(Eletric_Cars_sem_tesla2, aes(x = Tipo_Freio, y = Consumo_Medio_Energia))+
  geom_point() +
  labs(title = 'Relação do Consumo para cada Tipo do freio',
       x = 'Tipo do Freio', y = 'Consumo Médio de Energia')
#O primeiro disco de freio possui valores com consumos maiores mas a maioria
#dos dados estão nessa categoria logo acredito não ter relação, essa variável
#será removida

#Das variáveis categóricas irei usar apenas a Tração

Eletric_Cars_sem_tesla2 <- Eletric_Cars_sem_tesla2[, c(-1, -2, -3, -5)]
View(Eletric_Cars_sem_tesla2)

#Sobrou apenas 3 variáveis preditoras, vamos ver como elas se saem na predição
#do modelo, poderia criar novas variáveis com base nas varias variaveis disponiveis
#mas irei para uma primeira versão usar apenas essas mesmo

#Salvar o dataset com todas as devidas  transformações
write.csv(Eletric_Cars_sem_tesla2, file = 'C:/Users/greg/ProjetosML/Eletric_Cars')


###---Pré-Processamento de dados---###

#Irei usar one hot encoding para variável categórica nominal
Eletric_Cars_Final <- data.frame(model.matrix(~ .-1, data = Eletric_Cars_sem_tesla2))
View(Eletric_Cars_Final)

Eletric_Cars_sem_tesla2$Tracao <- as.factor(Eletric_Cars_sem_tesla2$Tracao)
Eletric_Cars_Final2 <- Eletric_Cars_sem_tesla2
levels(Eletric_Cars_Final2$Tracao) <- c(0, 1, 2)

#Normalização
normalizacao <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
  
}

#Cria um dataframe aplica um loop passando por cada linha das colunas especificadas
#e aplica a função de normalização e grava em um novo objeto
dados_norm <- as.data.frame(lapply(Eletric_Cars_Final[c(1, 5)], normalizacao))
colunas_norm <- c('Poder_Motor', 'Peso_Vazio_Minimo')
Eletric_Cars_Final[colunas_norm] <- dados_norm


###---Criação do modelo de ML---###

#1* Modelo: Regressão Linear 

library(caTools)

#Divisão dos dados
proporcao_treino <- 0.75

indices <- sample.split(Eletric_Cars_Final$Consumo_Medio_Energia, 
                        SplitRatio = proporcao_treino)

dados_treino <- subset(Eletric_Cars_Final, indices == TRUE)
dados_teste <- subset(Eletric_Cars_Final, indices == FALSE)


Linear_Regression_Model_V1 <- lm(Consumo_Medio_Energia ~., data = dados_treino)

summary(Linear_Regression_Model_V1)
#Uma das variáveis ficou com valores NAs nos coeficientes o que indica que o
#modelo não conseguiu calcular os coeficientes, isso ocorre principalmente
#por colinearidade ou falta de graus de liberdade.


#Pelo resumo estatistico apenas uma variável teve um nível alto de significancia 
#que foi o Peso, poder do motor da qual eu achava que seria a mais importante
#teve um p-value muito alto ou seja a probabilidade de que a variável NÃO
#seja relevante é bastante alta.

#Irei criar um modelo de regressão linear simples apenas com a variável peso
#e ver como ele se sai.


Linear_Regression_Model_V2 <- lm(Consumo_Medio_Energia ~ Peso_Vazio_Minimo,
                                 data = dados_treino)
summary(Linear_Regression_Model_V2)

#O modelo teve uma performance um pouco inferior mais como foi bem pouco e esse modelo 
#é mais simples por enquanto é o que irei usar.


#Na terceira versão do modelo irei mudar o método de conversão da variável categórica
Eletric_Cars_sem_tesla2$Tracao <- as.factor(Eletric_Cars_sem_tesla2$Tracao)
Eletric_Cars_Final2 <- Eletric_Cars_sem_tesla2
levels(Eletric_Cars_Final2$Tracao) <- c(0, 1, 2)


dados_norm2 <- as.data.frame(lapply(Eletric_Cars_Final2[c(1, 3)], normalizacao))
Eletric_Cars_Final2[colunas_norm] <- dados_norm2

dados_treino2 <- subset(Eletric_Cars_Final2, indices == TRUE)
dados_teste2 <- subset(Eletric_Cars_Final2, indices == FALSE)


Linear_Regression_Model_V3 <- lm(Consumo_Medio_Energia ~ ., data = dados_treino2)
summary(Linear_Regression_Model_V3)
#O ajuste resolveu o problema do valor NA no coeficiente da variável, mais a 
#performance continuou a mesma.

#Versão usando duas variáveis 
Linear_Regression_Model_V4 <- lm(Consumo_Medio_Energia ~ Tracao + Peso_Vazio_Minimo,
                                 data = dados_treino2)
summary(Linear_Regression_Model_V4)


#irei criar uma versão usando algumas variáveis que não foram usadas 

#Separação das variáveis 
Eletric_Cars_Final3 <- Eletric_Cars_sem_tesla[, -c(1, 2, 3, 7, 8)]
View(Eletric_Cars_Final3)

#Normalização
dados_norm3 <- as.data.frame(lapply(Eletric_Cars_Final3[-7], normalizacao))
colunas_norm2 <- colnames(dados_norm3)
Eletric_Cars_Final3[colunas_norm2] <- dados_norm3

#Divisão
dados_treino3 <- subset(Eletric_Cars_Final3, indices == TRUE)
dados_teste3 <- subset(Eletric_Cars_Final3, indices == FALSE)

#Criação da versão 5 do modelo
Linear_Regression_Model_V5 <- lm(Consumo_Medio_Energia ~ ., data = dados_treino3)
summary(Linear_Regression_Model_V5)


###ESCOLHENDO A MELHOR VERSÃO###
#A melhor versão do modelo para esse algoritmo foi a v4 com apenas duas variáveis,
#Peso e Tração, a última versão usando mais variáveis teve a mesma performance 
#que as demais, e a versão 2 usando apenas a variável peso teve uma performance abaixo
#como esse dataset é muito pequeno se eu usar um algoritmo mais complicado como
#Randon forest de regressão por mais que consiga uma performance melhor o modelo
#irá sofrer de overfitting

#Predições com o modelo escolhido
dados_teste_final <- dados_teste2[-1]
Linear_Regression_predict <- predict(Linear_Regression_Model_V4, dados_teste_final)
Linear_Regression_predict

#Cria um dataframe com os dados reais e os previstos
comparação <- data.frame(Valores_Reais = dados_teste_final$Consumo_Medio_Energia,
                         Valores_Previstos = Linear_Regression_predict)
comparação

#Plota em um gráfico de dispersão com a linha de regressão e o intervalo de confiança
ggplot(comparação, aes(x = Valores_Reais, y = Valores_Previstos)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'blue') +
  labs(x = 'Valores Reais', y = 'Valores Previstos', 
       title = 'Comparação entre valores reais e previstos')

saveRDS(Linear_Regression_Model_V4, file = 'Modelo_Regressao_Carros_Eletricos.rds')

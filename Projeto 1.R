---
 # title: "Relat�rio data_GRU"
#output: html_document
---
# Pr� processamento dos dados
# Leitura de dados   



setwd("C://Users//bella//Documents//Mestrado//Projeto")
data.gru <- read.csv("dados_gr.csv", sep = ";")
data.gru$Data<- NULL
data.gru$Movimenta��es <- NULL
head(data.gru)
tail(data.gru)


#Passo 2: Omitir dados n�o num�ricos e fazer teste
data.gru1 <- na.omit(data.gru)
na.fail(data.gru1)


#Passo 3: Gerar gr�ficos  para analisar comportamentos
par(mfrow= c(4,1))
plot(data.gru1[,2], type="l", xlab = "x" , ylab = "Frequ�ncia" , main = "Cancelados")
plot(data.gru1[,3], type="l", xlab = "x" , ylab = "Frequ�ncia" , main = "Realizados")
plot(data.gru1[,4], type="l", xlab = "x" , ylab = "Frequ�ncia" , main = "Realizados com atraso")
plot(data.gru1[,5], type="l", xlab = "x" , ylab = "Frequ�ncia" , main = "Planejados")


#Identificar outliers por coluna, encontrar m�nimos e m�ximos e ordenar a coluna em crescente e decrescente para analisar os dados.
# V�os Cancelados
min(data.gru1$Cancelado)
max(data.gru1$Cancelado)

novo.vet1 <-order(data.gru1$Cancelado, decreasing = T)
head(novo.vet1, n=10)
data.gru1[novo.vet1, 2]

novo.vet2 <-order(data.gru1$Cancelado, decreasing = F)
head(novo.vet2, n=10)
data.gru1[novo.vet2, 2]


#V�os realizads
min(data.gru1$Realizado)
max(data.gru1$Realizado)

novo.vet3 <-order(data.gru1$Realizado, decreasing = TRUE)
head(novo.vet3, n=10)
data.gru1[novo.vet3, 3]

novo.vet4 <-order(data.gru1$Realizado, decreasing = F)
data.gru1[novo.vet4, 3]
head(novo.vet4, n=10)


#V�os realizados com atraso
min(data.gru1$Realizado.com.atraso)
max(data.gru1$Realizado.com.atraso)

novo.vet5 <-order(data.gru1$Realizado.com.atraso, decreasing = TRUE)
head(novo.vet5, n=6)
data.gru1[novo.vet5, 4]

novo.vet6 <-order(data.gru1$Realizado.com.atraso, decreasing = F)
head(novo.vet6, n=6)
data.gru1[novo.vet6, 4]


#Retirando outliers
data.gru2 <- data.gru1
data.gru2<-data.gru2[!(data.gru2$"Realizado">=750 | 
                         data.gru2$"Realizado"<=450 |
                         data.gru2$"Realizado.com.atraso">=350 |
                         data.gru2$"Cancelado" > 205), ]

# Na coluna de datas, converter factor em data
head(data.gru2)
Date <- data.gru2[,1]
Date <- as.Date(Date, format = "%d/%m/%Y")
head(Date)
summary(data.gru2)

#Processamento de dados
#V�os Planejados 
Planejados <- data.gru2$Total.geral 
Planejados = data.gru2$Cancelado + data.gru2$Realizado + data.gru2$Realizado.com.atraso

#Porcentagem de falhas nos v�os (cancelados + realizados com atraso) / total geral
data.fails<- (data.gru2$Cancelado+data.gru2$Realizado.com.atraso) / data.gru2$Total.geral
windows()
plot(data.fails, type = "l")

#Verificar se a s�rie temporal � estacion�ria
library(tseries)
adf.test(data.fails)
#O valor p sugere que os dados s�o muito improv�veis, dada a hip�tese nula, 
#ent�o acredita-se na hip�tese alternativa (estacion�ria).

#Converter em data frame
data.matrix <- matrix(data.fails, length(data.fails), 1)
colnames(data.matrix)= c("Values") #nomeando a coluna
data.matrix = data.frame(data.matrix)


#Elbow m�todo, para identificar o n�mero ideal de clusters para o dataset
k.max <- 15
data <- data.matrix
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=20,iter.max = 15 )$tot.withinss})
wss
windows()
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#O gr�fico sugere estar entre quatro e cinco clusters, entretanto a partir de 5 clusters aparentemente n�o h� mudan�as significativas.

#Aplica��o de quatro diferentes algor�timos para clusterizar o dataset: HMM, SOM, K-means e hierarchical clustering               
#Primeiro algor�tmo: Cadeias de Markkov Escondidas (HMM, Hidden Markov Models)
set.seed(33)
library(depmixS4)

#Curva gaussian para testar os melhores valores loglikelihood (necess�rio menor BIC- Bayesian Information Criterion(n�mero de par�metros))
#Modelos com n�meros de estados diferentes
hmm.model2 <- depmix(Values~1, family = gaussian(), #criar modelo
                    nstates = 2, data=data.matrix)

hmm.fit2 <- fit(hmm.model2, verbose=F) #encontra os melhores par�metros (valores para os par�metros)

post.prob2 <- posterior(hmm.fit2)
matplot(post.prob2[,-1], type = "l", ylab = "Probability", xlab= "Days", main = "Posterior Prob. States")


hmm.model3 <- depmix(Values~1, family = gaussian(), #criar modelo
                    nstates = 3, data=data.matrix)
hmm.fit3 <- fit(hmm.model3, verbose=F) #encontra os melhores par�metros (valores para os par�metros)
post.prob3 <- posterior(hmm.fit3)
matplot(post.prob3[,-1], type = "l", ylab = "Probability", xlab= "Days", main = "Posterior Prob. States")


hmm.model4 <- depmix(Values~1, family = gaussian(), #criar modelo
                    nstates = 4, data=data.matrix)
hmm.fit4 <- fit(hmm.model4, verbose=F) #encontra os melhores par�metros (valores para os par�metros)
post.prob4 <- posterior(hmm.fit4)
matplot(post.prob4[,-1], type = "l", ylab = "Probability", xlab= "Days", main = "Posterior Prob. States")

hmm.model5 <- depmix(Values~1, family = gaussian(), #criar modelo
                    nstates = 5, data=data.matrix)
hmm.fit5 <- fit(hmm.model5, verbose=F) #encontra os melhores par�metros (valores para os par�metros)
post.prob5 <- posterior(hmm.fit5)
windows()
matplot(post.prob5[1:60,-1], type = "l", ylab = "Probability", xlab= "Days", main = "Posterior Prob. States")
legend(x='topleft', c('Regime 1','Regime 2', 'Regime 3', 'Regime 4', 'Regime 5'), fill=1:5, bty='n')
#__________________________________________


#Plotar gr�fico para confirmar se o n�mero ideal de clusters para HMM � o mesmo
plot(2:5, c(BIC(hmm.fit2),BIC(hmm.fit3), BIC(hmm.fit4),BIC(hmm.fit5)),
            ty = "b", ylab = "Bayesian")

#Plotar gr�ficos hmm.fit5
library(TTR) 
library(ggplot2)
library(reshape2)


#Adicionando a coluna Data.1 ao data
data.matrix.date <- data.matrix
data.matrix.date["Date"] <- c(Data.1)
data.matrix.date = data.matrix.date[c(2,1)]


dfu <- cbind(data.matrix.date, post.prob5[,2:6])
#Colocando em um longo formato
dfu<- melt(dfu, id="Date")

#Plotando o dataset com s�ries temporais de probabilidades
windows()
qplot(Date, value, data=dfu, geom ="line",
      main = paste("States"),ylim = c(0, 1),
      ylab = "State Probabilities") +
  facet_grid(variable~., scales = "free_y") + theme_bw()

 

#Curva gamma para testar os melhores valores loglikelihood (necess�rio menor BIC- Bayesian Information Criterion)
#Modelos com n= 2.
hmm.model.gamma2 <- depmix(Values~1, family = Gamma(link="log"), #criar modelo
                    nstates = 2, data=data.matrix)

hmm.fit.gamma2 <- fit(hmm.model.gamma2, verbose=F) #encontra os melhores par�metros (valores para os par�metros)
#A curva gamma apresentou valor BIC bastante alto, logo foi descartada, o projeto continuar� seguindo com a curva normal. 


#Verificar a lista de regimes no data frame e suas probabilidades de pertencer a cada regime e verificar se 5 os regimes s�o significantes. 
posterior(hmm.fit5)
tail(posterior(hmm.fit5), n=100)
#hmm.fit5 foi o que melhor se adequou para este dataset, logo 5 foi o n�mero de regimes escolhido para o o algor�tmo de HMM.



############################################### MAPAS AUTO-ORGANIZ�VEIS (SOM) ##############################
library(kohonen)
library(dummies)
library(ggplot2)
library(sp)
library(reshape2)
library(rgeos)

set.seed(323)

#Defini��o cores paleta
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

#Costumizando a paleta para o pacote kohonen
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
#Em kohonen as linhas dos dados s�o amostras e as colunas vari�veis
data.matrix.som <- (data.matrix)

#Tornando o data set uma matriz
#Centralizar e dimensionar todas as vari�veis para dar-lhes igual import�ncia durante
# o processo de treinamento do SOM (scale).
data.matrix.som = as.matrix(scale(data.matrix.som))

#Criar a Grade SOM  geralmente se especifica o tamanho da grade de treinamento antes de treinar.
#Hexagonal e Circular,s�o tpologias poss�veis.

som.grid <- somgrid(xdim = 15, ydim=15, topo = "hexagonal")

#Modelo: op��es para o n�mero de itera��es (rlen), as taxas de aprendizagem(alpha)
som.model <- som(data.matrix.som, grid=som.grid, rlen=15000, alpha=c(0.05,0.01), keep.data =TRUE)

#Gr�ficos, visualiza��o
#1 Progresso de Treinamento:
#� medida que as itera��es de treinamento do SOM progridem, a dist�ncia dos pesos de cada n� �s amostras
#representadas por esse n� � reduzida. Idealmente, essa dist�ncia deve atingir um patamar m�nimo.
#Esta op��o de plotagem mostra o progresso ao longo do tempo. Se a curva estiver diminuindo continuamente,
#mais itera��es ser�o necess�rias.
windows()
plot(som.model, type="changes")               

#Permite visualizar a contagem de quantas amostras s�o mapeadas para cada n�. Essa m�trica pode ser usada 
#como uma medida da qualidade do mapa - idealmente, a distribui��o da amostra � relativamente uniforme.
#N�s vazios indicam que o tamanho do seu mapa � muito grande para o n�mero de amostras. 
windows()
plot(som.model, type= "count", main="Node Counts", palette.name=coolBlueHotRed) 

#Mapa de qualidade, um bom resultado o mostra quase uniforme
windows()
plot(som.model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)

#Permite visualizar a dist�ncia entre cada n� e seus vizinhos. As �reas de baixa dist�ncia vizinha indicam grupos de n�s semelhantes. 
#�reas com grandes dist�ncias indicam que os n�s s�o muito mais dissimilares                
windows()
plot(som.model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)

#Os vetores de peso do n�, ou "c�digos", s�o compostos de valores normalizados das vari�veis originais usadas para gerar o SOM. 
#O vetor de pesos de cada n� � representativo,semelhante das amostras mapeadas para esse n�. Ao visualizar os vetores de peso no mapa, 
#pode-se ver padr�es na distribui��o de amostras e vari�veis
windows()
plot(som.model, type="codes")


# Plotar heatmap com as vari�veis scaled/normalizadas
windows()
plot(som.model, type = "property", property = som.model$codes[[1]], main= "Fails", palette.name=coolBlueHotRed)

#Potar heatmap para a vari�vel original
var <-1
var.unscaled <- aggregate(as.numeric(unlist(data.matrix)), by=list(som.model$unit.classif), FUN=mean, simplify=TRUE)[,2]
windows()
plot(som.model, type = "property", property=var.unscaled, main="Fails", palette.name=coolBlueHotRed)
rm(var.unscaled, var)


# Clustering os resultados 

# Mostra o WCSS m�trica para k-means para diferentes tamanhos de clustering, confirmar se entre 3 e 5 tamb�m podem
#ser tamanhos de clusters ideais
mydata <-unlist(som.model$codes)
mydata <- as.matrix(mydata)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2, var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
windows()
par(mar=c(5.1,4.1,4.1,2.1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")


# Formando grade de clusters
## Usando HC do agrupar codebook vectors
som.cluster <- cutree(hclust(dist(mydata)), 5)

# Mostrar gr�fico com clusters em diferentes cores	
windows()
plot(som.model, type="mapping", bgcol = pretty_palette[som.cluster], main = "Clusters"  )
add.cluster.boundaries(som.model, som.cluster)

#Mostrar o mesmo gr�fico adicionado os c�digos
windows()
plot(som.model, type="codes", bgcol = pretty_palette[som.cluster], main = "Clusters")
add.cluster.boundaries(som.model, som.cluster)
#Cinco clusters mostrou-se uma boa quantidade de clusters

# Stacking Hierarchical CLustering e SOM
som.cluster <- cutree(hclust(dist(as.vector(unlist(som.model$codes)))), 5)

# Stacking K-Means and SOM

similarity.vec <- as.vector(unlist(som.model$codes))
kmeans.model.som <- kmeans(similarity.vec, 5, iter.max = 100)


#Decodifica��o dados reais
decoded.hc<- som.cluster[som.model$unit.classif]
decoded.kmeans <- kmeans.model.som$cluster[som.model$unit.classif]

##########################################K-M�DIAS############################################

library(stats)
set.seed(2)

#Criar modelo
model.kmeans <- kmeans(data.matrix, 5, nstart = 20)#5 centr�ides, o r ir� tentar  20 come�os diferentes
windows()
plot(data.matrix.date, col=model.kmeans$cluster)
points(model.kmeans$centers, col="blue", pch=8, cex=2)


#Plotar gr�ficos
library(cluster)
library(fpc)

#Converter dataset em factor
data.matrix.fail <- data.matrix
colnames(data.matrix.fail) = c("Fail")
data.matrix.fac=as.factor(unlist(data.matrix.fail))

#Plotar gr�ficos
library(ggplot2)
model.kmeans$cluster <- as.factor(unlist(model.kmeans$cluster))
windows()
ggplot(data.matrix.fail, aes( Date, Fail, color = model.kmeans$cluster, ylab= "Fail")) + geom_point() 

##############################################AGRUPAMENTO HIER�RQUICO####################################
#Como foi utilizado o m�todo Elbow anteriormente no dataset, manteve-se a divis�o em 5 clusters
library(stats)
library(cluster)

model.hc <- hclust(dist(data.matrix))
windows()
plot(model.hc, cex = 0.6,  hang=-1)
# Dividir em 5 clusters
hc.cut <- cutree(model.hc, 5)
rect.hclust(model.hc, k=5, border=2:6)
table(hc.cut)


# Utilizando single-linkage 
clusters1 <- hclust(dist(data.matrix),method="single")
# Dividindo em 5  clusters
clusterCut1 <- cutree(clusters1, 5)
table(clusterCut1)
#Single n�o � uma boa escolha para esse conjunto de dados


# Utilizando average-linkage 
clusters2 <- hclust(dist(data.matrix),method="average")
# Dividindo em 5 clusters
clusterCut2 <- cutree(clusters2, 5)
table(clusterCut2)
#Average apesar de ser melhor que single, n�o mostrou a mesma 
#efici�nca que o m�todo complete, portanto complete foi o m�todo
#escolhido.

######################################### Ensemble ####################################

#Juntando clusters em um dataset
data.ens <- data.matrix

Date <- Data.1
data.ens["Date"] <- c(Date)

HMM <- (hmm.fit5@posterior$state)
data.ens["HMM"] <- c(HMM)

SOM1 <- decoded.kmeans
data.ens["SOM1"] <- c(SOM1)

SOM2 <- decoded.hc 
data.ens["SOM2"] <- c(SOM2)


KMEANS <- (model.kmeans$cluster)
data.ens["KMEANS"] <- c(KMEANS)

HC <- hc.cut
data.ens["HC"] <- c(HC)



#Ordenar colunas
data.ens= data.ens[,c(2,1,3,4, 5,6)]

#Plotar gr�fico de cada modelo e comparar
#HMM
data.matrix.hmm <- cbind(data.matrix, hmm.fit5@posterior$state)
data.matrix.hmm = as.data.frame(data.matrix.hmm)
data.matrix.hmm["Date"] = c(Data.1)
colnames(data.matrix.hmm) = c("Data", "HMM", "Date")
data.matrix.hmm = data.matrix.hmm[,c(3,1,2)]

#Geometric line
windows()
hml <-ggplot( data.matrix.hmm, aes(Date, Data, color =( data.matrix.hmm$HMM))) +geom_line()  
hml + scale_color_gradientn(colours = rainbow(5))

#Geometric points
windows()
hmg <- ggplot( data.matrix.hmm, aes(Date, Data, color = data.matrix.hmm$HMM)) +geom_point()
hmg + scale_color_gradientn(colours = rainbow(5))

#SOM
#Geometric line
data.matrix.som1 <- cbind(data.matrix$Values, decoded.kmeans)
data.matrix.som1  = data.frame(data.matrix.som1)
data.matrix.som1["Date"] = c(Data.1)
colnames(data.matrix.som1)= c("Data", "SOM1","Date")
data.matrix.som1 = data.matrix.som1[c(3,1,2)]

#Geometric line
windows()
soml1 <-ggplot( data.matrix.som1, aes(Date, Data, color =( data.matrix.som1$SOM1))) +geom_line()  
soml1 + scale_color_gradientn(colours = rainbow(5))

#Geometric points
windows()
somg1 <- ggplot( data.matrix.som1, aes(Date, Data, color = data.matrix.som1$SOM1)) +geom_point()
somg1 + scale_color_gradientn(colours = rainbow(5))

#SOM2
data.matrix.som2 <- cbind(data.matrix$Values, decoded.hc)
data.matrix.som2  = data.frame(data.matrix.som2)
data.matrix.som2["Date"] = c(Data.1)
colnames(data.matrix.som2)= c("Data","SOM2", "Date")
data.matrix.som2 = data.matrix.som2[c(3,1,2)]

#Geometric line
windows()
soml2 <-ggplot( data.matrix.som2, aes(Date, Data, color =( data.matrix.som2$SOM2))) +geom_line()  
soml2 + scale_color_gradientn(colours = rainbow(5))

#Geometric pints
windows()
somg2 <- ggplot( data.matrix.som2, aes(Date, Data, color = data.matrix.som2$SOM2)) +geom_point()
somg2 + scale_color_gradientn(colours = rainbow(5))

#kMEANS
data.matrix.km <- cbind(data.matrix$Values,  model.kmeans$cluster)
data.matrix.km = as.data.frame(data.matrix.km)
data.matrix.km["Date"] = c(Data.1)
colnames(data.matrix.km) =  c("Data", "KMEANS", "Date")
data.matrix.km = data.matrix.km[,c(3,1,2)]

#Geometric line
windows()
kml <-ggplot( data.matrix.km, aes(Date, Data, color =( data.matrix.km$KMEANS))) +geom_line()  
kml + scale_color_gradientn(colours = rainbow(5))

#Geometric points
windows()
kmg <- ggplot( data.matrix.km, aes(Date, Data, color = data.matrix.km$KMEANS)) +geom_point()
kmg + scale_color_gradientn(colours = rainbow(5))

#HC
data.matrix.hc <- cbind(data.matrix$Values,  hc.cut)
data.matrix.hc = as.data.frame(data.matrix.hc)
data.matrix.hc["Date"] = c(Data.1)
colnames(data.matrix.hc) =  c("Data",  "HC", "Date")
data.matrix.hc = data.matrix.hc[,c(3,1,2)]

#Geometric line
windows()
hcl <-ggplot( data.matrix.hc, aes(Date, Data, color =( data.matrix.hc$HC))) +geom_line()  
hcl + scale_color_gradientn(colours = rainbow(5))

#Geometric points
windows()
hcg <- ggplot( data.matrix.hc, aes(Date, Data, color = data.matrix.hc$HC)) +geom_point()
hcg + scale_color_gradientn(colours = rainbow(5))


#Matrix de Correla��o dos modelos 2 a 2
cor.hc.km <- cor(cbind(HC, KMEANS))
cor.hc.km
cor.hc.hm <- cor(cbind(HC, HMM))
cor.hc.hm
cor.km.hm <- cor(cbind(HMM, KMEANS))
cor.km.hm
cor.som1.hc <- cor(cbind(SOM1, HC))
cor.som1.hc
cor.som1.km <- cor(cbind(SOM1, KMEANS))
cor.som1.km
cor.som1.hmm <- cor(cbind(SOM1, HMM))
cor.som1.hmm
cor.som2.hc <- cor(cbind(SOM2, HC))
cor.som2.hc
cor.som2.km <- cor(cbind(SOM2, KMEANS))
cor.som2.km
cor.som2.hmm <- cor(cbind(SOM2, HMM))
cor.som2.hmm

##############################################KM�DIAS##########################
#Utilizando k-m�dias para fazer um unico agrupamento dos 4 modelos

#Elbow m�todo, para identificar o n�mero ideal de clusters para o dataset
k.max <- 15
data <- data.ens[,-1]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=20,iter.max = 15 )$tot.withinss})
wss
windows()
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#kmeans
library(stats)
set.seed(5554)

#Criar modelo
ens.model.kmeans <- kmeans(data.ens[,-1], 5, nstart = 20)#5 centr�ides, o r ir� tentar  20 come�os diferentes


#Plotar gr�ficos
library(cluster)
library(fpc)

#Geometric line
windows()
ens.plot <- ggplot( data.ens, aes(Date, Values, color = ens.model.kmeans$cluster)) +geom_line()
ens.plot + scale_color_gradientn(colours = rainbow(5))

windows()
plot(ens.model.kmeans$cluster)

#Geometric points
windows()
ens.plot <- ggplot( data.ens, aes(Date, Values, color = ens.model.kmeans$cluster)) +geom_point()
ens.plot + scale_color_gradientn(colours = rainbow(5))

data.final <- as.data.frame (ens.model.kmeans$cluster)
data.final["Date"]= c(Date)
colnames(data.final) = c("Clusters", "Data")
data.final = data.final[,c(2,1)]

library("clValid")
# comparing hierarchical and kmeans with internal validation indexes
clmethods <- c("hierarchical","kmeans", "som")
intern <- clValid(data.ens[,-1], nClust = 2:6, 
                  clMethods = clmethods, validation = "internal", method = "complete")

summary(intern)


library(factoextra)

# can change the clustering algorithm in FUN (ex kmeans)
fviz_nbclust(data.ens[,-1], FUN = hcut, method = "wss")+geom_vline(xintercept = 5, linetype = 2)
windows()
fviz_nbclust(data.ens[,-1], FUN = hcut, method = "silhouette", ylim =0.8)+geom_vline(xintercept = 5, linetype = 2)

###################################M�dia de cada cluster da dataset antes do ensemble######################################

clus1 <- data.matrix[which(ens.model.kmeans$cluster ==1),]
mean(clus1)
sd(clus1)

clus2 <- data.matrix[which(ens.model.kmeans$cluster ==2),]
mean(clus2)
sd(clus2)

clus3 <- data.matrix[which(ens.model.kmeans$cluster ==3),]
mean(clus3)
sd(clus3)

clus4 <- data.matrix[which(ens.model.kmeans$cluster ==4),]
mean(clus4)
sd(clus4)

clus5 <- data.matrix[which(ens.model.kmeans$cluster ==5),]
mean(clus5)
sd(clus5)

##############################An�lises futuras###############
data.final["Planejados"] = c(Planejados)
data.final["Values"] = c(data.fails)
data.final = data.final[,c(1,4,2,3)]

#Dados sobre o teor de proteína (em percentual \%) do 
#leite de vaca nas semanas após o parto.
#O gado é agrupado de acordo com a dieta que são alimentados
#Dieta: cevada sozinha, cevada e tremoços, ou com tremoços sozinhos.
#Fonte: Diggle, Liang e Zeger (1994)

#Os dados estão no pacote nlme

library(nlme)
library(lme4)

data(Milk)
dados = Milk
#Transformando os fatores em numéricos

dados$Cow2=as.numeric(dados$Cow)
dados$Diet2=as.numeric(as.factor(dados$Diet))
attach(dados)

#Visualizando como ficou nosso conjunto de dados
head(dados)
summary(dados)

##vamos fzr uma análise exploratória nos dados

boxplot(protein~Time*Diet2, col=c("lightgray", 
      "lightblue", "lightgreen"),legend= TRUE,dados)

#Pelo boxplot podemos perceber que não há um padrão entre nível de proteína
#para cada vaca dado o tipo de tratamento.

boxplot(protein~Diet, col=c("lightgray", 
        "lightblue", "lightgreen"),legend=TRUE,dados)


#Observamos que comparando apenas os tratamentos e o nível de protaína, 
#há um desempenho gradativo pra cada tipo de tratamento.
#Sendo que 1=barley,\\ 2=barley+lupins, 3=lupins

dadosGD = groupedData(protein ~ Time|Cow2, outer = ~Diet, data=dados)
plot(dadosGD, outer=T)

#olhando o perfil médio de cada vaca no tempo, observamos que não há 
#um padrão definido entre as vacas ou entre os tratamentos.\\
#Ao menos, na inspeção visual fica difícil determinar ou perceber 
#algum comportamento no perfil médio das vacas

plot(dadosGD)

#neste gráfico podemos observar o nível de proteína no leite da vaca 
#pelo tempo de medição (em semanas após o parto)

boxplot(protein~Time, dados)

#considerando apenas o tempo, notamos um nível elevado de proteína no 
#leite das vacas na primeira semana 
#após o parto comparada com as outras semanas.
#Nas demais semanas parece haver algum efeito aleatório no nível de 
#proteína do leite durante as.
#Agora, vamos tentar ajustar um modelo, testando primeiro a parte 
#aleatório e, após defini-la, testando a parte fixa do modelo.
#Testar parte aleatoria

modA=lmer(formula = protein ~ Time*Diet + (1| Cow), REML = F)
anova(modA)
modB=lmer(formula = protein ~ Time*Diet + (Time| Cow), REML = F)
anova(modB)

anova(modA, modB)


#O modelo escolhido é o modB, pois tem os menores valores de AIC e BIC e é o 
#estatisticamente significativo (Pr(>Chisq))

#Testar parte fixa dado que consideramos o modB na parte aleatória

modA1=lmer(formula = protein ~ Time + (Time| Cow),  REML = F)
modB1=lmer(formula = protein ~ Time + Diet + (Time| Cow),  REML = F)
modC1=lmer(formula = protein ~ Time * Diet + (Time| Cow), REML = F)

anova(modA1, modB1, modC1)

plot(modB1)
plot(modA1)
plot(modC1)


#Assim escolhemos o modB1, pois é estatisticamente significante, tem os menores valores de AIC e BIC. 
#O modC1 não foi escolhido, mas é interessante reparar que não há interação entre Time e Diet.
#Agora vamos analisa nosso modelo

summary(modB1)

#Agora vamos fazer um gráfico de qq-plot dos resítuduos do modelo, 
#pois há interesse de que este seja normal.

install.packages('lattice')
library(lattice)
library(ggplot2)

bwplot(dados$Cow2~resid(modB1),data=dados, 
       xlab = "", 
       ylab = "")

bwplot(dados$Diet2~resid(modB1),data=dados,xlab = "", 
       ylab = "")

bwplot(dados$protein~resid(modB1),data=dados,xlab = "", 
       ylab = "")

interaction.plot(dados$Time,dados$Cow,dados$protein,col=c(rep(2,25),
                rep(4,27),rep(3,27)),ylab = "Proteína",xlab = "Tempo",
                ylim=c(2,5),lty=c(1,1),legend=FALSE)

interaction.plot(dados$Time,dados$Diet,dados$protein,col=c(2,4,3),
                 ylim=c(3,4),lty=c(1,1))

ggplot(data = Milk, aes(x=Time, y=protein))

#geompoint inherits x=Time, y=protein

ggplot(data = Milk, aes(x=Time, y=protein)) + geom_point()

#color=Diet only applies to geom_boxplot

ggplot(data = Milk, aes(x=Time, y=protein))
p <- ggplot(data = Milk, aes(x=Time, y=protein)) + geom_point()
p + geom_smooth()
pro <- ggplot(Milk, aes(x=protein)) 
pro + geom_density()

p2 <- ggplot(Milk, aes(x=Time, y=protein))
p2 + geom_smooth()

#default is scale_fill_hue, would be the same if omitted

dDiet <- ggplot(Milk, aes(x=protein, fill=Diet)) + 
geom_density(alpha=1/3)
dDiet + scale_fill_hue()

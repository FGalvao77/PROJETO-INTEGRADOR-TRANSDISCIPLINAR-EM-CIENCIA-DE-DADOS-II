# Atividade 1 - Projeto Integrador Transdisciplinar em Ciência de Dados II #

## 1. Carregamento dos dados e instalando o pacote necessário para leitura ##

### carregando os pacotes
install.packages(c('readr'))

### ativando o pacote usado para fazer a leitura de dados csv e txt 
library(readr) 

### carregando os dados de uma url e instanciando na variável "dados_pacientes"
### salvei o arquivo no meu repositório pessoal do github
dados_pacientes <- read_csv('https://raw.githubusercontent.com/FGalvao77/datasets/main/pacientes.csv')

## 2. Análise exploratória dos dados ##

### verificando a classe do objeto criado
class(dados_pacientes) 

### verificando a dimensão do objeto
dim(dados_pacientes) # linhas e colunas

### visualizando o conjunto de dados (forma de tabela)
View(dados_pacientes)

### listando o nome das colunas
names(dados_pacientes)

### renomeando as colunas
colnames(dados_pacientes) <-
  c('id', 'idade_segurado', 'cod_procedimento',
    'valor_liberado')

### removendo a coluna "id" do conjunto de dados já que mesma
### não agrega nenhuma informação relevante para nossa análise
dados_pacientes$id <- NULL

### listando novamente o nome das colunas
names(dados_pacientes)

### quantas linhas e colunas (variáveis) há no conjunto de dados?
cat('A base de dados estudada possui', length(dados_pacientes), 
    'variáveis e', nrow(dados_pacientes), 'linhas.', '\n')

### visualizando as 10 primeiras linhas
head(dados_pacientes, 10)

### visualizando as 10 últimas linhas
tail(dados_pacientes, 10)

### visualizando o tipo de dado das colunas
str(dados_pacientes)

### contabilizando os valores ausentes da coluna 
sum(is.na(dados_pacientes))
#### temos 97 linhas com valores ausentes ("NA")

### visualizando a estatística descritiva do conjunto de dados
summary(dados_pacientes)
#### na coluna "cod_procedimento" e "valor_liberado" há valores do tipo "NA" 
#### 96 linhas e 1 linha, respectivamente

### agora vamos tratar desse problema

### primeiro vamos ver o quanto esses valores ausentes representa em frequência relativa
### para o total de linhas do conjunto de dados realizarei essa inspeção da coluna 
### "cod_procedimento" já que ela possui o maior número de valores "NA" 
sum(is.na(dados_pacientes['cod_procedimento'])) / 
  nrow(dados_pacientes) * 100
#### é menor que 0.4% dos dados

### temos várias maneiras de tratar esse problema:
  # 1. eliminar as linhas com valores "NA"
  # 2. eliminar a coluna com valores "NA"
  # 3. preencher os valores "NA" por algum medida numérica como:
    # a mediana da coluna
    # e entre outras

### exemplo 1
### o comando "na.omit" vai retirar todas as linhas que tenham pelo menos um NA
#### dados_pacientes <- na.omit(dados_pacientes)

### exemplo 2
### temos outro comando que retira apenas as linhas que tem "NA" nas colunas referenciadas
#### dados_pacientes <- dados_pacientes[!is.na(dados_pacientes['cod_procedimento', 
####                                                          'valor_liberado']),]

### exemplo 3
### preenchendo os valores ausentes pela média da coluna referenciada
#### dados_pacientes$cod_procedimento[is.na(df4$cod_procedimento)] <-
####  mean(df4$cod_procedimento, na.rm = TRUE)

### aqui utilizarei a primeira estratégia
### com o o comando "na.omit" irei eliminar
### todas as linhas que tenham pelo menos um NA
dados_pacientes <- na.omit(dados_pacientes)

### verificando se os valores foram eliminados
sum(is.na(dados_pacientes))
#### veja que, não há mais valores "NA"

## estatística descritiva geral do conjunto de dados
summary(dados_pacientes)

### criando tabelas de frequência simples 
### variáveis: idade_segurado, cod_procedimento e valor_liberado
table(dados_pacientes$idade_segurado)                  
table(dados_pacientes$cod_procedimento)               
table(dados_pacientes$valor_liberado)  

### vamos criar tabelas das frequências das instâncias presentes na variáveis e, 
# em seguida vamos visualiza-las 

### tabela de frequência das instâncias da coluna "idade_segurado"
idade <- prop.table(table(dados_pacientes$idade_segurado))
class(idade)
View(idade) # visualizando a tabela

### tabela de frequência das instâncias da coluna "cod_procedimento"
codigo <- prop.table(table(dados_pacientes$cod_procedimento))
View(codigo) # visualizando a tabela

### tabela de frequência das instâncias da coluna "valor_liberado"
idadeVSvalor <- prop.table(table(dados_pacientes$idade_segurado, 
                                 dados_pacientes$valor_liberado))
View(idadeVSvalor) # visualizando a tabela

### com a função "summary" extraindo a estatística descritiva da coluna "idade_segurado"
summary((dados_pacientes$idade_segurado))

### contabilizando da coluna "total_liberado"
sum(dados_pacientes$valor_liberado)

## 3. Visualização gráfica ##

### vamos tentar responder algumas perguntas visualmente, como:
  # Qual a distribuição das idades? É uma distribuição normal?
  # Existe alguma relação das variáveis idade, procedimento com o valor liberado?
  # E outras dúvidas que forem surgidas!

### habilitando o pacote para visualização gráfica
library(ggplot2)  

### histograma da distribuição da variável "idade_segurado"
ggplot(dados_pacientes, aes(x=idade_segurado)) + 
  geom_histogram(colour='white', bins=50) +
  scale_x_continuous(breaks = seq(0,110,5)) +
  ylab('qtde de segurados') +
  xlab('idade do segurado')

### histograma da "idade_segurado" com densidade
ggplot(dados_pacientes, aes(x=idade_segurado)) + 
  geom_histogram(aes(y=..density..), colour='black', 
                 fill='white', bins=50) +
  geom_density(alpha=.2, fill='#FF6666') +
  scale_x_continuous(breaks = seq(0,110,5)) +
  ylab('densidade') +
  xlab('idade do segurado')

### gráfico da densidade da "idade_segurado"
ggplot(dados_pacientes, aes(x=idade_segurado)) + 
  geom_density(alpha=.2, fill='#FF6666') +
  scale_x_continuous(breaks = seq(0,110,5)) +
  ylab('densidade') +
  xlab('idade do segurado')

### adicionando linha da média da distribuição das idades
ggplot(dados_pacientes, aes(x=idade_segurado)) + 
  geom_histogram(color='black', fill='gray', bins=50) + 
  geom_vline(aes(xintercept=mean(idade_segurado)),
             color='blue', linetype='dashed', size=1) + 
  scale_x_continuous(breaks = seq(0,110,5)) +
  ylab('qtde de segurados') +
  xlab('idade do segurado')

### gráfico de dispersão entre as variáveis "idade_segurado" e "valor_liberado"
ggplot(dados_pacientes, aes(x=idade_segurado, y=valor_liberado)) + 
  scale_x_continuous(breaks = seq(0,110,5)) +
  geom_point()+
  geom_smooth(formula = y ~ s(x, bs = 'cs'), method = 'gam')

### gráfico de dispersão entre as variáveis "idade_segurado" e "valor_liberado"
ggplot(dados_pacientes, aes(x=idade_segurado, y=valor_liberado)) + 
  scale_x_continuous(breaks = seq(0,110,5)) +
  geom_point()+
  geom_smooth(method=lm, formula = y ~ x)

### alterarando a cor de preenchimento do intervalo de confiança
ggplot(dados_pacientes, aes(x=idade_segurado, y=valor_liberado)) + 
  scale_x_continuous(breaks = seq(0,110,5)) +
  geom_point(shape=18, color='blue')+
  geom_smooth(method=lm,  linetype='dashed', formula = y ~ x,
              color='darkred', fill='blue')

### boxplot da distribuição das idades
ggplot(data = dados_pacientes, mapping = aes(x = idade_segurado)) + 
  scale_x_continuous(breaks = seq(0,110,5)) +
  geom_boxplot()

### histograma do "valor_liberado" adicionando linha da média 
ggplot(dados_pacientes, aes(x=valor_liberado)) + 
  geom_histogram(color='black', fill='gray', bins=50) + 
  geom_vline(aes(xintercept=mean(valor_liberado)),
             color='blue', linetype='dashed', size=1) + 
  ylab('qtde de segurados contemplados') +
  xlab('valor iberado')

### boxplot da distribuição do valor liberado
ggplot(data = dados_pacientes, mapping = aes(x = valor_liberado)) + 
  geom_boxplot()

### a biblioteca "skmir" fornece uma para visualização mais génerica
install.packages('skimr')
library(skimr)
  
skimr::skim(dados_pacientes)

### instanciando um gráfico de dispersão e salvando no objeto "p"
p <- ggplot(dados_pacientes, aes(x=idade_segurado, y=valor_liberado)) +
  scale_x_continuous(breaks = seq(0,110,5)) +
  geom_point(colour='blue') +
  geom_smooth(method=lm, colour='red', formula = y ~ x) +
  theme(legend.position="none")

### visualizando o gráfico de dispersão
p

### instalando a biblioteca para auxiliar nas visualizações agregadas
install.packages('ggExtra')
library(ggExtra)

### gráfico de dispersão + histograma como marginal
p1 <- ggMarginal(p, type="histogram", colour='white', bins=50) 
p1

### gráfico de dispersão + gráfico de densidade como marginal
p2 <- ggMarginal(p, type="density", colour='green')
p2

### gráfico de dispersão + gráfico de boxplot com marginal
p3 <- ggMarginal(p, type="boxplot")
p3

### vamos analisar a correlações entre as variáveis

### instalando o pacote
install.packages('corrplot')

### habilitando o pacote
library(corrplot)

correlacao <- dados_pacientes[, c('idade_segurado', 'valor_liberado', 
                                  'cod_procedimento')]
matriz_corr <- cor(correlacao)
matriz_corr

corrplot(matriz_corr)

corrplot(matriz_corr, method = 'color', 
         type = 'lower', tl.col = '#424242',
         tl.srt = 45, addCoef.col = '#ffffff', 
         col = colorRampPalette(c('gray', 'pink', 
                                  'blue'))(200), 
         tl.cex = 0.8,
         number.cex = 0.9,
         number.font = 1, 
         cl.cex = 0.9)


idadeVsvalor <- dados_pacientes[, c('idade_segurado', 'valor_liberado')]
matriz_corr1 <- cor(idadeVsvalor)
matriz_corr1

corrplot(matriz_corr1)

corrplot(matriz_corr1, method = 'color', 
         type = 'lower', tl.col = '#424242',
         tl.srt = 45, addCoef.col = '#ffffff', 
         col = colorRampPalette(c('#4D4D4D', 'black', 
                                  '#5288DB'))(200), 
         tl.cex = 0.8,
         number.cex = 0.9,
         number.font = 1, 
         cl.cex = 0.9)

procedimentoVsvalor <- dados_pacientes[, c('cod_procedimento', 'valor_liberado')]
matriz_corr2 <- cor(procedimentoVsvalor)
matriz_corr2

corrplot(matriz_corr2)

corrplot(matriz_corr2, method = 'color', 
         type = 'lower', tl.col = '#424242',
         tl.srt = 45, addCoef.col = '#ffffff', 
         col = colorRampPalette(c('#4D4D4D', 'black', 
                                  '#5288DB'))(200), 
         tl.cex = 0.8,
         number.cex = 0.9,
         number.font = 1, 
         cl.cex = 0.9)

procedimentoVsidade <- dados_pacientes[, c('cod_procedimento', 'idade_segurado')]
matriz_corr3 <- cor(procedimentoVsidade)
matriz_corr3

corrplot(matriz_corr3)

corrplot(matriz_corr3, method = 'color', 
         type = 'lower', tl.col = '#424242',
         tl.srt = 45, addCoef.col = '#ffffff', 
         col = colorRampPalette(c('#4D4D4D', 'black', 
                                  '#5288DB'))(200), 
         tl.cex = 0.8,
         number.cex = 0.9,
         number.font = 1, 
         cl.cex = 0.9)
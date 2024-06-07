### Install GGplot2 libraries
install.packages("ggplot2")

##### Using TXT file to load the data
setwd("C:/Users/Analytics/Desktop/Análise exploratória de dados/")
# **CASE: PERFIL DE IMOVEIS RESIDENCIAIS** #
### Leitura da base de dados
dados <- read.table(file = "Imoveis.txt",
                    sep = "\t",
                    header = TRUE)


#####Reading data from MySQL DB  to test the access directly into DataBase, 
#you can use the DBeaver Client to load data and only mention "\t" 
#as Column Delimeter during the import process

#Install package to connect on MySQL or others DBs
install.packages("DBI")

#Enable the library
library(DBI)

#Connect MySQL to get the info of table mytest.imoveis  

db <- dbConnect(RMySQL::MySQL(),
                user = "root",
                host = "192.168.15.51",
                password = "123Qwee!",
                dbname = "mytest",
                port = 3306)

#create a vector with all tables into the DB
list_tales <- dbListTables(db)

#create a vector with all fields into tables
list_columns_tables <- dbListFields(db, "imoveis")

#create a dataset with all fields into the table
df_table_imoveis <- dbReadTable(db, "imoveis")

#create a dataset running a query into DB
df_imoveis <- dbFetch(dbSendQuery(db, "SELECT * FROM imoveis"),n = -1)

dados <- dbFetch(dbSendQuery(db, "SELECT * FROM imoveis"),n = -1)

### Item (a)
table(dados$BAIRRO_IMOVEL)
prop.table(table(dados$BAIRRO_IMOVEL))

table(dados$BAIRRO_IMOVEL, dados$TIPO_IMOVEL)
round(prop.table(table(dados$BAIRRO, dados$TIPO_IMOVEL), 2), 3)

### Item (b)
summary(dados$METRAGEM)
table(dados$METRAGEM)


########Medidas resumo de posição 
summary(dados$METRAGEM)
####Media
mean(dados$METRAGEM)

####Mediana
median(dados$METRAGEM)

###Moda
names(sort(-table(dados$METRAGEM)))[1]

####Minimo
min(dados$METRAGEM)

####Maximo
max(dados$METRAGEM)

#####Quartis 
quantile(dados$METRAGEM) #Calcula todos os 4 quartis sem o uso do percentual
quantile(dados$METRAGEM,0.25)
quantile(dados$METRAGEM,0.50)
quantile(dados$METRAGEM,0.75)
quantile(dados$METRAGEM,1)

#####Percentis => Tentar identificar outliers
quantile(dados$METRAGEM,0.01)
quantile(dados$METRAGEM,0.99)

########Medidas resumo de dispersao
nrow(dados) #Usa para obter o numero de registros do seu dataframe
###Variancia
var(dados$METRAGEM)
var(dados$METRAGEM)*(nrow(dados)-1)/nrow(dados)


#####Desvio Padrao
sqrt(var(dados$METRAGEM)) # Calculo usando a funcao raiz quadrada
sd(dados$METRAGEM) #calculo desvio padrão diretamente


#####Coeficiente de variação
sd(dados$METRAGEM) / mean(dados$METRAGEM)  # Desvio Padrão / Media


####Amplitude
max(dados$METRAGEM) - min(dados$METRAGEM) 

####Amplitude Percentilica
quantile(dados$METRAGEM, 0.99) - quantile(dados$METRAGEM, 0.01) 





### Item (c)
boxplot(dados$METRAGEM ~ dados$VAGAS_GARAGEM,
        main = "Metragem versus vagas de garagem",
        xlab = "Vagas de garagem",
        ylab = "Metragem (em m2)",
        col  = "darkturquoise")

### Item (d)

hist(dados$COMERCIOS_RAIO_1KM,
     main = "Distribuicao da qtde. de estabelecimentos em ate 1km",
     xlab = "Qtde. de estabelec.",
     ylab = "Frequencia",
     col  = "darkturquoise",
     border = "white")


par(mfrow = c(2,2))
hist(dados$COMERCIOS_RAIO_1KM[dados$BAIRRO_IMOVEL == "Jardim Sol"],
     main = "Qtde. de estabelec.: Jardim Sol",
     xlab = "Qtde. de estabelecimentos",
     ylab = "Frequencia",
     xlim = c(0,40),
     col  = "darkturquoise",
     border = "white")
hist(dados$COMERCIOS_RAIO_1KM[dados$BAIRRO_IMOVEL == "Recanto Mar"],
     main = "Qtde. de estabelec.: Recanto Mar",
     xlab = "Qtde. de estabelecimentos",
     ylab = "Frequencia",
     xlim = c(0,40),
     col  = "darkturquoise",
     border = "white")
hist(dados$COMERCIOS_RAIO_1KM[dados$BAIRRO_IMOVEL == "Santa Rosa"],
     main = "Qtde. de estabelec.: Santa Rosa",
     xlab = "Qtde. de estabelecimentos",
     ylab = "Frequencia",
     xlim = c(0,40),
     col  = "darkturquoise",
     border = "white")
hist(dados$COMERCIOS_RAIO_1KM[dados$BAIRRO_IMOVEL == "Vila Verde"],
     main = "Qtde. de estabelec.: Vila Verde",
     xlab = "Qtde. de estabelecimentos",
     ylab = "Frequencia",
     xlim = c(0,40),
     col  = "darkturquoise",
     border = "white")


### Item (e)

table(dados$INCIDENCIA_LUZ, dados$BAIRRO_IMOVEL)
round(prop.table(table(dados$INCIDENCIA_LUZ, dados$BAIRRO_IMOVEL), 2), 2)

par(mfrow = c(1,1))
tab_perc <- prop.table(table(dados$INCIDENCIA_LUZ,
                             dados$BAIRRO_IMOVEL,
                             useNA = "ifany"), 2) * 100
barplot(height = tab_perc,
        col    = c("turquoise", "turquoise2", "turquoise4"),
        main   = "Incidencia de luz solar versus bairro",
        xlab   = "Bairro",
        ylab   = "Frequencia (%)")
legend(x      = "bottomright",
       legend = row.names(tab_perc),
       cex    = 0.8,
       fill   = c("turquoise", "turquoise2", "turquoise4"))

### Item (f)

options(scipen = 999)
hist(dados$VALOR_VENDA,
     main = "Distribuicao de precos dos imoveis",
     xlab = "Preco do imovel, em R$",
     ylab = "Frequencia",
     col  = "darkturquoise",
     border = "white")

### Item (g)

options(scipen = 999)
boxplot(dados$VALOR_VENDA ~ dados$BAIRRO_IMOVEL,
        main = "Distribuicao de precos dos imoveis, por bairro",
        xlab = "Bairro",
        ylab = "Preco do imovel, em R$",
        col  = "darkturquoise")

### Item (h)

options(scipen = 999)
library(scales)

plot(dados$VALOR_VENDA ~ dados$COMERCIOS_RAIO_1KM,
     main = "Precos dos imoveis versus qtde. de estabelec. comerciais",
     xlab = "Qtde. de estabelecimentos",
     ylab = "Preco do imovel, em R$",
     col  = alpha("darkturquoise", 0.4),
     pch = 19)






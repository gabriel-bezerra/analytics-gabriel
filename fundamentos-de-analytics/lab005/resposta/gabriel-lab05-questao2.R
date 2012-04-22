library("moments")

arquivo.de.dados <- commandArgs(trailingOnly = TRUE)[1]

dados <- read.table(file = arquivo.de.dados, header = TRUE)

## 2) Suponha que após a análise gráfica da questão anterior, você decidiu verificar o Zskewness e o
## Zkurtosis dos dados para os mesmos cenários.

## Escreva um script R que calcula o Zskewness e o Zkurtosis dos seguintes dados:

# Zskewness
z.skewness <- function(data)
    skewness(data) / sqrt(6 / length(data))

# Zkurtosis
z.kurtosis <- function(data)
    kurtosis(data) / sqrt(24 / length(data))


linha.de.resultado.para <- function(cenario, dados)
    data.frame(cenario = cenario,
               Zskewness = c(z.skewness(dados)),
               Zkurtosis = c(z.kurtosis(dados)))


## (i) intervalos em que as máquinas estiveram ocupadas;

intervalos.ocupadas <- dados[dados$ociosa == FALSE, ]

resultados.ocupadas <- linha.de.resultado.para("maquinas ocupadas", intervalos.ocupadas$intervalo)


## (ii) intervalos em que as máquinas estiveram ociosas;

intervalos.ociosas <- dados[dados$ociosa == TRUE, ]

resultados.ociosas <- linha.de.resultado.para("maquinas ociosas", intervalos.ociosas$intervalo)


## (iii) intervalos em que as máquinas estiveram ociosas separados por laboratório.

numero.de.laboratorios = length(levels(dados$laboratorio))

by.resultados.por.laboratorio <-
    by(intervalos.ociosas[, c("intervalo", "laboratorio")],
       list(laboratorio = intervalos.ociosas$laboratorio),
       function(x) {
           nome.do.laboratorio = levels(factor(x$laboratorio))[1]

           linha.de.resultado.para(paste("máquinas ociosas de", nome.do.laboratorio),
                                   x$intervalo)
       })

# converte by em data.frame
resultados.por.laboratorio <- do.call(rbind, by.resultados.por.laboratorio)


## Os resultados dos três itens devem ser salvos como uma tabela um único
## arquivo no formato *.txt. A tabela deve ter 3 colunas: cenário, Zskewness e Zkurtosis .

resultados <- rbind(resultados.ocupadas,
                    resultados.ociosas,
                    resultados.por.laboratorio)

write.table(resultados, file = "output-questao2.txt")


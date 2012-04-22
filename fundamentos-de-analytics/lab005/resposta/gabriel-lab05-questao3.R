library("nortest")

arquivo.de.dados <- commandArgs(trailingOnly = TRUE)[1]

dados <- read.table(file = arquivo.de.dados, header = TRUE)

## 3) Use os testes estatísticos Anderson-Darling e Shapiro-Wilk para verificar a normalidade de
## um conjunto de dados. Para tanto, considere as descrições abaixo:

## Escreva um script R que aplica os testes Anderson-Darling e Shapiro-Wilk sobre os
## seguintes dados:


linha.de.resultados.para <- function(cenario, teste, dados) {
    resultado.do.teste = teste(dados)

    data.frame(cenario = cenario,
               nome.do.teste = resultado.do.teste$method,
               valor.da.estatistica = resultado.do.teste$statistic,
               p.value = resultado.do.teste$p.value)
}

## (i) intervalos em que as máquinas estiveram ocupadas;

intervalos.ocupadas <- dados[dados$ociosa == FALSE, ]

resultados.ocupadas <- rbind(linha.de.resultados.para("maquinas ocupadas", shapiro.test, intervalos.ocupadas$intervalo),
                             linha.de.resultados.para("maquinas ocupadas", ad.test, intervalos.ocupadas$intervalo))


## (ii) intervalos em que as máquinas estiveram ociosas;

intervalos.ociosas <- dados[dados$ociosa == TRUE, ]

resultados.ociosas <- rbind(linha.de.resultados.para("maquinas ociosas", shapiro.test, intervalos.ociosas$intervalo),
                            linha.de.resultados.para("maquinas ociosas", ad.test, intervalos.ociosas$intervalo))


## (iii) intervalos em que as máquinas estiveram ociosas separados por laboratório.

numero.de.laboratorios = length(levels(dados$laboratorio))

by.resultados.por.laboratorio <-
    by(intervalos.ociosas[, c("intervalo", "laboratorio")],
       list(laboratorio = intervalos.ociosas$laboratorio),
       function(x) {
           nome.do.laboratorio = levels(factor(x$laboratorio))[1]

           rbind(linha.de.resultados.para(paste("maquinas ociosas de", nome.do.laboratorio), shapiro.test, x$intervalo),
                 linha.de.resultados.para(paste("maquinas ociosas de", nome.do.laboratorio), ad.test, x$intervalo))
       })

# converte by em data.frame
resultados.por.laboratorio <- do.call(rbind, by.resultados.por.laboratorio)


## Você deve salvar os resultados em um único arquivo com uma tabela com quatro colunas: nome do
## teste, cenário, valor da estatística (A ou W) e o valor do p-value.

resultados <- rbind(resultados.ocupadas,
                    resultados.ociosas,
                    resultados.por.laboratorio)

write.table(resultados, file = "output-questao3.txt")


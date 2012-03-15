arquivo.de.dados <- commandArgs(trailingOnly = TRUE)[1]

dados <- read.table(file = arquivo.de.dados, header = TRUE)

## 3) Use os testes estatísticos Anderson-Darling e Shapiro-Wilk para verificar a normalidade de
## um conjunto de dados. Para tanto, considere as descrições abaixo:

## Escreva um script R que aplica os testes Anderson-Darling e Shapiro-Wilk sobre os
## seguintes dados:



## (i) intervalos em que as máquinas estiveram ocupadas;

intervalos.ocupadas <- dados[dados$ociosa == FALSE, ]



## (ii) intervalos em que as máquinas estiveram ociosas;

intervalos.ociosas <- dados[dados$ociosa == TRUE, ]



## (iii) intervalos em que as máquinas estiveram ociosas separados por laboratório.

numero.de.laboratorios = length(levels(dados$laboratorio))

by(intervalos.ociosas[, c("intervalo", "laboratorio")],
   list(laboratorio = intervalos.ociosas$laboratorio),
   function(x) {
       nome.do.laboratorio = levels(factor(x$laboratorio))[1]

   })

## Você deve salvar os resultados em um único arquivo com uma tabela com quatro colunas: nome do
## teste, cenário, valor da estatística (A ou W) e o valor do p-value.

# write.table


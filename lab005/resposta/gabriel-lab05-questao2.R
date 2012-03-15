arquivo.de.dados <- commandArgs(trailingOnly = TRUE)[1]

dados <- read.table(file = arquivo.de.dados, header = TRUE)

## 2) Suponha que após a análise gráfica da questão anterior, você decidiu verificar o Zskewness e o
## Zkurtosis dos dados para os mesmos cenários.

## Escreva um script R que calcula o Zskewness e o Zkurtosis dos seguintes dados:

# Zskewness
# Zkurtosis

## (i) intervalos em que as máquinas estiveram ocupadas;

intervalos.ocupadas <- dados[dados$ociosa == FALSE, ]

# Zskewness
# Zkurtosis


## (ii) intervalos em que as máquinas estiveram ociosas;

intervalos.ociosas <- dados[dados$ociosa == TRUE, ]

# Zskewness
# Zkurtosis


## (iii) intervalos em que as máquinas estiveram ociosas separados por laboratório.

numero.de.laboratorios = length(levels(dados$laboratorio))

by(intervalos.ociosas[, c("intervalo", "laboratorio")],
   list(laboratorio = intervalos.ociosas$laboratorio),
   function(x) {
       nome.do.laboratorio = levels(factor(x$laboratorio))[1]

       # Zskewness
       # Zkurtosis
   })

## Os resultados dos três itens devem ser salvos como uma tabela um único
## arquivo no formato *.txt. A tabela deve ter 3 colunas: cenário, Zskewness e Zkurtosis .

# write.table


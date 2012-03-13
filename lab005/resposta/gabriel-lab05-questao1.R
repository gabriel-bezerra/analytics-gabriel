arquivo.de.dados <- commandArgs(trailingOnly = TRUE)[1]

dados <- read.table(file = arquivo.de.dados, header = TRUE)

## 1) Suponha que você deseja realizar uma análise gráfica para identificar se os tamanhos dos
## intervalos em que as máquinas permaneceram ociosas e os tamanhos dos intervalos em que
## elas permaneceram ocupadas seguem uma distribuição normal. Você deseja analisar isso
## com todos os intervalos juntos e separadamente para cada laboratório. Nesse sentido, siga as
## instruções abaixo:

## Escreva um script R que gera os gráficos FDA, FDP e as qqnorms dos seguintes
## dados: (i) intervalos em que as máquinas estiveram ocupadas; (ii) intervalos em que
## as máquinas estiveram ociosas; (iii) intervalos em que as máquinas estiveram ociosas
## separados por laboratório. Nos gráficos FDPs, plote duas linhas, uma com a densidade
## dos dados obtidos na base de dados e outra com a densidade de uma distribuição
## normal gerada com a média e o desvio padrão dos dados obtidos da base de dados.
## Para cada item solicitado deve ser gerada uma figura no formato *.png, portanto, ao
## todo devem ser geradas 3 figuras.

# TODO
#
# plotar gráficos em PNGs
# revisar labels dos gráficos
# revisar las = 1
# intervalos.ociosas <- dados[dados$ociosa == TRUE, ]
# intervalos.ociosas agrupados por laboratório

intervalos.ocupadas <- dados[dados$ociosa == FALSE, ]

# FDA
plot(ecdf(intervalos.ocupadas$intervalo),
     main = "FDA para os intervalos em que as máquinas ficaram ocupadas",
     xlab = "Tempo ocupada",
     las = 1)


# FDP
n = length(intervalos.ocupadas$intervalo)
media = mean(intervalos.ocupadas$intervalo)
desvio.padrao = sd(intervalos.ocupadas$intervalo)

plot(density(intervalos.ocupadas$intervalo), col = "blue", lwd = 1,
     main = "FDPs para os intervalos em que as máquinas ficaram ocupadas",
     las = 1)

lines(density(rnorm(n, media, desvio.padrao)), col = "red", lwd = 1)
legend("topright", c("Real", "Normal"), col = c("blue", "red"), lwd = 1)


# qqnorm
qqnorm(intervalos.ocupadas$intervalo, las = 1)
qqline(intervalos.ocupadas$intervalo)



## Gere os gráficos utilizando o seu script. Escreva um relatório apresentando os gráficos
## que você obteve e discutindo: (i) que relações você observa entre os gráficos FDA,
## FDP e qqnorm, apresente exemplos de alguns comportamentos observados no qqnorm
## e de como eles se manifestam na FDA e/ou na FDP; (ii) com base no qqnorm, na sua
## opinião, qual cenário avaliado mais se assemelha ao esperado de uma distribuição
## normal e qual menos se assemelha.


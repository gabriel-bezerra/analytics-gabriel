arquivo.de.dados <- commandArgs(trailingOnly = TRUE)[1]

dados <- read.table(file = arquivo.de.dados, header = TRUE)

## 1) Suponha que você deseja realizar uma análise gráfica para identificar se os tamanhos dos
## intervalos em que as máquinas permaneceram ociosas e os tamanhos dos intervalos em que
## elas permaneceram ocupadas seguem uma distribuição normal. Você deseja analisar isso
## com todos os intervalos juntos e separadamente para cada laboratório. Nesse sentido, siga as
## instruções abaixo:

## Escreva um script R que gera os gráficos FDA, FDP e as qqnorms dos seguintes
## dados:
## Nos gráficos FDPs, plote duas linhas, uma com a densidade
## dos dados obtidos na base de dados e outra com a densidade de uma distribuição
## normal gerada com a média e o desvio padrão dos dados obtidos da base de dados.
## Para cada item solicitado deve ser gerada uma figura no formato *.png, portanto, ao
## todo devem ser geradas 3 figuras.

# FDA
fda.plot <- function(dados, titulo) {
    plot(ecdf(dados),
         main = titulo,
         xlab = "Tempo",
         las = 1)
}

# FDP

fdp.norm.plot <- function(dados, titulo) {
    n = length(dados)
    media = mean(dados)
    desvio.padrao = sd(dados)

    plot(density(dados), col = "blue", lwd = 1,
         main = titulo,
         las = 1)

    lines(density(rnorm(n, media, desvio.padrao)), col = "red", lwd = 1)
    legend("topright", c("Real", "Normal"), col = c("blue", "red"), lwd = 1)
}

# qqnorm
qqnorm.plot <- function(dados, titulo) {
    qqnorm(dados,
           main = titulo,
           las = 1)
    qqline(dados)
}


## (i) intervalos em que as máquinas estiveram ocupadas;

intervalos.ocupadas <- dados[dados$ociosa == FALSE, ]

png(filename = "output-questao1-i.png", width = 480, height = 3 * 480)
par(mfrow = c(3,1))
    fda.plot(intervalos.ocupadas$intervalo, "FDA para os intervalos em que as máquinas estiveram ocupadas")
    fdp.norm.plot(intervalos.ocupadas$intervalo, "FDPs para os intervalos em que as máquinas estiveram ocupadas")
    qqnorm.plot(intervalos.ocupadas$intervalo,
                "Normal Q-Q Plot para os intervalos em que as máquinas estiveram ocupadas")
dev.off()


## (ii) intervalos em que as máquinas estiveram ociosas;

intervalos.ociosas <- dados[dados$ociosa == TRUE, ]

png(filename = "output-questao1-ii.png", width = 480, height = 3 * 480)
par(mfrow = c(3,1))
    fda.plot(intervalos.ociosas$intervalo, "FDAs para os intervalos em que as máquinas estiveram ociosas")
    fdp.norm.plot(intervalos.ociosas$intervalo, "FDPs para os intervalos em que as máquinas estiveram ociosas")
    qqnorm.plot(intervalos.ociosas$intervalo,
                "Normal Q-Q Plot para os intervalos em que as máquinas estiveram ociosas")
dev.off()


## (iii) intervalos em que as máquinas estiveram ociosas separados por laboratório.

numero.de.laboratorios = length(levels(dados$laboratorio))

png(filename = "output-questao1-iii.png", width = 3 * 480, height = numero.de.laboratorios * 480)
par(mfrow = c(numero.de.laboratorios,3))
    by(intervalos.ociosas[, c("intervalo", "laboratorio")],
       list(laboratorio = intervalos.ociosas$laboratorio),
       function(x) {
           nome.do.laboratorio = levels(factor(x$laboratorio))[1]

           fda.plot(x$intervalo,
                    paste("FDAs para os intervalos em que as máquinas de\n",
                          nome.do.laboratorio, "estiveram ociosas"))

           fdp.norm.plot(x$intervalo,
                         paste("FDPs para os intervalos em que as máquinas de\n",
                               nome.do.laboratorio, "estiveram ociosas"))

           qqnorm.plot(x$intervalo,
                       paste("Normal Q-Q Plot para os intervalos em que as máquinas de\n",
                             nome.do.laboratorio, "estiveram ociosas"))
       })
dev.off()


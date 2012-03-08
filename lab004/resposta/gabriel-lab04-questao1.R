library("ggplot2")

erro.intervalo.confianca.para.media <- function(n, desvio.padrao.amostral, nivel.de.significancia)
    (desvio.padrao.amostral / sqrt(n)) *
        ifelse(n < 30,
               qt(1 - (nivel.de.significancia / 2), df = n - 1),
               qnorm(1 - (nivel.de.significancia / 2)))

intervalo.de.confianca.para.a.media <- function(amostra, nivel.de.significancia) {
    media.amostral = mean(amostra)

    margem = erro.intervalo.confianca.para.media(length(amostra),
                                                 sd(amostra),
                                                 nivel.de.significancia)

    return(c(media.amostral - margem, media.amostral + margem))
}

## 1. Implemente um script R que plota dois gráficos:
## Para gerar os gráficos acima, considere nível de significância de 5%.

dados <- read.csv(file = "dados-deputados.csv")

nivel.de.significancia = 0.05


## Um gráfico png com os intervalos de confiança das médias dos gastos
## anuais dos deputados de cada uma das cinco regiões do Brasil. Observe
## o gráfico gerado e compare os gastos dos deputados de cada região. É
## possível concluir algo interessante com base nos intervalos de confiança?
## Existem intervalos maiores que outros? A que você atribui isso?

# Intervalos de confiança
ic.gastos.totais.por.regiao <- aggregate(dados$gastos.total,
                                         list(regiao = dados$regiao),
                                         function(x) intervalo.de.confianca.para.a.media(x,
                                                                                         nivel.de.significancia))
names(ic.gastos.totais.por.regiao) <- c("regiao", "ic")

# Médias
media.gastos.totais.por.regiao <-
    aggregate(dados$gastos.total,
              list(regiao = dados$regiao),
              mean)
names(media.gastos.totais.por.regiao) <- c("regiao", "media")

# Quantidade
n.gastos.totais.por.regiao <-
    aggregate(dados$gastos.total,
              list(regiao = dados$regiao),
              length)
names(n.gastos.totais.por.regiao) <- c("regiao", "n")

# Desvio padrão
sd.gastos.totais.por.regiao <-
    aggregate(dados$gastos.total,
              list(regiao = dados$regiao),
              sd)
names(sd.gastos.totais.por.regiao) <- c("regiao", "desvio.padrao")

# Merge
gastos.totais.por.regiao <- merge(ic.gastos.totais.por.regiao,
                                  media.gastos.totais.por.regiao,
                                  by = c("regiao"))

gastos.totais.por.regiao <- merge(gastos.totais.por.regiao,
                                  n.gastos.totais.por.regiao,
                                  by = c("regiao"))

gastos.totais.por.regiao <- merge(gastos.totais.por.regiao,
                                  sd.gastos.totais.por.regiao,
                                  by = c("regiao"))

gastos.totais.por.regiao


# Produz a figura
#png()
ggplot(gastos.totais.por.regiao, aes(regiao, media, ymin = ic[, 1], ymax = ic[, 2])) +
    geom_bar(fill = "white") +
    geom_errorbar()
#dev.off()

# TODO: colorir o gráfico
# TODO: labels do gráfico
# TODO: melhorar largura da barra de erro
# TODO: produzir a imagem em png

## Um gráfico png com os intervalos de confiança das proporções de
## presenças dos deputados em sessões durante o ano, sabendo que o
## número total de sessões que ocorreram no ano foi 202 e considerando o
## número médio de presenças contabilizadas por região. Observe o gráfico
## gerado compare as assiduidades dos deputados de cada região. É
## possível identificar algo interessante com base nos intervalos de
## confiança? É possível identificar alguma relação entre gastos e
## assiduidade? Comente.


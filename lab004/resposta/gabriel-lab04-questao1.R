library("ggplot2")

intervalo.de.confianca.para.a.media <- function(amostra, nivel.de.significancia) {
    tamanho.da.amostra = length(amostra)
    media.amostral = mean(amostra)
    desvio.padrao.amostral = sd(amostra)

    desvio.padrao.da.media = desvio.padrao.amostral / sqrt(tamanho.da.amostra)

    # TODO: calcular a margem do intervalo
    margem = 1

    return(c(media.amostral - margem, media.amostral + margem))
}

# 1. Implemente um script R que plota dois gráficos:
# Para gerar os gráficos acima, considere nível de significância de 5%.

# Um gráfico png com os intervalos de confiança das médias dos gastos
# anuais dos deputados de cada uma das cinco regiões do Brasil. Observe
# o gráfico gerado e compare os gastos dos deputados de cada região. É
# possível concluir algo interessante com base nos intervalos de confiança?
# Existem intervalos maiores que outros? A que você atribui isso?

dados <- read.csv(file = "dados-deputados.csv")

ic.gastos.totais.por.regiao <-
    aggregate(dados$gastos.total,
              list(regiao = dados$regiao),
              function(x) intervalo.de.confianca.para.a.media(x, 0.05))

#names(ic.gastos.totais.por.regiao) <- c("regiao", "min", "max")


media.gastos.totais.por.regiao <-
    aggregate(dados$gastos.total,
              list(regiao = dados$regiao),
              mean)

names(media.gastos.totais.por.regiao) <- c("regiao", "media")


media.gastos.totais.por.regiao

merge(ic.gastos.totais.por.regiao, media.gastos.totais.por.regiao, by = c("regiao"))

#ggplot(dados, aes("regiao", "media", y.min = , y.max =))


# Um gráfico png com os intervalos de confiança das proporções de
# presenças dos deputados em sessões durante o ano, sabendo que o
# número total de sessões que ocorreram no ano foi 202 e considerando o
# número médio de presenças contabilizadas por região. Observe o gráfico
# gerado compare as assiduidades dos deputados de cada região. É
# possível identificar algo interessante com base nos intervalos de
# confiança? É possível identificar alguma relação entre gastos e
# assiduidade? Comente.


library("nortest")

arquivo.de.dados <- commandArgs(trailingOnly = TRUE)[1]

dados <- read.table(file = arquivo.de.dados, header = TRUE)

## 4) Considerando os totais de tempo em que cada máquina permaneceu ociosa e os totais
## de tempo em que cada máquina permaneceu ocupada, implemente uma solução em R que
## permite utilizar os testes Anderson-Darling e Shapiro-Wilk para verificar se os tempos em que
## as máquinas permaneceram nesses estados seguem uma distribuição normal, antes e após
## uma transformação com sqrt e log. Escreva um relatório explicando sua solução, apresentando
## os resultados estatísticos que obteve, discutindo as conclusões que os resultados permitem
## tirar e comparando os resultados gerados pelos testes.


# Calcula tempo total ocupado e ocioso de cada máquina

intervalos.ocupadas <- dados[dados$ociosa == FALSE, ]
intervalos.ociosas <- dados[dados$ociosa == TRUE, ]

tempo.maquinas.ocupadas <- aggregate(intervalos.ocupadas$intervalo,
                                     list(maquina = intervalos.ocupadas$maquina),
                                     sum)
names(tempo.maquinas.ocupadas) <- c("maquina", "tempo")


tempo.maquinas.ociosas <- aggregate(intervalos.ociosas$intervalo,
                                    list(maquina = intervalos.ociosas$maquina),
                                    sum)
names(tempo.maquinas.ociosas) <- c("maquina", "tempo")


# Calcula conjuntos de dados

tempo.ocupadas = tempo.maquinas.ocupadas$tempo
tempo.ociosas = tempo.maquinas.ociosas$tempo

sqrt.tempo.ocupadas = sqrt(tempo.maquinas.ocupadas$tempo)
sqrt.tempo.ociosas = sqrt(tempo.maquinas.ociosas$tempo)

log.tempo.ocupadas = log(tempo.maquinas.ocupadas$tempo)
log.tempo.ociosas = log(tempo.maquinas.ociosas$tempo)


# Produz saidas de resultados

linha.de.resultado.para <- function(dados)
    data.frame(nome = deparse(substitute(dados)), # obtém character da expressão usada
                                                  # como argumento na chamada da função
               shapiro.p.value = shapiro.test(dados)$p.value,
               anderson.p.value = ad.test(dados)$p.value)


rbind(linha.de.resultado.para(tempo.ocupadas),
      linha.de.resultado.para(tempo.ociosas),
      linha.de.resultado.para(sqrt.tempo.ocupadas),
      linha.de.resultado.para(sqrt.tempo.ociosas),
      linha.de.resultado.para(log.tempo.ocupadas),
      linha.de.resultado.para(log.tempo.ociosas))


# Plota as distribuições
png(file = "output-questao4.png", width = 2 * 480, height = 3 * 480)
par(mfrow = c(3,2))
    plot(density(tempo.ocupadas))
    plot(density(tempo.ociosas))

    plot(density(sqrt.tempo.ocupadas))
    plot(density(sqrt.tempo.ociosas))

    plot(density(log.tempo.ocupadas))
    plot(density(log.tempo.ociosas))
dev.off()


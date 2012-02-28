# Script usado para calcular a resposta da questão 4

# Leitura do arquivo
data = read.table("atividade-maquinas-dsc.txt", header = TRUE)

# Funcoes de agregacao
aggregate.tempo <- function(ocioso, funcao.de.agregacao, ...) {
    tempo.por.laboratório = with(data[data$ociosa == ocioso, ],
                                 aggregate(intervalo,
                                           list(laboratorio),
                                           funcao.de.agregacao,
                                           ...)
                                 )
    colnames(tempo.por.laboratório) = c("laboratorio", "x")
    return(tempo.por.laboratório)
}

aggregate.tempo.ocupado <- function(funcao.de.agregacao, ...)
    aggregate.tempo(FALSE, funcao.de.agregacao, ...)

tempo.total.por.laboratorio = aggregate.tempo.ocupado(sum)
names(tempo.total.por.laboratorio) <- c("laboratorio", "tempo.total.ocupado")

print(tempo.total.por.laboratorio[order(tempo.total.por.laboratorio$tempo.total.ocupado, decreasing = TRUE), ])


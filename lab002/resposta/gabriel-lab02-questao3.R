# Leitura do arquivo
data = read.table("atividade-maquinas-dsc.txt", header = TRUE)

summary.tempo.por.maquina <- function() {
    summary.tempo.ocupado.por.maquina = with(data[data$ociosa == FALSE, ],
                                             aggregate(data[data$ociosa == FALSE, ]$intervalo,
                                                       by = list(laboratorio, maquina),
                                                       sum))
    names(summary.tempo.ocupado.por.maquina) <- c("laboratorio", "maquina", "tempo.ocupada")

    summary.tempo.ocioso.por.maquina = with(data[data$ociosa == TRUE, ],
                                            aggregate(data[data$ociosa == TRUE, ]$intervalo,
                                                      by = list(laboratorio, maquina),
                                                      sum))
    names(summary.tempo.ocioso.por.maquina) <- c("laboratorio", "maquina", "tempo.ociosa")

    summary.tempo.por.maquina = merge(summary.tempo.ocupado.por.maquina,
                                      summary.tempo.ocioso.por.maquina)


    summary.tempo.por.maquina$prop.ocupada = summary.tempo.por.maquina$tempo.ocupada /
                                             (summary.tempo.por.maquina$tempo.ociosa +
                                              summary.tempo.por.maquina$tempo.ocupada)

    return(summary.tempo.por.maquina)
}
summary.tempo.por.maquina <- summary.tempo.por.maquina()

top10.maquinas.ocupadas =
    with(summary.tempo.por.maquina,
         summary.tempo.por.maquina[order(prop.ocupada, decreasing = TRUE),
                                   c("maquina", "laboratorio", "prop.ocupada")][1:10, ])

write.table(top10.maquinas.ocupadas, file = "output-questao3.txt")

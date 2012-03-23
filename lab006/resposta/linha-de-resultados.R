
linha.de.resultados.para <- function(resultados.do.teste)
    data.frame(teste = resultados.do.teste$method,
               p.value = resultados.do.teste$p.value,
               dados = resultados.do.teste$data.name)



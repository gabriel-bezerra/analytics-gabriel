# Média
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


# Proporção
sd.proporcao <- function(n.sucessos, n.ensaios) {
    p.hat =  n.sucessos / n.ensaios
    return(sqrt(p.hat * (1 - p.hat)))
}

erro.intervalo.confianca.para.a.proporcao <- function(p.hat, n.ensaios, nivel.de.significancia)
    sqrt((p.hat * (1 - p.hat)) / n.ensaios) *
        ifelse(n.ensaios * p.hat < 10,
               qbinom(1 - (nivel.de.significancia / 2), n.ensaios, p.hat),
               qnorm(1 - (nivel.de.significancia / 2)))

intervalo.de.confianca.para.a.proporcao <- function(n.sucessos, n.ensaios, nivel.de.significancia) {
    p.hat =  n.sucessos / n.ensaios

    margem = erro.intervalo.confianca.para.a.proporcao(p.hat, n.ensaios, nivel.de.significancia)

    return(c(p.hat - margem, p.hat + margem))
}


require(RSNNS)

source("questao1.R")

janela.de.entrada <- dados.de.venda[, 1:3]
previsao <- dados.de.venda[, 4]

dados.de.treinamento <- splitForTrainingAndTest(janela.de.entrada, previsao)

ssePara <- function(model, dados.treinamento)
    sum((model$fittedTestValues - dados.treinamento$targetsTest)^2)

rmsePara <- function(model, dados.treinamento)
    sqrt(mean((model$fittedTestValues - dados.treinamento$targetsTest)^2))


ploteGraficosPara <- function(model) {
    #rodando o modelo para todo o conjunto de dados
    teste.todo.conjunto <- predict(model, janela.de.entrada)

    plot(previsao, type="l")
    lines(teste.todo.conjunto, type="l", col="green")

    #plotando gráfico da evolução do treinamento (a linha vermelha diz respeito ao conjunto de teste)
    plotIterativeError(model)
}


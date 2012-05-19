require(RSNNS)

source("questao1.R")

# Treinamento

janela.de.entrada <- dados.de.venda[, 1:3]
previsao <- dados.de.venda[, 4]

dados.de.treinamento <- splitForTrainingAndTest(janela.de.entrada, previsao)


# Funções para gerar redes neurais

mlpModel <- function(size, maxit, learnFuncParams, dados.treinamento = dados.de.treinamento)
    xModel(mlp, size, maxit, learnFuncParams, dados.treinamento)

elmanModel <- function(size, maxit, learnFuncParams, dados.treinamento = dados.de.treinamento)
    xModel(elman, size, maxit, learnFuncParams, dados.treinamento)

jordanModel <- function(size, maxit, learnFuncParams, dados.treinamento = dados.de.treinamento)
    xModel(jordan, size, maxit, learnFuncParams, dados.treinamento)

xModel <- function(modelName, size, maxit, learnFuncParams, dados.treinamento = dados.de.treinamento)
    do.call(modelName, list(x = dados.treinamento$inputsTrain,
                            y = dados.treinamento$targetsTrain,

                            size = size,
                            learnFuncParams = learnFuncParams,
                            maxit = maxit,

                            inputsTest = dados.treinamento$inputsTest,
                            targetsTest = dados.treinamento$targetsTest))

# Avaliação de desempenho

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

model.arguments.and.performance <- function(argumentList, model, dados.treinamento = dados.de.treinamento) {
    m = do.call(model, argumentList)

    sse = ssePara(m, dados.treinamento)
    rmse = rmsePara(m, dados.treinamento)

    ploteGraficosPara(m)

    data.frame(argumentList, sse, rmse)
}


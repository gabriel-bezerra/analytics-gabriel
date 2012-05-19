source("pre-treinamento.R")

model <- mlp(dados.de.treinamento$inputsTrain,
             dados.de.treinamento$targetsTrain,

             size=0, learnFuncParams = 0.3, maxit=100,

             inputsTest = dados.de.treinamento$inputsTest,
             targetsTest = dados.de.treinamento$targetsTest)


sse = ssePara(model, dados.de.treinamento)
rmse = rmsePara(model, dados.de.treinamento)

print(sse)
print(rmse)


ploteGraficosPara(model)

require("ggplot2")

dados <- read.csv(commandArgs(trailingOnly = TRUE))[, c("largura.petala", "comprimento.petala", "classe")]

grafico <- ggplot(dados, aes(x = largura.petala, y = comprimento.petala, colour = dados$classe)) +
               geom_point() +
               geom_abline(intercept = 2.5, slope = 0) +  # Reta que separa a iris setosa das demais
               geom_abline(intercept = 7.7, slope = -1.75, colour = "red") # Reta que separa a iris virginica e a
                                                                           # versicolor.

classificacao <- function(padrao)
    ifelse(padrao$comprimento.petala < 2.5,
           "Iris-setosa",
           ifelse(padrao$comprimento.petala < (-1.75*padrao$largura.petala + 7.7),
                  "Iris-versicolor",
                  "Iris-virginica")
    )

#FP, TP, FN, TN
fp.tp.fn.tn <-
    sapply(levels(dados$classe),
           function(classe) {
               TP <- classificacao(dados[dados$classe == classe, ]) == classe
               FP <- classificacao(dados[dados$classe != classe, ]) == classe
               TN <- classificacao(dados[dados$classe != classe, ]) != classe
               FN <- classificacao(dados[dados$classe == classe, ]) != classe

               P <- classificacao(dados[dados$classe == classe, ])
               N <- classificacao(dados[dados$classe != classe, ])

               list(TP = length(which(TP)),
                    FP = length(which(FP)),
                    TN = length(which(TN)),
                    FN = length(which(FN)),
                    TPR = length(which(TP)) / length(P),
                    FPR = length(which(FP)) / length(N))
           })

print(fp.tp.fn.tn)

# Precision, Recall, F-Measure
prec.rec.f <-
    sapply(levels(dados$classe),
           function(classe) {
               TP <- fp.tp.fn.tn[[c("TP"), classe]]
               FP <- fp.tp.fn.tn[[c("FP"), classe]]

               P <- length(classificacao(dados[dados$classe == classe, ]))

               PREC <- TP / (TP + FP)
               REC <- TP / P
               F.Measure <- 2 / ((1/PREC) + (1/REC))

               list(PREC = PREC,
                    REC = REC,
                    F.Measure = F.Measure)
           })

print(prec.rec.f)



## cleaning environment --------------------------------------
rm(list = ls())
graphics.off()


# workspace
my.path <- dirname(rstudioapi::getActiveDocumentContext()$path)

# paccotes --------------------------------------------------
packages <- c("tidyverse", "readxl")


# Install packages not yet installed

if (!require(packages)){
    for (ip in packages){
        install.packages(i, dependencies = TRUE)
        library(ip)}}

#limpando o environment novamente
rm(list = ls())



# DEF function for read  --------------------------------------------------------------------------------


Le_arquivos <- function(caminho = "C:/Arquivos R/specdata/specdata", intervalo){
    setwd(caminho)
    Dados_Empilhados <- data.frame()
    for (i in intervalo){
        if (i<10){
            Dados_Empilhados_auxiliar <- readr::read_csv(file = paste0("00", i, ".csv"), progress = F)
            Dados_Empilhados <- rbind(Dados_Empilhados, Dados_Empilhados_auxiliar)}
        else if (i<100){
            Dados_Empilhados_auxiliar <- readr::read_csv(paste0("0", i, ".csv"), progress = F)
            Dados_Empilhados <- rbind(Dados_Empilhados, Dados_Empilhados_auxiliar)}
        else {
            Dados_Empilhados_auxiliar <- readr::read_csv(paste0(i, ".csv"), progress = F)
            Dados_Empilhados <- rbind(Dados_Empilhados, Dados_Empilhados_auxiliar)}}
    return(Dados_Empilhados)
    }
    



 # Question 1 --------------------------------------------------------------------------------------------


pollutantmean <- function(poluente, dados_Leitura){
    dados <- Le_arquivos(intervalo = dados_Leitura)
    if (poluente == "nitrate"){
        media <- mean(x = dados$nitrate, na.rm = TRUE)}
    else if (poluente == "sulfate"){
        media <- mean(x = dados$sulfate, na.rm = TRUE)}
    return(media)}


# pollutantmean("nitrate", 70:72)
pollutantmean(poluente = "nitrate", dados_Leitura = 1:332)



# question 2 --------------------------------------------------------------------------------------------

complete <- function(arq = 1:332){
    dados <- Le_arquivos(intervalo = arq)
    id <- c(arq)
    p <- numeric()
    for (i in sort(arq)){
        dados_aux <- dados[dados$ID == i, ]
        if (i == min(arq)){
            p <- c(sum(complete.cases(dados_aux)))}
        else {
            p <- c(p, sum(complete.cases(dados_aux)))}}

    return(data.frame(ID = id, NOBS = p))}

RNGversion("3.5.1")  
set.seed(42)
cc <- complete(332:1)
use <- sample(332, 10)
print(cc[use, "NOBS"])



# Question 3 --------------------------------------------------------------------------------------------

correla <- function(obs = 0){
    dados <- complete()
    dados <- dados[dados$NOBS >= obs,]
    ids <- dados$ID
    dados_cor <- Le_arquivos(intervalo = ids)
    core <- numeric()
    for (i in ids){
        dados_2 <- as.data.frame(Le_arquivos(intervalo = dados$ i))
        Sulfato <- as.numeric(dados_2$sulfate)
        Nitrato <- as.numeric(dados_2$nitrate)
        core <- c(core, cor(x = Sulfato,y = Nitrato, use = "pairwise.complete.obs"))}
    return(core)
    }


correlação <- correla(400)



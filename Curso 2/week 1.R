
## cleaning environment --------------------------------------
rm(list = ls())
graphics.off()


# workspace
my.path <- dirname(rstudioapi::getActiveDocumentContext()$path)

# paccotes --------------------------------------------------
packages <- c("tidyverse", "readxl")


# Install packages not yet installed

if (!require(packages)){
    for (i in packages){
        install.packages(i, dependencies = TRUE)
        library(i)}}

#limpando o environment novamente
rm(list = ls())


# Set new Directory and read data -----------------------------------------------------------------------

dados <- as.data.frame(readxl::read_xlsx(path =  "hw1_data.xlsx"))

dados$Ozone <- as.numeric(dados$Ozone)
dados$Solar.R <- as.numeric(dados$Solar.R)


attach(dados)
dados
dados[c(1, 2),]
nrow(dados)
dados[c(nrow(dados)-1,nrow(dados)), ]
Ozone[47]

sum(is.na(Ozone)) 

mean(Ozone, na.rm = T)


soma_solar <- Solar.R[(Ozone>31 & Temp > 90)]
soma_solar <- soma_solar[!is.na(soma_solar)]
mean(soma_solar)

temp6 <- Temp[Month==6]
mean(temp6, na.rm = T)

maxmaio <- dados$Ozone[dados$Month==5]
max(maxmaio)

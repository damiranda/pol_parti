knitr::opts_chunk$set(echo = TRUE)
pacman::p_load(lavaan, # pacman para instalar / cargar 
               psych, # para funcion lowerMat
               Hmisc, # rcorr
               stargazer,
               xtable,
               semPlot,
               semTools,
               remedy)

#Análisis factorial exploratorio de las variables de organizaciones voluntarias y actividades políticas

fac <- fa(r = df_AFE, fm= "ml", nfactors=2, rotate="promax")
fac
factor.plot(fac, labels=rownames(fac$loadings))
scree.plot(df_AFE)

fac1 <- fa(r = df_AFE, fm= "ml", nfactors=2, rotate="varimax")
fac1
factor.plot(fac1, labels=rownames(fac$loadings))
scree.plot(df_AFE)

fac2 <- fa(r = df_AFE, fm= "ml", nfactors=3, rotate="promax")
fac2
factor.plot(fac2, labels=rownames(fac$loadings))
scree.plot(df_AFE)

fac3 <- fa(r = df_AFE, fm= "ml", nfactors=3, rotate="varimax")
fac3
factor.plot(fac3, labels=rownames(fac$loadings))
scree.plot(df_AFE)
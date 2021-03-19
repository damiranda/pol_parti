pacman::p_load(dplyr, sjlabelled,summarytools, ggplot2,ggExtra,lme4,texreg, lattice)

#Cancelar algún modelo
rm (m001)

names(data)

#Modelos nulos (sin predictores)
m001 = lmer(formal ~ (1|RBD), data)
m002 = lmer(nodisrruptivo ~ (1|RBD), data)
m003 = lmer(disrruptivo ~ (1|RBD), data)

summary(m001)
summary(m002)
summary(m003)

screenreg(m001)
screenreg(m002)
screenreg(m003)

#Correlación intraclase (Oscila entre 0 (sin variación entre grupos) y 1 (variación entre grupos pero sin variación dentro de los grupos)
0.027/(0.027+0.264) #Formal
0.009/(0.009+0.089) #Participación no disrruptiva
0.044/(0.044+0.353) #Participación disrruptiva

#Visualización de efectos aleatorios
qqmath(ranef(m001, condVar = TRUE))
qqmath(ranef(m002, condVar = TRUE))
qqmath(ranef(m003, condVar = TRUE))

#Modelos con intercepto aleatorio
#Hipótesis 1
m011 <- lmer(formal ~ 1  + SPI + factor(pp_voto_presi) + (1 |RBD), data)
screenreg(list(m001,m011), custom.model.names = c("Modelo Nulo", "Modelo 1.1"))

m012 <- lmer(disrruptivo ~ 1 + SPI + factor(act_marcha)  + (1 |RBD), data)
screenreg(list(m001,m012), custom.model.names = c("Modelo Nulo", "Modelo 1.2"))

m013 <- lmer(nodisrruptivo ~ 1 + SPI + factor(act_marcha)  + (1 |RBD), data)
screenreg(list(m001,m013), custom.model.names = c("Modelo Nulo", "Modelo 1.3"))

screenreg(list(m011,m012,m013), custom.model.names = c("Modelo 1.1", "Modelo 1.2", "Modelo 1.3"))

names(data)

#Hipótesis 2a y 2b
m021 <- lmer(formal ~ 1 + quintiles_ingresos_pc_factor + educ_padres_factor + total_libros + (1 |RBD), data)
screenreg(list(m001,m021), custom.model.names = c("Modelo Nulo", "Modelo 2.1"))

m022 <- lmer(disrruptivo ~ 1 + quintiles_ingresos_pc_factor + educ_padres_factor + total_libros + (1 |RBD), data)
screenreg(list(m002,m022), custom.model.names = c("Modelo Nulo", "Modelo 2.2"))

m023 <- lmer(nodisrruptivo ~ 1 + quintiles_ingresos_pc_factor + educ_padres_factor + total_libros + (1 |RBD), data)
screenreg(list(m003,m023), custom.model.names = c("Modelo Nulo", "Modelo 2.3"))

screenreg(list(m021,m022,m023), custom.model.names = c("Modelo 1.1", "Modelo 1.2", "Modelo 1.3"))



#Hipótesis 3
m031 <- lmer(formal ~ 1  + civico + apertura + (1 |RBD), data)
screenreg(list(m001,m031), custom.model.names = c("Modelo Nulo", "Modelo 3.1"))

m032 <- lmer(disrruptivo ~ 1  + civico + apertura + (1 |RBD), data)
screenreg(list(m002,m032), custom.model.names = c("Modelo Nulo", "Modelo 3.2"))

m033 <- lmer(nodisrruptivo ~ 1  + civico + apertura + (1 |RBD), data)
screenreg(list(m003,m033), custom.model.names = c("Modelo Nulo", "Modelo 3.3"))



#Hipótesis 4 -> Moderación de las variables de escuela sobre la relación
#entre recursos socioeconómicos con participación política
m041 <- lmer(formal ~ 1  +  + apertura + (1 |RBD), data)

m042 <- lmer(formal ~ 1  +  + apertura + (1 |RBD), data)

m043 <- lmer(formal ~ 1  + + apertura + (1 |RBD), data)


#Hipótesis 5 -> Moderación de las variables de escuela sobre
#la relación entre recursos socioeconómicos con participación política
m051 <- lmer(formal ~ 1  + + apertura + (1 |RBD), data)

m052 <- lmer(formal ~ 1  + + apertura + (1 |RBD), data)

m053 <- lmer(formal ~ 1  + + apertura + (1 |RBD), data)

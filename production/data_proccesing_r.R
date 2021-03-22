rm(list=ls())
options(scipen=999)
### TRATAMIENTO DE VARIABLES DE PARTICIPACIÓN POLÍTICA, SOCIALIZACIÓN POLÍTICA ESCOLAR, SEXO, CANTIDAD DE LIBROS

#Abrir base de datos desde Repositorio Github
est_data <- haven::read_dta(file = url("https://github.com/formacionciudadana/data-paces/blob/main/docs/paces/data/base_estudiantesv2.dta?raw=true"))
apod_data <- haven::read_dta(file = url("https://github.com/formacionciudadana/data-paces/blob/main/docs/paces/data/base_apoderadosv2.dta?raw=true"))

#Librerías necesarias para la preparación de datos
pacman::p_load(sjmisc, lavaan, car, sjlabelled, stargazer,
               dplyr, texreg, xtable, # Reporte a latex
               sjPlot, sjmisc, # reporte y gráficos
               skimr,
               corrplot, # grafico correlaciones
               xtable, # Reporte a latex
               Hmisc, # varias funciones
               psych, # fa y principal factors
               psy, # scree plot function
               nFactors, # parallel
               GPArotation, CTT, lavaan, semTable, GGally, QuantPsyc)                


sjlabelled::get_label(apod_data)


#Creación de data frame con variables a utilizar
#P31 A - C corresponden a Participación Formal
#P33 A - H corresponden a Participación Activista y Activista Disruptivo
#P38 A - E corresponden a Socialización político familiar indirecta
#P48 estudiantes A - C corresponden a Socialización político familiar directa
#P49 A - G corresponden a Apertura a la Discusión en el Aula, como dimensión de SPE
#P50 A - G corresponden a Estrategias de aprendizaje cívico activo en la escuela, como dimensión de SPE
#Control y caracterización. P58 Sexo. P68 estudiantes: número de libros. P55 Apoderados: ingresos.


est_data <- est_data %>% select(P31A, 
                                P31B,   
                                P31C,
                                P33A,
                                P33B,
                                P33C,
                                P33D,
                                P33E,
                                P33F,
                                P33G,
                                P33H,
                                P38A,
                                P38B,
                                P38C,
                                P38D,
                                P38E,
                                P48A,
                                P48B,
                                P48C,
                                P49A,
                                P49B,
                                P49C,
                                P49D,
                                P49E,
                                P49F,
                                P49G,
                                P50A,
                                P50B,
                                P50C,
                                P50D,
                                P50E,
                                P50F,
                                P50G,
                                P58,
                                P66,
                                P67,
                                P68,
                                REGION, 
                                Dependencia,
                                folio=FOLIO,
                                RBD)


apod_data <- apod_data %>% select(P17,
                                  P18,
                                  P19,
                                  P20A,
                                  P20B,
                                  P20C,
                                  P20D,
                                  P21A,
                                  P21B,
                                  P21C,
                                  P21D,
                                  P21E,
                                  P21G,
                                  P21H,
                                  P39,
                                  P41,
                                  P45,
                                  P47,
                                  P55,
                                  folio=FOLIO_EST)

#Participación política (base estudiantes)
est_data <- est_data %>% rename("int_mun" = P31A) #Intención de Voto en Municipales
est_data <- est_data %>% rename("int_pre" = P31B) #Intención de voto en Presidenciales
est_data <- est_data %>% rename("int_inf" = P31C) #Intención de informarse
est_data <- est_data %>% rename("par_fir" = P33A) #Desde A hasta H, actividades realizadas durante los últimos 12 meses
est_data <- est_data %>% rename("par_marau" = P33B)
est_data <- est_data %>% rename("par_marnoau" = P33C)
est_data <- est_data %>% rename("par_bloq" = P33D)
est_data <- est_data %>% rename("par_pared" = P33E)
est_data <- est_data %>% rename("par_toma" = P33F)
est_data <- est_data %>% rename("par_servcom" = P33G)
est_data <- est_data %>% rename("par_reupol" = P33H)


#Socialización política escolar (base estudiantes)
est_data <- est_data %>% rename("aper_abiert" = P49A) #Apertura a la discusión (49A a 49G)
est_data <- est_data %>% rename("aper_expr" = P49B)
est_data <- est_data %>% rename("aper_act" = P49C)
est_data <- est_data %>% rename("aper_dist" = P49D)
est_data <- est_data %>% rename("aper_estim" = P49E)
est_data <- est_data %>% rename("aper_punt" = P49F)
est_data <- est_data %>% rename("aper_reflx" = P49G)
est_data <- est_data %>% rename("civ_tall" = P50A) #Estrategias de aprendizaje cívico activo (50A a 50F)
est_data <- est_data %>% rename("civ_simul" = P50B)
est_data <- est_data %>% rename("civ_camp" = P50C)
est_data <- est_data %>% rename("civ_foros" = P50D)
est_data <- est_data %>% rename("civ_charlas" = P50E)
est_data <- est_data %>% rename("civ_medio" = P50F)
est_data <- est_data %>% rename("civ_comun" = P50G)


#Socialización política familiar 
est_data <- est_data %>% rename("conv_noticias" = P38A) #Conversación con los padres de noticias de Chile y el mundo
est_data <- est_data %>% rename("conv_colegio" = P38B) #Conversación con los padres sobre colegio
est_data <- est_data %>% rename("conv_permisos" = P38C) #Conversación con los padres sobre permisos
est_data <- est_data %>% rename("conv_amistades" = P38D) #Conversación con los padres sobre amigos
est_data <- est_data %>% rename("conv_sociopol" = P38E) #Conversación con los padres de temas politicos y sociales
est_data <- est_data %>% rename("pp_voto_presi" = P48A)#Participación política padres: Votar en presidenciales 2017
est_data <- est_data %>% rename("pp_marcha" = P48B) #Participación política padreS: Marchar
est_data <- est_data %>% rename("pp_act_comu" = P48C) #Participación política padres: Participar en actividades comunitarias 


#Socialización política familiar (base apod)
apod_data <- apod_data %>% rename("voto_apod" = P17) #Votación en las últimas elecciones
apod_data <- apod_data %>% rename("voto_prox" = P18) #Votación el domingo
apod_data <- apod_data %>% rename("voto_imp" = P19) #Importancia de elecciones

apod_data <- apod_data %>% rename("act_peticion" = P20A) #Actividad: petición 
apod_data <- apod_data %>% rename("act_marcha" = P20B) #Actividad: marcha 
apod_data <- apod_data %>% rename("act_voluntariado" = P20C) #Actividad: voluntariado 
apod_data <- apod_data %>% rename("act_reu_sociopol" = P20D) #Actividad: reunión sociopolítica 

apod_data <- apod_data %>% rename("org_juntavecinos" = P21A) #Org Voluntaria: Junta de vecinos  
apod_data <- apod_data %>% rename("org_iglesia" = P21B) #Org Voluntaria: iglesia 
apod_data <- apod_data %>% rename("org_ppol" = P21C) #Org Voluntaria: partido político 
apod_data <- apod_data %>% rename("org_sindicato" = P21D) #Org Voluntaria: sindicato  
apod_data <- apod_data %>% rename("org_asocprof" = P21E) #Org Voluntaria: asociación profesional 
apod_data <- apod_data %>% rename("org_caridad" = P21G) #Org Voluntaria: caridad 
apod_data <- apod_data %>% rename("org_cpadres" = P21H) #Org Voluntaria: centros de padres


#Nombrar variables de control y caractización socioeconómica
est_data <- est_data %>% rename("sexo" = P58)
apod_data <- apod_data %>% rename("sexo_apod" = P39)
est_data <- est_data %>% rename("region" = REGION) #Región
est_data <- est_data %>% rename("dependencia" = Dependencia) #Dependencia
est_data <- est_data %>% rename("ed_padre" = P66) #Reportada por estudiantes
est_data <- est_data %>% rename("ed_madre" = P67) #Reportada por estudiante
apod_data <- apod_data %>% rename("educ_apod" = P45) #Reportada por apoderados
apod_data <- apod_data %>% rename("ingresos" = P55) #Ingresos
apod_data <- apod_data %>% rename("cantidad" = P41) #Cantidad de personas en el hogar


#Etiquetar variables
#Etiquetar variables de participación (formal y activistas)
est_data$int_mun <- set_label(x = est_data$int_mun ,label = "Intención de voto en Municipales")
est_data$int_pre <- set_label(x = est_data$int_pre ,label = "Intención de voto en Presidenciales")
est_data$int_inf <- set_label(x = est_data$int_inf ,label = "Intención de informarse antes de elecciones")

est_data$par_fir <- set_label(x = est_data$par_fir ,label = "est_data: Firmar una petición")
est_data$par_marau <- set_label(x = est_data$par_marau ,label = "est_data: Marcha autorizada")
est_data$par_marnoau <- set_label(x = est_data$par_marnoau ,label = "est_data: Marcha no autorizada")
est_data$par_bloq <- set_label(x = est_data$par_bloq,label = "PARTICIPACIÓN: Bloqueo de calles")
est_data$par_pared <- set_label(x = est_data$par_pared,label = "PARTICIPACIÓN: Pintar Paredes")
est_data$par_toma <- set_label(x = est_data$par_toma ,label = "PARTICIPACION: EN tomas")
est_data$par_servcom <- set_label(x = est_data$par_servcom ,label = "PARTICIPACION: Trabajos comunitarios")
est_data$par_reupol <- set_label(x = est_data$par_reupol ,label = "PARTICIPACION: Reuniones políticas")


#Etiquetar variables de Socialización Política Escolar
#Apertura a la discusión en el aula
est_data$aper_abiert <- set_label(x = est_data$aper_abiert ,label = "APER: Estudiantes pueden manifestar desacuerdo en clases")
est_data$aper_expr <- set_label(x = est_data$aper_expr ,label = "APER: Profesores estimulan la opinión de estudiantes")
est_data$aper_act <- set_label(x = est_data$aper_act ,label = "APER: Estudiantes plantean temas de actualidad política en clases")
est_data$aper_dist <- set_label(x = est_data$aper_dist ,label = "APER: Estudiantes expresan opiniones diversas")
est_data$aper_estim <- set_label(x = est_data$aper_estim ,label = "APER: Profesores estimulan dialogo entre gente que opina en clases")
est_data$aper_punt <- set_label(x = est_data$aper_punt ,label = "APER: Profesores exponen temas desde diversos puntos de vista")
est_data$aper_reflx <- set_label(x = est_data$aper_reflx ,label = "APER: Profesores fomentan reflexión y crítica")
#Aprendizaje cívico activo
est_data$civ_tall <- set_label(x = est_data$civ_tall ,label = "ACA: Talleres y Jornadas") #ACA = aprendizaje cívico activo
est_data$civ_simul <- set_label(x = est_data$civ_simul ,label = "ACA: Simulación de elecciones")
est_data$civ_camp <- set_label(x = est_data$civ_camp ,label = "ACA: Campañas de elecciones de CAA")
est_data$civ_foros <- set_label(x = est_data$civ_foros ,label = "ACA: Foros y Debates")
est_data$civ_charlas <- set_label(x = est_data$civ_charlas ,label = "ACA: Charlas con invitados")
est_data$civ_medio <- set_label(x = est_data$civ_medio ,label = "ACA: Actividades Medioambientales")
est_data$civ_comun <- set_label(x = est_data$civ_comun ,label = "ACA: Servicio a la Comunidad")


#Etiquetar variables de Socialización Política Familiar
#Indirecta
est_data$conv_noticias <- set_label(x = est_data$conv_noticias ,label = "Conversación: sobre noticias de Chile y el mundo") 
est_data$conv_colegio <- set_label(x = est_data$conv_colegio ,label = "Conversación: sobre colegio y trabajos escolares")
est_data$conv_permisos <- set_label(x = est_data$conv_permisos ,label = "Conversación: sobre permisos")
est_data$conv_amistades <- set_label(x = est_data$conv_amistades ,label = "Conversación: sobre lo que haces con tus amigos")
est_data$conv_sociopol <- set_label(x = est_data$conv_sociopol ,label = "Conversación: sobre temas políticos y sociales") 


#Directa
est_data$pp_voto_presi <- set_label(x = est_data$pp_voto_presi ,label = "Part Politica: Votación en presidenciales 2017") 
est_data$pp_marcha <- set_label(x = est_data$pp_marcha ,label = "Part Política: Marchar") 
est_data$pp_act_comu <- set_label(x = est_data$pp_act_comu ,label = "Part Política: Participar en actividades comunitarias") 

apod_data$voto_apod <- set_label(x = apod_data$voto_apod ,label = "Voto: últimas elecciones") 
apod_data$voto_prox <- set_label(x = apod_data$voto_prox ,label = "Voto: próximo domingo") 
apod_data$voto_imp <- set_label(x = apod_data$voto_imp ,label = "Voto: importancia elecciones") 

apod_data$act_peticion <- set_label(x = apod_data$act_peticion ,label = "Actividad: petición") 
apod_data$act_marcha <- set_label(x = apod_data$act_marcha ,label = "Actividad: marcha")
apod_data$act_voluntariado <- set_label(x = apod_data$act_voluntariado ,label = "Actividad: voluntariado") 
apod_data$act_reu_sociopol <- set_label(x = apod_data$act_reu_sociopol ,label = "Actividad: reunión sociopolítica") 

apod_data$org_juntavecinos <- set_label(x = apod_data$org_juntavecinos ,label = "Org Voluntaria: Junta de vecinos") 
apod_data$org_iglesia <- set_label(x = apod_data$org_iglesia ,label = "Org Voluntaria: Iglesia") 
apod_data$org_ppol <- set_label(x = apod_data$org_ppol ,label = "Org Voluntaria: partido político") 
apod_data$org_sindicato <- set_label(x = apod_data$org_sindicato ,label = "Org Voluntaria: sindicato")
apod_data$org_asocprof <- set_label(x = apod_data$org_asocprof ,label = "Org Voluntaria: asociación profesional") 
apod_data$org_caridad <- set_label(x = apod_data$org_caridad ,label = "Org Voluntaria: caridad") 
apod_data$org_cpadres <- set_label(x = apod_data$org_cpadres ,label = "Org Voluntaria: centros de padres") 


#Etiquetar variables de control y recursos
est_data$sexo <- set_label(x = est_data$sexo,label = "Sexo")
apod_data$sexo_apod <- set_label(x = apod_data$sexo_apod,label = "Sexo apoderado")
est_data$region <- set_label(x = est_data$region ,label = "Region")
est_data$dependencia <- set_label(x = est_data$dependencia ,label = "Dependencia")
apod_data$ingresos <- set_label(x = apod_data$ingresos ,label = "Ingresos")
est_data$ed_madre <- set_label (x = est_data$ed_madre ,label ="Educación Madre")
est_data$ed_padre <- set_label (x = est_data$ed_padre ,label ="Educación Padre")
apod_data$educ_apod <- set_label (x = apod_data$educ_apod ,label ="Educación padre/madre")
apod_data$cantidad <- set_label (x = apod_data$cantidad ,label ="Cantidad de Personas en Hogar")


#Variables de apertura deben ser cambiadas en su orden (de positivo a negativo, borrar categorías 5 y 9)
#Variables de aprendizaje activo deben ser borrado categorías 3 y 9
#Decretar casos perdidos y eliminar sus antiguas etiquetas
# Participación política
est_data$int_mun <- set_na(est_data$int_mun, na = c(4,9), drop.levels = TRUE, as.tag = FALSE)
est_data$int_pre <- set_na(est_data$int_pre, na = c(4,9), drop.levels = TRUE, as.tag = FALSE)
est_data$int_inf <- set_na(est_data$int_inf, na = c(4,9), drop.levels = TRUE, as.tag = FALSE)
est_data$par_fir<- set_na(est_data$par_fir, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$par_marau <- set_na(est_data$par_marau, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$par_marnoau <- set_na(est_data$par_marnoau, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$par_toma <- set_na(est_data$par_toma, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$par_bloq <- set_na(est_data$par_bloq, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$par_pared <- set_na(est_data$par_pared, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$par_servcom <- set_na(est_data$par_servcom, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$par_reupol <- set_na(est_data$par_reupol, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)


# SPE
est_data$aper_abiert <- set_na(est_data$aper_abiert, na = c(5,9), drop.levels = TRUE, as.tag = FALSE) #Apertura a la discusión
est_data$aper_expr <- set_na(est_data$aper_expr, na = c(5,9), drop.levels = TRUE, as.tag = FALSE)
est_data$aper_act <- set_na(est_data$aper_act, na = c(5,9), drop.levels = TRUE, as.tag = FALSE)
est_data$aper_dist <- set_na(est_data$aper_dist, na = c(5,9), drop.levels = TRUE, as.tag = FALSE)
est_data$aper_estim <- set_na(est_data$aper_estim, na = c(5,9), drop.levels = TRUE, as.tag = FALSE)
est_data$aper_punt <- set_na(est_data$aper_punt, na = c(5,9), drop.levels = TRUE, as.tag = FALSE)
est_data$aper_reflx <- set_na(est_data$aper_reflx, na = c(5,9), drop.levels = TRUE, as.tag = FALSE)
est_data$civ_tall <- set_na(est_data$civ_tall, na = c(3,9), drop.levels = TRUE, as.tag = FALSE) # Estrategias de aprendizaje cívico activo
est_data$civ_simul <- set_na(est_data$civ_simul, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$civ_camp <- set_na(est_data$civ_camp, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$civ_foros <- set_na(est_data$civ_foros, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$civ_charlas <- set_na(est_data$civ_charlas, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$civ_medio <- set_na(est_data$civ_medio, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
est_data$civ_comun <- set_na(est_data$civ_comun, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)


# SPF
est_data$conv_noticias <- set_na(est_data$conv_noticias, na = c(5, 9), drop.levels = TRUE, as.tag = FALSE)
est_data$conv_colegio <- set_na(est_data$conv_colegio, na = c(5, 9), drop.levels = TRUE, as.tag = FALSE)
est_data$conv_permisos <- set_na(est_data$conv_permisos, na = c(5, 9), drop.levels = TRUE, as.tag = FALSE)
est_data$conv_amistades <- set_na(est_data$conv_amistades, na = c(5, 9), drop.levels = TRUE, as.tag = FALSE)
est_data$conv_sociopol <- set_na(est_data$conv_sociopol, na = c(5, 9), drop.levels = TRUE, as.tag = FALSE)


#SPFD
est_data$pp_voto_presi <- set_na(est_data$pp_voto_presi, na = c(3, 9), drop.levels = TRUE, as.tag = FALSE)
est_data$pp_marcha <- set_na(est_data$pp_marcha, na = c(9), drop.levels = TRUE, as.tag = FALSE)
est_data$pp_act_comu <- set_na(est_data$pp_act_comu, na = c(9), drop.levels = TRUE, as.tag = FALSE)

apod_data$voto_apod <- set_na(apod_data$voto_apod, na = c(9), drop.levels = TRUE, as.tag = FALSE)
apod_data$voto_prox <- set_na(apod_data$voto_prox, na = c(3, 9), drop.levels = TRUE, as.tag = FALSE)
apod_data$voto_imp <- set_na(apod_data$voto_imp, na = c(9), drop.levels = TRUE, as.tag = FALSE)

apod_data$act_peticion <- set_na(apod_data$act_peticion, na = c(9), drop.levels = TRUE, as.tag = FALSE)
apod_data$act_marcha <- set_na(apod_data$act_marcha, na = c(9), drop.levels = TRUE, as.tag = FALSE)
apod_data$act_voluntariado <- set_na(apod_data$act_voluntariado, na = c(9), drop.levels = TRUE, as.tag = FALSE)
apod_data$act_reu_sociopol <- set_na(apod_data$act_reu_sociopol, na = c(9), drop.levels = TRUE, as.tag = FALSE)

apod_data$org_juntavecinos <- set_na(apod_data$org_juntavecinos, na = c(9), drop.levels = TRUE, as.tag = FALSE)
apod_data$org_iglesia <- set_na(apod_data$org_iglesia, na = c(9), drop.levels = TRUE, as.tag = FALSE)
apod_data$org_ppol <- set_na(apod_data$org_ppol, na = c(9), drop.levels = TRUE, as.tag = FALSE)
apod_data$org_sindicato <- set_na(apod_data$org_sindicato, na = c(9), drop.levels = TRUE, as.tag = FALSE)
apod_data$org_asocprof <- set_na(apod_data$org_asocprof, na = c(9), drop.levels = TRUE, as.tag = FALSE)
apod_data$org_caridad <- set_na(apod_data$org_caridad, na = c(9), drop.levels = TRUE, as.tag = FALSE)
apod_data$org_cpadres <- set_na(apod_data$org_cpadres, na = c(9), drop.levels = TRUE, as.tag = FALSE)


#Variables de control y recursos socioeconómicos
est_data$sexo <- set_na(est_data$sexo, na = c(3,4,9), drop.levels = TRUE, as.tag = FALSE)
apod_data$sexo_apod <- set_na(apod_data$sexo_apod, na = c(3,9), drop.levels = TRUE, as.tag = FALSE)
apod_data$ingresos <- set_na(apod_data$ingresos, na = c(12, 13), drop.levels = TRUE, as.tag = FALSE)
est_data$ed_madre <- set_na(est_data$ed_madre, na = c(8, 9), drop.levels = TRUE, as.tag = FALSE)
est_data$ed_padre <- set_na(est_data$ed_padre, na = c(8, 9), drop.levels = TRUE, as.tag = FALSE)
apod_data$educ_apod <- set_na(apod_data$educ_apod, na = c(9), drop.levels = TRUE, as.tag = FALSE)
apod_data$cantidad <- set_na(apod_data$cantidad, na = c(999), drop.levels = TRUE, as.tag = FALSE)



#Operacionalización de variable ingresos
apod_data$ingresos_num[apod_data$ingresos == 1] <- 50500 
apod_data$ingresos_num[apod_data$ingresos == 2] <- 117500
apod_data$ingresos_num[apod_data$ingresos == 3] <- 156500
apod_data$ingresos_num[apod_data$ingresos == 4] <- 201500
apod_data$ingresos_num[apod_data$ingresos == 5] <- 257500
apod_data$ingresos_num[apod_data$ingresos == 6] <- 324500
apod_data$ingresos_num[apod_data$ingresos == 7] <- 403000
apod_data$ingresos_num[apod_data$ingresos == 8] <- 724000
apod_data$ingresos_num[apod_data$ingresos == 9] <- 1500000
apod_data$ingresos_num[apod_data$ingresos == 10] <- 2500000
apod_data$ingresos_num[apod_data$ingresos == 11] <- 3500000

frq(apod_data$ingresos_num)

apod_data$ingresos_pc <- apod_data$ingresos_num/apod_data$cantidad
apod_data$ingresos_pc <- trunc(apod_data$ingresos_pc)

# Construcción ingreso tramos como factor
apod_data$ingresos_factor <- factor(apod_data$ingresos, levels = c(1,2,3,4,5,6,7,8,9,10,11,99), 
                               labels = c("Menos de $101.000 mensuales líquidos",
                                          "De $101.001 a $134.000 mensuales líquidos",
                                          "De $134.001 a $179.000 mensuales líquidos",
                                          "De $179.001 a $224.000 mensuales líquidos",
                                          "De $224.001 a $291.000 mensuales líquidos",
                                          "De $291.001 a $358.000 mensuales líquidos",
                                          "De $358.001 a $448.000 mensuales líquidos",
                                          "De $448.001 a $1.000.000 mensuales líquidos",
                                          "De $1.000.001 a $2.000.000 mensuales líquidos",
                                          "De $2.000.001 a $3.000.000 mensuales líquidos",
                                          "Más de $3.000.000 mensuales líquidos",
                                          "Ns/Nr"))

# Construccion ingresos per capita quintiles
apod_data <- apod_data %>% mutate(quintiles_ingresos_pc = ntile(ingresos_pc,5))

#Recuperar NA
apod_data$quintiles_ingresos_pc[is.na(apod_data$quintiles_ingresos_pc)] <- 99

# Construcción ingresos per capita quintiles factor
apod_data$quintiles_ingresos_pc_factor <- factor(apod_data$quintiles_ingresos_pc, levels = c(1,2,3,4,5,99), labels = c("Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5", "Ns/Nr"))

## Eliminar 99 de la variable numerica
apod_data$quintiles_ingresos_pc[apod_data$quintiles_ingresos_pc == 99] <- NA



##################### Unir ambas bases de datos ############################
data <- left_join(x=est_data, y=apod_data, by="folio", suffix=c(".x", ".y"))



#Operacionalización de variable libros
data <- data %>% rename("est_libros" = P68) #Libros reportados por estudiantes
data <- data %>% rename("pad_libros" = P47) #Libros reportados por apoderados


data$est_libros <- set_label(x = data$est_libros,label = "Número de libros estudiantes")
data$pad_libros <- set_label(x = data$pad_libros,label = "Número de libros apoderados")


data$est_libros <- set_na(data$est_libros, na = c(8,9), drop.levels = TRUE, as.tag = FALSE)
data$pad_libros <- set_na(data$pad_libros, na = c(8,9), drop.levels = TRUE, as.tag = FALSE)


data$total_libros <- ifelse(is.na(data$pad_libros), data$est_libros, data$pad_libros)
data$total_libros <- as.factor(data$total_libros)
data$total_libros <- set_labels(data$total_libros,
                                labels=c( "Entre 0 y 10 libros"=1,
                                          "Entre 11 y 25 libros"=2,
                                          "Entre 26 y 100 libros"=3,
                                          "Entre 101 y 200 libros"=4,
                                          "Entre 201 y 500 libros"=5,
                                          "Más de 500 libros"=6))

#Se crea variable libros numérica
data$libros_num <- as.numeric(data$total_libros)


#Operacionalización variable educación padre y madre
# Crear variable nueva
data$educ_padres <- ifelse(data$ed_madre>data$ed_padre,data$ed_madre,data$ed_padre)

# Etiquetar variable nueva
data$educ_padres <- set_label(x = data$educ_padres,label = "Nivel educacional más alto de los padres")

# Factor
data$educ_padres_factor <- factor(data$educ_padres, levels = c(1,2,3,4,5), 
                                  labels = c("No completó 8vo Básico", "8vo básico", "Educación media", "Educación Técnica superior", "Educación universitaria o posgrados"))

# Etiquetar factor
data$educ_padres_factor <- set_label(x = data$educ_padres, label = "Nivel educacional más alto de los padres factor")



#Sexo estudiantes a variable dummy Hombre =1, Mujer = 0 
data$sexo <- car::recode(data$sexo, "1=1;2=0")
data$sexo <- set_labels(data$sexo,
                           labels=c( "Mujer"=0,
                                     "Hombre"=1))


#Se establece como factor
data$sexo <- as.factor(data$sexo)
class(data$sexo)



#Dependencia educacional a factor
data$dependencia <- as.factor(data$dependencia)
class(data$dependencia)



#Region a factor
data$region <- as.factor (data$region)
class(data$region)


#Voto apoderado a dummy
data$voto_apod <- car::recode(data$voto_apod, "2=0")
data$voto_apod <- set_labels(data$voto_apod,
                           labels=c( "No"=0,
                                     "Sí"=1))


#### CONVERSIÓN DE CATEGÓRICAS A DUMMY ####
# Variables dummy - Participación formal (data$int)
data$int_inf <- car::recode(data$int_inf, "1=0;2=1;3=1")
data$int_inf <- set_labels(data$int_inf,
                           labels=c( "No"=0,
                                     "Sí"=1))

data$int_mun <- car::recode(data$int_mun, "1=0;2=1;3=1")
data$int_mun <- set_labels(data$int_mun,
                           labels=c( "No"=0,
                                     "Sí"=1))

data$int_pre <- car::recode(data$int_pre, "1=0;2=1;3=1")
data$int_pre <- set_labels(data$int_pre,
                           labels=c( "No"=0,
                                     "Sí"=1))


#Dummy para variables de participación de estudiantes (disruptivo y no disruptivo)
data$par_fir <- car::recode(data$par_fir, "1=1;2=0")
data$par_fir <- set_labels(data$par_fir,
                                    labels=c( "No"=0,
                                              "Sí"=1))

data$par_marau <- car::recode(data$par_marau, "1=1;2=0")
data$par_marau <- set_labels(data$par_marau,
                                      labels=c( "No"=0,
                                                "Sí"=1))

data$par_marnoau <- car::recode(data$par_marnoau, "1=1;2=0")
data$par_marnoau <- set_labels(data$par_marnoau,
                                        labels=c( "No"=0,
                                                  "Sí"=1))

data$par_toma <- car::recode(data$par_toma, "1=1;2=0")
data$par_toma <- set_labels(data$par_toma,
                                     labels=c( "No"=0,
                                               "Sí"=1))

data$par_bloq <- car::recode(data$par_bloq, "1=1;2=0")
data$par_bloq <- set_labels(data$par_bloq,
                                     labels=c( "No"=0,
                                               "Sí"=1))

data$par_pared <- car::recode(data$par_pared, "1=1;2=0")
data$par_pared <- set_labels(data$par_pared,
                                      labels=c( "No"=0,
                                                "Sí"=1))

data$par_servcom <- car::recode(data$par_servcom, "1=1;2=0")
data$par_servcom <- set_labels(data$par_servcom,
                                        labels=c( "No"=0,
                                                  "Sí"=1))

data$par_reupol <- car::recode(data$par_reupol, "1=1;2=0")
data$par_reupol <- set_labels(data$par_reupol,
                                       labels=c( "No"=0,
                                                 "Sí"=1))


#Orden y conversión en variables de aprendizaje civico activo (data$civ_)
data$civ_tall <- car::recode(data$civ_tall, "1=1;2=0")
data$civ_tall <- set_labels(data$civ_tall,
                            labels=c( "No"=0,
                                      "Sí"=1))

data$civ_simul <- car::recode(data$civ_simul, "1=1;2=0")
data$civ_simul <- set_labels(data$civ_simul,
                             labels=c( "No"=0,
                                       "Sí"=1))

data$civ_camp <- car::recode(data$civ_camp, "1=1;2=0")
data$civ_camp <- set_labels(data$civ_camp,
                            labels=c( "No"=0,
                                      "Sí"=1))

data$civ_foros <- car::recode(data$civ_foros, "1=1;2=0")
data$civ_foros <- set_labels(data$civ_foros,
                             labels=c( "No"=0,
                                       "Sí"=1))

data$civ_charlas <- car::recode(data$civ_charlas, "1=1;2=0")
data$civ_charlas <- set_labels(data$civ_charlas,
                               labels=c( "No"=0,
                                         "Sí"=1))

data$civ_medio <- car::recode(data$civ_medio, "1=1;2=0")
data$civ_medio <- set_labels(data$civ_medio,
                             labels=c( "No"=0,
                                       "Sí"=1))

data$civ_comun <- car::recode(data$civ_comun, "1=1;2=0")
data$civ_comun <- set_labels(data$civ_comun,
                             labels=c( "No"=0,
                                       "Sí"=1))



####### Se genera un índice de las variables de socialización política en el aula data$civ #######
df_civico <- data.frame(data$civ_tall, data$civ_simul, data$civ_camp, 
                        data$civ_foros, data$civ_charlas, data$civ_medio, data$civ_comun)
#Correlaciones
tetrachoric(df_civico)

#Se genera un psych::alpha de cronbach para saber si los indicadores se comportan como índice

alfa_civ <- psych::alpha(df_civico)
alfa_civ

#Se genera un índice sumativo
data$civico <- (data$civ_tall + data$civ_simul + data$civ_camp + 
                data$civ_foros + data$civ_charlas + data$civ_medio + data$civ_comun)
frq(data$civico)



#Apertura a la discusión en el aula data$aper
df_apertura <- data.frame(data$aper_abiert, data$aper_expr, data$aper_dist, 
                            data$aper_estim, data$aper_punt, data$aper_reflx)

#Correlaciones
polychoric(df_apertura)

#Como todos los valorees de las correlaciones son arriba de 0,7, se integran todas
data$apertura <- (data$aper_abiert + data$aper_expr + data$aper_dist + 
                  data$aper_estim + data$aper_punt + data$aper_reflx)/6

#Se genera el alfa de cronbach para evaluar fiabilidad
alfa_conv <- psych::alpha(df_apertura)
alfa_conv



###### Operacionalización socialización política familiar ######
#Socialización política indirecta data$conv
df_conv <- data.frame(data$conv_sociopol, data$conv_noticias, data$conv_colegio, 
                      data$conv_permisos, data$conv_amistades)
#Correlaciones
polychoric(df_conv)

#Alfa de cronbach para fiabilidad
alfa_conv <- psych::alpha(df_conv)
alfa_conv

#Se realiza un índice con aquellas variables que tienen más sentido teórico y mayor correlación
data$SPI <- (data$conv_sociopol + data$conv_noticias)/2



###### Participación política de padres (base estudiantes) data$pp ######
#Variables dummy, participación política apod (data$pp_)
data$pp_act_comu <- car::recode(data$pp_act_comu, "1=1;2=0")
data$pp_act_comu <- set_labels(data$pp_act_comu,
                               labels=c( "No"=0,
                                         "Sí"=1))

data$pp_voto_presi <- car::recode(data$pp_voto_presi, "1=1;2=0;3:9=NA")
data$pp_voto_presi <- set_labels(data$pp_voto_presi,
                                 labels=c( "No"=0,
                                           "Sí"=1))


data$pp_marcha <- car::recode(data$pp_marcha, "1=1;2=0")
data$pp_marcha <- set_labels(data$pp_marcha,
                             labels=c( "No"=0,
                                       "Sí"=1))

df_pp <- data.frame(data$pp_act_comu, data$pp_marcha, data$pp_voto_presi)

#Correlaciones
tetrachoric(df_pp)

#Alfa de cronbach para evaluar fiabilidad
alfa_pp <- psych::alpha(df_pp)
alfa_pp

#Se agrega indicador a base de datos principal
data$pp_apod<- (data$pp_voto_presi + data$pp_act_comu + data$pp_marcha)



###### Participación política de padres (base apoderados) data$act ######
data$act_marcha <- car::recode(data$act_marcha, "2=0;1=1")
data$act_marcha <- set_labels(data$act_marcha,
                                labels=c( "No"=0,
                                          "Sí"=1))


data$act_voluntariado <- car::recode(data$act_voluntariado, "2=0")
data$act_voluntariado <- set_labels(data$act_voluntariado,
                                labels=c( "No"=0,
                                          "Sí"=1))


data$act_reu_sociopol <- car::recode(data$act_reu_sociopol, "2=0;1=1")
data$act_reu_sociopol <- set_labels(data$act_reu_sociopol,
                                labels=c( "No"=0,
                                          "Sí"=1))


data$act_peticion <- car::recode(data$act_peticion, "2=0;1=1")
data$act_peticion <- set_labels(data$act_peticion,
                                labels=c( "No"=0,
                                          "Sí"=1))


df_act <- data.frame(data$act_voluntariado, data$act_marcha, 
                     data$act_peticion, data$act_reu_sociopol)

#Correlaciones
tetrachoric(df_act)

#Alfa de cronbach para evaluar fiabilidad
alfa_act <- psych::alpha(df_act)
alfa_act

#Se agrega a la base de datos principal
data$act_apod <- (data$act_marcha + 
                  data$act_peticion + data$act_reu_sociopol)

frq(data$act_apod)



##### #Participacion política de padres, (base apoderados, organizaciones voluntaria), data$org ######
#Recodificación: No miembro = 0, Miembro = 1
data$org_asocprof <- car::recode(data$org_asocprof, "1=0;2=1;3=1")
data$org_asocprof <- set_labels(data$org_asocprof,
                           labels=c( "No"=0,
                                     "Sí"=1))

data$org_caridad <- car::recode(data$org_caridad, "1=0;2=1;3=1")
data$org_caridad <- set_labels(data$org_caridad,
                                labels=c( "No"=0,
                                          "Sí"=1))

data$org_cpadres <- car::recode(data$org_cpadres, "1=0;2=1;3=1")
data$org_cpadres <- set_labels(data$org_cpadres,
                                labels=c( "No"=0,
                                          "Sí"=1))

data$org_iglesia <- car::recode(data$org_iglesia, "1=0;2=1;3=1")
data$org_iglesia <- set_labels(data$org_iglesia,
                                labels=c( "No"=0,
                                          "Sí"=1))

data$org_juntavecinos <- car::recode(data$org_juntavecinos, "1=0;2=1;3=1")
data$org_juntavecinos <- set_labels(data$org_juntavecinos,
                                labels=c( "No"=0,
                                          "Sí"=1))

data$org_ppol <- car::recode(data$org_ppol, "1=0;2=1;3=1")
data$org_ppol <- set_labels(data$org_ppol,
                                labels=c( "No"=0,
                                          "Sí"=1))

data$org_sindicato<- car::recode(data$org_sindicato, "1=0;2=1;3=1")
data$org_sindicato <- set_labels(data$org_sindicato,
                                labels=c( "No"=0,
                                          "Sí"=1))

#Data frame con variables de organizaciones voluntarias
df_org <- data.frame(data$org_asocprof, data$org_caridad, data$org_cpadres, 
                     data$org_iglesia, data$org_juntavecinos, data$org_ppol,
                     data$org_sindicato)

#Correlaciones poly para reevaluar indicador
tetrachoric(df_org)

#Alfa de Cronbach para evaluar fiabilidad de indicador
alfa_org <- psych::alpha(df_org)
alfa_org

#Se agrega índice sumatico de organizaciones voluntarizs
data$org_vol <- (data$org_asocprof + data$org_caridad +  data$org_cpadres + 
                  data$org_iglesia + data$org_juntavecinos + data$org_ppol +
                  data$org_sindicato)/7

frq(data$org_vol)


########################## Análisis Factorial Confirmatorio #############################
#Especificación del Modelo
cfa_1 <- '
# latent variables
formal =~ int_inf + int_pre + int_mun
nodisruptivo =~ par_fir + par_servcom + par_reupol + par_marau
disruptivo =~ par_marnoau + par_toma + par_bloq + par_pared'

#Selección de variables
fit_1 <- cfa(cfa_1,data, ordered=c("par_fir","par_marau","par_marnoau","par_toma","par_bloq", "par_pared", "par_servcom", "par_reupol","int_mun","int_pre","int_inf"))

#Resumen ajuste general
show(fit_1) 

#Resumen de indicadores
fitMeasures(fit_1, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea")) 

#Selección de variables originales con folio
data2 <- data %>% select("folio", "par_fir","par_marau","par_marnoau","par_toma","par_bloq", "par_pared", "par_servcom", "par_reupol","int_mun","int_pre","int_inf") %>% na.omit()

#Se predicen los puntajes factoriales por variables
predict <- predict(fit_1)

#Puntajes factoriales se guardan como df
scores <- as.data.frame(predict)

#Se unen los puntajes factoriales con la variable con el folio
data2 = bind_cols(data2, scores)

#Nueva base de datos con folio y las variables con puntajes factoriales
data3 = data2 %>% select(folio, formal, nodisruptivo, disruptivo)

#Merge
data = left_join(x = data, y=data3, by="folio")

#Resumen bdd
frq(data)

#Guardar base nueva
save(data, file = "input/data.RData")


# Guardar base de datos apoderados
save(apod_data, file = "input/apod_data.RData")


# Guardar base de datos estudiantes
save(est_data, file = "input/est_data.RData")
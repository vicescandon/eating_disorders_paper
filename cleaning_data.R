##################
#TESIS
##################

library(pacman)
p_load(readxl, readr, tidyverse, writexl, haven, fixest, survey)

# Leer bases 2006
data2006jov <- read_csv("~/CIDE/Tésis/datos2/2006jov.csv")
data2006hog <- read.csv("~/CIDE/Tésis/datos2/2006hogar.csv")
data2006int<- read.csv("~/CIDE/Tésis/datos2/2006int.csv")
# Leer bases 2012
data2012jov <- read_csv("~/CIDE/Tésis/datos2/2012jov.csv")
data2012hog <- read_csv("~/CIDE/Tésis/datos2/2012hogar.csv")
data2012int <- read_csv("~/CIDE/Tésis/datos2/2012int.csv")
# Leer bases 2018
data2018jov <- read_delim("~/CIDE/Tésis/datos2/2018jov.csv")
data2018hog <- read_delim("~/CIDE/Tésis/datos2/2018hogar.csv")
data2018viv <- read_delim("~/CIDE/Tésis/datos2/2018vivienda.csv")
data2018int <- read_delim("~/CIDE/Tésis/datos2/2018int.csv")

# Merge 2006 por folio_v y folio (unir jov, hog e int)
data2006jov$folio_v<-as.numeric(data2006jov$folio_v) #convertir folio a numerico, sin esto no funciona left_join()
data2006jov$folio<-as.numeric(data2006jov$folio)
data2006<-left_join(data2006int,data2006hog,by=c("folio_v","folio"))
data2006<-left_join(data2006jov,data2006,by=c("folio_v","folio","folioi"))
data2006<- data2006 %>% 
  mutate(.,d2006=1,
         d2008=0,
         d2012=0)

# Merge 2012 por folio e intp (unir jov, hog e int)
data2012<-left_join(data2012int,data2012hog,by="folio")
data2012<-left_join(data2012jov,data2012,by=c("folio","intp"))
data2012<-data2012 %>% 
  mutate(.,d2012=1,
         d2006=0,
         d2018=0)

# Merge 2018
data2018<-data2018hog %>% 
  select(-NUMREN) %>% 
  left_join(.,data2018viv,by=c("UPM","VIV_SEL"))
data2018<-left_join(data2018int,data2018,by=c("UPM","VIV_SEL","HOGAR"))
data2018<-left_join(data2018jov,data2018,by=c("UPM","VIV_SEL","HOGAR","NUMREN"))
data2018<-data2018 %>% 
  mutate(.,d2018=1,
         d2006=0,
         d2012=0)

rm(data2006hog,data2006int,data2006jov,data2012hog,data2012int,data2012jov,data2018hog,data2018int,data2018jov,data2018viv)

#renombrar variables 

# data2006 <- data2006 %>% 
#   rename(engordar = d309,
#          comer_demasiado = d310,
#          perder_control = d311,
#          vomitar = d312,
#          ayuno = d313,
#          dieta = d314,
#          ejercicio = d315,
#          pastillas = d316,
#          diureticos = d317,
#          laxantes = d318,
#          bien_telefono = h505h, #teléfono
#          computadora = h505f, #computadora
#          #double check bien_telefono y computadora
#          engordar = d401a,
#          comer_demasiado = d401b,
#          perder_control = d401c,
#          vomitar = d401d,
#          ayuno = d401e,
#          dieta = d401f,
#          ejercicio = d401g,
#          pastillas = d401h,
#          celular = h60124,
#          internet = h60118,
#          computadora = h60117,
#          #controles de joven
#          sexo = dsexo,
#          edad = dedad,
#          agresion_sufrir = d601,
#          agresion_substancias = d602a,
#          agresion_ahorcamiento = d602b,
#          agresion_armafuego = d602c,
#          agresion_punzocortante = d602d,
#          agresion_empujonelevado = d602e,
#          agresion_golpes = d602f,
#          agresion_sexual = d602g,
#          agresion_envenenamiento = d602h,
#          agresion_verbal = d602j,
#          agresion_otro = d602k,
#          agresion_nsnr = d602l,
#          agresion_esp = d602esp,
#          agresion_motivo = d603,
#          agresion_motivo_esp = d603esp,
#          agresion_lugar = d604,
#          agresion_quienatendio = d606,
#          agresion_quienatendio_esp = d606esp,
#          agresion_bajoefectos = d607,
#          agresion_bajoefectos_esp = d607esp,
#          agresion_efectopermanente = d608,
#          agresion_efectopermanente_esp = d608esp,
#          #otros posibles outcomes de salud mental
#          auto_daño = d701,
#          auto_medicamento = d702a,
#          auto_narcotico = d702b,
#          auto_alcohol = d702c,
#          auto_hidrocarburo = d702d,
#          auto_insecticida = d702e,
#          auto_quimicos = d702f,
#          auto_ahorcamiento = d702g,
#          auto_armafuego = d702h,
#          auto_quemadura = d702i,
#          auto_punzocortante = d702j,
#          auto_vehiculomov = d702k,
#          auto_otro = d702l,
#          auto_nsnr = d702m,
#          auto_esp = d702esp,
#          auto_recibe_trat = d703,
#          #Hogar y vivienda
#          numero_habitantes = h101,
#          gastocompartido_comida = h102,
#          numero_gastoseparado_comida = h103,
#          vive_madre = h208,
#          vive_padre = h209,
#          donde_consulta1 = h210a,
#          donde_consulta2 = h210b,
#          afiliacion_serviciomedico1 = h211a,
#          afiliacion_serviciomedico2 = h211b,
#          lengua_indigena = h212,
#          indigena = h215,
#          leer_escribir = h216,
#          atiende_escuela = h217,
#          escolaridad_grado = h218a,
#          escolaridad_nivel = h218b,
#          trabaja = h221,
#          ultima_enfermedad = h302,
#          ultima_enfermedad_esp = h302esp,
#          atencion_salud = h302a,
#          motivo_no_atencion_salud = h305a,
#          # específicamente hogar
#          material_muros = h501,
#          material_techo = h502,
#          material_piso = h503,
#          cocina = h504,
#          numero_dormitorios = h505,
#          total_cuartos = h506,
#          luz = h507,
#          disponibilidad_agua = h508,
#          drenaje = h513,
#          combustible = h514,
#          eliminacion_basura = h515,
#          tenencia = h516,
#          calefaccion = h520,
#          numero_focos = h523,
#          #bienes
#          otro_inmueble = h60101,
#          automovil = h60102,
#          camioneta = h60103,
#          moto = h60104,
#          otro_vehiculo = h60105,
#          television = h60106,
#          tv_paga = h60107,
#          refrigerador = h60112,
#          estufa = h60113,
#          lavadora = h60115,
#          computadora = h60117,
#          microondas = h60119,
#          aire_acondicionado = h60125,
#          gasto_hogar = h70202,
#          gasto_educacion = h70203,
#          gasto_entretenimiento = h70206,
#          gasto_vestimenta = h70311,
#          gasto_viajes = h70403,
#          gasto_autos = h70404,
#          ponderador= pondef
#          ) 





data2012 <- data2012 %>% 
  rename(engordar = d401a,
         comer_demasiado = d401b,
         perder_control = d401c,
         vomitar = d401d,
         ayuno = d401e,
         dieta = d401f,
         ejercicio = d401g,
         pastillas = d401h,
         celular = h60124,
         internet = h60118,
         computadora = h60117,
         #controles de joven
         sexo = dsexo,
         edad = dedad,
         agresion_sufrir = d601,
         # agresion_substancias = d602a,
         # agresion_ahorcamiento = d602b,
         # agresion_armafuego = d602c,
         # agresion_punzocortante = d602d,
         # agresion_empujonelevado = d602e,
         # agresion_golpes = d602f,
         # agresion_sexual = d602g,
         # agresion_envenenamiento = d602h,
         # agresion_verbal = d602j,
         # agresion_otro = d602k,
         # agresion_nsnr = d602l,
         # agresion_esp = d602esp,
         # agresion_motivo = d603,
         # agresion_motivo_esp = d603esp,
         # agresion_lugar = d604,
         # agresion_quienatendio = d606,
         # agresion_quienatendio_esp = d606esp,
         # agresion_bajoefectos = d607,
         # agresion_bajoefectos_esp = d607esp,
         # agresion_efectopermanente = d608,
         # agresion_efectopermanente_esp = d608esp,
         #otros posibles outcomes de salud mental
         auto_daño = d701,
         # auto_medicamento = d702a,
         # auto_narcotico = d702b,
         # auto_alcohol = d702c,
         # auto_hidrocarburo = d702d,
         # auto_insecticida = d702e,
         # auto_quimicos = d702f,
         # auto_ahorcamiento = d702g,
         # auto_armafuego = d702h,
         # auto_quemadura = d702i,
         # auto_punzocortante = d702j,
         # auto_vehiculomov = d702k,
         # auto_otro = d702l,
         # auto_nsnr = d702m,
         # auto_esp = d702esp,
         # auto_recibe_trat = d703,
         #Hogar y vivienda
         numero_habitantes = h101,
         # gastocompartido_comida = h102,
         # numero_gastoseparado_comida = h103,
         vive_madre = h208,
         vive_padre = h209,
         donde_consulta1 = h210a,
         # donde_consulta2 = h210b,
         afiliacion_serviciomedico1 = h211a,
         afiliacion_serviciomedico2 = h211b,
         lengua_indigena = h212,
         indigena = h215,
         leer_escribir = h216,
         atiende_escuela = h217,
         escolaridad_grado = h218a,
         escolaridad_nivel = h218b,
         trabaja = h221,
         ultima_enfermedad = h302,
         ultima_enfermedad_esp = h302esp,
         atencion_salud = h302a,
         motivo_no_atencion_salud = h305a,
         # específicamente hogar
         material_muros = h501,
         material_techo = h502,
         material_piso = h503,
         cocina = h504,
         numero_dormitorios = h505,
         total_cuartos = h506,
         luz = h507,
         disponibilidad_agua = h508,
         drenaje = h513,
         # combustible = h514,
         eliminacion_basura = h515,
         # tenencia = h516,
         calefaccion = h520,
         numero_focos = h523,
         #bienes
         otro_inmueble = h60101,
         automovil = h60102,
         camioneta = h60103,
         moto = h60104,
         otro_vehiculo = h60105,
         television = h60106,
         tv_paga = h60107,
         refrigerador = h60112,
         estufa = h60113,
         lavadora = h60115,
         computadora = h60117,
         microondas = h60119,
         aire_acondicionado = h60125,
         gasto_hogar = h70202,
         gasto_educacion = h70203,
         gasto_entretenimiento = h70206,
         gasto_vestimenta = h70311,
         gasto_viajes = h70403,
         gasto_autos = h70404,
         ponderador= pondef
         )

#Conversión de categorías 2012

#Variables tca, fuera de 1,2,3,4 hacer NA
data2012[c("engordar","comer_demasiado","perder_control","vomitar","ayuno","dieta","ejercicio","pastillas" )] <- 
  lapply(data2012[c("engordar","comer_demasiado","perder_control","vomitar","ayuno","dieta","ejercicio","pastillas" )], function(x) ifelse(x %in% c(1, 2, 3, 4), x, NA))
#variables tca dummies
data2012[c("engordar","comer_demasiado","perder_control","vomitar","ayuno","dieta","ejercicio","pastillas" )] <- 
  lapply(data2012[c("engordar","comer_demasiado","perder_control","vomitar","ayuno","dieta","ejercicio","pastillas" )], function(x) ifelse(x == 1, 0, ifelse(x %in% c(2, 3, 4), 1, NA)))

#tca's a una sola variable
variables_tca <- c("engordar", "comer_demasiado", "perder_control", "vomitar", 
                   "ayuno", "dieta", "ejercicio", "pastillas")
data2012$tca <- ifelse(rowSums(data2012[ , variables_tca] == 1, na.rm = TRUE) > 0, 1, 0)




#variables que se harás dummies y el ns/nr se hace NA
var_dummies<-c("computadora","internet","celular","agresion_sufrir")
data2012[var_dummies] <- 
  lapply(data2012[var_dummies], function(x) ifelse(x == 1, 1, ifelse(x == 2, 0, NA)))

#variable sexo convertir a dummie mujer y renonbrarla
data2012$sexo <- ifelse(data2012$sexo == 1,0,1)
data2012 <- data2012 %>% 
  rename(mujer = sexo)

#variable auto_daño 1 y 2 hacer 1, lo demás 0, o NA para nsnr
data2012$auto_daño <- ifelse(data2012$auto_daño %in% c(1, 2), 1,  # Si es 1 o 2 → 1
                             ifelse(data2012$auto_daño == 9, NA, 0))  # Si es 9 → NA, cualquier otro → 0

#variables viven padres omadres, binaria, 1 es sí vive
data2012$vive_madre <- ifelse(data2012$vive_madre == 1,1,0)
data2012$vive_padre <- ifelse(data2012$vive_padre == 1,1,0)

#variable donde consulta hacerlas dummies
data2012 <- data2012 %>%
  mutate(
    donde_consulta_imss = ifelse(donde_consulta1 == 1, 1, 0),
    donde_consulta_issste = ifelse(donde_consulta1 == 2, 1, 0),
    donde_consulta_isssteestatal = ifelse(donde_consulta1 == 3, 1, 0),
    donde_consulta_pemex = ifelse(donde_consulta1 == 4, 1, 0),
    donde_consulta_defensamarina = ifelse(donde_consulta1 == 5, 1, 0),
    donde_consulta_centrosalud = ifelse(donde_consulta1 == 6, 1, 0),
    donde_consulta_imssprospera = ifelse(donde_consulta1 == 7, 1, 0),
    donde_consulta_farmacia = ifelse(donde_consulta1 == 8, 1, 0),
    donde_consulta_privado = ifelse(donde_consulta1 == 9, 1, 0),
    donde_consulta_automedica = ifelse(donde_consulta1 == 10, 1, 0),
    donde_consulta_esp = ifelse(donde_consulta1 == 77, 1, 0),
    donde_consulta_noatiende = ifelse(donde_consulta1 == 12, 1, 0),
    donde_consulta_ns = ifelse(donde_consulta1 == 99, 1, 0)
  )

#variable afiliacion hacerlas dummies
data2012 <- data2012 %>%
  mutate(
    afiliacion_imss = ifelse(afiliacion_serviciomedico1 == 1, 1, 0),
    afiliacion_issste = ifelse(afiliacion_serviciomedico1 == 2, 1, 0),
    afiliacion_isssteestatal = ifelse(afiliacion_serviciomedico1 == 3, 1, 0),
    afiliacion_pemex = ifelse(afiliacion_serviciomedico1 == 4, 1, 0),
    afiliacion_defensamarina = ifelse(afiliacion_serviciomedico1 == 5, 1, 0),
    afiliacion_seguropopular = ifelse(afiliacion_serviciomedico1 == 6, 1, 0),
    afiliacion_privado = ifelse(afiliacion_serviciomedico1 == 7, 1, 0),
    afiliacion_otra = ifelse(afiliacion_serviciomedico1 == 8, 1, 0),
    afiliacion_no = ifelse(afiliacion_serviciomedico1 == 9, 1, 0),
    afiliacion_nosabe = ifelse(afiliacion_serviciomedico1 == 99, 1, 0)
  )

#variable lengua indigena a dummies
data2012$lengua_indigena <- ifelse(data2012$lengua_indigena == 1,1,0)
#variable leer y escribir a dummies
data2012$leer_escribir <- ifelse(data2012$leer_escribir == 1,1,0)
#variable atiende_escuela a dummies
data2012$atiende_escuela <- ifelse(data2012$atiende_escuela == 1,1,0)
#variable trabaja a dummies
data2012$trabaja <- ifelse(data2012$trabaja == 1,1,0)

#Variable muros nueva categoría
data2012 <- data2012 %>%
  mutate(
    material_muros_cat = case_when(
      material_muros == 1 ~ "Material de desecho",
      material_muros == 2 ~ "Lámina de cartón",
      material_muros == 3 ~ "Lámina de asbesto o metálica",
      material_muros == 4 ~ "Carrizo, bambú o palma",
      material_muros == 5 ~ "Embarro, bajareque o paja",
      material_muros == 6 ~ "Madera",
      material_muros == 7 ~ "Adobe",
      material_muros == 8 ~ "Tabique, ladrillo, block, piedra, cantera, cemento o concreto"
    )
  )
data2012$material_muros_cat <- factor(data2012$material_muros_cat)


#variables techo a categórica
data2012 <- data2012 %>%
  mutate(
    material_techo_cat = case_when(
      material_techo == 1 ~ "Material de desecho",
      material_techo == 2 ~ "Lámina de cartón",
      material_techo == 3 ~ "Lámina metálica",
      material_techo == 4 ~ "Lámina de asbesto",
      material_techo == 5 ~ "Palma o paja",
      material_techo == 6 ~ "Madera o tejamanil",
      material_techo == 7 ~ "Terrado con viguería",
      material_techo == 8 ~ "Teja",
      material_techo == 9 ~ "Losa de concreto o viguetas con bovedilla"
    )
  )
data2012$material_techo_cat <- factor(data2012$material_techo_cat)

#Variable piso a categórica
data2012 <- data2012 %>%
  mutate(
    material_piso_cat = case_when(
      material_piso == 1 ~ "Tierra",
      material_piso == 2 ~ "Cemento o firme",
      material_piso == 3 ~ "Madera, mosaico u otro recubrimiento"
    )
  )
data2012$material_piso_cat <- factor(data2012$material_piso_cat)

#variable cocina a dummy
data2012$cocina <- ifelse(data2012$cocina == 1,1,0)
#variable luz a dummy
data2012$luz <- ifelse(data2012$luz == 1,1,0)
#variable disponibilidad_agua reducida a 1 si hay agua entubada en el hogar, 0 en otro caso
data2012$disponibilidad_agua <- ifelse(data2012$disponibilidad_agua == 1,1,0)
#variable drenaje reducida a 1 si hay drenaje o desague conectado a red pública, 0 en otro caso
data2012$drenaje <- ifelse(data2012$drenaje == 1,1,0)
#variable eliminación basura 1 si pasan por ella al hogar, 0 en otro caso
data2012$eliminacion_basura <- ifelse(data2012$eliminacion_basura == 1,1,0)
#variable calefacción 1 en caso de sí, 0 otherwise
data2012$calefaccion <- ifelse(data2012$calefaccion == 1,1,0)
#variables bienes dummy
bienes_2012_d<-c("otro_inmueble","automovil","camioneta","moto","otro_vehiculo","television","tv_paga","refrigerador","estufa","lavadora","computadora","microondas","aire_acondicionado")
data2012[bienes_2012_d] <- lapply(data2012[bienes_2012_d], function(x) {
  ifelse(x == 1, 1, ifelse(x == 2, 0, NA))
})

#variable escolaridad, se usará la acumulada de años ya cursados, considerando kinder como 3 años
data2012 <- data2012 %>%
  mutate(escolaridad = case_when(
    escolaridad_grado > 6 ~ NA_real_,                                # Si 'grado' es mayor a 6, asigna NA
    escolaridad_nivel == 0 ~ escolaridad_grado,                      # No asiste a la escuela
    escolaridad_nivel == 1 ~ escolaridad_grado,                      # Preescolar/kínder
    escolaridad_nivel == 2 ~ 3 + escolaridad_grado,                    # Primaria: 3 años de preescolar + avance en primaria
    escolaridad_nivel == 3 ~ 3 + 6 + escolaridad_grado,                # Secundaria: 3 (preescolar) + 6 (primaria) + avance en secundaria
    escolaridad_nivel == 4 ~ 3 + 6 + 3 + escolaridad_grado,            # Preparatoria: 3+6+3 + avance en prepa
    escolaridad_nivel == 5 ~ 3 + 6 + 3 + escolaridad_grado,            # Normal básica (se toma como prepa)
    escolaridad_nivel == 6 ~ 3 + 6 + escolaridad_grado,                # Estudios técnicos o comerciales con primaria terminada: 3 (preescolar) + 6 (primaria) + avance en ese nivel
    TRUE ~ NA_real_                                                  # Para otros casos atípicos
  ))

#generar id_municipio con 00_000
data2012$ENT <- sprintf("%02d", as.numeric(data2012$entidad))     # Estado con 2 dígitos
data2012$munici <- sprintf("%03d", as.numeric(data2012$munici)) # Municipio con 3 dígitos

# Ahora las unimos en una nueva variable de 5 dígitos
data2012$id_mun <- paste0(data2012$ENT, data2012$munici)



data2018 <- data2018 %>%
  rename(engordar = P4_1_1.x,
         comer_demasiado = P4_1_2.x,
         perder_control = P4_1_3.x,
         vomitar = P4_1_4.x,
         ayuno = P4_1_5.x,
         dieta = P4_1_6.x,
         ejercicio = P4_1_7.x,
         pastillas =P4_1_8.x,
         diureticos = P4_1_9,
         laxantes = P4_1_10,
         computadora = P6_1_5,
         celular = P6_1_6,
         internet = P6_1_7,
         
         #controles de joven
         sexo = SEXO.x,
         edad = EDAD.y,
         agresion_sufrir = P7_1,
         # agresion_substancias = d602a,#
         # agresion_ahorcamiento = d602b,#
         # agresion_armafuego = d602c,#
         # agresion_punzocortante = d602d,#
         # agresion_empujonelevado = d602e,#
         # agresion_golpes = d602f,#
         # agresion_sexual = d602g, ##
         # agresion_envenenamiento = d602h, ##
         # agresion_verbal = d602j, ##
         # agresion_otro = d602k,#
         # agresion_nsnr = d602l, #
         # agresion_esp = d602esp, # 
         # agresion_motivo = P7_3,
         # agresion_motivo_esp = d603esp, #
         # agresion_lugar = P7_5.x,
         # agresion_quienatendio = P7_4.x,
         # agresion_quienatendio_esp = d606esp, #
         # agresion_bajoefectos = P7_6,
         # agresion_bajoefectos_esp = d607esp, #
         # agresion_efectopermanente = d608, #
         # agresion_efectopermanente_esp = d608esp, #
         #otros posibles outcomes de salud mental
         auto_daño = P7_17, 
         # auto_medicamento = d702a, #
         # auto_narcotico = d702b, #
         # auto_alcohol = d702c, #
         # auto_hidrocarburo = d702d, #
         # auto_insecticida = d702e, #
         # auto_quimicos = d702f, #
         # auto_ahorcamiento = d702g, #
         # auto_armafuego = d702h, #
         # auto_quemadura = d702i, #
         # auto_punzocortante = d702j, #
         # auto_vehiculomov = d702k, #
         # auto_otro = d702l, #
         # auto_nsnr = d702m, #
         # auto_esp = d702esp, #
         # auto_recibe_trat = d703, #
         #Hogar y vivienda
         numero_habitantes = P2_3.y,
         # gastocompartido_comida = P2_1.y,
         # numero_gastoseparado_comida = P2_2,
         vive_madre = P3_7.y,
         vive_padre = P3_8.y,
         donde_consulta_imss = P3_9_01,
         donde_consulta_issste = P3_9_02,
         donde_consulta_isssteestatal = P3_9_03,
         donde_consulta_pemex = P3_9_04,
         donde_consulta_defensa = P3_9_05,
         donde_consulta_marina = P3_9_06,
         donde_consulta_centrosalud = P3_9_07,
         donde_consulta_imssprospera = P3_9_08,
         donde_consulta_farmacia = P3_9_09,
         donde_consulta_privado = P3_9_10,
         donde_consulta_automedica = P3_9_11,
         donde_consulta_esp = P3_9_77,
         donde_consulta_noatiende = P3_9_12,
         donde_consulta_ns = P3_9_99,
         afiliacion_imss = P3_10_01,
         afiliacion_issste = P3_10_02,
         afiliacion_isssteestatal = P3_10_03,
         afiliacion_pemex = P3_10_04,
         afiliacion_defensa = P3_10_05,
         afiliacion_marina = P3_10_06,
         afiliacion_seguropopular = P3_10_07,
         afiliacion_imssprospera = P3_10_08,
         afiliacion_privado = P3_10_09,
         afiliacion_otra = P3_10_10,
         afiliacion_no = P3_10_11,
         afiliacion_nosabe = P3_10_99,
         lengua_indigena = P3_11,
         # indigena = P3_11, #
         leer_escribir = P3_18,
         atiende_escuela = P3_13,
         escolaridad_grado = GRADO,
         escolaridad_nivel = NIVEL,
         trabaja = P3_21,
         # ultima_enfermedad = h302, #
         # ultima_enfermedad_esp = h302esp, #
         # atencion_salud = h302a, #
         # motivo_no_atencion_salud = h305a, #
         # específicamente hogar
         material_muros = P1_2.y,
         material_techo = P1_1.y,
         material_piso = P1_3.y,
         cocina = P1_6,
         numero_dormitorios = P1_4.y,
         total_cuartos = P1_5.y,
         luz = P1_11.y,
         disponibilidad_agua = P1_12.y,
         drenaje = P1_15,
         # combustible = P1_9.y,
         eliminacion_basura = P1_19,
         # tenencia = P1_23,
         calefaccion = P1_20,
         # numero_focos = h523, #
         #bienes
         otro_inmueble = P1_25_1, 
         automovil = P1_25_2,
         camioneta = P1_25_3,
         moto = P1_25_4,
         otro_vehiculo = P1_25_5,
         television = P6_1_1,
         tv_paga = P6_1_2,
         refrigerador = P6_1_11,
         estufa = P6_1_12,
         lavadora = P6_1_14,
         computadora = P6_1_5,
         microondas = P6_1_15,
         aire_acondicionado = P1_24_5, 
         # gasto_hogar = h70202, #
         # gasto_educacion = h70203, #
         # gasto_entretenimiento = h70206, #
         # gasto_vestimenta = h70311, #
         # gasto_viajes = h70403, #
         # gasto_autos = h70404 #
         ponderador = F_10A19
         )

#Conversión categorías 2018

#variables tca dummies y juntar past, lax y diur en past, eliminar las que ya no se usan(diur y lax)
data2018[c("engordar","comer_demasiado","perder_control","vomitar","ayuno","dieta","ejercicio","pastillas","diureticos","laxantes")] <- 
  lapply(data2018[c("engordar","comer_demasiado","perder_control","vomitar","ayuno","dieta","ejercicio","pastillas","diureticos","laxantes")], function(x) ifelse(x == 1, 0, ifelse(x %in% c(2, 3, 4), 1, NA)))
data2018$pastillas <- ifelse(data2018$pastillas == 1 | data2018$diureticos == 1 | data2018$laxantes == 1, 1, 0)
data2018 <- data2018 %>% 
  select(-c(diureticos, laxantes))
#tca's a una sola variable
variables_tca <- c("engordar", "comer_demasiado", "perder_control", "vomitar", 
                   "ayuno", "dieta", "ejercicio", "pastillas")
data2018$tca <- ifelse(rowSums(data2018[ , variables_tca] == 1, na.rm = TRUE) > 0, 1, 0)



#variables que se harás dummies y el ns/nr se hace NA
var_dummies<-c("computadora","internet","celular","agresion_sufrir","auto_daño")
data2018[var_dummies] <- 
  lapply(data2018[var_dummies], function(x) ifelse(x == 1, 1, ifelse(x == 2, 0, NA)))

#variable sexo convertir a dummie mujer y renombrarla
data2018$sexo <- ifelse(data2018$sexo == 1,0,1)
data2018 <- data2018 %>% 
  rename(mujer = sexo)

#variable edad como numerico y sin 0 previo
data2018$edad <- as.numeric(data2018$edad)

#variable auto_daño 1 y 2 hacer 1, lo demás 0, o NA para nsnr
data2018$auto_daño <- ifelse(data2018$auto_daño %in% c(1, 2), 1,0)  # Si es 9 → NA, cualquier otro → 0

#variable numero de habitantes a numérica y hacer NA espacios en blanco
data2018$numero_habitantes <- as.numeric(data2018$numero_habitantes)

#variables viven padres omadres, binaria, 1 es sí vive
data2018$vive_madre <- ifelse(data2018$vive_madre == 1,1,0)
data2018$vive_padre <- ifelse(data2018$vive_padre == 1,1,0)

#variable afiliación y consulta: mergear defensa y marina en una sola
data2018 <- data2018 %>%
  mutate(donde_consulta_defensamarina = ifelse(donde_consulta_defensa == 1 | donde_consulta_marina == 1, 1, 0))
data2018 <- data2018 %>%
  mutate(afiliacion_defensamarina = ifelse(afiliacion_defensa == 1 | afiliacion_marina == 1, 1, 0))

#variable lengua indigena a dummies
data2018$lengua_indigena <- ifelse(data2018$lengua_indigena == 1,1,0)
#variable leer y escribir a dummies
data2018$leer_escribir <- ifelse(data2018$leer_escribir == 1,1,0)
#variable atiende_escuela a dummies
data2018$atiende_escuela <- ifelse(data2018$atiende_escuela == 1,1,0)

#Variable escolaridad nivel y grado numérica
data2018$escolaridad_grado<-as.numeric(data2018$escolaridad_grado)
data2012$escolaridad_grado<-as.numeric(data2012$escolaridad_grado)

#variable trabaja a dummies
data2018$trabaja <- ifelse(data2018$trabaja == 1,1,0)

#variable muros a categoría
data2018 <- data2018 %>%
  mutate(
    material_muros_cat = case_when(
      material_muros == 1 ~ "Material de desecho",
      material_muros == 2 ~ "Lámina de cartón",
      material_muros == 3 ~ "Lámina de asbesto o metálica",
      material_muros == 4 ~ "Carrizo, bambú o palma",
      material_muros == 5 ~ "Embarro, bajareque o paja",
      material_muros == 6 ~ "Madera",
      material_muros == 7 ~ "Adobe",
      material_muros == 8 ~ "Tabique, ladrillo, block, piedra, cantera, cemento o concreto"
    )
  )
data2018$material_muros_cat <- factor(data2018$material_muros_cat)

#variables techo a categórica
data2018$material_techo<-as.numeric(data2018$material_techo)
data2018 <- data2018 %>%
  mutate(
    material_techo_cat = case_when(
      material_techo == 1 ~ "Material de desecho",
      material_techo == 2 ~ "Lámina de cartón",
      material_techo == 3 ~ "Lámina metálica",
      material_techo == 4 ~ "Lámina de asbesto",
      material_techo == 5 ~ "Palma o paja",
      material_techo == 6 ~ "Madera o tejamanil",
      material_techo == 7 ~ "Terrado con viguería",
      material_techo == 8 ~ "Teja",
      material_techo == 9 ~ "Losa de concreto o viguetas con bovedilla"
    )
  )
data2018$material_techo_cat <- factor(data2018$material_techo_cat)

#Variable piso a categórica
data2018 <- data2018 %>%
  mutate(
    material_piso_cat = case_when(
      material_piso == 1 ~ "Tierra",
      material_piso == 2 ~ "Cemento o firme",
      material_piso == 3 ~ "Madera, mosaico u otro recubrimiento"
    )
  )
data2018$material_piso_cat <- factor(data2018$material_piso_cat)

#variable cocina a dummy
data2018$cocina <- ifelse(data2018$cocina == 1,1,0)

#variable numero dormitorios y total_cuartos numerica
data2018$numero_dormitorios<-as.numeric(data2018$numero_dormitorios)
data2018$total_cuartos<-as.numeric(data2018$total_cuartos)

#variable luz a dummy
data2018$luz <- ifelse(data2018$luz == 1,1,0)

#variable disponibilidad_agua reducida a 1 si hay agua entubada en el hogar, 0 en otro caso
data2018$disponibilidad_agua <- ifelse(data2018$disponibilidad_agua == 1,1,0)

#variable drenaje reducida a 1 si hay drenaje o desague conectado a red pública, 0 en otro caso
data2018$drenaje <- ifelse(data2018$drenaje == 1,1,0)

#variable eliminación basura 1 si pasan por ella al hogar, 0 en otro caso
data2018$eliminacion_basura <- ifelse(data2018$eliminacion_basura == 1,1,0)

#variable calefacción 1 en caso de sí, 0 otherwise
data2018$calefaccion <- ifelse(data2018$calefaccion == 1,1,0)

bienes_2018_d<-c("otro_inmueble","automovil","camioneta","moto","otro_vehiculo","television","tv_paga","refrigerador","estufa","lavadora","computadora","microondas","aire_acondicionado")
bienes_2012_d<-c("otro_inmueble","automovil","camioneta","moto","otro_vehiculo","television","tv_paga","refrigerador","estufa","lavadora","computadora","microondas","aire_acondicionado")
data2018[bienes_2018_d] <- lapply(data2018[bienes_2018_d], function(x) {
  ifelse(x == 1, 1, 0)
})

#variable escolaridad, se usará la acumulada de años ya cursados, considerando kinder como 3 años
#pasar primero a numéric
data2018$escolaridad_nivel<-as.numeric(data2018$escolaridad_nivel)
data2018 <- data2018 %>%
  mutate(escolaridad = case_when(
    escolaridad_nivel > 6 ~ NA_real_,                                # Si 'grado' es mayor a 6, asigna NA
    escolaridad_grado == 0 ~ escolaridad_nivel,                      # No asiste a la escuela
    escolaridad_grado == 1 ~ escolaridad_nivel,                      # Preescolar/kínder
    escolaridad_grado == 2 ~ 3 + escolaridad_nivel,                    # Primaria: 3 años de preescolar + avance en primaria
    escolaridad_grado == 3 ~ 3 + 6 + escolaridad_nivel,                # Secundaria: 3 (preescolar) + 6 (primaria) + avance en secundaria
    escolaridad_grado == 4 ~ 3 + 6 + 3 + escolaridad_nivel,            # Preparatoria: 3+6+3 + avance en prepa
    escolaridad_grado == 5 ~ 3 + 6 + 3 + escolaridad_nivel,            # Normal básica (se toma como prepa)
    escolaridad_grado == 6 ~ 3 + 6 + escolaridad_nivel,                # Estudios técnicos o comerciales con primaria terminada: 3 (preescolar) + 6 (primaria) + avance en ese nivel
    TRUE ~ NA_real_                                                  # Para otros casos atípicos
  ))

# meter id de municipio en data2018
#leer .dta de vivendas2018
viviendas18<-read_dta("C:/Users/victo/OneDrive/Documents/CIDE/Tésis/datos2/viviendas18.dta")
#reduzco base a tener solo id_mun y variables que me ayuden a mergear
viviendas18<-viviendas18[c("UPM","VIV_SEL","ENT","CVE_MUN")]
#renombrar variables en base data2018 para que sea mergeable, solo ENT (entidad) cambia
data2018 <- data2018 %>% 
  rename(ENT = ENT.x)
#merge
data2018 <- merge(data2018,
                    viviendas18,
                    by = c("UPM", "VIV_SEL", "ENT"),
                    all.x = TRUE)  # Para conservar todos los registros de la base principal

#generar id_municipio con 00_000
data2018$ENT <- sprintf("%02d", as.numeric(data2018$ENT))     # Estado con 2 dígitos
data2018$CVE_MUN <- sprintf("%03d", as.numeric(data2018$CVE_MUN)) # Municipio con 3 dígitos

# Ahora las unimos en una nueva variable de 5 dígitos
data2018$id_mun <- paste0(data2018$ENT, data2018$CVE_MUN)


######################################### Unión de bases ###########################################
# Obtener nombres de las columnas comunes
columnas_comunes <- intersect(names(data2012), names(data2018))


# Filtrar solo las columnas comunes y unir
data_total <- bind_rows(
  select(data2012, all_of(columnas_comunes)),
  select(data2018, all_of(columnas_comunes))
)


############################### Extraer base #############################
rm(data2006,viviendas18)

#################### UNIR IV #############################
iv <- read_csv("~/CIDE/Tésis/datos2/serie oferta internet mexico mensual municipio.csv")
#Recortar base a solo accesos activos a final de 2013 y 2018
iv <- iv %>% 
  filter(FECHA %in% c("15-Jan-13","15-Dec-18"))
#renombrar variable de año
iv <- iv %>% 
  rename(year=...7)
#generar id_muni con base de iv
#primero hacer que tengan 0 a la izq
iv$K_ENTIDAD <- sprintf("%02d", iv$K_ENTIDAD)
iv$K_MUNICIPIO <- sprintf("%03d", iv$K_MUNICIPIO)
# Ahora las unimos en una nueva variable de 5 dígitos
iv$id_mun <- paste0(iv$K_ENTIDAD, iv$K_MUNICIPIO)

####### unión IV con ensanut #############
#recorda IV primero a solo variables necesarias
iv <- iv %>% 
  select(c("id_mun", "FECHA", "A_TOTAL_E"))
#unión
ensanut_iv <- data_total %>% 
  left_join(iv, by = "id_mun")

############## forma de iv, 2013 a 2012 y 2018 a 2018 ##############

# Filtra, agrupa y resume la oferta de 2013
oferta_2013 <- iv %>%
  filter(FECHA == "15-Jan-13") %>%
  group_by(id_mun) %>%
  summarise(offer_2013 = sum(A_TOTAL_E, na.rm = TRUE))

# Filtra, agrupa y resume la oferta de 2018
oferta_2018 <- iv %>%
  filter(FECHA == "15-Dec-18") %>%
  group_by(id_mun) %>%
  summarise(offer_2018 = sum(A_TOTAL_E, na.rm = TRUE))

#oferta 2013 que contenga mismos municipios que 2018 pero con ceros los que no aparecen en 2013
# Hacer join con todos los municipios de 2018
oferta_2013_completa <- oferta_2018 %>%
  select(id_mun) %>%
  left_join(oferta_2013, by = "id_mun") %>%
  mutate(offer_2013_completa = if_else(is.na(offer_2013), 0, offer_2013)) %>% 
  select(-offer_2013)

ensanut_iv <- data_total %>%
  left_join(oferta_2013, by = "id_mun") %>%
  left_join(oferta_2018, by = "id_mun") %>% 
  left_join(oferta_2013_completa, by = "id_mun")

#para cada caso
ensanut_iv <- ensanut_iv %>%
  mutate(
    broadband = case_when(
      d2012 == 1 ~ offer_2013,
      d2018 == 1 ~ offer_2018,
      TRUE       ~ NA_real_  # Para cualquier otro caso, o si hubiera más años
    )
  )

#para cada caso: broadband sin llenar 0
ensanut_iv <- ensanut_iv %>%
  mutate(
    broadband_full = case_when(
      d2012 == 1 ~ offer_2013_completa,
      d2018 == 1 ~ offer_2018,
      TRUE       ~ NA_real_  # Para cualquier otro caso, o si hubiera más años
    )
  )

#hacer broadband comparable, <<servicios activos a internet por cada 50 jóvenes encuestados>>
# Generar el indicador "por cada 50 jóvenes"
broadband_full_est_base <- ensanut_iv %>%
  group_by(id_mun, d2018) %>% 
  summarise(
    # Suponemos que en cada grupo (municipio y año) el valor de acceso_internet es constante.
    acceso_internet_total = first(broadband_full),  
    total_young         = sum(ponderador, na.rm = TRUE)
  ) %>%
  mutate(
    ratio = acceso_internet_total / total_young,
    broadband_full_est = ratio * 50
  ) %>%
  ungroup()
#inputar en base grande ensanut_iv en su municipio y año correspondiente
ensanut_iv <- ensanut_iv %>%
  left_join(broadband_full_est_base %>% select(id_mun, d2018, broadband_full_est),
            by = c("id_mun", "d2018"))

#hacer log boradband_full estandarizada y no estandarizada
ensanut_iv <- ensanut_iv %>% 
  mutate(logbroadband_full=log(1 + broadband_full),
         logbroadband_full_est=log(1 + broadband_full_est))

#drenaje a nivel municipio
# 1) Calcular la proporción ponderada de hogares con drenaje en cada municipio y año
munis_drenaje <- ensanut_iv %>%
  group_by(id_mun, d2018) %>%
  summarise(
    ponderado_con_drenaje = sum(drenaje * ponderador, na.rm = TRUE),
    total_ponderado      = sum(ponderador, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    proporcion_drenaje = ponderado_con_drenaje / total_ponderado
  ) %>%
  select(id_mun, d2018, proporcion_drenaje)

# 2) Insertar ese valor en la base a nivel individuo
ensanut_iv <- ensanut_iv %>%
  left_join(munis_drenaje, by = c("id_mun", "d2018"))

#eliminar variables innecesarias que se crearon para manipular
ensanut_iv <- ensanut_iv %>%
  select(-offer_2013, -offer_2018, -offer_2013_completa)

#meter poblacion de conapo
conapo <- read_excel("conapo.xlsx")
conapo <- conapo %>% 
  select(c(CLAVE,AÑO,POB_MIT_MUN))

#agrupo por municipio y año y sumo poblacion
conapo_pob <- conapo %>%
  filter(AÑO %in% c(2012, 2018)) %>%
  group_by(CLAVE, AÑO) %>%
  summarise(
    poblacion = sum(POB_MIT_MUN, na.rm = TRUE),
    .groups = "drop"           # evita mensajes de agrupamiento
  )

#preparo para merge
conapo_pob <- conapo_pob %>% 
  mutate(d2018=if_else(AÑO==2018,1,0)) %>% 
  select(-AÑO) %>% 
  rename(id_mun = CLAVE) 

conapo_pob$id_mun <- sprintf("%05d", conapo_pob$id_mun)


ensanut_iv <- ensanut_iv %>% 
  left_join(conapo_pob %>% select(id_mun, d2018, poblacion),
            by = c("id_mun", "d2018"))

#pesar IV con poblacion
ensanut_iv <- ensanut_iv %>%
  mutate(broadband_full_1000 = broadband_full / poblacion * 1000)

##### agregar trends #########
#crear bariable gruped by municipio
pob_2013 <- conapo %>% 
  select(c(CLAVE,AÑO,POB_MIT_MUN)) %>% 
  filter(AÑO == 2012) %>% 
  rename(id_mun = CLAVE) %>% 
  group_by(id_mun) %>% 
  summarise(
  poblacion_2013 = sum(POB_MIT_MUN, na.rm = TRUE))
pob_2013$id_mun <- sprintf("%05d", pob_2013$id_mun)


#unir 

ensanut_iv <- ensanut_iv %>% 
  left_join(pob_2013, by = "id_mun")

#crear interacción población con FE
ensanut_iv <- ensanut_iv %>% 
  mutate(trend_pob = poblacion_2013*d2018)

############## sacar base ##################
setwd("C:/Users/victo/OneDrive/Documents/CIDE/Tésis/scripts")
write_xlsx(ensanut_iv,"data_ensanut_iv_unbalanced.xlsx")
write_dta(ensanut_iv, "data_ensanut_iv_unbalanced.dta")

# ############## extrar IV para mapa ###########
# #renombrar variables para join en QGIS
# oferta_2013 <- oferta_2013 %>% 
#   rename(CVEGEO=id_mun)
# oferta_2018 <- oferta_2018 %>% 
#   rename(CVEGEO=id_mun)
# oferta_2013_completa <- oferta_2013_completa %>% 
#   rename(CVEGEO=id_mun)
# 
# #exportar
# write_xlsx(oferta_2013,"oferta_2013.xlsx")
# write_xlsx(oferta_2013_completa,"oferta_2013_completa.xlsx")
# write_xlsx(oferta_2018,"oferta_2018.xlsx")

#crear y exportar panel balanceado
# Paso 1: identificar municipios con datos en ambos años (sin agrupar la base completa)
munis_balanceados <- ensanut_iv %>%
  distinct(id_mun, d2018) %>%   # solo municipios y año
  count(id_mun) %>%             # contar cuántos años hay por municipio
  filter(n == 2) %>%            # quedarse solo con los que tienen los dos años
  pull(id_mun)                  # extraer vector de id_mun

# Paso 2: filtrar la base original sin perder nivel individual
ensanut_iv_balanceado <- ensanut_iv %>%
  filter(id_mun %in% munis_balanceados)

write_xlsx(ensanut_iv_balanceado,"data_ensanut_iv_balanced.xlsx")
write_dta(ensanut_iv_balanceado, "data_ensanut_iv_balanced.dta")
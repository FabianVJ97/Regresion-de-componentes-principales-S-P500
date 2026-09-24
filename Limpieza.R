# CARGAR BASE DEL S&P 500

ruta_de_datos = file.choose()
datos_sp500 = readRDS(ruta_de_datos)

sp500 = datos_sp500$empresas
matriz_precios = datos_sp500$precios

#Empresas con NA?
na_empresa = colSums( is.na(matriz_precios))
# Porcentaje de datos faltantes por empresa
porcentaje_na = 100 * na_empresa / nrow(matriz_precios)
sort(porcentaje_na, decreasing = TRUE)

#A esta fecha 16 empresas presentan algun porcentaje de NA
#destacando HONA, FDXF , Q , SNDK con sobre el 70% de datos faltantes

#Acontinuacion se evaluara que empresas precentan NA de forma atipica
# Cuartiles
Q1 = quantile(porcentaje_na, 0.25)
Q3 = quantile(porcentaje_na, 0.75)

# Rango intercuartilico
RIC = Q3 - Q1

# Limite superior
limite = Q3 + 1.5 * RIC

Q1
Q3
RIC
limite

#Efectivamente como la mayoria de las empresas no contienen datos faltantes
#se detectaran como atipicas todas las empresas que si las tengan 

#Deteccion de empresas atipicas
empresas_atipicas = porcentaje_na[porcentaje_na > limite]
sort(empresas_atipicas, decreasing = TRUE) 

nombres_empresas_atipicas = names(empresas_atipicas)

# Seleccionar solamente las empresas atipicas
matriz_atipicas = matriz_precios[, nombres_empresas_atipicas, drop = FALSE]

# Aqui se busca distinguir si los datos faltantes corresponden a
# periodos anteriores a la primera observacion disponible, o si
# existen valores NA dentro del periodo efectivo de cotizacion
# de las empresas identificadas como atipicas.

patron_na = data.frame(
  Empresa = colnames(matriz_atipicas),
  Total_NA = colSums(is.na(matriz_atipicas)),
  Porcentaje_NA = round(
    100 * colSums(is.na(matriz_atipicas)) /
      nrow(matriz_atipicas),
    2
  ),
  NA_Internos = 0,
  Observaciones_Periodo = 0
)

for (j in seq_len(ncol(matriz_atipicas))) {
  
  x = matriz_atipicas[, j]
  
  observados = which(!is.na(x))
  
  if (length(observados) > 1) {
    
    inicio = min(observados)
    fin = max(observados)
    
    # Cantidad de NA dentro del periodo observado
    patron_na$NA_Internos[j] =
      sum(is.na(x[inicio:fin]))
    
    # Longitud del periodo efectivo de observacion
    patron_na$Observaciones_Periodo[j] =
      fin - inicio + 1
  }
}

# Porcentaje de NA internos respecto al periodo efectivo
patron_na$Porcentaje_NA_Internos =
  round(
    100 * patron_na$NA_Internos /
      patron_na$Observaciones_Periodo,
    3
  )

# Ordenar desde la empresa con mayor porcentaje total de NA
patron_na = patron_na[
  order(patron_na$Porcentaje_NA, decreasing = TRUE),
]

rownames(patron_na) = NULL



#---DESCRIPCION DE LA TABLA DE DATOS FALTANTES

# La tabla patron_na resume el comportamiento de los datos
# faltantes de las empresas previamente identificadas como
# atipicas.

# Empresa:
#   Simbolo bursatil de la empresa.

# Total_NA:
#   Numero total de valores faltantes (NA) de la empresa
#   considerando todo el periodo analizado.

# Porcentaje_NA:
#   Porcentaje de datos faltantes respecto al numero total de
#   fechas contenidas en la matriz de precios.

# NA_Internos:
#   Numero de valores faltantes ubicados entre la primera y la
#   ultima observacion disponible de la empresa.

# Observaciones_Periodo:
#   Numero de fechas comprendidas entre la primera y la ultima
#   observacion disponible de la empresa, ambas inclusive.
#   Este valor representa la longitud de su periodo efectivo de
#   observacion dentro de la matriz.

# Porcentaje_NA_Internos:
#   Porcentaje de datos faltantes dentro del periodo efectivo
#   de observacion de la empresa:

#       100 * NA_Internos / Observaciones_Periodo

# Esto permite distinguir entre empresas cuyos valores faltantes
# se concentran antes de su primera observacion disponible y
# empresas que presentan interrupciones dentro de su propia
# serie historica.

patron_na

# CRITERIO PARA LA ELIMINACION DE EMPRESAS

# Se establece como criterio conservar aquellas empresas que
# presenten al menos un 80% de cobertura temporal respecto al
# periodo total analizado.
# Esto equivale a permitir como maximo un 20% de datos faltantes.

# El criterio busca evitar la imputacion de grandes bloques de
# observaciones inexistentes, principalmente en empresas cuya
# primera observacion disponible ocurre considerablemente despues
# del inicio del periodo de estudio.

# Dado que la mayor parte de estos NA se encuentra antes de la
# primera observacion disponible y no corresponde a interrupciones
# dentro de la serie, no resulta apropiado imputarlos como si fueran
# datos faltantes aleatorios.

# Por lo tanto:
#   Porcentaje_NA <= 20%  -> Conservar
#   Porcentaje_NA >  20%  -> Eliminar

# Los NA internos se analizan separadamente, ya que representan
# interrupciones efectivas dentro de una serie ya observada.


# Umbral maximo permitido de datos faltantes
umbral_na = 20

patron_na$Decision = ifelse( patron_na$Porcentaje_NA <= umbral_na, 
                             "Conservar","Eliminar")

patron_na

empresas_eliminar =
  patron_na$Empresa[
    patron_na$Decision == "Eliminar"
  ]

empresas_conservar =
  patron_na$Empresa[
    patron_na$Decision == "Conservar"
  ]

empresas_eliminar
empresas_conservar

# Dada la información obtenida, se evaluará la conveniencia de conservar
# las empresas "CEG", "HOOD", "APP", "COIN" y "EXE". Posteriormente,
# se determinará el procedimiento a seguir con "FISV", cuyo patrón de
# datos faltantes es distinto al observado en las empresas anteriores.
#...
# La decisión de mantener estas empresas no debe basarse únicamente en
# su porcentaje individual de datos faltantes. Su inclusión puede obligar
# a desplazar la fecha inicial del período común de análisis, provocando
# la eliminación de observaciones válidas para todas las demás empresas.
#
# Por lo tanto, conservar una empresa con una serie histórica más corta
# puede implicar una pérdida global de información considerablemente mayor
# que la pérdida asociada a eliminar únicamente dicha empresa.
#
# En consecuencia, se evaluará el costo de conservar cada empresa en
# términos de la cantidad de fechas y observaciones que deberían
# eliminarse del resto de la matriz para obtener un período común completo.


# ============================================================
# EVALUACION DEL COSTO DE CONSERVAR EMPRESAS
# ============================================================

# Las empresas con coberturas considerablemente menores ya fueron
# identificadas para su eliminacion. A continuacion se evalua el
# efecto de conservar o eliminar "CEG", "HOOD", "APP", "COIN" y
# "EXE" sobre el periodo comun de observacion de toda la matriz.
#
# Para cada escenario se calcula:
#
# - Numero de empresas conservadas.
# - Primera fecha comun disponible.
# - Numero de fechas disponibles desde dicha fecha.
# - Tamaño efectivo potencial de la matriz, definido como:
#
#             Informacion = N_fechas * N_empresas
#
# Este criterio permite comparar la informacion aportada por mantener
# una empresa con la cantidad de observaciones del resto de las
# empresas que se pierden al desplazar el inicio del periodo.

# ============================================================
# EVALUACION DE EMPRESAS CON COBERTURA TEMPORAL PARCIAL
# ============================================================

# Las empresas con una cobertura temporal muy reducida ya fueron
# identificadas previamente y se excluyen de esta etapa del analisis.

empresas_eliminar_previas = c(
  "HONA", "FDXF", "Q", "SNDK", "GEV",
  "SOLV", "RDDT", "VLTO", "KVUE", "GEHC"
)


# ============================================================
# 1. MATRIZ PARA LA EVALUACION DE ESCENARIOS
# ============================================================

# Se construye una matriz temporal excluyendo las empresas cuya
# eliminacion ya fue determinada.

matriz_revision = matriz_precios[
  ,
  !colnames(matriz_precios) %in% empresas_eliminar_previas,
  drop = FALSE
]


# FISV se excluye temporalmente de esta evaluacion, debido a que
# presenta un NA interno. Su caso sera analizado posteriormente
# de manera separada.

matriz_revision = matriz_revision[
  ,
  colnames(matriz_revision) != "FISV",
  drop = FALSE
]


# ============================================================
# 2. DEFINICION DE LOS ESCENARIOS
# ============================================================

# Se evaluara el efecto de eliminar progresivamente las empresas
# que restringen el inicio comun de la matriz.

escenarios = list(
  
  "Conservar todas" =
    character(0),
  
  "Eliminar CEG" =
    c("CEG"),
  
  "Eliminar CEG + HOOD" =
    c("CEG", "HOOD"),
  
  "Eliminar CEG + HOOD + APP" =
    c("CEG", "HOOD", "APP"),
  
  "Eliminar CEG + HOOD + APP + COIN" =
    c("CEG", "HOOD", "APP", "COIN"),
  
  "Eliminar CEG + HOOD + APP + COIN + EXE" =
    c("CEG", "HOOD", "APP", "COIN", "EXE")
)


# ============================================================
# 3. CONSTRUCCION DE LA TABLA DE RESPALDO
# ============================================================

tabla_respaldo = data.frame()


for (nombre in names(escenarios)) {
  
  # Empresas que se eliminan en el escenario actual
  eliminar = escenarios[[nombre]]
  
  
  # Construir la matriz correspondiente al escenario
  matriz_temp = matriz_revision[
    ,
    !colnames(matriz_revision) %in% eliminar,
    drop = FALSE
  ]
  
  
  # Primera observacion disponible para cada empresa
  primera_obs = sapply(
    seq_len(ncol(matriz_temp)),
    function(j) {
      min(which(!is.na(matriz_temp[, j])))
    }
  )
  
  
  # El inicio comun esta determinado por la empresa cuya
  # primera observacion disponible ocurre mas tarde.
  inicio_comun = max(primera_obs)
  
  
  # Numero de fechas disponibles desde el inicio comun
  # hasta el final del periodo.
  n_fechas =
    nrow(matriz_temp) - inicio_comun + 1
  
  
  # Numero de empresas conservadas
  n_empresas =
    ncol(matriz_temp)
  
  
  # Cantidad total de informacion efectiva de la matriz:
  #
  #      Informacion = numero de fechas * numero de empresas
  
  informacion =
    n_fechas * n_empresas
  
  
  # Agregar resultados a la tabla
  tabla_respaldo = rbind(
    tabla_respaldo,
    data.frame(
      Escenario = nombre,
      Empresas = n_empresas,
      Inicio_Comun =
        as.Date(index(matriz_temp)[inicio_comun]),
      Fechas_Comunes = n_fechas,
      Informacion_Total = informacion
    )
  )
}


# ============================================================
# 4. COMPARACION ENTRE ESCENARIOS
# ============================================================

# Cambio en la cantidad de informacion respecto al
# escenario inmediatamente anterior.

tabla_respaldo$Cambio_Informacion =
  c(
    NA,
    diff(tabla_respaldo$Informacion_Total)
  )


# Cambio porcentual respecto al escenario inmediatamente anterior.

tabla_respaldo$Cambio_Porcentual =
  round(
    100 *
      tabla_respaldo$Cambio_Informacion /
      c(
        NA,
        head(tabla_respaldo$Informacion_Total, -1)
      ),
    2
  )


# Numero de fechas recuperadas respecto al escenario
# inmediatamente anterior.

tabla_respaldo$Fechas_Recuperadas =
  c(
    NA,
    diff(tabla_respaldo$Fechas_Comunes)
  )


# Ganancia absoluta de informacion respecto al
# escenario original, donde se conservan todas las empresas.

tabla_respaldo$Ganancia_Desde_Original =
  tabla_respaldo$Informacion_Total -
  tabla_respaldo$Informacion_Total[1]


# Ganancia porcentual de informacion respecto al
# escenario original.

tabla_respaldo$Ganancia_Porc_Original =
  round(
    100 *
      tabla_respaldo$Ganancia_Desde_Original /
      tabla_respaldo$Informacion_Total[1],
    2
  )


# ============================================================
# 5. TABLA FINAL DE RESPALDO
# ============================================================

tabla_respaldo


# ============================================================
# 6. INTERPRETACION DE LOS ESCENARIOS
# ============================================================

# La eliminacion de CEG permite recuperar 120 fechas para las
# empresas restantes, incrementando la cantidad total de informacion
# de la matriz en aproximadamente un 10.01%.
#
# Al eliminar adicionalmente HOOD se recuperan otras 73 fechas,
# generando un incremento adicional del 5.43%.
#
# En conjunto, la eliminacion de CEG y HOOD incrementa la informacion
# disponible desde 576624 hasta 668850 observaciones, lo que equivale
# a una ganancia aproximada del 15.99% respecto al escenario original.
#
# En cambio, eliminar APP de manera individual no resulta conveniente,
# ya que permite recuperar solamente una fecha y reduce la cantidad
# total de informacion de la matriz en aproximadamente un 0.13%.
#
# APP y COIN presentan fechas iniciales muy cercanas, por lo que su
# eliminacion debe analizarse de manera conjunta y no individual.
#
# Aunque continuar eliminando APP, COIN y EXE permite aumentar
# nuevamente el numero total de observaciones disponibles, las
# ganancias marginales son progresivamente menores y requieren
# sacrificar variables adicionales.
#
# Por lo tanto, se adopta como criterio de equilibrio entre cobertura
# temporal y cobertura transversal eliminar CEG y HOOD, conservando
# APP, COIN y EXE para las etapas posteriores del analisis.
#
# Finalmente, FISV sera evaluada de manera independiente, debido a
# que presenta un NA interno y, por lo tanto, corresponde a un
# problema diferente al inicio tardio de las series.


index(matriz_precios)[
  is.na(matriz_precios$FISV)
]

#que ocurrio con FISV EL "2025-11-12" ? por que no hay datos?

# FISV si cotizo el 2025-11-12. En dicha fecha, la accion registro
# un precio de cierre de 64.38 USD.

# Dado que el valor faltante corresponde a una omision de la fuente
# utilizada y no a la ausencia real de cotizacion, el NA sera
# reemplazado por el valor original observado para esa fecha.


matriz_precios["2025-11-12", "FISV"] = 64.38

# ============================================================
# ELIMINACION DE EMPRESAS CON DATOS FALTANTES
# ============================================================

# Se eliminan todas las empresas identificadas previamente en
# patron_na, excepto FISV. Esta empresa se conserva debido a que
# su unico dato faltante fue identificado y reemplazado por el
# valor original observado para dicha fecha.

empresas_eliminar = setdiff(
  patron_na$Empresa,
  "FISV"
)

empresas_eliminar

matriz_precios_dep = matriz_precios[
  ,
  !colnames(matriz_precios) %in% empresas_eliminar,
  drop = FALSE
]

dim(matriz_precios)

dim(matriz_precios_dep)
# ============================================================
# NUEVA REVISION DE DATOS FALTANTES
# ============================================================

colSums(is.na(matriz_precios_dep))
sum(is.na(matriz_precios_dep))

# ============================================================
# GUARDAR MATRIZ DE PRECIOS DEPURADA
# ============================================================

saveRDS(
  matriz_precios_dep,
  file = "matriz_precios_limpia.rds"
)


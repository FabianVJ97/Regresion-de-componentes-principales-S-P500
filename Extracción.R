
library(httr2)
library(ggplot2)
library(quantmod)

url = "https://scanner.tradingview.com/america/scan"

consulta = list(
  symbols = list(symbolset = list("SYML:SP;SPX")),
  columns = list("name","description","sector"),
  sort = list(sortBy = "name",sortOrder = "asc"),
  range = list(0, 600)
)

respuesta = request(url) |>
  req_method("POST") |>
  req_headers(
    `User-Agent` = "Mozilla/5.0",
    Origin = "https://es.tradingview.com",
    Referer = "https://es.tradingview.com/symbols/SPX/components/"
  ) |>
  req_body_json(consulta, auto_unbox = TRUE) |>
  req_perform() |>
  resp_body_json(simplifyVector = FALSE)

# Extraer datos
sp500 = do.call(
  rbind,
  lapply(respuesta$data, function(x) {
    data.frame(
      Simbolo = as.character(x$d[[1]]),
      Empresa = as.character(x$d[[2]]),
      Sector = as.character(x$d[[3]]),
      stringsAsFactors = FALSE
    )
  })
)

sp500 = sp500[order(sp500$Empresa), ] # Ordenar alfabeticamente por empresa
sp500$Numero = seq_len(nrow(sp500)) # Agregar numeracion
sp500 = sp500[, c("Numero", "Simbolo", "Empresa", "Sector")] #Numero como primera columna
rownames(sp500) = NULL # Reiniciar nombres de las filas

#AED de SECTOR

Sector = as.factor(sp500$Sector)

#---Tabla de frecuencia

#Frecuencia absoluta
fi = table(Sector)

#Frecuencia relativa
hi = prop.table(fi)

#Porcentaje
porcentaje = 100 * hi

#Tabla final
tabla = data.frame(
  Categoria = names(fi),
  fi = as.vector(fi),
  hi = round(as.vector(hi), 3),
  Porcentaje = round(as.vector(porcentaje), 2)
)

tabla

#grafico sector
ggplot(tabla,
  aes(x = reorder(Categoria, fi), y = fi)) +
  geom_col() + geom_text( aes(label = fi),hjust = -0.2, size = 3.5) +
  coord_flip() + labs(title = "Empresas del S&P 500 por sector",
                      x = "Sector", y = "Frecuencia absoluta") +
  expand_limits(y = max(tabla$fi) * 1.1) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

##---Descarga de datos precios his. de las emp.

fecha_inicio = as.Date("2021-01-01")
fecha_fin = as.Date("2026-09-21")


#Adaptar simbolos de TradingView al formato de Yahoo

sp500$Simbolo_Yahoo = gsub( "\\.", "-", sp500$Simbolo )

# Ejemplo:
# BRK.B -> BRK-B
# BF.B  -> BF-B

precios = list() #Lista donde se guardaran los precios
fallidos = character() # Tickers que no pudieron descargarse

#Descargar cada empresa

for (i in seq_len(nrow(sp500))) {
  
  simbolo = sp500$Simbolo_Yahoo[i]
  
  cat(i, "/", nrow(sp500), " - Descargando:", simbolo, "\n")

  datos = try( getSymbols( simbolo,
                           src = "yahoo",
                           from = fecha_inicio, to = fecha_fin + 1,
                           auto.assign = FALSE, warnings = FALSE),
               silent = TRUE)
  
  
#Si la descarga fue exitosa

  if (!inherits(datos, "try-error")) {
    precio = Ad(datos)
    colnames(precio) = sp500$Simbolo[i]
    precios[[sp500$Simbolo[i]]] = precio
    
    #Si no existen datos
  } else {
    fallidos = c(fallidos, sp500$Simbolo[i])
  }
  
  Sys.sleep(0.1)
}


#UNIR TODAS LAS EMPRESAS

matriz_precios = do.call(merge, c(precios, all = TRUE))


#Guardar datos para evitar descargar cada vez 
#que se ejecuta el codigo

datos_sp500 = list(empresas = sp500,precios = matriz_precios)

saveRDS(datos_sp500,file = "datos_sp500.rds")






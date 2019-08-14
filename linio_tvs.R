library(RSelenium)



rD  <- rsDriver(port = 4532L, browser = "firefox", version = "latest", chromever = "76.0.3809.25",
                geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
                verbose = TRUE, check = TRUE)


remDr <- rD$client

for (i in 1:5){
  
  tvs_url <- "https://www.linio.com.pe/c/tv-y-video/televisores?page="
  
  tvs_url_i <- paste0(tvs_url,i)
  
  remDr$navigate(tvs_url_i)
  
  Sys.sleep(10)
  
  
  data = remDr$findElements(
    using = "xpath", 
    value = '/html/body/div[3]/main/div[1]/div[5]/div/div'
  )
  
  rawData = sapply(
    X = data, 
    FUN = function(elem) elem$getElementText()
  )
  
  splitted = sapply(
    X = rawData, 
    FUN = strsplit, 
    split = "\n"
  )
  
  # 
  # sapply(splitted, '[', 1)
  # sapply(splitted, '[', 2)
  # sapply(splitted, '[', 3)
  
  assign(paste('linio_tvs_pag_', i, sep=''), data.frame(
    ecommerce = "linio",
    #marca = sapply(splitted, "[", 2), #We don't need the "comparar" text so we start from 2
    producto = sapply(splitted, "[", 1),
    precio_antes = sapply(splitted, "[", 2),
    precio_actual = ifelse(lengths(splitted)==2, sapply(splitted, "[", 2), sapply(splitted, "[", 3))
  ))
  
  
}




#print(type(splitted[[1]]))
splitted[[62]]

### Combining the dfs #######


linio_tvs <- rbind(linio_tvs_pag_1, linio_tvs_pag_2, linio_tvs_pag_3,
                       linio_tvs_pag_4, linio_tvs_pag_5)






##### Marca

linio_tvs$marca <- ifelse(grepl("samsung", linio_tvs$producto, ignore.case = T), "samsung",linio_tvs$producto) 

linio_tvs$marca <- ifelse(grepl("lg", linio_tvs$producto, ignore.case = T), "lg",linio_tvs$marca) 

linio_tvs$marca <- ifelse(grepl("haier", linio_tvs$producto, ignore.case = T), "haier",linio_tvs$marca)

linio_tvs$marca <- ifelse(grepl("panasonic", linio_tvs$producto, ignore.case = T), "panasonic",linio_tvs$marca)

linio_tvs$marca <- ifelse(grepl("sony", linio_tvs$producto, ignore.case = T), "sony",linio_tvs$marca)

linio_tvs$marca <- ifelse(grepl("sharp", linio_tvs$producto, ignore.case = T), "sharp",linio_tvs$marca)

linio_tvs$marca <- ifelse(grepl("aoc", linio_tvs$producto, ignore.case = T), "aoc",linio_tvs$marca)

linio_tvs$marca <- ifelse(grepl("philips", linio_tvs$producto, ignore.case = T), "philips",linio_tvs$marca)

linio_tvs$marca <- ifelse(grepl("hyundai", linio_tvs$producto, ignore.case = T), "hyundai",linio_tvs$marca)

linio_tvs$marca <- ifelse(grepl("hisense", linio_tvs$producto, ignore.case = T), "hisense",linio_tvs$marca)

linio_tvs$marca <- ifelse(grepl("orizon", linio_tvs$producto, ignore.case = T), "orizon",linio_tvs$marca)

linio_tvs$marca <- ifelse(grepl("imaco", linio_tvs$producto, ignore.case = T), "imaco",linio_tvs$marca)

linio_tvs$marca <- ifelse(grepl("daewoo", linio_tvs$producto, ignore.case = T), "daewoo",linio_tvs$marca)

###






linio_tvs$precio_antes <- gsub("S/", "", linio_tvs$precio_antes)
linio_tvs$precio_actual <- gsub("S/", "", linio_tvs$precio_actual)


linio_tvs$precio_antes <- gsub(" -.*", "", linio_tvs$precio_antes)
linio_tvs$precio_actual <- gsub(" -.*", "", linio_tvs$precio_actual)


linio_tvs$precio_antes <- gsub("desde ", "", linio_tvs$precio_antes)
linio_tvs$precio_actual <- gsub("desde ", "", linio_tvs$precio_actual)




linio_tvs$precio_antes <- gsub(",", "", linio_tvs$precio_antes)
linio_tvs$precio_actual <- gsub(",", "", linio_tvs$precio_actual)




linio_tvs$precio_antes <- trimws(linio_tvs$precio_antes, which = "both")
linio_tvs$precio_actual <- trimws(linio_tvs$precio_actual, which = "both")


linio_tvs$precio_antes <- gsub(" \\(Normal\\)", "", linio_tvs$precio_antes)
linio_tvs$precio_antes <- gsub(" \\(Internet\\)", "", linio_tvs$precio_antes)
linio_tvs$precio_actual <- gsub(" \\(Internet\\)", "", linio_tvs$precio_actual)



linio_tvs$pulgadas <- sub(".*?(\\d+['\"]).*", "\\1", linio_tvs$producto)



linio_tvs$pulgadas <- sub('"', "", linio_tvs$pulgadas)
linio_tvs$pulgadas <- sub("'", "", linio_tvs$pulgadas)


linio_tvs$pulgadas <- as.character(linio_tvs$pulgadas)

linio_tvs <- linio_tvs[!(nchar(linio_tvs$pulgadas)>2),]



linio_tvs$pulgadas <- sub("'", "", linio_tvs$pulgadas)

linio_tvs$pulgadas <- as.numeric(linio_tvs$pulgadas)

str(linio_tvs)

linio_tvs$precio_antes <- as.numeric(linio_tvs$precio_antes)
linio_tvs$precio_actual <- as.numeric(linio_tvs$precio_actual)








#### estaba reemplanazando los precios actuales por precios anteriores.
#linio_tvs$precio_antes <- ifelse(is.na(linio_tvs$precio_antes), linio_tvs$precio_actual, linio_tvs$precio_antes)

### Ordenar columnas

#linio_tvs <- linio_tvs[,c(1,5,2,3,4,6)]


linio_tvs <- as.data.frame(apply(linio_tvs[,],2,tolower))

linio_tvs <- linio_tvs[rowSums(is.na(linio_tvs)) != ncol(linio_tvs), ]


colnames(linio_tvs)
### Ordenar columnas

linio_tvs <- linio_tvs[,c(1,5,2,3,4,6)]


# file <- paste(as.character(Sys.Date()), "linio-tvs", sep = "-")
# 
# linio_tvs_csv <- paste(file, "csv", sep = ".")

write.csv(linio_tvs, "linio-tvs.csv", row.names = F)


# l <- replicate(
#   132,
#   list(sample(letters, 20)),
#   simplify = FALSE
# )

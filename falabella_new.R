library(RSelenium)



rD  <- rsDriver(port = 4512L, browser = "firefox", version = "latest", chromever = "latest",
                geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
                verbose = TRUE, check = TRUE)


remDr <- rD$client

for (i in 1:7){
  
  tvs_url <- "https://www.falabella.com.pe/falabella-pe/category/cat210477/TV-Televisores?page="
  
  tvs_url_i <- paste0(tvs_url,i)
  
  remDr$navigate(tvs_url_i)
  
  Sys.sleep(10)

  
  data = remDr$findElements(
    using = "xpath", 
    value = "/html/body/div[2]/main/div[2]/div/div/section[2]/div[2]/div"
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
  
  
  
  assign(paste('falabella_tvs_pag_', i, sep=''), data.frame(
    ecommerce = "falabella",
    marca = sapply(splitted, "[", 2), #We don't need the "comparar" text so we start from 2
    producto = sapply(splitted, "[", 3),
    precio_antes = ifelse(nchar(sapply(splitted, "[", 4))>3, NA, sapply(splitted, "[", 6)),
    precio_actual = ifelse(nchar(sapply(splitted, "[", 4))<=3, sapply(splitted, "[", 5), sapply(splitted, "[", 4))
  ))


}



### Combining the dfs #######


falabella_tvs <- rbind(falabella_tvs_pag_1, falabella_tvs_pag_2, falabella_tvs_pag_3,
                       falabella_tvs_pag_4, falabella_tvs_pag_5, falabella_tvs_pag_6,
                       falabella_tvs_pag_7)







falabella_tvs$precio_antes <- gsub(",", "", falabella_tvs$precio_antes)
falabella_tvs$precio_actual <- gsub(",", "", falabella_tvs$precio_actual)


falabella_tvs$precio_antes <- gsub("S/", "", falabella_tvs$precio_antes)
falabella_tvs$precio_actual <- gsub("S/", "", falabella_tvs$precio_actual)



falabella_tvs$precio_antes <- trimws(falabella_tvs$precio_antes, which = "both")
falabella_tvs$precio_actual <- trimws(falabella_tvs$precio_actual, which = "both")


falabella_tvs$precio_antes <- gsub(" \\(Normal\\)", "", falabella_tvs$precio_antes)
falabella_tvs$precio_antes <- gsub(" \\(Internet\\)", "", falabella_tvs$precio_antes)
falabella_tvs$precio_actual <- gsub(" \\(Internet\\)", "", falabella_tvs$precio_actual)



falabella_tvs$pulgadas <- sub(".*?(\\d+['\"]).*", "\\1", falabella_tvs$producto)



falabella_tvs$pulgadas <- sub('"', "", falabella_tvs$pulgadas)
falabella_tvs$pulgadas <- sub("'", "", falabella_tvs$pulgadas)


falabella_tvs$pulgadas <- as.character(falabella_tvs$pulgadas)

falabella_tvs <- falabella_tvs[!(nchar(falabella_tvs$pulgadas)>2),]



falabella_tvs$pulgadas <- sub("'", "", falabella_tvs$pulgadas)

falabella_tvs$pulgadas <- as.numeric(falabella_tvs$pulgadas)

str(falabella_tvs)

falabella_tvs$precio_antes <- as.numeric(falabella_tvs$precio_antes)
falabella_tvs$precio_actual <- as.numeric(falabella_tvs$precio_actual)



str(falabella_tvs)


falabella_tvs$precio_antes <- ifelse(is.na(falabella_tvs$precio_antes), falabella_tvs$precio_actual, falabella_tvs$precio_antes)

### Ordenar columnas

#falabella_tvs <- falabella_tvs[,c(1,5,2,3,4,6)]


falabella_tvs <- as.data.frame(apply(falabella_tvs[,],2,tolower))


# file <- paste(as.character(Sys.Date()), "falabella-tvs", sep = "-")
# 
# falabella_tvs_csv <- paste(file, "csv", sep = ".")

write.csv(falabella_tvs, "falabella-tvs.csv", row.names = F)

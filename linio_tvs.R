library(RSelenium)



rD  <- rsDriver(port = 4514L, browser = "firefox", version = "latest", chromever = "latest",
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
  
  
  
  #utf8ToInt(splitted[[1]][38]) 
  #utf8ToInt(splitted[[1]][125]) 
  
 
  
  assign(paste('linio_tvs_pag_', i, sep=''), data.frame(
    ecommerce = "linio",
    #marca = sapply(splitted, "[", 2), #We don't need the "comparar" text so we start from 2
    producto = sapply(splitted, "[", 1),
    precio_antes = sapply(splitted, "[", 2),
    precio_actual = sapply(splitted, "[", 3)
  ))
  
  
}




#print(type(splitted[[1]]))
splitted[[62]]

### Combining the dfs #######


linio_tvs <- rbind(linio_tvs_pag_1, linio_tvs_pag_2, linio_tvs_pag_3,
                       linio_tvs_pag_4, linio_tvs_pag_5)







linio_tvs$precio_antes <- gsub(",", "", linio_tvs$precio_antes)
linio_tvs$precio_actual <- gsub(",", "", linio_tvs$precio_actual)


linio_tvs$precio_antes <- gsub("S/", "", linio_tvs$precio_antes)
linio_tvs$precio_actual <- gsub("S/", "", linio_tvs$precio_actual)



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



str(linio_tvs)


linio_tvs$precio_antes <- ifelse(is.na(linio_tvs$precio_antes), linio_tvs$precio_actual, linio_tvs$precio_antes)

### Ordenar columnas

#linio_tvs <- linio_tvs[,c(1,5,2,3,4,6)]


linio_tvs <- as.data.frame(apply(linio_tvs[,],2,tolower))


# file <- paste(as.character(Sys.Date()), "linio-tvs", sep = "-")
# 
# linio_tvs_csv <- paste(file, "csv", sep = ".")

write.csv(linio_tvs, "linio-tvs.csv", row.names = F)


# l <- replicate(
#   132,
#   list(sample(letters, 20)),
#   simplify = FALSE
# )

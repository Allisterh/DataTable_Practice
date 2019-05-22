##--------------------------------------------------------------------
##----- Brian VandenAkker
##--------------------------------------------------------------------
  
  # Clear the workspace
  rm(list = ls()) # Clear environment
  gc()            # Clear memory
  cat("\f")       # Clear the console
  # Set working directory and path to data
  setwd("C:/Users/brian/Documents/SoftwareTools/HW3")
  # Prepare needed libraries
  library.list <- c("stringr", "data.table")
  for (i in 1:length(library.list)) {
    if (!library.list[i] %in% rownames(installed.packages())) {
      install.packages(library.list[i], repos="http://cran.rstudio.com/", dependencies=TRUE)
    }
    library(library.list[i], character.only=TRUE)
  }
  rm(library.list, i)

  
##--------------------------------------------------------------------
##----- Q1 - import data and rename variables
##--------------------------------------------------------------------
  
  # Import data as data.table
  sales <- fread("iowa.liquor.r.hw3.csv")
  # Rename variables
  old.var <- colnames(sales)
  new.var <- tolower(old.var)
  new.var <- gsub(" ", ".", new.var)
  new.var <- gsub("[()]", "", new.var)
  #Use faster data.table function to rename variables
  setnames(sales, old.var, new.var)
  # Replace empty strings with NAs
  vars <- which(sapply(sales, is.character)) 
  for (i in vars) {
    set(sales, i = grep("^$|^ $", sales[[i]]), j = i, value = NA)}
  rm(i, new.var, old.var, vars)

##--------------------------------------------------------------------
##----- Q2 - store registry
##--------------------------------------------------------------------
  
  # Q2.1 - Filter out unique store records
  #Locates unique combinations of specified variables
  stores <- unique(sales[, .(store.number, store.name, address, store.location, city, zip.code, county)]) 
  #1925 unique store numbers, therefore we have duplicates to address
  uniqueN(stores[,store.number])
  #Drop these variables (except store.number) from sales
  sales[, c("store.name", "store.location", "city", "zip.code", "county", "address") := NULL]
  
  # Q2.2 - Filter out store GPS coordinates from location variable
  #Isolate coordinates
  stores[, c("location") := tstrsplit(store.location, "[()]", keep = 2)]
  #Specify latitude/longitude
  stores[, c("store.latitude", "store.longitude") := tstrsplit(location, ",")]
  
  # Q2.3 - Drop location variable
  stores[, c("store.location", "location") := NULL]
  
  # Q2.4 - Average GPS coordinates for stores
  stores[, ':='(store.longitude = as.numeric(store.longitude), 
                store.latitude = as.numeric(store.latitude))
        ][, ':='(store.latitude = mean(store.latitude),
                 store.longitude = mean(store.longitude)),
          by = store.number]
  
  # Q2.5 - Removing duplicates
  stores <- unique(stores)
  
  # Q2.6 - Fix address, city and county names
  stores <- stores[, c("address","city","county") := lapply(.SD, str_to_title), .SDcols = c("address","city","county") #Homogenize city, county, and address variables
                  ][, address := gsub("[.,]", "", address) #Remove periods and commas in address
                  ][, store.name := gsub("\\#.*","", store.name) #Remove pound numbers in store name
                  ][, store.name := gsub("\"", "", store.name) #Remove double quotes
                  ][, store.name := gsub("/.*", "", store.name) #Remove all after '/' in store name
                  ][, store.name := gsub(" +", " ", store.name) #Remove double spaces in store name and address
                  ][, address := gsub(" +", " ", address)
                  ][, store.name := gsub("^\\s+|\\s+$", "", store.name) #Remove leading and trailing spaces in store name and address
                  ][, address := gsub("^\\s+|\\s+$", "", address)
                  #Apply common address shortcuts
                  ][grepl("Avenue", address), address:= gsub("Avenue","Ave", address)
                  ][grepl("Road", address, ignore.case = TRUE), address := gsub("Road","Rd", address)
                  ][grepl("Street", address, ignore.case = TRUE), address := gsub("Street","St", address)
                  ][grepl("Highway", address, ignore.case = TRUE), address := gsub("Highway","Hwy", address)
                  ][grepl("West", address, ignore.case = TRUE), address := gsub("West","W", address)
                  ][grepl("South", address, ignore.case = TRUE), address := gsub("South","S", address)
                  ][grepl("East", address, ignore.case = TRUE), address := gsub("East","E", address)
                  ][grepl("North", address, ignore.case = TRUE), address := gsub("North","N", address)]
  
  # Q2.7 - Remove duplicates
  #Dropping ten too many here...
  stores <- unique(stores)
  
  # Q2.8 - add zipcode/city/county matches
  geo <- fread("iowa.geographies.csv")
  #Generate new variable
  geo[, match := 1]
  #Rename zipcode to match stores data.table
  geo[, zip.code := zipcode][,zipcode := NULL]
  #Make as factor for merge
  geo[, ':='(zip.code = factor(zip.code), 
             city = factor(city),
             county = factor(county))]
  stores[, ':='(zip.code = factor(zip.code), 
                city = factor(city),
                county = factor(county))]
  #Merge to identify locations that agree with geo data.table
  stores <- merge(stores, geo, by = c("zip.code","city","county"), all.x = TRUE, all.y = FALSE)
  stores[is.na(match), match := 0]
  #We see 383 zero's in match variable
  stores[,.N, match == 0]
  rm(geo)
  
  # Q2.9 - remove duplicates with match = 0
  #Locate duplicates by row number
  stores <- stores[, ':='(duplicate = (duplicated(stores[,store.number]) | duplicated(stores[,store.number], fromLast = TRUE)))]
  #Locate duplicates with both 0/1 match
  stores <- stores[,ingeo := max(match), by = store.number]
  #Remove the duplicates with match = 0 which have both 0/1 match markers
  stores <- stores[ingeo == 0 | !(match == 0 & duplicate == TRUE)]
  #Dropping one too many here...
  stores[,.N, match == 0]
  stores[, c("match", "duplicate", "ingeo") := NULL]
  
  
##--------------------------------------------------------------------
##----- Q3 - cleaning sales data
##--------------------------------------------------------------------
  
  # Q3.1 - Convert sales and prices to proper format
  sales <- sales[, ':='(state.bottle.retail = as.numeric(gsub("[$]", "", state.bottle.retail)),
                         sale.dollars = as.numeric(gsub("[$]", "", sale.dollars)))]
                                                          
  # Q3.2 - Create subcategory variable
  sales <- sales[, ':='(subcategory = str_to_title(category.name),
                        category.name = NULL)]
  
  # Q3.3 - Create proper category variable
  sales <- sales[, category := word(subcategory, -1)
                ][, category := gsub("s", "", category)
                ][, category := gsub("ie", "y", category)]
  
  
##--------------------------------------------------------------------
##----- Q4 - export cleaned data
##--------------------------------------------------------------------
  
  fwrite(sales, file = "sales.csv")
  fwrite(stores, file = "stores.csv")
  
  

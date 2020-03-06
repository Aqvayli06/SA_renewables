# 
# anekcum_uffir <- data_frame(
#   user = c("Farid", "user"),
#   password = c("Ali", "passwd"),
#   password_hash = sapply(c("Ali", "passwd"), sodium::password_store),
#   permissions = c("admin", "standard"),
#   name = c("User One", "User Two")
# )
# saveRDS(anekcum_uffir, "anekcum_uffir.rds")
tasga_n_tufra <- "./ayen_nnidhen/anekcum_uffir.rds"
# anekcum_uffir <- readRDS(tasga_n_tufra)
rnu_negh_ekkes_amdan <- function(tasga_n_tufra = NULL,#RDS negh akka kra SQL 
                                 acu="rnu",#rnu negh ekkes akka ar zda "beddel kra"
                                 isem = "babi", awal_uffir = "mamou"){
  #-----------------------
  anekcum_uffir <- readRDS(tasga_n_tufra)
  if(acu == "ekkes"){
    if(isem %in% anekcum_uffir$user & anekcum_uffir$permissions[anekcum_uffir$user==isem]!="admin"){
      anekcum_uffir <- anekcum_uffir[anekcum_uffir$user!=isem,]
    }else{
      print("The User does not exist")
      return(FALSE)
    }
  }
  if( acu == "rnu"){
    if(! (isem %in% anekcum_uffir$user)){
      amdan_amaynut <- data_frame(
        user = isem,
        password = awal_uffir,
        password_hash = sapply(awal_uffir, sodium::password_store),
        permissions =awal_uffir,
        name = isem
      )
      anekcum_uffir <- rbind(anekcum_uffir,amdan_amaynut)
    }else{
      print("The User exists already!")
      return(FALSE)
    }
  }
  saveRDS(anekcum_uffir,tasga_n_tufra)
  #-----------------------
  return(TRUE)
}
# tasga_n_tufra <- "C:/Users/user/Documents/Saldae_Analytics/Saldae-Analytics-Platform/ayen_nnidhen/anekcum_uffir.rds"
# a <- rnu_negh_ekkes_amdan(tasga_n_tufra = tasga_n_tufra,acu = "ekkes",isem = "Farid")

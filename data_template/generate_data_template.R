# a <- read.csv("C:/Users/user/Documents/Saldae_Analytics/Data/RSFSXMVN.csv")
# b <- read.csv("C:/Users/user/Documents/Saldae_Analytics/Data/RSFSDPN.csv")
# c <- read.csv("C:/Users/user/Documents/Saldae_Analytics/Data/RSFSXMVN.csv")
# 
# a <- a[,c("date","value")]
# 
# b <- b[,c("date","value")]
# 
# c <- c[,c("date","value")]
# 
# colnames(a) <- c("date","sales")
# colnames(a) <- c("date","price")
# colnames(a) <- c("date","demand")
# d <- rbind(a,b,c)
# d <- d%>%tidyr::spread()
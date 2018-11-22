# Pathogen models
fdf <- list()
fdf[[1]] <- function(Temp, t_res){0.0003*Temp*(Temp-10.4057)/t_res} # BTV
fdf[[2]] <- function(Temp, t_res){(-0.132+0.0092*Temp)/t_res} # WNV
fdf[[3]] <- function(Temp, t_res){(0.019*(Temp-13.3))/t_res} # SBV
fdf[[4]] <- function(Temp, t_res){(-0.1393+0.008*Temp)/t_res} # DENV
fdf[[5]] <- function(Temp, t_res){(Temp-14/130)/t_res} # diro
fdf[[6]] <- function(Temp, t_res){(0.000126*Temp+(Temp-14.244)*sqrt(34.4-Temp))/t_res} # malaria

#eips <- function(Temp, pathogen = "WNV", t_res = 24){
#  res <- matrix(nrow = length(Temp) * length(pathogen), ncol = 2)
#  colnames(res) <- c("eip", "pathogen")
#  vec <-  which((c("BTV", "WNV", "SBV", "DENV", "dirofilaria", "malaria") %in% c(pathogen)) == T)
#  res[,1] <- unlist(lapply(fdf[c(vec)], function(x) x(Temp, t_res)))
#  res[,2] <- rep(c("BTV", "WNV", "SBV", "DENV", "dirofilaria", "malaria")[vec], each = length(Temp))
#  return(data.frame(res))
#}

path <- c("BTV", "WNV", "SBV", "DENV", "dirofilaria", "malaria")

eips <- function(T_data = data, T_var = TT_TU, pathogen = "WNV", t_res = 24){

  vec <-  which((path %in% c(pathogen)) == T)

  ggg <- lapply(vec, function(x){
    T_data %>%
      group_by(ID) %>%
      mutate(eips = fdf[[x]](TT_TU, 24), pathogen = path[x])
  })

  do.call(rbind, ggg)
}


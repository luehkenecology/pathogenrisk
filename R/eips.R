# Pathogen models
fdf <- list()
fdf[[1]] <- function(Temp, t_res){0.0003*Temp*(Temp-10.4057)/t_res} # BTV
fdf[[2]] <- function(Temp, t_res){(-0.132+0.0092*Temp)/t_res} # WNV
fdf[[3]] <- function(Temp, t_res){(0.019*(Temp-13.3))/t_res} # SBV
fdf[[4]] <- function(Temp, t_res){(-0.1393+0.008*Temp)/t_res} # DENV
fdf[[5]] <- function(Temp, t_res){(Temp-14/130)/t_res} # diro
fdf[[6]] <- function(Temp, t_res){(0.000126*Temp+(Temp-14.244)*sqrt(34.4-Temp))/t_res} # malaria


eips <- function(Temp, pathogen = "WNV", t_res = 24){
  vec <-  which((c("BTV", "WNV", "SBV", "DENV", "dirofilaria", "malaria") %in% c(pathogen)) == T)
  res <- lapply(fdf[c(vec)], function(x) x(Temp, t_res))
  res2 <- data.frame(do.call(cbind, res))
  dimnames(res2)[[2]] <- c("BTV", "WNV", "SBV", "DENV", "dirofilaria", "malaria")[vec]
  return(res2)
}


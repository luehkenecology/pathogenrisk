# Pathogen models
fdf <- list()
fdf[[1]] <- function(Temp){0.0003*Temp*(Temp-10.4057)} # BTV
fdf[[2]] <- function(Temp){(-0.132+0.0092*Temp)} # WNV
fdf[[3]] <- function(Temp){(0.019*(Temp-13.3))} # SBV
fdf[[4]] <- function(Temp){(-0.1393+0.008*Temp)} # DENV
fdf[[5]] <- function(Temp){(Temp-14/130)} # diro
fdf[[6]] <- function(Temp){(0.000126*Temp+(Temp-14.244)*sqrt(34.4-Temp))} # malaria


eips <- function(Temp, pathogen = "WNV"){
  vec <-  which((c("BTV", "WNV", "SBV", "DENV", "dirofilaria", "malaria") %in% c(pathogen)) == T)
  res <- lapply(fdf[c(vec)], function(x) x(Temp))
  do.call(cbind, res)
}


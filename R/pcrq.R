pcrq <- function(y, x, tau = 0.5) {

  n <- dim(x)[1]  ;  p <- dim(x)[2]
  R <- diag(p)
  R <- rbind( R, -R, 1, -1 )
  r <- c( rep(0, p), rep(-1, p), 1, -1 )
  len <- length(tau)

  if ( len == 1 ) {

    mod <- quantreg::rq(y ~ x - 1, data = data.frame(y = y, x = x), tau = tau,
                        method = "fnc", R = R, r = r)
    be <- mod$coefficients
    mae <- sum( abs (y - x %*% be) ) / n

  } else {
    mod <- quantreg::rq(y ~ x - 1, data = data.frame(y = y, x = x), tau = tau,
                        method = "fnc", R = R, r = r)
    be <- mod$coefficients
    mae <- Rfast::colmeans( abs (y - x %*% be) )
    names(mae) <- colnames(be)
  }

  list(be = be, mae = mae)
}

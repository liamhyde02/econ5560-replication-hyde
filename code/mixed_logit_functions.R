# Panel mixed logit by simulated maximum likelihood, in base R.
#
# Same specification as the team's mlogit/xlogit code: every coefficient is an
# independent normal random parameter, draws are shared across a respondent's
# six choice sets (panel), 500 Halton draws. The Halton sequences follow the
# convention used by xlogit (one prime per random parameter, starting at 2, the
# first 100 points dropped, consecutive points assigned respondent by
# respondent), so results can be compared with the team's Python run.
#
# We use base R because the mlogit package could not be installed on the
# machine used for the clean run (no access to CRAN).

halton_seq <- function(length, prime, drop = 100) {
  n <- length + drop
  s <- numeric(n)
  filled <- 1L
  t <- 1L
  while (filled < n) {
    d <- 1 / prime^t
    size <- filled
    i <- 1L
    while (i < prime && filled < n) {
      m <- min(n - filled, size)
      s[(filled + 1):(filled + m)] <- s[1:m] + d * i
      filled <- filled + m
      i <- i + 1L
    }
    t <- t + 1L
  }
  s[(drop + 1):(drop + length)]
}

PRIMES <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47)

# Standard normal draws: list of K matrices, each n_panels x n_draws.
normal_draws <- function(n_panels, n_draws, K) {
  lapply(seq_len(K), function(k) {
    matrix(qnorm(halton_seq(n_panels * n_draws, PRIMES[k])), nrow = n_panels, ncol = n_draws, byrow = TRUE)
  })
}

# Build the model data. d must be sorted by respondent, choice set, alternative,
# with exactly one chosen alternative per choice set. For each choice set we keep
# the two non-chosen alternatives as differences from the chosen one, so that
# P(choice) = 1 / (1 + sum_j exp(D_j b)).
prepare_data <- function(d, vars) {
  d <- d[order(d$record_id, d$Cid, d$Alt), ]
  chosen <- d[d$Choice == 1, c("Cid", vars)]
  other <- d[d$Choice == 0, c("record_id", "Cid", vars)]
  ch <- chosen[match(other$Cid, chosen$Cid), vars]
  D <- as.matrix(other[, vars]) - as.matrix(ch)
  set_id <- match(other$Cid, unique(other$Cid))        # choice set of each row
  panel_of_set <- match(d$record_id[match(unique(other$Cid), d$Cid)], unique(d$record_id))
  list(D = D, set_id = set_id, n_sets = max(set_id), panel_of_set = panel_of_set,
       panel_of_row = panel_of_set[set_id], n_panels = length(unique(d$record_id)), vars = vars)
}

# Conditional (fixed-coefficient) logit: negative log-likelihood and gradient.
mnl_negll <- function(b, md) {
  e <- exp(drop(md$D %*% b))
  S <- 1 + rowsum(e, md$set_id, reorder = TRUE)[, 1]
  sum(log(S))
}
mnl_grad <- function(b, md) {
  e <- exp(drop(md$D %*% b))
  S <- 1 + rowsum(e, md$set_id, reorder = TRUE)[, 1]
  q <- e / S[md$set_id]
  colSums(md$D * q)
}

# Mixed logit. theta = (means of the K coefficients, then their K standard deviations).
mxl_core <- function(theta, md, Z) {
  K <- ncol(md$D)
  mu <- theta[1:K]
  s <- theta[K + 1:K]
  W <- matrix(drop(md$D %*% mu), nrow(md$D), ncol(Z[[1]]))
  for (k in 1:K) if (s[k] != 0) W <- W + (md$D[, k] * s[k]) * Z[[k]][md$panel_of_row, ]
  E <- exp(W)
  S <- 1 + rowsum(E, md$set_id, reorder = TRUE)               # n_sets x R
  logP <- -rowsum(log(S), md$panel_of_set, reorder = TRUE)     # n_panels x R
  P <- exp(logP)
  L <- rowMeans(P)
  list(E = E, S = S, P = P, L = L, mu = mu, s = s, K = K)
}

mxl_negll <- function(theta, md, Z) {
  -sum(log(mxl_core(theta, md, Z)$L))
}

mxl_grad <- function(theta, md, Z) {
  cc <- mxl_core(theta, md, Z)
  Q <- cc$E / cc$S[md$set_id, ]                                # share of each non-chosen alt
  w <- cc$P / (ncol(cc$P) * cc$L)                              # weight of each draw, n_panels x R
  g_mu <- g_s <- numeric(cc$K)
  for (k in 1:cc$K) {
    A <- rowsum(md$D[, k] * Q, md$panel_of_row, reorder = TRUE)  # n_panels x R
    g_mu[k] <- sum(w * A)
    g_s[k] <- sum(w * A * Z[[k]])
  }
  c(g_mu, g_s)                                                 # gradient of the negative LL
}

fit_mxl <- function(md, start, n_draws = 500, max_rounds = 10) {
  Z <- normal_draws(md$n_panels, n_draws, ncol(md$D))
  # The coefficients differ in size by a factor of about 1,000, so both
  # optimizers scale each parameter by its current size. We alternate nlminb
  # (PORT quasi-Newton) and BFGS until a round improves the log-likelihood by
  # less than 1e-8; one optimizer alone can stop early on this surface.
  par <- start
  value <- mxl_negll(par, md, Z)
  for (round in seq_len(max_rounds)) {
    sc <- pmax(abs(par), 1e-4)
    o1 <- nlminb(par, mxl_negll, mxl_grad, md = md, Z = Z, scale = 1 / sc,
                 control = list(iter.max = 2000, eval.max = 4000, rel.tol = 1e-14))
    o2 <- optim(o1$par, mxl_negll, mxl_grad, md = md, Z = Z, method = "BFGS",
                control = list(maxit = 5000, reltol = 1e-14, parscale = pmax(abs(o1$par), 1e-4)))
    improvement <- value - o2$value
    par <- o2$par
    value <- o2$value
    if (improvement < 1e-8) break
  }
  g <- mxl_grad(par, md, Z)
  H <- optimHess(par, mxl_negll, mxl_grad, md = md, Z = Z)
  V <- solve(H)
  names(par) <- c(md$vars, paste0("sd.", md$vars))
  dimnames(V) <- list(names(par), names(par))
  list(coef = par, vcov = V, loglik = -value, rounds = round,
       max_abs_scaled_gradient = max(abs(g * pmax(abs(par), 1e-4))),
       hessian_positive_definite = all(eigen(H, symmetric = TRUE, only.values = TRUE)$values > 0),
       n_draws = n_draws)
}

fit_mnl <- function(md, start = rep(0, ncol(md$D))) {
  opt <- optim(start, mnl_negll, mnl_grad, md = md, method = "BFGS",
               control = list(maxit = 2000, reltol = 1e-14))
  names(opt$par) <- md$vars
  list(coef = opt$par, loglik = -opt$value, convergence = opt$convergence)
}

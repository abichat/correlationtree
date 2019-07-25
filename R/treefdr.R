
EstHyper <- getFromNamespace("EstHyper", "StructFDR")
AdjStats <- getFromNamespace("AdjStats", "StructFDR")
PermFDR  <- getFromNamespace("PermFDR",  "StructFDR")

#' False Discovery Rate (FDR) Control Integrating Prior Tree Structure for Microbiome Data
#'
#' Modification of \code{StructFDR::TreeFDR} function to remove safety nets.
#'
#' @param X a data matrix, rows are the features and columns are the samples.
#' @param Y a vector of the phenotypic values, where association tests are being assessed.
#' @param tree tree an object of \code{phylo} class
#' @param test.func a function that performs the actual tests. It takes X, Y
#' and ... as the inputs, and returns a list with two slots p.value and e.sign,
#' which are vectors of p-values and signs of the effects.
#' @param perm.func a function that performs the permutation tests. It takes X,
#' Y and ... as the inputs, and returns a list with two slots X and Y, which
#' contain the permuted data.
#' @param eff.sign a logical value indicating whether the direction of the
#' effects should be considered. If it is true (default), negative and positive
#' effects provide conflicting information.
#' @param B the number of permutations. The default is 20. If computation time
#' is not a big concern, B=100 is suggested to achieve excellent reproducibility
#' between different runs.
#' @param q.cutoff the quantile cutoff to determine the feature sets to
#' estimate the number of false positives under the null. This cutoff is to
#' protect the signal part of the distributions. The default is 0.5.
#' @param alpha the exponent applied to the distance matrix. Large values have
#' more smoothing effects for closely related species. The default is 1. If the
#' underlying structure assumption is considered to be very strong, robustness
#' can be improved by decreasing the value to 0.5.
#' @param ... further arguments such as covariates to be passed to test.func


#' @importFrom StructFDR Ztransform
#' @importFrom stats cophenetic quantile
#' @return
#' @export
#'
#' @examples
TreeFDR2 <- function (X, Y, tree, test.func, perm.func, eff.sign = TRUE,
                      # adaptive = c("Fisher", "Overlap"), alt.FDR = c("BH", "Permutation"),
                      B = 20, q.cutoff = 0.5, alpha = 1, ...) {
  # adaptive <- match.arg(adaptive)
  # alt.FDR <- match.arg(alt.FDR)
  if (is.null(rownames(X)) | is.null(tree$tip.label)) {
    warning("Both the data matrix and the tree should have labels (rownames, tip.label) to avoid potential errors!\n")
  }
  else {
    if (sum(!(rownames(X) %in% tree$tip.label))) {
      stop("Some features in the data matrix are not in the tree! Please check!\n")
    }
    else {
      if (sum(!(tree$tip.label %in% rownames(X)))) {
        warning("The tree have more features than the data matrix! \n")
      }
    }
  }
  D <- (cophenetic(tree))^alpha
  if (!is.null(rownames(X)) & !is.null(tree$tip.label)) {
    D <- D[rownames(X), rownames(X)]
  }
  cat("Test on original data sets  ...\n")
  test.obs <- test.func(X, Y, ...)
  if (!is.list(test.obs) | !all(c("e.sign", "p.value") %in%
                                names(test.obs))) {
    stop("test.func should return a list with names e.sign and p.valueif z.transform=TRUE! Please check!\n")
  }
  null.ind <- test.obs$p.value >= quantile(test.obs$p.value,
                                           1 - q.cutoff)
  z.obs <- Ztransform(test.obs$p.value, test.obs$e.sign, eff.sign)
  cat("Test on permuted data sets  ...\n")
  z.perm <- z.perm2 <- matrix(NA, nrow(X), B)
  for (i in 1:B) {
    perm.obj <- perm.func(X, Y, ...)
    if (!is.list(perm.obj) | !all(c("X", "Y") %in% names(perm.obj))) {
      stop("perm.func should return a list with names X and Y! Please check!\n")
    }
    X.perm <- perm.obj$X
    Y.perm <- perm.obj$Y
    test.perm <- test.func(X.perm, Y.perm, ...)
    z.perm[, i] <- z.perm2[, i] <- Ztransform(test.perm$p.value,
                                                         test.perm$e.sign, eff.sign)
    z.perm2[!null.ind, i] <- z.obs[!null.ind]
  }
  # if (alt.FDR == "Permutation") {
  #   cat("Perform ordinary permutation-based FDR control ...\n")
  #   if (eff.sign == TRUE) {
  #     p.adj0 <- PermFDR(abs(z.obs), abs(z.perm), rep(TRUE,
  #                                                    length(z.obs)))
  #   }
  #   else {
  #     p.adj0 <- PermFDR(z.obs, z.perm, rep(TRUE, length(z.obs)))
  #   }
  # }
  # if (alt.FDR == "BH") {
  #   cat("Perform ordinary BH-based FDR control ...\n")
  #   p.adj0 <- p.adjust(test.obs$p.value, "fdr")
  # }
  cat("Estimating hyperparameter ... \n")
  error <- try(obj <- EstHyper(y = z.obs, D = D))
  if (inherits(error, "try-error")) {
    # cat("Hyperparameter estimation failed! Ordinary permutation-based FDR control will be used!\n")
    cat("Hyperparameter estimation failed!\n")
    # p.adj <- p.adj0
    # k <- NULL
    # rho <- NULL
    # z.adj <- NULL
    return(NULL)
  }
  else {
    cat("Structure-based adjustment ...\n")
    k <- obj[1]
    rho <- obj[2]
    mu <- obj[3]
    V <- exp(-1 * rho * D)
    z.adj <- stat.o <- AdjStats(y = z.obs, V = V, k = k, mu = mu)[, 1]
    stat.p <- AdjStats(y = z.perm2, V = V, k = k, mu = mu)
    if (eff.sign == TRUE) {
      stat.o <- abs(stat.o)
      stat.p <- abs(stat.p)
    }
    p.adj <- PermFDR(stat.o, stat.p, null.ind)
    # if (adaptive == "Overlap") {
    #   fdr.cutoff <- 0.2
    #   pct.cutoff <- 0.5
    #   ind0 <- p.adj0 <= fdr.cutoff
    #   ind <- p.adj <= fdr.cutoff
    #   if (sum(p.adj[ind0] <= fdr.cutoff) < pct.cutoff *
    #       sum(ind0)) {
    #     cat("Potential over-adjustment! Alternative FDR control will be used!\n")
    #     p.adj <- p.adj0
    #     k <- NULL
    #     rho <- NULL
    #     z.adj <- NULL
    #   }
    # }
    # if (adaptive == "Fisher") {
    #   fdr.cutoff <- 0.2
    #   ind0 <- p.adj0 <= fdr.cutoff
    #   ind <- p.adj <= fdr.cutoff
    #   n <- nrow(X)
    #   test.p <- fisher.test(matrix(c(sum(ind), sum(ind0),
    #                                  n - sum(ind), n - sum(ind0)), 2, 2), alternative = "less")$p.value
    #   if (test.p <= 0.05) {
    #     cat("Potential over-adjustment! Alternative FDR control will be used!\n")
    #     p.adj <- p.adj0
    #     k <- NULL
    #     rho <- NULL
    #     z.adj <- NULL
    #   }
    # }
  }
  cat("Done!\n")
  return(list(p.adj = p.adj, p.unadj = test.obs$p.value, z.adj = z.adj,
              z.unadj = z.obs, k = k, rho = rho))
}

#' Rhythmicity test for repeated measures (random intercept model)
#'
#' Re-implementation of `RepeatedCircadian::rpt_rhythmicity()` (version
#' 0.0.0.9000). The original passes `formals(isSingular)$tol` to
#' `lme4::.makeCC()`. Since lme4 2.0, this default is the unevaluated call
#' `getSingTol()` instead of a number, which makes every fit fail ("must have a
#' numeric 'tol' component"). This copy is identical apart from using a numeric
#' tolerance (1e-4, the old lme4 default; it has no effect because the singular
#' check is set to "ignore") and from computing the Kenward-Roger test only once.
#'
#' NOTE: Revisit before release/publication. If RepeatedCircadian is fixed
#' upstream, switch back to calling `RepeatedCircadian::rpt_rhythmicity()`.
#'
#' @param tt Numeric vector of time points
#' @param yy Numeric vector of measurements
#' @param id Vector of subject IDs
#' @param period Period of the fitted cosine wave
#' @param method Either "LR" (likelihood-ratio test) or "F" (Kenward-Roger F
#'   test)
#'
#' @importFrom lme4 lmer lmerControl .makeCC
#' @importFrom lmtest lrtest
#' @importFrom pbkrtest KRmodcomp getKR
#'
#' @returns A list with the test statistic, p-value, amplitude (`A`), phase
#'   (`phi`), mesor (`basal`), and standard deviations of the random intercept
#'   (`sigma_alpha`) and residuals (`sigma_0`)
rpt_rhythmicity <- function(tt, yy, id, period = 24, method = "LR") {
  temp_data <- data.frame(
    y = yy,
    EE = sin(2 * pi / period * tt),
    FF = cos(2 * pi / period * tt),
    id = id
  )

  ctrl <- lme4::lmerControl(
    check.conv.singular = lme4::.makeCC(action = "ignore", tol = 1e-4),
    check.conv.hess = lme4::.makeCC(action = "ignore", tol = 1e-06),
    check.conv.grad = lme4::.makeCC("ignore", tol = 0.002, relTol = NULL)
  )
  rdm_int_h1 <- lme4::lmer(
    y ~ EE + FF + (1 | id),
    data = temp_data,
    REML = FALSE,
    control = ctrl
  )
  rdm_int_h0 <- lme4::lmer(
    y ~ (1 | id),
    data = temp_data,
    REML = FALSE,
    control = ctrl
  )

  # Wave parameters
  coefs <- stats::coef(summary(rdm_int_h1))
  C0 <- coefs[1, "Estimate"]
  EE <- coefs[2, "Estimate"]
  FF <- coefs[3, "Estimate"]
  A <- sqrt(EE^2 + FF^2)
  phi <- atan2(FF, EE) / (2 * pi / period)
  tempVcov <- as.data.frame(summary(rdm_int_h1)$varcor)
  sigma_alpha <- tempVcov[1, "sdcor"]
  sigma_0 <- tempVcov[2, "sdcor"]

  # Test H1 against H0
  if (method == "LR") {
    LR_rdm_int <- lmtest::lrtest(rdm_int_h0, rdm_int_h1)
    stat <- LR_rdm_int[2, 4]
    pvalue <- LR_rdm_int[2, 5]
  } else if (method == "F") {
    kr <- pbkrtest::KRmodcomp(rdm_int_h0, rdm_int_h1)
    stat <- pbkrtest::getKR(kr, name = "Fstat")
    pvalue <- pbkrtest::getKR(kr, name = "p.value")
  } else {
    stop("'method' must be either \"LR\" or \"F\".", call. = FALSE)
  }

  return(list(
    statistic = stat,
    pvalue = pvalue,
    A = A,
    phi = phi,
    basal = C0,
    sigma_alpha = sigma_alpha,
    sigma_0 = sigma_0
  ))
}

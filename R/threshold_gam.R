threshold.gam <- 
  function (formula, #gam formula construction possible with smoother
            data, #data.frame
            threshold.name, # colname of threshold variable to test within data
            nthd, #number of steps between lower & upper bound
            a = 0.2, #probability for lower bound quantile, 0:1
            b = 0.8, #probability for upper bound quantile, 0:1
            ...) # additional argument to gam() for gam.res & res
  {
    require(mgcv)
    this.envir <- environment()
    threshold.variable <- eval(parse(text = paste("data", threshold.name, 
                                                  sep = "$")))
    lower <- quantile(threshold.variable, prob = a, na.rm = TRUE)
    upper <- quantile(threshold.variable, prob = b, na.rm = TRUE)
    rv <- NULL
    gcvv <- NULL
    mgcv <- 1e+06 #start value for mgcv to be optimised
    mr <- NA
    for (r in seq(lower, upper, (upper - lower)/nthd)) {
      data$r <- r
      gam.res <- with(data, {
        attach(data)
        gam(formula, ...)
      })
      detach("data")
      rv <- c(rv, r)
      gcvv <- c(gcvv, gam.res$gcv.ubre)
      if (gam.res$gcv.ubre < mgcv) {
        mr <- r #update mr to hold current threshold position (r) if it improves (lowers) GCV
        mgcv <- gam.res$gcv.ubre
      }
    }
    data$r <- mr
    res <- with(data, {
      attach(data)
      gam(formula, ...)
    }) #rerun gam, gets returned invisibly, used to plot residual points & hist
    detach("data")
    class(res) <- c("tgam", "gam", "glm", "lm")
    invisible(list(res = res, mr = mr, mgcv = mgcv, gcvv = gcvv[order(rv)], 
                   rv = sort(rv)))
  }

threshold.gam.cv<-
  function (response.name, data, ...) 
  {
    data <- na.omit(data)
    cv <- 0
    y <- eval(parse(text = paste("data", response.name, sep = "$")))
    pred <- pred.err <- y * 0
    data.old <- data
    n <- dim(data)[1]
    for (i in 1:n) {
      case.delete <- data.old[i, ]
      data <- data.old[-i, ]
      res <- threshold.gam(data = data, ...)
      case.delete$r <- res$mr
      l1 <- list(data = data, case.delete = case.delete)
      pred.err[i] <- y[i] - (pred[i] <- with(l1, {
        attach(l1$data)
        predict(res$res, newdata = case.delete)
      }))
      detach(l1$data)
      cv <- cv + pred.err[i]^2
    }
    cv <- cv/n
    invisible(list(cv = cv, pred.err = pred.err, pred = pred))
  }

gam.cv<-
  function (response.name, data, ...) 
  {
    data <- na.omit(data)
    cv <- 0
    y <- eval(parse(text = paste("data", response.name, sep = "$")))
    pred <- pred.err <- y * 0
    data.old <- data
    n <- dim(data)[1]
    for (i in 1:n) {
      case.delete <- data.old[i, ]
      data <- data.old[-i, ]
      res <- gam(data = data, ...)
      case.delete$r <- res$mr
      l1 <- list(data = data, case.delete = case.delete)
      pred.err[i] <- y[i] - (pred[i] <- with(l1, {
        attach(l1$data)
        predict(res, newdata = case.delete)
      }))
      detach(l1$data)
      cv <- cv + pred.err[i]^2
    }
    cv <- cv/n
    invisible(list(cv = cv, pred.err = pred.err, pred = pred))
  }

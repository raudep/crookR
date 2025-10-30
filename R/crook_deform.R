#' Apply a synthetic crook/krok deformation to a stem point cloud (LAS or XYZ)
#'
#' @description
#' Generalization of your `Create_Krok()` that supports `lidR::LAS` or
#' data.frames, arbitrary azimuth, and safer handling of edge cases.
#'
#' @param x LAS or data.frame with X,Y,Z (case-insensitive if data.frame).
#' @param az numeric degrees. Azimuth of lateral rotation (0 = X axis, 90 = Y axis). Default 0.
#' @param spar smoothing parameter for `stats::smooth.spline` (0..1).
#' @param krok_length range of the deformation along the stem's length
#' @param krok_start start height of the crook
#' @param krok_type single- or double-directional deviation
#' @param krok_deviation extent of the deviation
#' @param inflektion_X placement of double directional deviation
#' @param inflektion_ext placement of double directional deviation
#' @return Same class as input (LAS or data.frame object with crook deformation)
#' @import data.table stats
#' @export

crook_deform <- function(x,
                         krok_length = 0.5,
                         krok_start = 4,
                         krok_type = c('2dir','1dir'),
                         krok_deviation = 0.1,
                         inflektion_X = 1/4,
                         inflektion_ext = 1/2,
                         az = 0,
                         spar = 0.8) {
  krok_type <- match.arg(krok_type)

  # Extract XYZ & remember how to put it back
  is_las <- inherits(x, "LAS")
  if (is_las) {
    DT <- data.table::as.data.table(x@data)
    Xcol <- "X"; Ycol <- "Y"; Zcol <- "Z"
  } else {
    nm <- names(x); map <- setNames((nm), nm)
    req <- c("X","Y","Z")
    if (!all(req %in% map)) stop("Input data.frame must have x,y,z columns (any case)")
    DT <- data.table::as.data.table(x)
    Xcol <- names(map)[map == "X"][1]
    Ycol <- names(map)[map == "Y"][1]
    Zcol <- names(map)[map == "Z"][1]
    data.table::setnames(DT, c(Xcol,Ycol,Zcol), c("X","Y","Z"))
    Xcol <- "X"; Ycol <- "Y"; Zcol <- "Z"
  }

  if (krok_length <= 0) return(if (is_las) x else as.data.frame(DT))


  # Local 2D frame: translate, rotate by -az so bend acts along +X'
  x0 <- min(DT[[Xcol]], na.rm=TRUE)
  y0 <- min(DT[[Ycol]], na.rm=TRUE)
  DT[, `:=`(X = X - x0, Y = Y - y0)]
  th <- -az * pi/180; cth <- cos(th); sth <- sin(th)
  Xp <- DT[[Xcol]] * cth - DT[[Ycol]] * sth
  Yp <- DT[[Ycol]] * cth + DT[[Xcol]] * sth


  Z <- DT[[Zcol]]; s <- krok_start; l <- krok_length
  idx_within <- which(Z >= s & Z < s + l)
  idx_above <- which(Z >= s + l)
  if (!length(idx_within)) {
    DT[[Xcol]] <- DT[[Xcol]] + x0; DT[[Ycol]] <- DT[[Ycol]] + y0
    if (is_las) { x@data <- DT; return(x) } else return(as.data.frame(DT))
  }


  Xp1 <- Xp


  if (krok_type == '1dir') {
    tan1 <- (l / krok_deviation); if (!is.finite(tan1) || tan1 == 0) tan1 <- .Machine$double.eps
    idx1 <- idx_within
    Xp1[idx1] <- Xp[idx1] + ((Z[idx1] - s) / tan1)


    zw <- Z[idx1]; x1w <- Xp1[idx1]; xw <- Xp[idx1]
    if (length(unique(zw)) >= 4) {
      kroked_center <- stats::smooth.spline(x=zw, y=x1w, spar=spar)
      normal_center <- stats::smooth.spline(x=zw, y=xw, spar=spar)
      delta <- stats::predict(normal_center, zw)$y - stats::predict(kroked_center, zw)$y
    } else delta <- rep(mean(Xp[idx1] - Xp1[idx1]), length(idx1))


    Xp[idx1] <- Xp[idx1] + delta
    if (length(idx_above)) Xp[idx_above] <- Xp[idx_above] + tail(delta, 1)


  } else { # 2dir
    ip <- inflektion_X; infX <- ip * l; infZ <- l - infX; ext <- pmax(1e-9, inflektion_ext) * krok_deviation
    tan1 <- (infX / krok_deviation); if (!is.finite(tan1) || tan1 == 0) tan1 <- .Machine$double.eps
    tan2 <- (infZ / ext); if (!is.finite(tan2) || tan2 == 0) tan2 <- .Machine$double.eps


    idx1 <- which(Z >= s & Z < s + infX)
    idx2 <- which(Z >= s + ip*l & Z < s + l)
    Xp1[idx1] <- Xp[idx1] + ((Z[idx1] - s) / tan1)
    Xp1[idx2] <- Xp[idx2] + krok_deviation - (((Z[idx2] - (s + ip * l)) / tan2))


    idxw <- sort(c(idx1, idx2)); zw <- Z[idxw]; x1w <- Xp1[idxw]; xw <- Xp[idxw]
    if (length(unique(zw)) >= 4) {
      kroked_center <- stats::smooth.spline(x=zw, y=x1w, spar=spar)
      normal_center <- stats::smooth.spline(x=zw, y=xw, spar=spar)
      delta <- stats::predict(normal_center, zw)$y - stats::predict(kroked_center, zw)$y
    } else delta <- rep(mean(Xp[idxw] - Xp1[idxw]), length(idxw))


    Xp[idxw] <- Xp[idxw] + delta
    if (length(idx_above)) Xp[idx_above] <- Xp[idx_above] + tail(delta, 1)
  }

  # Back-rotate and translate
  th2 <- az * pi/180; c2 <- cos(th2); s2 <- sin(th2)
  Xnew <- Xp * c2 - Yp * s2
  Ynew <- Xp * s2 + Yp * c2
  DT[[Xcol]] <- Xnew + x0; DT[[Ycol]] <- Ynew + y0


  if (is_las) { x@data <- DT; return(x) } else return(as.data.frame(DT))
}

#' Backwards-compatible wrapper keeping your original name/signature
#' @inheritParams crook_deform
#' @param tree_stem tree stem point cloud (lidR::LAS or data.frame)
#' @return Same class as input (LAS or data.frame object with crook deformation)
#' @export
#'
Create_Krok <- function(tree_stem,
                        krok_length = 0.5,
                        krok_start = 4,
                        krok_type ='2dir',
                        krok_deviation = 0.1,
                        inflektion_X=1/4,
                        inflektion_ext=1/2,
                        az = 0,
                        spar = 0.8){
  crook_deform(tree_stem, krok_length, krok_start, krok_type, krok_deviation,
               inflektion_X, inflektion_ext, az, spar)
}

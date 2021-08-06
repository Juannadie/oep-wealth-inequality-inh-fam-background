top10share <- function (x,w) {
  q9 <- (wtd.quantile(x, q=0.9, weight=w, na.rm = T ))
  sum10share <- sum(x[x>q9]*w[x>q9])
  totalsum <- sum(x*w)
  share <- sum10share/totalsum
  return(share)
}

bottom10share <- function (x,w) {
  q1 <- (wtd.quantile(x, q=0.1, weight=w, na.rm = T ))
  sum10share <- sum(x[x<q1]*w[x<q1])
  totalsum <- sum(x*w)
  share <- sum10share/totalsum
  return(share)
}

top20share <- function (x,w) {
  q8 <- (wtd.quantile(x, q=0.8, weight=w, na.rm = T ))
  sum20share <- sum(x[x>q8]*w[x>q8])
  totalsum <- sum(x*w)
  share <- sum20share/totalsum
  return(share)
}

bottom20share <- function (x,w) {
  q2 <- (wtd.quantile(x, q=0.2, weight=w, na.rm = T ))
  sum20share <- sum(x[x<q2]*w[x<q2])
  totalsum <- sum(x*w)
  share <- sum20share/totalsum
  return(share)
}

extreme10ratio <- function (x,w) {
  q9 <- (wtd.quantile(x, q=0.9, weight=w, na.rm = T ))
  sum10shareup <- sum(x[x>q9]*w[x>q9])
  totalsum <- sum(x*w)
  sharetop <- sum10shareup/totalsum
  q1 <- (wtd.quantile(x, q=0.1, weight=w, na.rm = T ))
  sum10sharedown <- sum(x[x<q1]*w[x<q1])
  totalsum <- sum(x*w)
  sharebottom <- sum10sharedown/totalsum
  extreme10ratio <- sharetop/sharebottom
  return(extreme10ratio)
}

extreme20ratio <- function (x,w) {
  q8 <- (wtd.quantile(x, q=0.8, weight=w, na.rm = T ))
  sum20shareup <- sum(x[x>q8]*w[x>q8])
  totalsum <- sum(x*w)
  sharetop <- sum20shareup/totalsum
  q2 <- (wtd.quantile(x, q=0.2, weight=w, na.rm = T ))
  sum20sharedown <- sum(x[x<q2]*w[x<q2])
  totalsum <- sum(x*w)
  sharebottom <- sum20sharedown/totalsum
  extreme20ratio <- sharetop/sharebottom
  return(extreme20ratio)
}

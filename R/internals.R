#' Moving average function
#'
#'
#' @source Directly from the \code{forecast} package.
#' @keywords internals
#' @importFrom stats cor ts ksmooth tsp tsp<-
#' @noRd

ma <- function(x, order, centre = TRUE)
{
    tt <- 1:length(x)
    if(order%%2) #odd
    {
        temp1 <- ts(ksmooth(tt,x, x.points=tt,bandwidth = order-1)$y)
        j <- trunc(order/2)
        temp1[c(1:j,length(x)-(1:j)+1)] <- NA
    }
    else
    {
        temp1 <- ts(ksmooth(tt,x, x.points=tt+0.5,bandwidth = order-1)$y)
        j <- trunc(order/2)
        temp1[c(1:(j-1),length(x)-(1:j)+1)] <- NA
        if(centre)
        {
            temp2 <- ksmooth(tt,x, x.points=tt-0.5,bandwidth = order-1)$y
            temp2[c(1:j,length(x)-(1:(j-1))+1)] <- NA
            temp1 <- ts((temp1+temp2)/2)
        }
    }
    tsp(temp1) <- tsp(x)
    return(temp1)
}

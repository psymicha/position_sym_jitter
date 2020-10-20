############################################################
# Author, Date: 
#     Michael Kramer, 20.10.2020
# Purpose: 
#     Enable symmetric jittering for ggplot2
# Version: 
#     V1-0
# Requires:
#     ggplot2, dplyr
# Comments:
#     jitters x and y axis by default (as position_jitter does)
#     to prevent from jittering y axis use geom_point(position = position_sym_jitter(height = 0))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

PositionSymJitter <- ggproto("PositionSymJitter", PositionJitter,
                           required_aes = c("x", "y"),
                           default_aes = aes(shape = 19, colour = "black"),
                           draw_key = draw_key_point,

    compute_layer = function (self, data, params, layout) 
        {
        
        trans_x <- if (params$width > 0) 
            function(x) sym_jitter(x, y, amount = params$width)
        trans_y <- if (params$height > 0) 
               function(x) sym_jitter(x, y, amount = params$height)
        transform_position(data, trans_x, trans_y)
    }
  
)
    
position_sym_jitter <- function(width = NULL, height = 0) {
  ggproto(NULL, PositionSymJitter, width = width, height = height)
}

sym_jitter <- function (x, y=NULL, factor = 1, amount = NULL) 
    {
    if (length(x) == 0L) 
        return(x)
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    z <- diff(r <- range(x[is.finite(x)]))
    if (z == 0) 
        z <- abs(r[1L])
    if (z == 0) 
        z <- 1
    if (is.null(amount)) {
        d <- diff(xx <- unique(sort.int(round(x, 3 - floor(log10(z))))))
        d <- if (length(d)) 
            min(d)
        else if (xx != 0) 
            xx/10
        else z/10
        amount <- factor/5 * abs(d)
    }
    else if (amount == 0) 
        amount <- factor * (z/50)

    if(is.null(y)) y <- rep(1, length(x))

    data.frame(x, y) %>%
        group_by(x, y) %>%
        mutate(n = row_number(),
               ntot = n(),
               xl = abs(-amount - amount) / ntot,
               jit = n * xl,
               jit = jit - mean(jit)) -> tx
    return(x + tx$jit)
}


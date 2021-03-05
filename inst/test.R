if(FALSE){
  devtools::load_all("~/Dropbox/code/R/packages/nam218grads")
  load_packages()
}

library(stars)
library(nam218grads)

uri = grads_uri()

X <- x <- open_grads(uri[1])

var <- c("ugrd10m", "vgrd10m")
bb = c(-77.0, -51.5, 37.9, 56.7)
time = get_time(x)[1:3]
lev = get_lev(x)[1:2]
form = 'stars'

r <- sapply(var,
            function(v){
              get_var(x, v, bb = bb, time = time, lev = lev, form = form)
            }, simplify = FALSE)
R <- Reduce(c, r) %>%
  stats::setNames(var)


A = get_var(x, var, bb = bb, lev = lev, time = time, form = 'array')



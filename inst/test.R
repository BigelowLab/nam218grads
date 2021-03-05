if(FALSE){
  devtools::load_all("~/Dropbox/code/R/packages/nam218grads")
  load_packages()
}

library(stars)
library(nam218grads)

uri = grads_uri()

X <- open_grads(uri[1])


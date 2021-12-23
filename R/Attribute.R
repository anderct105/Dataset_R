# R8.2
setClass(Class = "Attribute",
         slots = c("data", "size", "type"),
         prototype = list("data" = NULL, "size" = 0, "type" = NULL))


attribute <- function(data) {
  if (is.null(data)) {
    a <- new("Attribute", data = data, size = 0, type = NULL)
  }
  else {
    a <- new("Attribute", data = data, size = length(data), type = typeof(data))
  }
}


# R8.1 
setGeneric(name="estandarize", def= function(v) standardGeneric("estandarize"))
setMethod(f="estandarize",
          signature="Attribute",
          definition=function(v) {
              if (is.null(v@data)) {
                stop("You must provide a vector")
              } 
              else if(class(v@data) == "numeric" || class(v@data) == "double") {
              media <- mean(v@data)
              modulo <- norm(v@data, type="2")
              return(attribute((v@data - media) / modulo))
              }else {
                return(v)
              }
            }
            )

setGeneric(name="normalize", def= function(v) standardGeneric("normalize"))
setMethod(f="normalize",
          signature="Attribute",
          definition=function(v) {
            if(class(v@data) == "numeric" || class(v@data) == "double") {
            return(attribute(v@data/max(v@data)))
            }else {
              v
            }
          })

setGeneric(name="discretizeEF", def= function(v, num.bins=3){ standardGeneric("discretizeEF")})
setMethod(f="discretizeEF",
          signature=c(v="Attribute"),
          definition=function(v, num.bins=3) {
            if(class(v@data) == "numeric" || class(v@data) == "double") {
              
            len <- length(v@data)
            step <- round(len / num.bins)
            sorted <- sort(v@data)
            count <- 1
            levels <- c()
            while (count * step <= len) {
              levels <- append(levels, sorted[count * step])
              count <- count + 1
            }
            if (count * step != len) {
              levels <- append(levels, tail(sorted, n=1))
            }
            x <- getLabels(sorted, levels)
            return(attribute(factor(x)))
            } else {
              return(v)
            }
            })

setGeneric(name="discretizeEW", def= function(v, num.bins=3)standardGeneric("discretizeEW"))
setMethod(f="discretizeEW",
          signature=c(v="Attribute"),
          definition=function(v, num.bins=3) {
            if(class(v@data) == "numeric" || class(v@data) == "double") {
              minimum <- min(v@data)
              maximum <- max(v@data)
              step <- (maximum - minimum) / num.bins
              levels <- (minimum + step) * 1:num.bins
              x <- getLabels(v@data, levels)
            return(attribute(factor(x)))
            } else {
                return(v)
              }
            })

setGeneric(name="entropy", def= function(v){ standardGeneric("entropy")})
setMethod(f="entropy",
          signature="Attribute",
          definition=function(v) {
            if ((v@type == "factor" || v@type == "character")) {
              v@data <- factor(v@data)
            }
            res <- 0
            for (label in levels(factor(v@data))) {
              prob <- length(which(v@data == label)) / length(v@data)
              res <- res - (prob * log(prob, 2))
            }
            return(res)})

setGeneric(name="variance", def= function(v){ standardGeneric("variance")})
setMethod(f="variance",
          signature="Attribute",
          definition=function(v) {
            return(var(v@data))})



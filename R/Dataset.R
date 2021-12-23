setClass(Class = "Dataset",
         slots = c("index" = "Attribute", "size" = "numeric", "columns" = "ANY", "clase" = "Attribute"),
         prototype = list("index" = attribute(NULL),"size" = 0, "columns" = attribute(NULL), "clase" = attribute(NULL)))

dataset <- function() {
  return(new("Dataset"))
}

dataset <- function(columns, clase=attribute(NULL)) {
  n <- -1
  for (c in columns) {
    if (class(c) != "Attribute") {
      stop("You must provide attribute objects for both columns and class")
    }
    if (n != -1 && c@size != n) {
      stop("All the columns must have the same length")
    }
    else {
      n <- c@size
    }
  }
  if (class(clase) != "Attribute") {
    stop("The class must be an attribute object")
  }
  if (n != -1 && clase@size != n) {
    stop("The length of the class must be the same as the length of the columns")
  }
  index <- 1:n
  d <- new("Dataset", columns=columns, size=length(columns),clase=clase, index=attribute(index))
  return(d)
}

setGeneric(name="colVar", def= function(d){ standardGeneric("colVar")})
setMethod(f="colVar",
          signature="Dataset",
          definition=function(d) {
            return(sapply(d@columns, FUN = variance))})

setGeneric(name="colEntropy", def= function(d){ standardGeneric("colEntropy")})
setMethod(f="colEntropy",
          signature="Dataset",
          definition=function(d) {
            return(sapply(d@columns, FUN = entropy))})

setGeneric(name="discretizeMatrix", def= function(d, method="EW", num.bins=3){ standardGeneric("discretizeMatrix")})
setMethod(f="discretizeMatrix",
          signature=c(d="Dataset"),
          definition=function(d, method="EW", num.bins=3) {
            if (method == "EW") {
              return(dataset(sapply(d@columns, FUN = discretizeEW, d@clase)))  
            } else if (method == "EF") {
              return(dataset(sapply(d@columns, FUN = discretizeEF), d@clase))
            } else {
              stop("The methods allowed are EW and EF")
            }
            })

setGeneric(name="normalizeMatrix", def= function(d){ standardGeneric("normalizeMatrix")})
setMethod(f="normalizeMatrix",
          signature="Dataset",
          definition=function(d) {
            return(dataset(sapply(d@columns, FUN = normalize), d@clase))})



setGeneric(name="estandarizeMatrix", def= function(d){ standardGeneric("estandarizeMatrix")})
setMethod(f="estandarizeMatrix",
          signature="Dataset",
          definition=function(d) {
            return(dataset(sapply(d@columns, FUN = estandarize), d@clase))})

setGeneric(name="correlationPlot", def= function(d){ standardGeneric("correlationPlot")})
setMethod(f="correlationPlot",
          signature="Dataset",
          definition=function(d) {
            matrix <- NULL
            for (a in d@columns) {
              if (class(a@data) == "numeric" || class(a@data) == "double") {
                cat(1)
                if (is.null(matrix)) {
                  matrix <- a@data
                } else {
                matrix <- cbind(matrix, a@data)
                }
              }
            }
            if (!is.null(matrix)) {
              cor <- cor(as.matrix(matrix))
              df <- melt(cor)
              names(df) <- c("VAR1", "VAR2", "Marginal")
              ggp <- ggplot(as.data.frame(df), mapping = aes(x = VAR1, y = VAR2, fill = Marginal))
              ggp + geom_tile() + theme_bw() + theme(panel.border = element_blank(), panel.grid = element_blank(),axis.ticks = element_blank())
            }
            })

setGeneric(name="normalizedEntropyPlot", def= function(d){ standardGeneric("normalizedEntropyPlot")})
setMethod(f="normalizedEntropyPlot",
          signature="Dataset",
          definition=function(d) {
            e <- c()
            nams <- c()
            cont <- 1
            for (col in d@columns) {
                max <- log(length(levels(factor(col@data))),2)
                entr <- entropy(col) / max
                e <- c(e, entr)
                nams <- c(nams, paste("VAR", cont, collapse=""))
                cont <- cont + 1
            }
            res <- data.frame("VAR" = nams, "ENTROPY" = e)
            ggp <- ggplot(res, mapping = aes(x = VAR, y=ENTROPY, fill = VAR))
            ggp + geom_bar(stat = "identity") + theme_minimal()
            })

#' Function to generate an identity permutation
#'
#' @description This function creates an identity permutation
#' @param size numeric value indicating the size of the permutation
#' @return A new object of class \code{\linkS4class{Permutation}} equal to the
#' identity permutation of size \code{size}
#'
getIdentityPermutation <- function(size) {
  return(permutation(1:size))
}

#' Function to generate a random permutation
#'
#' @description This function creates a random permutation
#' @param size numeric value indicating the size of the permutation
#' @return A new object of class \code{\linkS4class{Permutation}} representing
#' a random permutation of size \code{size}
#'
getRandomPermutation <- function (size) {
  permu <- sample(1:size)
  return(permutation(permu))
}

#' Function to generate permutations from scores
#'
#' @description This function creates a permutation or a set of permutations
#' based on one or more sets of scores or ratings
#' @param scores either a vector or a matrix with the scores. Each set of
#' scores has to be a row in the matrix
#' @param type a character argument indicating the type of permutation.
#' Valid values are \code{'ranking'} (default value) and \code{'ordering'}
#' @param decreasing a logical value to indicate which score is better. If
#' \code{FALSE} (default value), higher scores are better
#' @return Either an object or a list of objects of class
#' \code{\linkS4class{Permutation}} corresponding to the permutations derived
#' from the scores
#' @examples
#' scores <- matrix(rnorm(100, 1, 3), ncol=10)
#' permus <- getPermutationFromScores (scores)
#' scores[5, ]
#' permus[[5]]
#'
getPermutationFromScores <- function (scores, type="ranking", decreasing=FALSE) {
  if(is.matrix(scores)) {
    getPermutations(scores, type, decreasing)
  } else {
    switch(type,
           "ranking"={
             if (!decreasing) {
               scores <- -1 * scores
             }
             permu <- rank(scores, ties.method="random")
           },
           "ordering"={
             permu <- order(scores, decreasing=decreasing)
           },
           {
             warning("El parametro type solo puede ser 'ranking' u 'ordering'")
             permu <- NULL
           })
    return(permutation(permu))
  }
}

# Esta funcion no hace falta documentarla igual, ya que no ira en la ayuda
# En cualquier caso, y aunque aqui no se haga por espacio, es importante
# documentar bien el codigo para que se comprensible
getPermutations <- function(scores, type, decreasing){
  lst <- lapply(1:nrow(scores),
                FUN=function(i){
                  permu <- getPermutationFromScores(scores[i, ],
                                                    type=type,
                                                    decreasing=decreasing)
                  return(permu)
                })
  return(lst)
}


#' Function to get the first order marginals
#'
#' @description This function estimates, from a list of permutations, the first
#' order marginals
#' @param permutations a list of objects of class \code{\linkS4class{Permutation}}
#' @param smoothed a logical value to indicate whether the estimation should
#' be smoothed (\code{FALSE} by default)
#' @return A matrix containing the first order marginals
#' @examples
#' scores <- matrix(rnorm(100, 1, 3), ncol=10)
#' permus <- getPermutationFromScores (scores)
#' getFirstOrderMarginals(permus)
#'
getFirstOrderMarginals <- function (permutations, smoothed=FALSE) {
  aux <- lapply(permutations, FUN=function(e) return(e@permutation))
  permu.set <- do.call(rbind, aux)
  size <- ncol(permu.set)
  if (!smoothed) {
    getMarginal <- function (vector)
    {
      aux <- sapply(1:size, FUN=function(i) {return(sum(vector==i))})
      return (aux / sum(aux))
    }
  } else {
    getMarginal <- function (vector)
    {
      aux <- sapply(1:size, FUN=function(i) {return(sum(vector==i))})
      return ((aux + 1) / (sum(aux) + size))
    }
  }
  return(apply(permu.set, MARGIN=2, FUN=getMarginal))
}

# Ejercicio R5.1
readCSV <- function(path) {
  data <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
  return(data)
}

?file.create
# Ejercicio R5.2
createLog <- function(name) {
  file.create(name)
}

writeLog <- function(path, message) {
  con <- file(path, open = 'w')
  writeLines(paste(Sys.Date(), " - ", message, sep = " "), con = con)
  close(con = con)
}

# Ejercicio R5.5 
not <- function(x) {return(!x)}

not_v <- function(v) {return(sapply(v, FUN = not))}

roc <- function(d) {
  # d is a data.frame
  s <- sort(d[,1], index.return = TRUE)
  d[,1] <- d[,1][s$ix]
  d[,2] <- d[,2][s$ix]
  thresholds <- seq(0, 1, by=0.1) 
  tprs <- c()
  fprs <- c()
  for (t in thresholds) {
    res <- sapply(d[,1], FUN = function(x){x >= t})
    tp <- sum(as.numeric(res) * as.numeric(d[,2]))
    tn <- sum(as.numeric(not_v(res)) * as.numeric(not_v(d[,2])))
    fp <- sum(as.numeric(res) * as.numeric(not_v(d[,2])))
    fn <- sum(as.numeric(not_v(res)) * as.numeric(d[,2]))
    tprs <- c(tprs, tp / (tp + fn)) 
    fprs <- c(fprs, fp / (fp + tn))
  }
  plot(fprs, tprs, type = "b")
  return(list(fprs, tprs))
}


getLabels <- function(x, levels) {
  res <- c()
  for (value in x) {
    range <- getValueRange(value, levels)
    if (range == 1) {
      label <- paste("(inf,", levels[range],  ")", collapse = "")
    } 
    else if (range > length(levels)) {
      label <- paste("(", levels[range-1], ",inf)", collapse="")
    } 
    else {
      label <- paste("(", levels[range-1], ",", levels[range], ")", collapse = "")
    }
    res <- append(res, label)
  }
  return(res)
}

getValueRange <- function(x, levels) {
  pos <- 1
  for (range in levels) {
    if (x < range) {
      return(pos)
    }
    pos <- pos + 1
  }
  return(length(levels)+1)
}


#' Generate a list of text grammatically correctly
#' 
#' @param x a vector to be put into a list.
#' @param oxford.comma Should the Oxford comma be used?
#' @return A character string of the vector input separated by commas and the
#'   word "and" as appropriate.
#' @export
comma.and <- function(x, oxford.comma=TRUE) {
  ret <- as.character(x)
  if (length(ret) == 0) {
    warning("comma.and was given zero-length input")
    ret <- ""
  } else if (length(ret) == 1) {
    ## Do nothing
  } else if (length(ret) == 2) {
    ## Only put an "and" between the inputs
    ret <- paste(ret, collapse=" and ")
  } else {
    ## Put commas between most of the entries, put "and" between the last two,
    ## and put an Oxford comma in before "and" if requested.
    thisand <- " and "
    if (oxford.comma) {
      thisand <- ", and "
    }
    ret <- paste(paste(ret[-length(ret)], collapse=", "),
                 ret[length(ret)], sep=thisand)
  }
  ret
}

library(dplyr)

listtable <- function(data, listdef, name.col="Description", count=1) {
  ret <- data.frame(stringsAsFactors=FALSE)
  tmprow <- data.frame(Description=NA, stringsAsFactors=FALSE)[,c(),drop=FALSE]
  name.col.current <- paste(name.col, count, sep=".")
  for (i in seq_along(listdef)) {
    if (is.list(listdef[[i]])) {
      subresult <- listtable(data, listdef[[i]], name.col=name.col, count=count + 1)
      tmp.wrapper <- data.frame(X=names(listdef)[i],
                                stringsAsFactors=FALSE)
      names(tmp.wrapper) <- name.col.current
      subresult <- cbind(tmp.wrapper, subresult)
      ret <- bind_rows(ret, subresult)
    } else if (is.character(listdef[[i]])) {
      tmprow[,names(listdef)[i]] <- data[[listdef[[i]]]]
    }
  }
  if (ncol(tmprow) > 0) {
    ret <- bind_rows(tmprow, ret)
  }
  ## Sort the columns of the output
  depth.name.count <- length(grep(paste0("^", name.col), names(ret)))
  if (depth.name.count > 0) {
    depth.names <- paste(name.col, count:(count + depth.name.count - 1), sep=".")
  } else {
    depth.names <- c()
  }
  ret <- ret[,c(depth.names, setdiff(names(ret), depth.names)), drop=FALSE]
  attr(ret, "name.col") <- name.col
  class(ret) <- c("listtable", class(ret))
  ret
}

summary.listtable <- function(object, ..., indent=" --- ", na.string="",
                              name.col=attr(object, "name.col")) {
  name.cols <- grep(paste0("^", name.col), names(object), value=TRUE)
  data.cols <- setdiff(names(object), name.cols)
  ret <- data.frame()
  if (length(name.cols) == 0) {
    ## Fully recursed
    ret <- object[,data.cols]
  } else {
    ret <- data.frame()
    ## Operate on the first level of grouping and then recurse for subsequent levels
    for (n in unique(object[[name.cols[1]]])) {
      mask.n <- object[[name.cols[1]]] %in% n
      if (identical(NA, n)) {
        header <- data.frame()
        content <- object[,data.cols]
      } else {
        header <- data.frame(X=n, stringsAsFactors=FALSE)
        names(header) <- name.col
        content <-
          summary.listtable(object[mask.n, c(name.cols[-1], data.cols)], ...,
                            indent=indent, na.string=na.string, name.col=name.col)
      }
      ## Clean up the name.col
      if (name.col %in% names(content)) {
        mask.na <- is.na(content[[name.col]])
        if (sum(mask.na) == 1) {
          content[mask.na, name.col] <- header[[name.col]]
          if (any(!mask.na)) {
            content[!mask.na, name.col] <- paste0(indent, content[!mask.na, name.col])
          }
        } else {
          content[mask.na, name.col] <- na.string
          content[,name.col] <- paste0(indent, content[,name.col])
          content <- dplyr::bind_rows(header, content)
        }
      } else { ## name.col isn't in the names
        if (nrow(content) == 1) {
          content <- dplyr::bind_cols(header, content)
        } else {
          content[,name.col] <- paste0(indent, content[,name.col])
          content <- dplyr::bind_rows(header, content)
        }
      }
      ret <- dplyr::bind_rows(ret, content)
    }
  }
  class(ret) <- c("summary.listtable", setdiff(class(ret), "listtable"))
  attr(ret, "name.col") <- name.col
  ret
}

as.data.frame.summary.listtable <- function(x, ..., digits=3, na.string="") {
  name.col <- attr(x, "name.col")
  charcount <- nchar(x[[name.col]])
  x[[name.col]] <- sprintf(paste0("%-", max(charcount), "s"), x[[name.col]])
  for (n in names(x)) {
    if (is.numeric(x[[n]])) {
      mask.na <- is.na(x[[n]])
      x[[n]] <- PKNCA::signifString(x[[n]], digits=digits)
      x[[n]][mask.na] <- na.string
    }
  }
  class(x) <- "data.frame"
  x
}

subkable <- function(x) {
  charcount <- pmax(sapply(as.list(x), FUN=function(x) max(nchar(x))),
                    nchar(names(x))) + 1
  colsep <- c("+", rep(c("|", "+"), nrow(x) + 1))
  ret <- colsep
  for (i in seq_len(ncol(x))) {
    spformat <- paste0("%-", charcount[i], "s ")
    ret <- paste0(ret,
                  ## Add the header
                  c(strrep("-", charcount[i] + 1),
                    sprintf(spformat, names(x)[i]),
                    # Separate the header from the content
                    strrep("=", charcount[i] + 1),
                    # Add the content
                    rbind(sprintf(spformat, x[[i]]),
                          strrep("-", charcount[i] + 1))),
                  colsep)
  }
  knitr::asis_output(paste(c(ret, "\n"), collapse="\n"))
}

# Technique from http://htmlpreview.github.io/?https://github.com/wilkelab/cowplot/blob/master/inst/doc/shared_legends.html

plot_grid_one_legend <- function(...) {
  args <- list(...)
  ## Extract the legend from the first plot.
  grobs <- ggplotGrob(args[[1]])$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
  ## Concatenate the plots together
  ret <- list()
  for (i in seq_along(args)) {
    ret[[i]] <- args[[i]] + theme(legend.position="none")
  }
  ret[[i + 1]] <- legend
  ret
}

# Font Color Format (from http://stackoverflow.com/questions/29067541/rmarkdown-how-to-change-the-font-color)
colortext <- function(x, color, outputFormat) {
  if (missing(outputFormat))
    outputFormat <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(outputFormat == 'latex')
    paste("\\textcolor{",color,"}{",x,"}",sep="")
  else if(outputFormat == 'html')
    paste("<font color='",color,"'>",x,"</font>",sep="")
  else
    x
}

colortextbg <- function(x, color, alphapct=20, outputFormat) {
  if (missing(outputFormat))
    outputFormat <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(outputFormat == 'latex') {
    # Requires \usepackage{color}
    paste("\\colorbox{",color, "!", alphapct, "}{",x,"}",sep="")
  } else if(outputFormat == 'html') {
    color <- col2rgb(color, alpha=TRUE)
    color["alpha",] <- color["alpha",] * alphapct/100
    paste("<font style='background-color: ",
          paste0("#", paste0(as.hexmode(color), collapse="")), "'>",
          x,"</font>",sep="")
  } else {
    x
  }
}

knit_print.sessionInfo <- function(x, ...) {
  if (missing(x))
    x <- sessionInfo()
  packagename <- function(x) {
    sha <- ifelse("GithubSHA1" %in% names(x),
                  paste(", GithubSHA1:", x$GithubSHA1),
                  "")
    sprintf("%s(%s%s)", x$Package, x$Version, sha)
  }
  otherpkg <- sapply(x$otherPkgs, FUN=packagename)
  namespacepkg <- sapply(x$loadedOnly, FUN=packagename)
  ret <-
    c(paste("*", c(x$R.version$version.string,
                   paste("Platform:", x$platform))),
      paste("* Locale:", paste(strsplit(x$locale, ";")[[1]], collapse=", ")),
      "* Packages",
      paste("    * Base:", paste(x$basePkgs, collapse=", ")),
      paste("    * Attached:", paste(otherpkg, collapse="; ")),
      paste("    * Namespaces (not attached):", paste(namespacepkg, collapse="; ")))
  knitr::asis_output(paste(ret, collapse="\n"))
}

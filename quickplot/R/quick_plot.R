#' Quickly make a boxplot or scatterplot
#'
#' @param x a vector with class `numeric` or `factor`.
#' @param y a vector with class `numeric`.
#' @param xlab character indicating the x-axis label. Default is NULL.
#' @param ylab character indicating the y-axis label. Default is NULL.
#' @param title character indicating the main title. Default is NULL.
#' @param interaction_variable a vector with class `factor`. Default is NULL.
#' @param interaction_lab character indicating the legend label/title for the interaction variable. Default is NULL.
#'
#' @return A ggplot object.
#' @import ggplot2 tibble tidyr
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' quickplot(x = factor(rbinom(100, 1, 0.4)), y = c(1:100), xlab = "Knows R Studio (0=no, 1=yes)", ylab = "Final Grade (%)")
quick_plot <- function(x, y, xlab=NULL, ylab=NULL, title=NULL, interaction_variable=NULL, interaction_lab=NULL) {
  if(!is.null(xlab) && class(xlab) != "character") {
    warning("'xlab' should be of class 'character'!")
  }
  else if(!is.null(ylab) && class(ylab) != "character") {
    warning("'ylab' should be of class 'character'!")
  }
  else if(!is.null(title) && class(title) != "character") {
    warning("'title' should be of class 'character'!")
  }
  else if(!is.null(interaction_lab) && class(interaction_lab) != "character") {
    warning("'interaction_lab' should be of class 'character'!")
  }
  else if(class(x) == "factor" && is.numeric(y)) {
    (tibble::as_tibble(data.frame(x, y)) %>%
       tidyr::drop_na() %>% # NA handling
       ggplot2::ggplot(ggplot2::aes(x=x, y=y, fill=x)) +
       ggplot2::geom_boxplot() +
       ggplot2::labs(y = ylab, x = xlab, fill = xlab) +
       ggplot2::ggtitle(title) +
       ggplot2::theme_minimal())
  }
  else if(is.numeric(x) && is.numeric(y) && !is.factor(interaction_variable)) {
    (tibble::as_tibble(data.frame(x, y)) %>%
       tidyr::drop_na() %>% # NA handling
       ggplot2::ggplot(ggplot2::aes(x=x, y=y, show.legend=FALSE)) +
       ggplot2::geom_point() +
       ggplot2::labs(y = ylab, x = xlab) +
       ggplot2::ggtitle(title) +
       ggplot2::theme_minimal())
  }
  else if(is.numeric(x) && is.numeric(y) && is.factor(interaction_variable)) {
    (tibble::as_tibble(data.frame(x, y, interaction_variable)) %>%
       tidyr::drop_na() %>% # NA handling
       ggplot2::ggplot(ggplot2::aes(x=x, y=y)) +
       ggplot2::geom_point(ggplot2::aes(color=interaction_variable)) +
       ggplot2::labs(y = ylab, x = xlab) +
       ggplot2::ggtitle(title) +
       ggplot2::scale_color_discrete(name=interaction_lab) +
       ggplot2::theme_minimal())
  }
  else stop("Please check your variable types, as x must be numeric or factor, while y must be numeric.")
}

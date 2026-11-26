p <- c("shiny","ggplot2")
m <- p[!vapply(p,requireNamespace,logical(1),quietly=TRUE)]
if(length(m)) install.packages(m,repos="https://cloud.r-project.org")

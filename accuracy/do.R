library(ggplot2)
library(ggthemes)
library(grid)
library(scales)

format_si <- function(...) {
  # Format a vector of numeric values according
  # to the International System of Units.
  # http://en.wikipedia.org/wiki/SI_prefix
  #
  # Based on code by Ben Tupper
  # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
  # Args:
  #   ...: Args passed to format()
  #
  # Returns:
  #   A function to format a vector of strings using
  #   SI prefix notation
  #
 
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")
 
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
 
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)

    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}

data <- read.table("data.csv", sep=",", header=TRUE)
data$name <- factor(data$name, levels=unique(as.character(data$name)) )

data <- data[data$name != "retainer",]
data <- data[data$name != "large",]

#data <- data[data$limit >= 524288,]

#scale <- 72 * 2.5
scale <- 1

pdf("data.pdf", width=8*scale, height=1.7*scale)
ggplot(data) +
    geom_abline(intercept=0, slope=1, color="gray") +
    geom_abline(intercept=0, slope=2) +
    geom_linerange(aes(x=limit, ymax=pmax(valgrind, block), ymin=census)) +
    labs(x="", y="") +
    scale_x_continuous(labels=format_si(), breaks=c(300e6,600e6)) +
    scale_y_continuous(breaks=c(300e6,600e6,900e6,1200e6), labels=c("300 M", "600 M", "900 M", "1200 M")) +
    facet_wrap(~ name, nrow=1) +
    theme_bw() +
    theme(axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=10),
          strip.background = element_rect(fill="white", color="white"),
          plot.margin = unit(c(0,0.6,-0.4,-0.3),"cm"),
          panel.border = element_blank())
dev.off()

# ggplot(data) + geom_abline(intercept=0, slope=1) + geom_point(aes(limit, valgrind)) + geom_point(aes(limit,census),color="blue") + geom_point(aes(limit,block),color="red")
# ggplot(data[data$name=="retainer",]) + geom_abline(intercept=0, slope=1) + geom_abline(intercept=0, slope=2) + geom_point(aes(limit, valgrind)) + geom_point(aes(limit,census),color="blue") + geom_point(aes(limit,block),color="red")

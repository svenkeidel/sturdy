library(ggplot2)
library(tikzDevice)
library(stringr)
library(dplyr)
library(viridis)

data = read.csv("bench.csv")

data <- data %>%
  filter(Algorithm != "Parallel") %>%
  mutate(
    # Order Algorithms
    Algorithm = ordered(Algorithm, levels=c("ADI", "ChaoticInner", "ChaoticOuter")),

    # Group benchmarks into suits
    Suite = factor(ifelse(str_detect(Function, "gabriel"), "Gabriel", "Scala-AM")),

    # Convert runtime to Microseconds
    Runtime = Mean * 10e+6,
    RuntimeStd = Stddev * 10e+6,

    # Extract benchmark name from file name
    Benchmark = str_match(Function, ".*/(.*)\\..*")[,2]
  )

## derivInner <- data %>% filter(Algorithm == "ChaoticInner", Benchmark == "deriv")
## derivOuter <- data %>% filter(Algorithm == "ChaoticOuter", Benchmark == "deriv")
## sprintf("Speedup for deriv (ChaoticInner/ChaoticOuter): %f", derivInner$Runtime / derivOuter$Runtime)

baseline <- "ADI"
data <- data %>%
  group_by(Benchmark) %>%
  mutate(
    RuntimeBaseline = Runtime[which(Algorithm == baseline)],
    RuntimeBaselineStd = RuntimeStd[which(Algorithm == baseline)],

    # Normalize runtime for the baseline algorithm
    Speedup = RuntimeBaseline / Runtime,

    # The speedup is the division of two noncorrelated, normally distributed random variables X and Y.
    # The result `X/Y` is a ratio distribution whose standard deviation can be computed as follows:
    SpeedupStd = abs(RuntimeBaseline / Runtime) *
                 sqrt(
                   RuntimeBaselineStd^2 / RuntimeBaseline^2 +
                   RuntimeStd^2 / Runtime^2
                 ),

    # Normalize the number of evaluated calls for ChaoticOuter
    Evaluated = Evaluated / Evaluated[which(Algorithm == baseline)]
  )

AlgorithmLabels <- c(
  # "$\\varphi_{\\mathtt{parallel}}$",
  "$\\varphi_{\\mathtt{topmost}}$",
  "$\\varphi_{\\mathtt{innermost}}$",
  "$\\varphi_{\\mathtt{outermost}}$"
  )

plottingOptions = list(
  geom_col(width=0.8, position=position_dodge2(width=1,preserve="total")),
  facet_grid(cols=vars(Suite), scale="free_x", space="free"),
  scale_fill_viridis(discrete = TRUE, name = "Algorithm", labels = AlgorithmLabels),
  theme_minimal(),
  theme(legend.title = element_blank(),
        # legend.background = element_rect(fill="white", size=0.05),
        legend.position = c(0.47,0.80),
        # legend.position = "bottom",
        legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(color="black", angle = 45),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 12))
)

# Plot Runtime
p <- ggplot(data, aes(x=Benchmark, y=Speedup, fill=Algorithm, order=Algorithm)) +
    plottingOptions +
    geom_errorbar(aes(ymin=Speedup-SpeedupStd, ymax=Speedup+SpeedupStd), width=.5, position=position_dodge(0.8)) +
    # coord_cartesian(ylim=c(0,4)) +
    ylab("speedup over $\\varphi_{\\mathtt{topmost}}$")
# ggsave(paste("runtime.pdf", sep=""), plot=p, device="pdf")
tikz(file=paste("runtime.tex", sep=""), standAlone=F, width=5.5, height=2.5)
print(p)
dev.off()

# p <- ggplot(data, aes(x=Benchmark, y=Evaluated, fill=Algorithm, order=Algorithm)) +
#     ylab("\\# evaluated calls / $\\varphi_{\\mathtt{chaotic}}(\\mathtt{inner})$") +
#     plottingOptions
# # ggsave(paste("evaluated.pdf", sep=""), plot=p, device="pdf", width=7, height=3)
# tikz(file=paste("evaluated.tex", sep=""), standAlone=F, width=5.5, height=3)
# print(p)
# dev.off()

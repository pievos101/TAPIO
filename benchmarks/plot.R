# Run from repository root after copying the run's summary.csv to benchmarks/.
library(ggplot2)
s <- read.csv("benchmarks/summary.csv")
stopifnot(nrow(s)==16L, all(s$n==10L))
d <- do.call(rbind,lapply(c("ARI","NMI"),function(metric) {
    data.frame(case=s$case, eta=s$eta, method=s$method, metric=metric,
               mean=s[[metric]], se=s[[paste0(metric,"_sd")]]/sqrt(s$n))
}))
d$Case <- factor(d$case, levels=c("regular","irregular"),
                 labels=c("Regular times","Irregular times (discretized)"))
d$Method <- factor(d$method, levels=c("main","inductive"),
                   labels=c("Transductive main","Inductive iTAPIO"))
p <- ggplot(d,aes(eta,mean,colour=Method,fill=Method,group=Method)) +
    geom_ribbon(aes(ymin=pmax(0,mean-se),ymax=pmin(1,mean+se)),
                alpha=0.12,colour=NA) +
    geom_line(linewidth=0.9) + geom_point(size=2.4) +
    facet_grid(Case~metric) +
    scale_x_continuous(breaks=c(1,3,6,9)) +
    scale_y_continuous(limits=c(0,1.02),breaks=seq(0,1,0.25)) +
    scale_colour_manual(values=c("#D57920","#255F85")) +
    scale_fill_manual(values=c("#D57920","#255F85")) +
    labs(title="Inductive and transductive longTAPIO across noise levels",
         subtitle="Same held-out patients; mean ± standard error across 10 fresh runs",
         x="Observation noise (eta)", y="Agreement with simulated true clusters",
         colour=NULL,fill=NULL) +
    theme_classic(base_size=12) + theme(legend.position="bottom")
ggsave("man/figures/noise-accuracy.png",p,width=10,height=6.6,dpi=160,bg="white")

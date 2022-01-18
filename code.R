#for most analyses (scatterplot):
plot1<-ggplot(total_RINs, aes(x = Survival, y = RIN))
plot1 + geom_point()
plot2 <- plot1 + geom_point(aes(color=factor(Species)),size=4)
plot2
plot3<-plot2+theme_bw()
plot3
cor.test(Treatment, RIN, method="kendall")
wilcox.test(RIN~Treatment,paired=FALSE)
#for selaginella barplot:
summary.cyrt <- data.frame(Pod=levels(as.factor(cyrtopodium$Pod)),
     meanRIN=tapply(cyrtopodium$RIN, cyrtopodiheaum$Pod, mean),
     sd=tapply(cyrtopodium$RIN, cyrtopodium$Pod, sd))
D<-ggplot(summary.cyrt, aes(x = Pod, y = meanRIN)) + 
     geom_bar(stat = "identity", position="dodge",fill="mediumaquamarine") +
     geom_errorbar(aes(ymin=meanRIN-sd, ymax=meanRIN+sd), width=.3, color="darkblue") + theme_bw()
D + geom_signif(comparisons = list(c("1", "2")), 
                map_signif_level=TRUE)

my_comparisons <- list( c("1", "2"))
D + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 10)
#for validation boxplot:
P<-ggplot(validation, aes(x=Treatment, y=RIN, fill=Treatment)) + geom_boxplot() +
     guides(fill=FALSE) + theme_bw()
#add significance
P + geom_signif(comparisons = list(c("fresh", "1B")), 
              map_signif_level=TRUE)
#or
my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
P + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)
library(ggsignif)
library(ggpubr)

#kruskal wallis for validation experiment:
kruskal.test(RIN~Treatment)
#post-hoc Tukey's (load PCMCR):
posthoc.kruskal.nemenyi.test(x=RIN, g=Treatment, method="Tukey")

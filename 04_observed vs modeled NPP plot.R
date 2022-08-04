file.choose()
library(ggplot2)
library(ggthemes)
NPP <- read.csv("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\Data\\NPP\\obs vs pred NPP_obs from raw data.csv")

npp.plot <- ggplot(NPP, aes(x=pred.npp, y=obs.npp, colour=site)) +
  geom_point(size=2) +
  geom_abline(slope=1, intercept=0)+
  geom_errorbar(aes(ymin=obs.npp-obs.npp.se, ymax=obs.npp+obs.npp.se))+
  theme(text = element_text(size=10))+
  xlab("Modeled NPP (g m-2)")+
  ylab("Observed NPP (g m-2)")+
  ylim(50,850)+
  xlim(50,850)+
  theme_few()

summary(lm(obs.npp~pred.npp, data=NPP))
npp.model <- lm(obs.npp~pred.npp, data=NPP)
summary(npp.model)
confint(npp.model)

ggsave(filename="C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_Feb2017\\Figures\\Observed vs Modeled NPP.pdf",plot=npp.plot,width=5.0,height=3.5,units="in",dpi=600)

library(plyr)
ddply(NPP, .(site), summarize,
      npp=mean(pred.npp)*0.55)




#PREBAL

setwd("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/EMBENS Rpath")
require(ggplot2)
require(dplyr)
load_from_EwE <- "yes"

basic_estimates <- read.csv('C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/SBNS unbalanced ecopath model - basic estimates_start_values.csv')
if (load_from_EwE == "yes"){
  basic_estimates <- basic_estimates[-which(is.na(basic_estimates$X)),-c(4, 5, 14)]

  colnames(basic_estimates) <- c("X","FG","TL","B","Z","P","Q","EE","PQ","BA","BAY")
  basic_estimates$P[which(is.na(basic_estimates$P))] <- basic_estimates$Z[which(is.na(basic_estimates$P))]

}
basic_estimates <- basic_estimates %>%
  arrange(basic_estimates$TL)

basic_estimates$X <- factor(basic_estimates$X, levels = c(basic_estimates$X))
basic_estimates$B <- as.numeric(basic_estimates$B)
basic_estimates$RN <- as.numeric(rownames(basic_estimates))

# biomass
lm_eqn <- function(df){
  m <- lm(B ~ RN, df);
  eq <- substitute(italic(y) == b %.% italic(x) + a*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


ggplot(data = basic_estimates, aes(x = X, y = B))+
  geom_point() +
  scale_y_log10() +
  geom_smooth(data = basic_estimates, aes(x = RN, y = B),method=lm, se= FALSE, colour = 'black') +
  geom_text(x = 35, y = 1, label = lm_eqn(basic_estimates), parse = TRUE) +
  xlab("Functional group") +
  ylab(expression(paste("Biomass (t / km"^2,")"))) +
  theme_bw()

# production
lm_eqn <- function(df){
  m <- lm(P ~ RN, df);
  eq <- substitute(italic(y) == b %.% italic(x) + a*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

ggplot(data = basic_estimates, aes(x = X, y = P))+
  geom_point() +
  scale_y_log10() +
  geom_smooth(data = basic_estimates, aes(x = RN, y = P), method=lm, se= FALSE, colour = 'black') +
  geom_text(x = 35, y = 1, label = lm_eqn(basic_estimates), parse = TRUE)+
  xlab("Functional group") +
  ylab("Production/biomass") +
  theme_bw()

# consumption
lm_eqn <- function(df){
  m <- lm(Q ~ RN, df);
  eq <- substitute(italic(y) == b %.% italic(x) + a*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

ggplot(data = basic_estimates, aes(x = X, y = Q))+
  geom_point() +
  scale_y_log10() +
  geom_smooth(data = basic_estimates, aes(x = RN, y = Q),method=lm, se= FALSE, colour = 'black') +
  geom_text(x = 35, y = 1, label = lm_eqn(basic_estimates), parse = TRUE)+
  xlab("Functional group") +
  ylab("Consumption/biomass") +
  theme_bw()

# production/consumption
ggplot(data = basic_estimates, aes(x = X, y = PQ))+
  geom_point() +
  geom_hline(yintercept = 0.1, linetype='dashed') +
  geom_hline(yintercept = 0.3, linetype='dashed')+
  xlab("Functional group") +
  ylab("Production/consumption") +
  theme_bw()


# Postbal
setwd("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/EMBENS Rpath")
require(ggplot2)
require(dplyr)
load_from_EwE <- "yes"

basic_estimates <- read.csv('C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/Manuscripts/Technical report/benthos split/EMBENS_SP-Basic estimates - balanced_2.csv', header = TRUE, sep = ",")
if (load_from_EwE == "yes"){
  basic_estimates <- basic_estimates[-which(is.na(basic_estimates$X)),-c(4, 5, 14)]

  colnames(basic_estimates) <- c("X","FG","TL","B","Z","P","Q","EE","PQ","BA","BAY")
  basic_estimates$P[which(is.na(basic_estimates$P))] <- basic_estimates$Z[which(is.na(basic_estimates$P))]

}

basic_estimates <- basic_estimates %>%
  arrange(basic_estimates$TL)

basic_estimates$X <- factor(basic_estimates$X, levels = c(basic_estimates$X))
#basic_estimates$RN <- basic_estimates$TL
basic_estimates$TL <- factor(basic_estimates$TL, levels = c(basic_estimates$TL))

basic_estimates$B <- as.numeric(basic_estimates$B)
basic_estimates$RN <- as.numeric(rownames(basic_estimates))

# biomass
lm_eqn <- function(df){
  m <- lm(log10(B) ~ RN, df);
  eq <- substitute(italic(y) == b %.% italic(x) + a*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


ggplot(data = basic_estimates, aes(x = X, y = B))+
  geom_point() +
  scale_y_log10() +
  geom_smooth(data = basic_estimates, aes(x = RN, y = B),method=lm, se= FALSE, colour = 'black') +
  geom_text(x = 35, y = 1, label = lm_eqn(basic_estimates), parse = TRUE) +
  xlab("Functional group") +
  ylab(expression(paste("Biomass (t.km"^-2,")"))) +
  theme_bw()


# production
lm_eqn <- function(df){
  m <- lm(log10(P) ~ RN, df);
  eq <- substitute(italic(y) == b %.% italic(x) + a*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

ggplot(data = basic_estimates, aes(x = X, y = P))+
  geom_point() +
  scale_y_log10() +
  geom_smooth(data = basic_estimates, aes(x = RN, y = P), method=lm, se= FALSE, colour = 'black') +
  geom_text(x = 35, y = 1, label = lm_eqn(basic_estimates), parse = TRUE)+
  xlab("Functional group") +
  ylab(expression(paste("Production/biomass (year"^-1,")"))) +
  theme_bw()

# consumption
lm_eqn <- function(df){
  m <- lm(log10(Q) ~ RN, df);
  eq <- substitute(italic(y) == b %.% italic(x) + a*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

ggplot(data = basic_estimates, aes(x = X, y = Q))+
  geom_point() +
  scale_y_log10() +
  geom_smooth(data = basic_estimates, aes(x = RN, y = Q),method=lm, se= FALSE, colour = 'black') +
  geom_text(x = 35, y = 1, label = lm_eqn(basic_estimates), parse = TRUE)+
  xlab("Functional group") +
  ylab(expression(paste("Consumption/biomass (year"^-1,")"))) +
  theme_bw()

# production/consumption
ggplot(data = basic_estimates, aes(x = X, y = PQ))+
  geom_point() +
  geom_hline(yintercept = 0.1, linetype='dashed') +
  geom_hline(yintercept = 0.3, linetype='dashed')+
  xlab("Functional group") +
  ylab(expression(paste("Production/consumption (year"^-1,")"))) +
  theme_bw()


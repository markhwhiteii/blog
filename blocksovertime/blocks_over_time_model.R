library(brms)
library(tidyverse)
dat <- read_csv("data89to16wallnba.csv") %>% 
  filter(mptot >= 500) %>% 
  mutate(player=as.factor(player),
         year=year-1989)

set.seed(1839)
mod <- brm(drtg ~ blkp100*year + (1+blkp100|player),
           prior=c(set_prior("normal(0,5)", class="b"),
                   set_prior("student_t(3,0,10)", class="sd"),
                   set_prior("lkj(2)", class="cor")),
           data=dat, warmup=1000, iter=2000, chains=4)

blk_year_fixef <- as.data.frame(fixef(mod))

blk_year_ss <- data.frame(year=rep(NA, length(1989:2016)),
                          beta0=rep(NA, length(1989:2016)),
                          beta1=rep(NA, length(1989:2016)))

for (i in unique(dat$year)) {
  blk_year_ss$year[which(i==unique(dat$year))] <- i + 1989
  blk_year_ss$beta0[which(i==unique(dat$year))] <- blk_year_fixef[1,1] + blk_year_fixef[3,1] * i
  blk_year_ss$beta1[which(i==unique(dat$year))] <- blk_year_fixef[2,1] + blk_year_fixef[4,1] * i
}

p1 <- ggplot(blk_year_ss[blk_year_ss$year %in% c(1989, 1998, 2007, 2016),]) +
  scale_x_continuous(name="Blocks Per 100 Possessions", limits=c(0,9)) + 
  scale_y_continuous(name="Defensive Rating", limits=c(85,120)) +
  geom_abline(aes(intercept=beta0, slope=beta1, linetype=factor(beta1))) +
  scale_linetype(name="Year", labels=c("2016", "2007", "1998", "1989")) +
  theme_minimal() +
  theme(text=element_text(size=16))

blk_year_int_posterior <- posterior_samples(mod)$`b_blkp100:year`
sum(blk_year_int_posterior<0)/length(blk_year_int_posterior)

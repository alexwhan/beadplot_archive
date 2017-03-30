library(mpwgaim)
library(asreml)
library(mpMap)

set.seed(1234)
m4_int_qtl <- mpcross2int(genomap::m4_cross, gen.type = "mpInterval")

nqtl.col <- dim(genomap::m4_cross$qtlgeno$finals)[2]
mmat <- matrix(nrow=360, ncol=4*nqtl.col/2)
for (ii in 1:(nqtl.col/2)) {
  qtl.fac <- factor(genomap::m4_cross$qtlgeno$finals[,ii])
  mmat[,(4*ii-3):(4*ii)] <- model.matrix(~qtl.fac - 1)
}
## Effects
m4_qtl <- matrix(data=c(1, 142, 0.354, -0.354, -0.354, 0.354,
                        2, 162, 0.354, -0.354, -0.354, 0.354,
                        5, 78, 0.354, -0.354, -0.354, 0.354),
                 nrow=3, ncol=6, byrow=TRUE)

qtl.sizes <- as.vector(t(m4_qtl[,3:6]))
qtl.effect <- mmat %*% qtl.sizes
## Polygenic variance
pvar <- 0.1
## Function to calculate approximate percentage variance for each QTL
perc.var <- function(m4_qtl, poly.var) {
  nfounders <- dim(m4_qtl)[2]-2
  prob <- 1/nfounders
  varq <- diag(rep(prob,nfounders)) - rep(prob,nfounders) %*% t(rep(prob,nfounders))
  gvar <- apply(m4_qtl[, -c(1,2)], 1, function(el, varq) sum((el %*% varq) * el), varq)
  totvar <- sum(gvar)+pvar
  perc.var <- 100*gvar/totvar
  round(perc.var,1)
}
## Percentage variance for each QTL
percvar <- perc.var(m4_qtl, pvar)
percvar
## Setup simulated data for analysis
ngeno <- 360
nrep <- 3
id <- factor(rep(paste0("L", 1:ngeno),nrep))
pheno.data <- data.frame(y = 0, Rep=nrep, id=id)
set.seed(10)
ee <- rnorm(ngeno*nrep,0,0.05)
uu <- rnorm(ngeno,0,0.05)
pheno.data$y <- 10 + rep(qtl.effect,3) + sqrt(1/2)*c(uu,uu,uu) + ee
sim.asr0 <- asreml(y ~ 1, random = ~ id, data=pheno.data)
sim.qtl <- mpwgaim(sim.asr0, pheno.data, m4_int_qtl, merge.by = "id",
                   verboseLev=0, gen.type="interval", na.method.X='include',
                   data.name = "sim.data")
m4_summary <- summary(sim.qtl, m4_int_qtl)

save(m4_summary, file = "data/m4_summary.rda")

library(qtl)
library(dplyr)
source("archive/beadplotFun.R")

set.seed(123)
#Make a map
sim_map <- sim.map(len = sample(20:40, 5), sample(15:40, 5), include.x = FALSE)
names(sim_map) <- paste0(1:5, "A")

#Make a cross object
sim_cross <- sim.cross(sim_map)

#Pull out the map elements from $geno
sim_cross$map <- lapply(sim_cross$geno, function(x) return(x$map))
sim_df <- ggGenMap(sim_cross, printPlot = TRUE)

#Simulate qtl table
qtl_sum <- sim_df %>% 
  filter(row_number() %in% sample(1:nrow(.), 10)) %>% 
  mutate(Size = rnorm(10, 10, 1),
         X..var = sample(10:20, 10),
         LOGP = runif(10, 5, 10), #Current function *must* have a LOGP column...
         phenotype = sample(c("Pheno1", "Pheno2"), 10, replace = TRUE),
         Founder = "NOFOUNDER",  #This is also a hard coded requirement, duh.
         Founder.Prob = 1,
         Founder.LOGP = 1) %>% #More archaic requirements
  rename(Chromosome = id, #Ugh, this is another hardcoded requirement
         dist..cM. = distance) #and yet another! this is because of wgaim output format

#Make a beadplot
beadPlot(qtl_sum, sim_df, "phenotype", "main", colourBarName = "Significance", qtlPointRange = c(2, 7))

mapStructure(qtl_sum, sim_df, "phenotype", type = "main")

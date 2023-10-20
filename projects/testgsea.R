dat.gct <- read.delim(file="./gene_tpm_liver.gct", skip=2)
dat.gct <- read.delim(file="./gene_reads_adipose_subcutaneous.gct", skip=2)
library(enrichplot)
library(pathview)
library(clusterProfiler)
# Let's just assume that we only want the first column
fold <- dat.gct$GTEX.11DXY.0526.SM.5EGGQ
genes <- dat.gct$Description


# Let's use a subset for now
#' Seed - 10473

set.seed(10473)

sub_rows <- sample(1:nrow(dat.gct), size = 1000, replace = FALSE)
sub_dd <- dat.gct[sub_rows,]  %>%
  # tibble::column_to_rownames(var = )
  mutate(sumVar = rowSums(select(., contains("GTEX"))))  %>%
  select(Name, sumVar)
  
names(sub_dd) <- c("gene", "expression")

ranked_dd <- sub_dd %>%
  arrange(desc(expression)) %>%
  mutate(phen = sample(0:1, nrow(.), replace=TRUE))  
  # mutate(across(expression, ~log(., 2)))


p <- 1

# How do we define a "hit" vs. a "not-hit"
hits <- ranked_dd[which(ranked_dd$expression > 10), ]
not_hits <- which(ranked_dd$expression <= 10)
hit_sum <- c()
# n_r = 
organism = "org.Hs.eg.db"
BiocManager::install(organism, character.only = TRUE)
library(organism, character.only = TRUE)
list <- ranked_dd$expression
names(list) <- ranked_dd$gene
gseGO(list, 
      ont = "ALL", 
      # keyType = 'ENSEMBL', 
      nPerm = 10000, 
      # minGSSize = 3, 
      # maxGSSize = 800, 
      # pvalueCutoff = 0.05, 
      verbose = FALSE, 
      OrgDb = organism, 
      pAdjustMethod = "none")
  

# reading in data from deseq2
df = read.csv("./projects/drosphila_example_de.csv", header=TRUE)

# we want the log2 fold change 
original_gene_list <- df$log2FoldChange

# name the vector
names(original_gene_list) <- df$X

# omit any NA values 
gene_list<-na.omit(original_gene_list)

# sort the list in decreasing order (required for clusterProfiler)
gene_list = sort(gene_list, decreasing = TRUE)

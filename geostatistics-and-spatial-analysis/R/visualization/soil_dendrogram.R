# Load data
all_pproxy <- read.csv("/Users/adelejoyeux/Downloads/CB24/GRANULO/al-shape-pies_pour_R3.csv")

# etiqueter les donnees et creer un data frame
feature_name <- c('GSM','LOI','CaCO3','ALD','NSP','LW','Distribution','sigma_phy_16','Texture')
colnames(all_pproxy) <- feature_name

# explore le jeu de donnes
str(all_pproxy)
summary(all_pproxy)
any(is.na(all_pproxy))

# scale the data
seeds_df_sc <- as.data.frame(scale(all_pproxy))
summary(seeds_df_sc)

# matrice de distance methode euclidienne
dist_mat <- dist(seeds_df_sc, method = 'euclidean')

# linkage method = average
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg, hang = -1, cex = 0.6)

# separer les deux clusters obtenus
cut_avg <- cutree(hclust_avg, k = 5)

# mettre en couleur avec cadres
#plot(hclust_avg, , hang = -1, cex = 0.6)
#rect.hclust(hclust_avg , k = 5, border = 2:6)

# autre plot
plot(as.phylo(hclust_avg), type = "unrooted", cex = 0.6,
     no.margin = TRUE)





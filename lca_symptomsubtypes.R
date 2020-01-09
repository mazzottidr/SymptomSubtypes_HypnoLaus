#### Running LCA
#### Diego Mazzotti
#### January 2020

library(dplyr)
library(poLCA)
library(ggplot2)
library(reshape2)
library(tidyr)
library(gplots)

# Load processed data
data <- read.csv("data/data_ready.csv", stringsAsFactors = T)

data <- data %>%
        sapply(as.numeric) %>%
        as.data.frame()


# Run LCA
LCAresults <- list()
for (k in 1:10) {
        set.seed(1234)
        LCAresults[[paste0("LCA_", k)]] <- poLCA(as.matrix(dplyr::select(data, -id)) ~ 1, maxiter=10000, nclass = k, nrep=75, data=dplyr::select(data, -id), na.rm=F, graphs = F)
}

saveRDS(LCAresults, "LCAresults_HypnoLaus.Rdata")
LCAresults <- readRDS("LCAresults_HypnoLaus.Rdata")

# Get BIC values
bic_results <- sapply(LCAresults, "[[", "bic")

# And plot them
pdf("Scree_plot_of_BIC_HypnoLaus.pdf")
p <- ggplot(data=data.frame(BIC=bic_results), aes(x=1:10, y=BIC, group=1)) +
        geom_line() +
        geom_point() + 
        scale_x_continuous(breaks = 1:10) + 
        theme_bw() +
        xlab("Cluster solution (K)")
p
dev.off()



# Get cluster membership for all cluster definitions from 1 to 10:
LCA_membership_df <- as.data.frame(sapply(LCAresults, "[[", "predclass"))

# Add nsrrid column
LCA_membership_df <- data.frame(id=data$id, LCA_membership_df, stringsAsFactors = F)

# Merge symptoms_clean with LCA_membership_df by id:
symptoms_with_LCAresults_df <- merge(data, LCA_membership_df, by = "id")

# Save
write.csv(symptoms_with_LCAresults_df, "symptoms_with_LCAresults_df_HypnoLaus.csv", row.names = F)

# Create the heatmaps

# Set names of clusters to iterate
LC_names <- names(LCAresults)[-1] # not meaningful for 1 cluster

# Set names of symptoms
symps <- names(data)[2:16]

# Create heatmaps
i=1
for (LC_n in LC_names) {
        
        melt_df <- melt(symptoms_with_LCAresults_df, measure.vars = symps)
        
        c_idx <- i+4
        
        res <- melt_df %>%
                group_by_(LC_n, "variable", "value") %>%
                summarise (n = n()) %>%
                filter(!is.na(value)) %>%
                mutate(freq = n / sum(n, na.rm = T)) %>%
                filter(value==2 | value==3 | value==4) %>%
                #filter(!is.na(freq)) %>%
                #drop_na() %>%
                dplyr::select(LC_n, variable, value, freq) %>%
                spread(LC_n, freq) %>%
                mutate(variable_name=paste0(variable, "_", value)) %>%
                ungroup() %>%
                dplyr::select(variable_name, everything()) %>%
                dplyr::select(c(1,4:c_idx)) %>%
                replace(., is.na(.), 0)
        
        syms <- as.data.frame(res[,-1])
        
        rownames(syms) <- res$variable_name
        colnames(syms) <- paste0("Cluster_", colnames(syms))
        syms.break<-c(0,seq(0.05,1.0,0.05))
        
        cellnote<-round(100*syms,2)
        
        cellnote[1:nrow(cellnote),]<-round(100*syms[1:nrow(syms),],1)
        
        cellnote[1:nrow(cellnote),] <- data.frame(sapply(cellnote[1:nrow(cellnote),], format, digits=3, justify="none"), stringsAsFactors = F)
        cellnote[1:nrow(cellnote),] <- sapply(cellnote[1:nrow(cellnote),], paste0, "%")
        
        
        
        png(paste0(LC_n, "_clusters.png"), height=3600, width=3200)
        heatmap.2(as.matrix(syms), Rowv=F, scale="row", Colv=F, dendrogram="none", keysize=2.25, cellnote=cellnote, notecol='white',
                  density.info="none", trace="none", cexRow=6.75, cexCol=6.9, notecex=6.75, colsep=(c(1,2,3)), sepwidth=c(0.05,0.05),
                  col=c(colorRampPalette(c("grey",  "red3"))(100)), margins=c(24,90), srtCol=0, adjCol=c(0.5,1.0),
                  key=T, key.title=NA, key.par=list(cex=3.9), lmat = rbind(c(0,3),c(2,1),c(0,4)), lwid = c(1.5,5), lhei = c(0.25,5.25,0.7), main = )
        
        
        dev.off()
        
        i=i+1
        
        
}

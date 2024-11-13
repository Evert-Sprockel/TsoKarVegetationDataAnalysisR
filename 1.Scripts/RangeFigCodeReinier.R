#### RANGES FIGURE

# Input data: matrix with sites as rows, species as columns
# Productivity is added as the first column
IDmat_B <- IDmat_B %>% mutate(Prod = Data_prod$MJ[match(rownames(IDmat_B), Data_prod$Site)], .before=1)

# Derive maximum, minimum, mean elevation & abundance of each species
# Mean elevation = abundance-weighted
Prhigh <- vector()
for(i in 2:ncol(IDmat_B)){
  Prhigh[i] <- max(as.numeric(IDmat_B[IDmat_B[,i] > 0, "Prod"]))}
Prlow <- vector()
for(i in 2:ncol(IDmat_B)){
  Prlow[i] <- min(as.numeric(IDmat_B[IDmat_B[,i] > 0, "Prod"]))}
Prmean <- vector()
for(i in 2:ncol(IDmat_B)){
  Prmean[i] <- sum((IDmat_B[,i] * as.numeric(IDmat_B[IDmat_B[,i] >= 0, "Prod"])) / sum(IDmat_B[,i]))}
Prq75 <- vector()
for(i in 2:ncol(IDmat_B)){
  Prq75[i] <- quantile(as.numeric(IDmat_B[IDmat_B[,i] > 0, "Prod"]), 0.75)}

Rangestats <- rbind(IDmat_B, setNames(Prlow, names(IDmat_B)), 
                    setNames(Prmean, names(IDmat_B)), 
                    setNames(Prhigh, names(IDmat_B)),
                    setNames(Prq75, names(IDmat_B))) %>%
# Slice is to drop the sites (n=41) rows
  subset(select = -Prod) %>% slice(42:45) 
# transpose & name columns
Rangestats <- as.data.frame(t(Rangestats)) %>%
  rename(Prlow = '42', Prmean = '43', Prhigh = '44', Prq75 = '45') %>%
  rownames_to_column(var = "Specs")
# Exclude unidentified species (genus spec.) columns
Rangestats <- Rangestats[(Rangestats$Specs %in% ID_species$Specs),] 
# Add group & abundance columns, then incidental occurrences
Rangestats$Group = ID_species$Group[match(Rangestats$Specs, ID_species$Specs)]
Rangestats$Count = ID_species$Count[match(Rangestats$Specs, ID_species$Specs)]
Rangestats$Inc = cut(Rangestats$Count, breaks = c(0,1,Inf), labels = c("1", "2"))

# To plot all records as dots
Recs <- ID_obs[(ID_obs$Specs %in% ID_species$Specs),] %>%
  left_join(Data_prod[,c('Site', 'MJ')], by = "Site")
#  left_join(Rangestats[,c('Specs', 'Prmean')], by = "Specs") %>%
Recs$Inc = cut(Recs$Count, breaks = c(0,1,Inf), labels = c("3", "4"))
Recs <- Recs %>% subset(select = c(Group, Specs, Inc, MJ))

# Per group; arrange central points by increasing mean productivity
Range_Dip <- Rangestats[order(Rangestats$Prmean),] %>% filter(Group == 'Diptera') %>% mutate(Seq = seq(nrow(Range_Dip), 1))
Range_Hem <- Rangestats[order(Rangestats$Prmean),] %>% filter(Group == 'Hemiptera') %>% mutate(Seq = seq(nrow(Range_Hem), 1))
Range_Col <- Rangestats[order(Rangestats$Prmean),] %>% filter(Group == 'Coleoptera') %>% mutate(Seq = seq(nrow(Range_Col), 1))
Range_Ara <- Rangestats[order(Rangestats$Prmean),] %>% filter(Group == 'Araneae') %>% mutate(Seq = seq(nrow(Range_Ara), 1))
Range_Ant <- Rangestats[order(Rangestats$Prmean),] %>% filter(Group == 'Anthophila') %>% mutate(Seq = seq(nrow(Range_Ant), 1))
Range_For <- Rangestats[order(Rangestats$Prmean),] %>% filter(Group == 'Formicidae') %>% mutate(Seq = seq(nrow(Range_For), 1))
Range_Ort <- Rangestats[order(Rangestats$Prmean),] %>% filter(Group == 'Orthoptera') %>% mutate(Seq = seq(nrow(Range_Ort), 1))

# Per group; arrange by increasing mean productivity, to plot range as points
Range_Dip <- Range_Dip %>% subset(select = c(Group, Specs, Inc, Prmean, Seq)) %>% rename(MJ=Prmean) %>% mutate(Group=1)
Recs_Dip <- Recs %>% filter(Group == 'Diptera') %>%   left_join(Range_Dip[,c('Specs', 'Seq')], by = "Specs")  %>% mutate(Group=2)
Recs_Dip <- rbind(Range_Dip, Recs_Dip) %>% arrange(Group, Seq)
Range_Hem <- Range_Hem %>% subset(select = c(Group, Specs, Inc, Prmean, Seq)) %>% rename(MJ=Prmean) %>% mutate(Group=1)
Recs_Hem <- Recs %>% filter(Group == 'Hemiptera') %>%   left_join(Range_Hem[,c('Specs', 'Seq')], by = "Specs")  %>% mutate(Group=2)
Recs_Hem <- rbind(Range_Hem, Recs_Hem) %>% arrange(Group, Seq)
Range_Col <- Range_Col %>% subset(select = c(Group, Specs, Inc, Prmean, Seq)) %>% rename(MJ=Prmean) %>% mutate(Group=1)
Recs_Col <- Recs %>% filter(Group == 'Coleoptera') %>%   left_join(Range_Col[,c('Specs', 'Seq')], by = "Specs")  %>% mutate(Group=2)
Recs_Col <- rbind(Range_Col, Recs_Col) %>% arrange(Group, Seq)
Range_Ara <- Range_Ara %>% subset(select = c(Group, Specs, Inc, Prmean, Seq)) %>% rename(MJ=Prmean) %>% mutate(Group=1)
Recs_Ara <- Recs %>% filter(Group == 'Araneae') %>%   left_join(Range_Ara[,c('Specs', 'Seq')], by = "Specs")  %>% mutate(Group=2)
Recs_Ara <- rbind(Range_Ara, Recs_Ara) %>% arrange(Group, Seq)
Range_Ant <- Range_Ant %>% subset(select = c(Group, Specs, Inc, Prmean, Seq)) %>% rename(MJ=Prmean) %>% mutate(Group=1)
Recs_Ant <- Recs %>% filter(Group == 'Anthophila') %>%   left_join(Range_Ant[,c('Specs', 'Seq')], by = "Specs")  %>% mutate(Group=2)
Recs_Ant <- rbind(Range_Ant, Recs_Ant) %>% arrange(Group, Seq)
Range_For <- Range_For %>% subset(select = c(Group, Specs, Inc, Prmean, Seq)) %>% rename(MJ=Prmean) %>% mutate(Group=1)
Recs_For <- Recs %>% filter(Group == 'Formicidae') %>%   left_join(Range_For[,c('Specs', 'Seq')], by = "Specs")  %>% mutate(Group=2)
Recs_For <- rbind(Range_For, Recs_For) %>% arrange(Group, Seq)
Range_Ort <- Range_Ort %>% subset(select = c(Group, Specs, Inc, Prmean, Seq)) %>% rename(MJ=Prmean) %>% mutate(Group=1)
Recs_Ort <- Recs %>% filter(Group == 'Orthoptera') %>%   left_join(Range_Ort[,c('Specs', 'Seq')], by = "Specs")  %>% mutate(Group=2)
Recs_Ort <- rbind(Range_Ort, Recs_Ort) %>% arrange(Group, Seq)

# Plots per species group: range shown as points in grey
precs_Dip <- ggplot() +
  geom_point(data=Recs_Dip, aes(x=MJ, y=Seq, shape=Inc, color=Inc, size=Inc, alpha=Inc), show.legend = FALSE) +
  scale_shape_manual(values = c('1'=3,'2'=19, '3'=16, '4'=16)) +
  scale_colour_manual(values = c('1'="#252525",'2'="#1B9E77",'3'="#C0C0C0",'4'="#7F7F7F")) +
  scale_size_manual(values = c('1'=0.7, '2'=1, '3'=1, '4'=1)) +
  scale_alpha_manual(values = c('1'=1, '2'=1, '3'=0.5, '4'=0.5)) +
  scale_x_continuous(limits = c(0, 95), breaks = c(30, 60, 90), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 127), breaks = c(0,50,100), expand = c(0, 0)) +
  labs(y='Diptera', x='Productivity [MJ]')+ theme_ipsum_pub(base_size = 8.5, axis_title_size = 9) +
  theme(plot.title = element_text(size=11, face='plain', hjust=0.5), axis.line = element_line(linewidth=0.4, linetype="solid", colour ="black"), 
        plot.margin = margin(t=5, r=5, b=5, l=5))
precs_Hem <- ggplot() +
  geom_point(data=Recs_Hem, aes(x=MJ, y=Seq, shape=Inc, color=Inc, size=Inc, alpha=Inc), show.legend = FALSE) +
  scale_shape_manual(values = c('1'=3,'2'=19, '3'=16, '4'=16)) +
  scale_colour_manual(values = c('1'="#252525",'2'="#1B9E77",'3'="#C0C0C0",'4'="#7F7F7F")) +
  scale_size_manual(values = c('1'=0.7, '2'=1, '3'=1, '4'=1)) +
  scale_alpha_manual(values = c('1'=1, '2'=1, '3'=0.5, '4'=0.5)) +
  scale_x_continuous(limits = c(0, 95), breaks = c(30, 60, 90), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 110), breaks = c(0,50,100), expand = c(0, 0)) +
  labs(y='Heteroptera', x='Productivity [MJ]')+ theme_ipsum_pub(base_size = 8.5, axis_title_size = 9) +
  theme(plot.title = element_text(size=11, face='plain', hjust=0.5), axis.line = element_line(linewidth=0.4, linetype="solid", colour ="black"), 
        plot.margin = margin(t=5, r=5, b=5, l=5))
precs_Col <- ggplot() +
  geom_point(data=Recs_Col, aes(x=MJ, y=Seq, shape=Inc, color=Inc, size=Inc, alpha=Inc), show.legend = FALSE) +
  scale_shape_manual(values = c('1'=3,'2'=19, '3'=16, '4'=16)) +
  scale_colour_manual(values = c('1'="#252525",'2'="#1B9E77",'3'="#C0C0C0",'4'="#7F7F7F")) +
  scale_size_manual(values = c('1'=0.7, '2'=1, '3'=1, '4'=1)) +
  scale_alpha_manual(values = c('1'=1, '2'=1, '3'=0.5, '4'=0.5)) +
  scale_x_continuous(limits = c(0, 95), breaks = c(30, 60, 90), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 86), breaks = c(0,35,70), expand = c(0, 0)) +
  labs(y='Coleoptera', x='Productivity [MJ]')+ theme_ipsum_pub(base_size = 8.5, axis_title_size = 9) +
  theme(plot.title = element_text(size=11, face='plain', hjust=0.5), axis.line = element_line(linewidth=0.4, linetype="solid", colour ="black"), 
        plot.margin = margin(t=5, r=5, b=5, l=5))
precs_Ara <- ggplot() +
  geom_point(data=Recs_Ara, aes(x=MJ, y=Seq, shape=Inc, color=Inc, size=Inc, alpha=Inc), show.legend = FALSE) +
  scale_shape_manual(values = c('1'=3,'2'=19, '3'=16, '4'=16)) +
  scale_colour_manual(values = c('1'="#252525",'2'="#1B9E77",'3'="#C0C0C0",'4'="#7F7F7F")) +
  scale_size_manual(values = c('1'=0.7, '2'=1, '3'=1, '4'=1)) +
  scale_alpha_manual(values = c('1'=1, '2'=1, '3'=0.5, '4'=0.5)) +
  scale_x_continuous(limits = c(0, 95), breaks = c(30, 60, 90), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 80), breaks = c(0,35,70), expand = c(0, 0)) +
  labs(y='Araneae', x='Productivity [MJ]')+ theme_ipsum_pub(base_size = 8.5, axis_title_size = 9) +
  theme(plot.title = element_text(size=11, face='plain', hjust=0.5), axis.line = element_line(linewidth=0.4, linetype="solid", colour ="black"), 
        plot.margin = margin(t=5, r=5, b=5, l=5))
precs_Ant <- ggplot() +
  geom_point(data=Recs_Ant, aes(x=MJ, y=Seq, shape=Inc, color=Inc, size=Inc, alpha=Inc), show.legend = FALSE) +
  scale_shape_manual(values = c('1'=3,'2'=19, '3'=16, '4'=16)) +
  scale_colour_manual(values = c('1'="#252525",'2'="#1B9E77",'3'="#C0C0C0",'4'="#7F7F7F")) +
  scale_size_manual(values = c('1'=0.7, '2'=1, '3'=1, '4'=1)) +
  scale_alpha_manual(values = c('1'=1, '2'=1, '3'=0.5, '4'=0.5)) +
  scale_x_continuous(limits = c(0, 95), breaks = c(30, 60, 90), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 89), breaks = c(0,35,70), expand = c(0, 0)) +
  labs(y='Anthophila', x='Productivity [MJ]')+ theme_ipsum_pub(base_size = 8.5, axis_title_size = 9) +
  theme(plot.title = element_text(size=11, face='plain', hjust=0.5), axis.line = element_line(linewidth=0.4, linetype="solid", colour ="black"), 
        plot.margin = margin(t=5, r=5, b=5, l=5))
precs_For <- ggplot() +
  geom_point(data=Recs_For, aes(x=MJ, y=Seq, shape=Inc, color=Inc, size=Inc, alpha=Inc), show.legend = FALSE) +
  scale_shape_manual(values = c('1'=3,'2'=19, '3'=16, '4'=16)) +
  scale_colour_manual(values = c('1'="#252525",'2'="#1B9E77",'3'="#C0C0C0",'4'="#7F7F7F")) +
  scale_size_manual(values = c('1'=0.7, '2'=1, '3'=1, '4'=1)) +
  scale_alpha_manual(values = c('1'=1, '2'=1, '3'=0.5, '4'=0.5)) +
  scale_x_continuous(limits = c(0, 95), breaks = c(30, 60, 90), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 17), breaks = c(0,10), expand = c(0, 0)) +
  labs(y='Formicidae', x='Productivity [MJ]')+ theme_ipsum_pub(base_size = 8.5, axis_title_size = 9) +
  theme(plot.title = element_text(size=11, face='plain', hjust=0.5), axis.line = element_line(linewidth=0.4, linetype="solid", colour ="black"), 
        plot.margin = margin(t=5, r=5, b=5, l=5))
precs_Ort <- ggplot() +
  geom_point(data=Recs_Ort, aes(x=MJ, y=Seq, shape=Inc, color=Inc, size=Inc, alpha=Inc), show.legend = FALSE) +
  scale_shape_manual(values = c('1'=3,'2'=19, '3'=16, '4'=16)) +
  scale_colour_manual(values = c('1'="#252525",'2'="#1B9E77",'3'="#C0C0C0",'4'="#7F7F7F")) +
  scale_size_manual(values = c('1'=0.7, '2'=1, '3'=1, '4'=1)) +
  scale_alpha_manual(values = c('1'=1, '2'=1, '3'=0.5, '4'=0.5)) +
  scale_x_continuous(limits = c(0, 95), breaks = c(30, 60, 90), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 19), breaks = c(0,10), expand = c(0, 0)) +
  labs(y='Orthoptera', x='Productivity [MJ]')+ theme_ipsum_pub(base_size = 8.5, axis_title_size = 9) +
  theme(plot.title = element_text(size=11, face='plain', hjust=0.5), axis.line = element_line(linewidth=0.4, linetype="solid", colour ="black"), 
        plot.margin = margin(t=5, r=5, b=5, l=5))

# Combined plot
tiff("rangefig3.tiff", width = 8, height = 8, units = 'in', res = 1200)
grid.arrange(
  precs_Dip, precs_Hem,
  precs_Col, precs_Ara,
  precs_Ant, precs_For, precs_Ort,
  nrow = 3, ncol = 3,
  layout_matrix = cbind(c(1,1,2,2), c(3,3,4,4), c(5,5,6,7))
)
dev.off()
#===============================================================================
# Descriptive Data Analysis
# Arnold & Schulz: "Reaching For The Threshold"
# Chris Arnold, University of Birmingham, October 2025
#===============================================================================

# -- 1 What type of MPRs do exist? ---------------------------------------------

# 1.1 Treaties by Chapter ------------------------------------------------------
# recoding for display purpose
thresh.temp <- rep(1, nrow(dat.treaty))
thresh.temp[dat.treaty$entry_force_threshold_binary == 0] <- 2
# Proportional Counts
tab.chapter.thresh.prop <- prop.table(table(thresh.temp, dat.treaty$chapter_title), 2)
reorder.tables <- order(tab.chapter.thresh.prop[1,])
# reorder for display purpose 
tab.chapter.thresh.prop <- tab.chapter.thresh.prop[,reorder.tables]
# Absolute Counts
tab.chapter.thresh.count <- table(thresh.temp, dat.treaty$chapter_title)
tab.chapter.thresh.count <- tab.chapter.thresh.count[, reorder.tables]
# Renaming some categories to save space
chapter.areas <- attributes(tab.chapter.thresh.prop)$dimnames[[2]]
chapter.areas <- car::recode(chapter.areas, "'Charter of the United Nations and Statute of the International Court of Justice' = 
  'Charter of the UN and Statute of the ICJ';
   'Privileges and Immunities, Diplomatic and Consular Relations, etc'='Diplomatic and Consular Relations'")

# Plotting 
# Figure 2 main manuscript
pdf("descr_MPR_p_chap.pdf", height = 9, width = 14)
# Left panel with percentages 
par(mar=c(5, 0.5, 2, 0), mfrow = c(1,2), fig=c(0,0.27,0,1), lend=1)
plot(c(1,1), type = "n", bty = 'n', yaxt = "n", xaxt = 'n',
     main = 'Proportion of Treaties \n with MPR',
     xlim = c(0, 1), ylim = c(.6, length(chapter.areas)+.4), 
     xlab = "Percent", ylab = "")
axis(1, at = seq(0, 1, .2), labels = seq(0, 100, 20))
# plot the cases with threshold
for (i in 1:length(tab.chapter.thresh.prop[1,])){
  lines(c(0,tab.chapter.thresh.prop[1,][i]),
        c(i,i),
        lwd = 16, col =  cardiffblue)
}
# plot the cases without threshold
for (i in 1:length(tab.chapter.thresh.prop[2,])){
  if(tab.chapter.thresh.prop[2,][i] != 0){
    lines(c(tab.chapter.thresh.prop[1,][i], 1),
          c(i,i),
          lwd = 16, col =  cardiffgold)  
  } 
}

# Core panel with labels 
par(mar=c(5, 0, 2, 0), fig=c(0.27,.53,0,1), new = TRUE)
plot(c(1,1), type = "n", bty = 'n', yaxt = "n", xaxt = "n",
     xlim = c(-5, 5), ylim = c(.6, length(chapter.areas)+.4), xlab = "", ylab = "")
text(0, seq(1, length(chapter.areas)), labels = chapter.areas, cex =1, 
     adj=c(.5, .5))
# Right panel 
par(mar=c(5, 0, 2, 1), fig=c(0.53,1,0,1), new = TRUE, lend = 1)
plot(c(1,1), type = "n", bty = 'n', yaxt = "n",
     main = 'Absolute Nr of Treaties with MPR',
     xlim = c(0, 160), ylim = c(.6, length(chapter.areas)+.4), 
     xlab = "Count", ylab = "")
# plot the cases with threshold
for (i in 1:length(tab.chapter.thresh.count[1,])){
  lines(c(0,tab.chapter.thresh.count[1,][i]),
        c(i-0.17,i-0.17),
        lwd = 8, col =  cardiffblue)
}
# plot the cases without threshold
for (i in 1:length(tab.chapter.thresh.count[2,])){
  if(tab.chapter.thresh.count[2,][i] != 0){
    lines(c(0,tab.chapter.thresh.count[2,][i]),
          c(i+0.17,i+0.17),
          lwd = 8, col =  cardiffgold)  
  }
}
legend(120,27, col = c(cardiffblue, cardiffgold), pch =15, 
       legend = c("MPR", "No MPR"), box.col = 'white', cex = 1.2)
dev.off()


# 1.2 How many treaties have which kind of MPR (venn diagram) ------------------
# Overview over the different threshold combinations
c3 = cbind(as.integer(dat.treaty$threshold_2_dummy),
           as.integer(dat.treaty$threshold_1_dummy), 
           dat.treaty$threshold_3_qualified)
a <- vennCounts(c3)
# Plotting
# Figure 1 main manuscript
pdf('descr_Threshold_Combinations.pdf', width = 10, height = 10)
vennDiagram(a, 
            names = c('EIF Time', 'Numerical\nThreshold', 
                      'Qualified\nThreshold'))
dev.off()


# quick description of the distribution of numerical thresholds
median(dat.treaty$threshold_1_number[dat.treaty$threshold_1_number !=0])

# Plotting 
# Figure 5 Appendix
pdf('appendix_descr_threshold_Combinations_fr.pdf', width = 10, height = 5)
c3.fr = data.frame(as.integer(dat.treaty$threshold_2_dummy[dat.treaty$public_or_common_good == 1]),
                as.integer(dat.treaty$threshold_1_dummy[dat.treaty$public_or_common_good == 1]),
                dat.treaty$threshold_3_qualified[dat.treaty$public_or_common_good == 1])
a <- vennCounts(c3.fr)
intersect.counts <- a[,4][-1]
names(intersect.counts) <- c(
  'Qualified Threshold', 'Numerical Threshold', 'Qualified & Numerical Threshold', 
  'EIF Time', 'Qualified Threshold & EIF Time', 'Numerical Threshold & EIF Time',
  'Qualified Threshold & Numerical Threshold & EIF Time'
)
intersect.counts <- rev(intersect.counts[c(1,2,4,3,5,6,7)])
intersect.share <- intersect.counts/sum(intersect.counts)
names(intersect.share) <- names(intersect.counts)
par(mar= c(5,22,1,1) ,lend = 1)
plot(c(1,1), type = "n", bty = 'n', yaxt = "n", xaxt = 'n',
     xlim = c(0, 1), ylim = c(.6, length(intersect.share)+.4), 
     xlab = "Percent", ylab = "")
axis(1, at = seq(0, 1, .2))
axis(2, at = seq(1, length(intersect.share)), lab = names(intersect.share), 
     las =1)
# plot the cases with threshold
for (i in 1:length(intersect.share)){
  lines(c(0,intersect.share[i]),
        c(i+0.2,i+0.2),
        lwd = 16, col =  cardiffblue)
}
c3.nfr = data.frame(as.integer(dat.treaty$threshold_2_dummy[dat.treaty$public_or_common_good == 0]),
                   as.integer(dat.treaty$threshold_1_dummy[dat.treaty$public_or_common_good == 0]),
                   dat.treaty$threshold_3_qualified[dat.treaty$public_or_common_good == 0])
a <- vennCounts(c3.nfr)
intersect.counts <- a[,4][-1]
names(intersect.counts) <- c(
  'Qualified Threshold', 'Numerical Threshold', 'Qualified & Numerical Threshold', 
  'EIF Time', 'Qualified Threshold & EIF Time', 'Numerical Threshold & EIF Time',
  'Qualified Threshold & Numerical Threshold & EIF Time'
)
intersect.counts <- rev(intersect.counts[c(1,2,4,3,5,6,7)])
intersect.share <- intersect.counts/sum(intersect.counts)
names(intersect.share) <- names(intersect.counts)
# plot the cases with threshold
for (i in 1:length(intersect.share)){
  lines(c(0,intersect.share[i]),
        c(i-0.2,i-0.2),
        lwd = 16, col =  cardiffred)
}
legend('topright', legend = c('Free Riding', 'No Free Riding'), 
       col = c(cardiffblue, cardiffred), pch = 15, box.col = 'white')
dev.off()


# -- 1.3 Histogram over Level of Thresholds ------------------------------------

# Visualisation 1: How often do we see each MPR? Simple Histogram
threshold.1.table <- table(dat.treaty$threshold_1_number[dat.treaty$anythreshold])
threshold.1.table.categories <- as.integer(attributes(threshold.1.table)$dimnames[[1]])
threshold.1.table.frequencies <- as.integer(threshold.1.table)

# plotting
# Figure 4b Manuscripy
pdf("descr_histogram_thresholds.pdf", height = 5, width = 6)
par(lend=1)
plot(c(1,1), type = "n", bty = 'n', yaxt = "n", xaxt = "n",
     xlim = c(0, 150), ylim = c(0, 60), xlab = "", ylab = "")

pos.axis.thresholds <- 0
for (i in 1:length(threshold.1.table.categories)){
  lines(c(threshold.1.table.categories[i],threshold.1.table.categories[i]),
        c(pos.axis.thresholds,pos.axis.thresholds + threshold.1.table.frequencies[i]),
        lwd = 2.5, col =  cardiffblue)
}

axis(1, las = 1)
axis(1, las = 1, labels = FALSE, at = seq(10, 140, 10))
axis(2, las = 1)
mtext("Frequency", side = 2, line = 3)
mtext("Threshold", side = 1, line = 3)
dev.off()



# Visualisation 2: connecting nr. of parties to thresholds. 
nr.parties.treaty.w.threshold <- table(dat.treaty$nr.signatory.states[dat.treaty$threshold_1_number != 0])
threshold.1.table <- table(dat.treaty$threshold_1_number)[-1]

alphagrey = rgb(col2rgb("grey70")[1], col2rgb("grey70")[2],
                col2rgb("grey70")[3], alpha = 80, maxColorValue = 255)

nr.parties.treaty.w.threshold.categories <- as.integer(attributes(nr.parties.treaty.w.threshold)$dimnames[[1]])
nr.parties.treaty.w.threshold.frequencies <- as.integer(nr.parties.treaty.w.threshold)
threshold.1.table.categories <- as.integer(attributes(threshold.1.table)$dimnames[[1]])
threshold.1.table.frequencies <- as.integer(threshold.1.table)
dat.treaty.hist <- subset(dat.treaty, dat.treaty$threshold_1_number != 0)
pdf("appendix_histogram_thresholds_and_parties.pdf", height = 9, width = 14)
# Setting up canvas
par(mar = c(5.1, 4.1, 2, 5.1))
plot(c(1,1), type = "n", bty = 'n', yaxt = "n", xaxt = "n",
     xlim = c(-20, 70), ylim = c(0, 200), xlab = "", ylab = "")
axis(1, at = c(-20, -15, -10),
     labels = c(10, 5, 0), xlim = c(-20, 0))
axis(1, at = c(10, 30, 50, 70),
     labels = c(0, 20, 40, 60), xlim = c(0, 70))
mtext("Frequency", side = 1, line = 3,  at = 40)
mtext("Frequency", side = 1, line = 3,  at = -15)
axis(2, las = 1)
mtext("Nr of Parties to a Treaty", side = 2, line = 3)
axis(4, las = 1)
mtext("Threshold", side = 4, line = 3)

# hist parties
pos.axis.parties <- -10
for (i in 1:length(nr.parties.treaty.w.threshold.categories)){
  lines(c(pos.axis.parties,pos.axis.parties - nr.parties.treaty.w.threshold.frequencies[i]),
        c(nr.parties.treaty.w.threshold.categories[i],nr.parties.treaty.w.threshold.categories[i]),
        lwd = 2, col =  cardiffred, lend = 1)
}

# hist thresholds
pos.axis.thresholds <- 10
for (i in 1:length(threshold.1.table.categories)){
  lines(c(pos.axis.thresholds,pos.axis.thresholds + threshold.1.table.frequencies[i]),
        c(threshold.1.table.categories[i],threshold.1.table.categories[i]),
        lwd = 2, col =  cardiffblue, lend = 1)
}

# connecting lines
# back to the treaty level data -- but the reduced version
for (i in 1:length(dat.treaty.hist$countries.p.treaty)){
  lines(c(pos.axis.parties, pos.axis.thresholds),
        c(dat.treaty.hist$countries.p.treaty[i], dat.treaty.hist$threshold_1_number[i]),
        col = alphagrey)
}
dev.off()


# Frequency of different time lags
# filter those where there is no threshold
threshold_2_time_any <- dat.treaty$threshold_2_time[dat.treaty$anythreshold]
threshold.2.table <- table(threshold_2_time_any)
threshold.2.table.categories <- as.integer(attributes(threshold.2.table)$dimnames[[1]])
threshold.2.table.frequencies <- as.integer(threshold.2.table)

# Plotting
# Figure 4a main manuscript
pdf("descr_histogram_time_thresholds.pdf", height = 5, width = 6)
plot(c(1,1), type = "n", bty = 'n', yaxt = "n", xaxt = "n",
     xlim = c(0, 800), ylim = c(0, 125), xlab = "", ylab = "")

for (i in 1:length(threshold.2.table.categories)){
  lines(c(threshold.2.table.categories[i], threshold.2.table.categories[i]),
        c(0,threshold.2.table.frequencies[i]),
        lwd = 2.5, col = cardiffblue, lend = 2)
}
axis(1, las = 1, at = c(0,30, 90, 180, 365, 730))
mtext("Days", side = 1, line = 3)
axis(2, las = 1)
mtext("Frequency", side = 2, line = 3)
dev.off()


# -- 1.4 How did MPRs develop over time? ---------------------------------------
tab.thresh.x.year <- table(dat.treaty$treaty_year, dat.treaty$entry_force_threshold_binary)
years <- as.integer(attributes(tab.thresh.x.year)$dimnames[[1]])
nothresh <- tab.thresh.x.year[,1]
thresh <- tab.thresh.x.year[,2]
deltathresh <- thresh - nothresh

## Plotting
# Figure 2 Appendix
pdf("descr_MPR_x_year.pdf", height = 5, width = 7)
par(mfrow = c(1,1), fig=c(0,1,0,1), mar=c(5, 5, 2, 2))
plot(c(1,1), type = "n", bty = 'n', las = 1,
     xlim = c(min(years)-2, max(years)+2), ylim = c(0, max(tab.thresh.x.year)), 
     xlab = "Year", ylab = "Count")
for (i in 1:length(years)){
  if(nothresh[i]!=0){
    lines(c(years[i]-0.1, years[i]-0.1),
          c(0, nothresh[i]),
          col = cardiffred.t, lwd = 2, lend = 1) 
  }
}
for (i in 1:length(years)){
  if(thresh[i]!=0){
    lines(c(years[i]+0.1, years[i]+0.1),
          c(0, thresh[i]), 
          col = cardiffblue.t, lwd = 2, lend = 1) 
  }
}
lines(lowess(years, thresh), col = cardiffblue, lwd = 2)
lines(lowess(years, nothresh), col = cardiffred, lwd = 2)
legend(2006, 10, col = c(cardiffblue, cardiffred), pch =15, 
       legend = c("MPR", "No MPR"), box.col = 'white')
dev.off()


# -- 1.5 Where are the boilerplates? -------------------------------------------

dat.treaty.at <- subset(dat.treaty, dat.treaty$anythreshold == 1)
dat.treaty.num <- subset(dat.treaty, dat.treaty$threshold_1_dummy == TRUE)

# recoding for display purpose
boilerplate <- rep(1, nrow(dat.treaty.num))
boilerplate[dat.treaty.num$threshold_1_boilerplate_dummy == FALSE] <- 2
# Proportional Counts
tab.chapter.boilerplate <- prop.table(table(boilerplate, dat.treaty.num$chapter_title), 2)
reorder.tables <- order(tab.chapter.boilerplate[1,])
# reorder for display purpose 
tab.chapter.boilerplate <- tab.chapter.boilerplate[,reorder.tables]
# Absolute Counts
tab.chapter.boilerplate.count <- table(boilerplate, dat.treaty.num$chapter_title)
tab.chapter.boilerplate.count <- tab.chapter.boilerplate.count[, reorder.tables]
# Renaming some categories to save space
chapter.areas <- attributes(tab.chapter.boilerplate)$dimnames[[2]]
chapter.areas <- car::recode(chapter.areas, "'Charter of the United Nations and Statute of the International Court of Justice' = 
  'Charter of the UN and Statute of the ICJ';
   'Privileges and Immunities, Diplomatic and Consular Relations, etc'='Diplomatic and Consular Relations'")



# Plotting 
# Figure 3 appendix
pdf("appendix_descr_boilerplate_p_chap.pdf", height = 9, width = 14)
# Left panel with percentages 
par(mar=c(5, 0.5, 2, 0), mfrow = c(1,2), fig=c(0,0.27,0,1), lend=1)
plot(c(1,1), type = "n", bty = 'n', yaxt = "n", xaxt = 'n',
     main = 'Proportion of Treaties \n with Boilerplate',
     xlim = c(0, 1), ylim = c(.6, length(chapter.areas)+.4), 
     xlab = "Percent", ylab = "")
axis(1, at = seq(0, 1, .2), labels = seq(0, 100, 20))
# plot the cases with threshold
for (i in 1:length(tab.chapter.boilerplate[1,])){
  lines(c(0,tab.chapter.boilerplate[1,][i]),
        c(i,i),
        lwd = 16, col =  cardiffblue)
}
# plot the cases without threshold
for (i in 1:length(tab.chapter.boilerplate[2,])){
  if(tab.chapter.boilerplate[2,][i] != 0){
    lines(c(tab.chapter.boilerplate[1,][i], 1),
          c(i,i),
          lwd = 16, col =  cardiffgold)  
  } 
}

# Core panel with labels 
par(mar=c(5, 0, 2, 0), fig=c(0.27,.53,0,1), new = TRUE)
plot(c(1,1), type = "n", bty = 'n', yaxt = "n", xaxt = "n",
     xlim = c(-5, 5), ylim = c(.6, length(chapter.areas)+.4), xlab = "", ylab = "")
text(0, seq(1, length(chapter.areas)), labels = chapter.areas, cex =1, 
     adj=c(.5, .5))

# Right panel 
par(mar=c(5, 0, 2, 1), fig=c(0.53,1,0,1), new = TRUE, lend = 1)
plot(c(1,1), type = "n", bty = 'n', yaxt = "n",
     main = 'Absolute Nr of Treaties with Boilerplates',
     xlim = c(0, 60), ylim = c(.6, length(chapter.areas)+.4), 
     xlab = "Count", ylab = "")
# plot the cases with threshold
for (i in 1:length(tab.chapter.boilerplate.count[1,])){
  lines(c(0,tab.chapter.boilerplate.count[1,][i]),
        c(i-0.17,i-0.17),
        lwd = 8, col =  cardiffblue)
}
# plot the cases without threshold
for (i in 1:length(tab.chapter.boilerplate.count[2,])){
  if(tab.chapter.boilerplate.count[2,][i] != 0){
    lines(c(0,tab.chapter.boilerplate.count[2,][i]),
          c(i+0.17,i+0.17),
          lwd = 8, col =  cardiffgold)  
  }
}
legend(43,28, col = c(cardiffblue, cardiffgold), pch =15, 
       legend = c("Boilerplates", "No Boilerplate"), box.col = 'white', cex = 1.2)
dev.off()




# Boilerplates in the different threshold combinations
c3.bp = data.frame(as.integer(dat.treaty.num$threshold_2_dummy)[dat.treaty$threshold_1_boilerplate_dummy == 1],
                   as.integer(dat.treaty.num$threshold_1_dummy)[dat.treaty$threshold_1_boilerplate_dummy == 1],
                   dat.treaty.num$threshold_3_qualified[dat.treaty$threshold_1_boilerplate_dummy == 1]
                   )
a <- vennCounts(c3.bp)
intersect.counts <- a[,4][-1]
names(intersect.counts) <- c(
  'Qualified Threshold', 'Numerical Threshold', 'Qualified & Numerical Threshold', 
  'EIF Time', 'Qualified Threshold & EIF Time', 'Numerical Threshold & EIF Time',
  'Qualified Threshold & Numerical Threshold & EIF Time'
)
intersect.counts.bp <- rev(intersect.counts[c(2,3,6,7)])


c3.nbp = data.frame(as.integer(dat.treaty.num$threshold_2_dummy)[dat.treaty$threshold_1_boilerplate_dummy == 0],
                   as.integer(dat.treaty.num$threshold_1_dummy)[dat.treaty$threshold_1_boilerplate_dummy == 0],
                   dat.treaty.num$threshold_3_qualified[dat.treaty$threshold_1_boilerplate_dummy == 0]
)
a <- vennCounts(c3.nbp)
intersect.counts <- a[,4][-1]
names(intersect.counts) <- c(
  'Qualified Threshold', 'Numerical Threshold', 'Qualified & Numerical Threshold', 
  'EIF Time', 'Qualified Threshold & EIF Time', 'Numerical Threshold & EIF Time',
  'Qualified Threshold & Numerical Threshold & EIF Time'
)
intersect.counts.nbp <- rev(intersect.counts[c(2,3,6,7)])


# Plotting
# Figure 4 Appendix
pdf('appendix_descr_threshold_Combinations_boilerplate.pdf', width = 10, height = 5)
par(mar= c(5,22,1,1) ,lend = 1)
plot(c(1,1), type = "n", bty = 'n', yaxt = "n", xaxt = 'n',
     # main = 'Proportion of Treaties \n with Boilerplate MPR',
     xlim = c(0, 200), ylim = c(.6, length(intersect.counts.bp)+.4), 
     xlab = "Counts", ylab = "")
axis(1)
axis(2, at = seq(1, length(intersect.counts.bp)), lab = names(intersect.counts.bp), 
     las =1 )
# plot the cases with threshold
for (i in 1:length(intersect.counts.bp)){
  lines(c(0,intersect.counts.bp[i]),
        c(i+0.107,i+0.107),
        lwd = 15, col =  cardiffblue)
}
for (i in 1:length(intersect.counts.bp)){
  lines(c(0,intersect.counts.nbp[i]),
        c(i-0.107,i-0.107),
        lwd = 15, col =  cardiffred)
}
legend('topright', legend = c('Boilerplate', 'No Boilerplate'), 
       col = c(cardiffblue, cardiffred), pch = 15, box.col = 'white')
dev.off()



# -- 1.6 Threshold levels and actions by chapter -------------------------------
dat.treaty$chapter_title_trellis <- dat.treaty$chapter_title
dat.treaty$chapter_title_trellis <- car::recode(dat.treaty$chapter_title, "'Charter of the United Nations and Statute of the International Court of Justice' =
'UN Charter & ICJ Statute';
'Transport and Communications' = 'Transport & Communications';
'Pacific Settlement of International Disputes'='Pacific Settlement of Int. Disputes';
'Privileges and Immunities, Diplomatic and Consular Relations, etc' = 'Diplomatic & Consular Relations';
'Narcotic Drugs and Psychotropic Substances'='Narcotic Drugs ';
'Refugees and Stateless Persons'='Refugees & Stateless Persons';
'Educational and Cultural Matters'='Educational & Cultural Matters';
'International Trade and Development'='Trade & Development'")

dat.treaty$graphic.title <- dat.treaty$chapter_title_trellis

# Plotting
# Figure 3 main manuscript
# TODO check what is going wrong here
# pdf('signatures_consents_tbb_p_chapter.pdf', width = 10, height = 8)
# stripParams <- list(cex=0.6, lines = 1.5)
# customLayout <- c(4, 7)
# p.sign <- xyplot(nr.signatory.states ~ threshold_1_number| graphic.title,
#        data = dat.treaty,
#        par.strip.text = stripParams,
#        xlab = "Numerical MPR",
#        ylab = "Number of States",
#        col = cardiffblue,
#        xlim = c(-10,160), ylim = c(-10,210),
#        key=list(space="right",
#                 points=list(col=c(cardiffblue, cardiffred), pch = c(1,4)),
#                 text=list(c("Signatures"," Ratifications"))
#        ),
#        layout = customLayout
# )
# 
# p.rat <- xyplot(nr.accept.to.be.bound ~ threshold_1_number| graphic.title,
#        data = dat.treaty, type=c("g", "p"),
#        par.strip.text = stripParams,
#        xlab = "Numerical MPR",
#        ylab = "Number of States",
#        pch = 4,
#        col = cardiffred,
#        xlim = c(-10,160), ylim = c(-10,210),
#        key=list(space="right",
#                 points=list(col=c(cardiffblue, cardiffred), pch = c(1,4)),
#                 text=list(c("Signatures","Ratifications"))
#        ),
#        layout = customLayout
# )
# p.rat + as.layer(p.sign)
# dev.off()

pdf("signatures_consents_tbb_p_chapter.pdf", width = 10, height = 8)
tryCatch({
  stripParams <- list(cex = 0.6, lines = 1.5)
  customLayout <- c(4, 7)
  
  p.sign <- xyplot(nr.signatory.states ~ threshold_1_number | graphic.title,
                   data = dat.treaty,
                   par.strip.text = stripParams,
                   xlab = "Numerical MPR",
                   ylab = "Number of States",
                   col = cardiffblue,
                   xlim = c(-10,160), ylim = c(-10,210),
                   key = list(space = "right",
                              points = list(col = c(cardiffblue, cardiffred), pch = c(1,4)),
                              text   = list(c("Signatures"," Ratifications"))),
                   layout = customLayout
  )
  
  p.rat <- xyplot(nr.accept.to.be.bound ~ threshold_1_number | graphic.title,
                  data = dat.treaty, type = c("g","p"),
                  par.strip.text = stripParams,
                  xlab = "Numerical MPR",
                  ylab = "Number of States",
                  pch = 4, col = cardiffred,
                  xlim = c(-10,160), ylim = c(-10,210),
                  key = list(space = "right",
                             points = list(col = c(cardiffblue, cardiffred), pch = c(1,4)),
                             text   = list(c("Signatures","Ratifications"))),
                  layout = customLayout
  )
  
  print(p.rat + as.layer(p.sign))   # <- MUST PRINT the combined trellis object
}, finally = {
  dev.off()                         # <- ALWAYS close the device even if an error occurs
})







chapter.areas <- car::recode(chapter.areas, "'Charter of the United Nations and Statute of the International Court of Justice' = 
  'Charter of the UN and Statute of the ICJ';
   'Privileges and Immunities, Diplomatic and Consular Relations, etc'='Diplomatic and Consular Relations'")

nr.signatory.states <- rep(NA, length(treaty_code))
for (i in 1:length(treaty_code)){
  i <- 2
  dat.temp <- dat[dat$treaty_code == treaty_code[i],]
  # who signed?
  dat.temp$treaty_action_category
  table(dat.temp$treaty_action_category==5)[2]
  nr.signatory.states[i] <- (
    table(dat.temp$treaty_action_category==5)[2])
}



# -- 1.7 boilerplate vs. free riding -----------------------------------------------------------------
table.fr.bp <- table(dat.treaty$public_or_common_good[dat.treaty$threshold_1_dummy == TRUE],
dat.treaty$threshold_1_boilerplate_dummy[dat.treaty$threshold_1_dummy == TRUE])

table.fr.bp
round(prop.table(table.fr.bp, 1), 2)


# -- 2 General Descriptives ----------------------------------------------------
dim(dat.treaty)
table(dat.treaty$anythreshold)
prop.table(table(dat.treaty$anythreshold))

threshold.1.table
threshold.2.table




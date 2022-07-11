


rm(list = ls())

dat <- read.csv("../../Doktorat/Datasets/08_EventvsSignal/ecrvema_share_revised.csv")
dat <- dat[dat$condition %in% 1, ]

dat <- dat[,c("participantID","beginTime.ema","initials_clean","warmColdOther")]
dat$date <- as.POSIXct(dat$beginTime.ema,format="%Y-%m-%dT%H:%M:%SZ",tz = "EST5EDT")
drop <- which(dat$initials_clean %in% c("SKIPPED","NO_ANSWER",""))
dat <- dat[!(1:nrow(dat) %in% drop),]
write.csv(dat[,c("participantID","date","initials_clean","warmColdOther")],"test_dat_ecrvema.csv")

id <- sample(unique(dat$participantID),1)
id.varname <- "participantID"
alter.varname <- "initials_clean"
timepoint <- 4


get.end.net <- function(dataset, id.varname = "participantID", id){
  require(igraph)
  require(ggraph)
  dat <- dataset[dataset[,id.varname] %in% id,]
  max.timepoints <- nrow(dat)
  sub <- dat[1:timepoint,]
  alters <- c("Ego",unique(dat[,alter.varname]))
  
  emptynet <- matrix(0, length(alters),length(alters))
  rownames(emptynet) <- alters
  colnames(emptynet) <- alters
  
  net <- layoutnet <-emptynet
  
  # loop to define general layout
  for(row in 1:nrow(dat)){
    tmp <- dat[row,]
    tmp.net <- emptynet
    if(nrow(tmp) > 0){
      new.tie <- rep(0, length(alters))
      if(is.na(tmp[,alter.varname])) next()
      new.tie[which(tmp[,alter.varname] == alters)] <- 1
      layoutnet[1,] <- layoutnet[1,]  + as.matrix(new.tie)
    }
  }
  
  return(layoutnet)
}


get.empty.net <-  function(dataset, id.varname = "participantID", id){
  
  require(igraph)
require(ggraph)
dat <- dataset[dataset[,id.varname] %in% id,]
max.timepoints <- nrow(dat)
sub <- dat[1:timepoint,]
alters <- c("Ego",unique(dat[,alter.varname]))

emptynet <- matrix(0, length(alters),length(alters))
rownames(emptynet) <- alters
colnames(emptynet) <- alters

net <- layoutnet <-emptynet
emptynet[1,-1] <- 1
empty.layout <- create_layout(graph =emptynet, layout = 'fr')
return(empty.layout)
}



egonet_plot <- function(dataset, id.varname = "participantID", id, end.layout = NULL, empty.layout = NULL, end.network = NULL,
                      alter.varname = "initials_clean", timepoint = 1, aggregated = T) {
  
  require(igraph)
  require(ggraph)
  #empty.layout <- get.empty.net(dataset,id.varname = id.varname, id )
  if(is.null(end.layout)) end.layout <- empty.layout
  
  dat <- dataset[dataset[,id.varname] %in% id,]
  max.timepoints <- nrow(dat)
  if(timepoint>max.timepoints) timepoint <- max.timepoints
  sub <- dat[1:timepoint,]
  alters <- c("Ego",unique(dat[,alter.varname]))
  
  emptynet <- matrix(0, length(alters),length(alters))
  rownames(emptynet) <- alters
  colnames(emptynet) <- alters
  
  net <- layoutnet <-emptynet
  
  # define momentary state
  for(row in 1:nrow(sub)){
    tmp <- sub[row,]
    tmp.net <- emptynet
    
    
    if(nrow(tmp) > 0){
      new.tie <- rep(0, length(alters))
      if(is.na(tmp[,alter.varname])) next()
      new.tie[which(tmp[,alter.varname] == alters)] <- 1
      
      tmp.net[1,] <- as.matrix(new.tie)
      net[1,] <- net[1,]  + as.matrix(tmp.net[1,])
    }
  }
  
  
  #end.layout <- create_layout(graph =layoutnet, layout = 'fr')
 
  
  intermediate.layout <- function(net = tmp.net, end.network = end.network, 
                                  empty.layout = empty.layout, end.layout = end.layout, 
                                  step = NULL, max.timepoints = max.timepoints){
    actor.nr <- which(net[1,] == 1)
    out <- empty.layout
    
    x.move <- (end.layout$x - empty.layout$x) / max.timepoints
    y.move <- (end.layout$y - empty.layout$y) / max.timepoints
    
    #out[actor.nr,"x"] <- empty.layout[actor.nr,"x"]+x.move[actor.nr]*step*end.network[1,actor.nr]
    #out[actor.nr,"y"] <- empty.layout[actor.nr,"y"]+y.move[actor.nr]*step*end.network[1,actor.nr]
    out[,"x"] <- empty.layout[,"x"]+x.move*step*end.network[1,]
    out[,"y"] <- empty.layout[,"y"]+y.move*step*end.network[1,]
    return(out)
  }
  

  layout.l <- intermediate.layout(net = tmp.net, empty.layout = empty.layout, end.network = end.network,
                                  end.layout = end.layout, step = timepoint, max.timepoints = max.timepoints)
  #layout.l <- end.layout
  #cols.int <- colorRampPalette(c("orange", "forestgreen"))
  #int.C.col <- cols.int(100)[tmp$IntIGselfC_int]
  vertex.border <- 0
  vertex.color <- ifelse(colnames(tmp.net) == "Ego","blue","forestgreen")
  vertex.color[colSums(tmp.net, na.rm = T) < 1 & !(colnames(tmp.net) == "Ego")] <- "gray90"
  
  if(aggregated) {
    tmp.net <- net
    title <- "Aggregated Egocentric Network \n"
    
    vertex.color2 <- ifelse(colnames(tmp.net) == "Ego","blue","forestgreen")
    vertex.color2[colSums(net, na.rm = T) < 1 & !(colnames(net) == "Ego")] <- "gray90"
    
    layout.by.weight <- function(net, layout.l){
      new.layout <- layout.l[,1:2]
      weight <- net[1,]
      x.dist <- new.layout[,1]- new.layout[1,1]
      y.dist <- new.layout[,2]- new.layout[1,2]
      
      new.layout$x <- new.layout$x-x.dist*log(1+weight)
      #new.layout$y <- new.layout$y-y.dist*log(1+weight)
      return(new.layout)
    }
    
    egonet <- ggraph(net, layout = layout.l[,1:2])+ 
      geom_edge_link(alpha = .15, aes(width = weight, color = weight, label = weight))+ 
      #geom_edge_text(alpha = .25, aes(text = log(weight)))+ 
      geom_node_point(color = vertex.color2, alpha = 0.6, size = 4)+
      geom_node_text(aes(label = alters, vjust = 1.5), size = 3) +
      #ggtitle(paste0("Aggregate ego-network")) +
      theme_graph() +
      ggtitle(title) +
      theme(legend.position = 'none')  +
      coord_cartesian(xlim = c(min(empty.layout[,1]),max(empty.layout[,1])), ylim =c(min(empty.layout[,2]),max(empty.layout[,2]))) 
    
    egonet
    
    }else{
    title <- paste0("Momentary Egocentric Network \nDate: ", tmp$date)
    
    egonet <-  ggraph(tmp.net, layout = layout.l[,1:2])+ 
      geom_edge_link(alpha = 0.6, width = 2)+ 
      geom_node_point(color = vertex.color,alpha = 0.6, size = 4)+
      geom_node_text(aes(label = alters, vjust = 1.5#, family = "Times"
      ), size = 3) +
      ggtitle(title) +
      theme_graph() +
      coord_cartesian(xlim = c(min(empty.layout[,1]),max(empty.layout[,1])), ylim =c(min(empty.layout[,2]),max(empty.layout[,2]))) 
    }
  

  
  egonet
  return(egonet)
  #return(list(egonet = egonet, layout = layout.l))
}

system("rm video/plots/*.jpg")
dataset <- dat
#jpeg("video/plots/frame%04d.jpg", width = 1920, height = 1080)
for(id in unique(dataset$participantID)[12]){
n <- nrow(dataset[dataset$participantID %in% id,])

# set layouts
#end.layout <- create_layout(graph = get.end.net(dat,id.varname = "participantID", id ),layout = 'fr')
empty.layout <- get.empty.net(dat,id.varname = "participantID", id )
end.network <- get.end.net(dataset,id.varname = "participantID", id )
get.end.layout <- function(end.network, empty.layout){
  out <- empty.layout
  max.edgeweight <- max(end.network)
  distance.divide <- max.edgeweight+2
  x.ego <- out[1,1]
  y.ego <- out[1,2]
  for(row in 2:nrow(out)){
    if(end.network[1,row] == 1) next
    out[row,1] <- out[row,1]-((out[row,1]-x.ego)/distance.divide)*end.network[1,row]
    out[row,2] <- out[row,2]-((out[row,2]-y.ego)/distance.divide)*end.network[1,row]
  }
  return(out)
}

end.layout<-get.end.layout(end.network,empty.layout)
end.layout[1]-empty.layout[1]
for(i in 1:n){
cat(paste0("\r ", i, " out of ", n))

p.moment <- egonet_plot(dat, id = id, end.layout = end.layout, 
                        empty.layout = empty.layout, end.network = end.network,
            aggregated = F, timepoint = i)
p.agg <- egonet_plot(dat, id = id, end.layout = end.layout, end.network = end.network,
                     empty.layout = empty.layout, 
                        aggregated = T, timepoint = i)

plot(ggpubr::ggarrange(p.moment, p.agg))
ggsave(paste0("video/plots/egonet_vis_t",sprintf("%03d", i),".jpg"), width = 10, height =5)

}
#dev.off()

# render video
system(paste0("rm video/egonet_vis_id",id,".mp4; ffmpeg -framerate 2 -i video/plots/egonet_vis_t%03d.jpg -b 10000k -vb 2073600 video/egonet_vis_id",id,".mp4"))
#system("rm video/evolution_networks_sci_v4_no_subtext.mp4; ffmpeg -framerate 24 -i video/out_%04d.jpg -b 10000k -vb 2073600 video/evolution_networks_sci_v4_no_subtext.mp4")
#duration
}


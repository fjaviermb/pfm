library("grid")
library("gridExtra")
library(gtable)


render.table.grob <- function(df, title.label = '') {
  
  mytheme <- gridExtra::ttheme_default(
    core = list(padding=unit(c(4, 4), "mm"))
  )
  
  table.data <- tableGrob(df, rows <- NULL, theme = mytheme)
  
  table.title <- textGrob(title.label,gp=gpar(fontsize=14))
  table.title.padding <- unit(5,"mm")
  
  table.graph <- gtable_add_rows(
    table.data, 
    heights = grobHeight(table.title) + table.title.padding,
    pos = 0)
  table.graph <- gtable_add_grob(
    table.graph, 
    table.title, 
    1, 1, 1, ncol(table.graph))
  
  return(table.graph)
  
}

render.two.tables<-function(df1, title.label.1, df2, title.label.2, filename = NULL ) {

  table.graph.1 <- render.table.grob(df1, title.label.1)
  table.graph.2 <- render.table.grob(df2, title.label.2)
  
  if( ! is.null(filename)) {

    h1 = grid::convertHeight(sum(table.graph.1$heights), "in", TRUE)+0.03
    w1 = grid::convertWidth(sum(table.graph.1$widths), "in", TRUE)+0.2
    
    h2 = grid::convertHeight(sum(table.graph.2$heights), "in", TRUE)+0.03
    w2 = grid::convertWidth(sum(table.graph.2$widths), "in", TRUE)+0.2
    
    
    multiple.grob <- arrangeGrob(table.graph.1, table.graph.2, nrow=2)
    
    ggplot2::ggsave(filename, multiple.grob, width=w1, height=h1+h2+0.08)
  
  }
  
  # Do render in any case
  grid.newpage()
  grid.arrange(multiple.grob)
  
}

# ' Plot a table from a dataframe using grid and gridExtra library
render.table<-function(df, title.label = NULL, filename = NULL ) {
  
  mytheme <- gridExtra::ttheme_default(
    core = list(padding=unit(c(4, 4), "mm"))
  )
  
  table.data <- tableGrob(df, rows <- NULL, theme = mytheme)
  
  table.graph <- NULL
  
  if( ! is.null(title.label) ) {
  
    table.title <- textGrob(title.label,gp=gpar(fontsize=14))
    table.title.padding <- unit(5,"mm")
    
    table.graph <- gtable_add_rows(
      table.data, 
      heights = grobHeight(table.title) + table.title.padding,
      pos = 0)
    table.graph <- gtable_add_grob(
      table.graph, 
      table.title, 
      1, 1, 1, ncol(table.graph))
  }
  
  if( ! is.null(filename)) {
    h = grid::convertHeight(sum(table.graph$heights), "in", TRUE)+0.04
    w = grid::convertWidth(sum(table.graph$widths), "in", TRUE)+0.2
    ggplot2::ggsave(filename, table.graph, width=w, height=h)
  }
  
  # Do render in any case
  grid.newpage()
  
  if( ! is.null(table.graph )) {
    grid.draw(table.graph)
  } else {
    grid.arrange(table.data)
  }

  
}

# 'Customized render for bars
render.bar <- function(data.ds, title, xlabel, ylabel, aesParam, rotation = NULL, filename = NULL ) {
  
  rotationTheme <- theme()
  
  if( !is.null (rotation) ) {
    rotationTheme <- theme(axis.text.x = element_text(angle = rotation, hjust = 1) )
  } 
  
  backgroundColor <- c('#FAFAFA', '#E2E2E2', '#FAFAFA')

  bar.graph <- ggplot(data=data.ds, aesParam) +
    geom_text(vjust=-0.3) + 
    geom_bar(stat="identity", fill = '#4A90D2', color = '#4F93D0')+
    labs(x=xlabel, y=ylabel, title=title)+  
    theme(plot.title = element_text( hjust=0.5, size = rel(1.5), colour = "black")) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = backgroundColor, colour = NA)) +
    rotationTheme
  
  if( ! is.null(filename)) {
    #h = grid::convertHeight(sum(table.graph$heights), "in", TRUE)+0.04
    #w = grid::convertWidth(sum(table.graph$widths), "in", TRUE)+0.2
    ggplot2::ggsave(filename, bar.graph) #, width=w, height=h)
  }
  
  # Render graph
  grid.arrange(bar.graph)
  
  
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# From: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
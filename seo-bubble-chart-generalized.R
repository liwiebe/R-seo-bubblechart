# Written by Lindsey Wiebe, March 2015
# Helped along a lot by Flowing Data article:
# http://flowingdata.com/2010/11/23/how-to-make-bubble-charts/

# The function to create the plot and save it
# quarter must be a data frame pulled from the CSV
seoPlot <- function(quarter,i) {
  
  # Set up some parameters before creating the plot
  radius <- sqrt( quarter$Impressions / pi ) # Size by area, not radius
  xmed <- median( quarter$CTR ) # for drawing the median lines - calculated from inputs
  ymed <- median( quarter$Position )
  yrange <- rev( range( quarter$Position ) ) # for reversing the y axis
  
  # Prepare to save the plot
  png(paste0( i, ".png"), width=1280, height=800)
  
  # Draw the plot
  symbols(quarter$CTR, quarter$Position, circles=radius,
          inches=0.75, fg="white", bg="gray", xlab="Clickthrough Rate", 
          ylab="Average Position", main="Key Terms Comparison", 
          xaxt="n", ylim = yrange )
  text(quarter$CTR, quarter$Position, quarter$Term, cex=0.75)
  axis(1, at=pretty(quarter$CTR), lab=paste0(pretty(quarter$CTR)*100,"%"), las=TRUE) # make nicer x axis
  
  # Draw median lines
  abline(h=ymed, lty=3)
  abline(v=xmed, lty=3)
  
  # Save the plot
  dev.off()
}

# Look for the files to run on
files <- (Sys.glob("search*.csv"))

# Execute the function on each file found
for (i in 1:length(files)) {
  quarterData <- read.csv(files[[i]],header=TRUE) # put the data into a variable
  currentQuarter <- gsub("(.csv)", "", files[[i]]) # Removes file suffix from filename
  seoPlot(quarterData, currentQuarter)
}

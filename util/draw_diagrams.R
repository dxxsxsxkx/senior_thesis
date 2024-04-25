# This script draws a diagram of the electoral process. 

# Ligrary
library(DiagrammeR)

grViz(" 
digraph surveillance_diagram {
  
  # graph statement
  #################
  graph [layout = dot,
         rankdir = LR,
         overlap = true,
         fontsize = 20]
  
  # nodes
  #######
  node [shape = square,           # shape = circle
       fixedsize = true
       width = 1.5]               # circle の大きさ
  
  First [label = 'Potential \n Candidates']
  Second [label = 'Candidates']
  Third [label = 'Elected']

  # edges
  #######
  First   -> Second [label = 'Nomination']
  Second   -> Third [label = 'Election']
}
")


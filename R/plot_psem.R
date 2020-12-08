#' Plot psem
#'
#' Create a diagram of piecewiseSEM model
#'
#' @param model piecewiseSEM model obtained from psem()
#' @param layout layout type. "dot" or "neato"
#' @param render if FALSE, only return object without viewing
#'
#' @return a figure or if render = FALSE, a character of the DOT code.
#' @export
#'
#' @importFrom stringr str_detect
#' @importFrom checkmate assert_class assert_choice assert_logical
#' @importFrom piecewiseSEM coefs
#' @importFrom DiagrammeR create_node_df create_edge_df create_graph render_graph add_global_graph_attrs delete_global_graph_attrs
plot_psem <- function(model, layout = "dot", render = TRUE){

  checkmate::assert_class(model, "psem")
  checkmate::assert_choice(layout, c("dot", "neato"))
  checkmate::assert_logical(render)


  #function defaults for plotting
  node_attrs = data.frame(shape = "rectangle", color = "black",
                          fillcolor = "white")
  edge_attrs = data.frame(style = "solid", color="black")
  alpha=0.05
  digits = 3

  # get variables for plot --------------------------------------------------------------------

  #get the coefficients table
  ctab <- coefs(model)
  ctab$Response <- as.character(ctab$Response)
  ctab$Predictor <- as.character(ctab$Predictor)

  #define type based on ~~ symbol
  ctab$type <- "regression"
  ctab$type[stringr::str_detect(ctab$Response, "^~~*")] <- "correlation"

  #clean names
  ctab$Response <- gsub("~~", "", ctab$Response)
  ctab$Predictor <- gsub("~~", "", ctab$Predictor)


  # make a nodes DF ---------------------------------------------------------

  unique_nodes <- unique(c(ctab$Response, ctab$Predictor))
  nodes <- create_node_df(n = length(unique_nodes),
                          nodes = unique_nodes,
                          type = "lower",
                          label = unique_nodes)
  nodes <- cbind(nodes, node_attrs)
  nodes[] <- lapply(nodes, as.character)
  nodes$id <- as.numeric(nodes$id)


  # make an edges DF --------------------------------------------------------

  edges <- create_edge_df(
    from = match(ctab$Predictor, unique_nodes),
    to = match(ctab$Response, unique_nodes),
    type = ctab$type,
    label = round(ctab$`Std.Estimate`, digits)
  )

  edges <- data.frame(edges, edge_attrs)
  edges$color <- as.character(edges$color)
  edges$color[as.numeric(edges$label) < 0] <- "red"
  edges[] <- lapply(edges, as.character)
  edges$id <- as.numeric(edges$id)
  edges$from <- as.numeric(edges$from)
  edges$to <- as.numeric(edges$to)
  edges$style[which(ctab$P.Value>alpha)] <- "dashed"
  edges$color[which(ctab$P.Value>alpha)] <- "grey"
  edges$label <- round(ctab$`Std.Estimate`, digits)

  edges$dir <- "forward"
  edges$dir[edges$type == "correlation"] <- "both"

  #arrow thickness
  edges$penwidth <- 1

  vals <- edges$label[edges$style == "solid"] %>% as.numeric %>% abs #for significant values
  min_pen <- 1
  max_pen <- 8 #choose max line with
  penw <- (vals - min(vals)) * (max_pen - min_pen)/(max(vals) - min(vals)) + min_pen
  edges$penwidth[edges$style == "solid"] <- penw


  # put graph together ------------------------------------------------------

  sem_graph <- create_graph(nodes, edges, directed=TRUE, attr_theme = "default")
  render_graph(sem_graph)

  neato_graph <- sem_graph %>%
    add_global_graph_attrs("fixedsize", "false", "node") %>%
    add_global_graph_attrs("width", "1.5", "node") %>%
    add_global_graph_attrs("fontcolor", "black", "node") %>%
    add_global_graph_attrs("len", "5", "edge") %>%
    add_global_graph_attrs("splines", "true", "graph")

  if (layout == "neato") {

    if (render == FALSE) {
      neato_graph
    } else {
      render_graph(neato_graph)
    }

  } else if (layout == "dot"){
    dot_graph <- neato_graph %>%
      add_global_graph_attrs(attr = "layout",
                             value = "dot",
                             attr_type = "graph") %>%
      #add_global_graph_attrs("rankdir", "TB", "graph") %>% #left to right
      #add_global_graph_attrs("ranksep", "2", "graph") %>% #bit more space horizontally
      add_global_graph_attrs("fontsize", "16", "node") %>%
      add_global_graph_attrs("fontsize", "12", "edge") %>%
      delete_global_graph_attrs("len", "edge")

    if (render == FALSE) {
      dot_graph
    } else {
      render_graph(dot_graph)
    }
  }
}

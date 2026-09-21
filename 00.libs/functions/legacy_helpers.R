
# 对所有的全国土样重新命名
rename_jgf <- function(df){
  colnames(df)[ colnames(df) %in% Group[,2] ] <- Group[,1]
  return(df)
}

# Summarise the most abundant taxa. Kept here because both the microbiome and
# ARG workflows use the same calculation.
get_top_taxa <- function(df, id_col = "ID", top_n = 10) {
  num_cols <- vapply(df, is.numeric, logical(1))
  if (!any(num_cols)) stop("数据框中没有数值列！")

  df_mean <- df %>%
    dplyr::mutate(Mean_Abundance = rowMeans(dplyr::select(., dplyr::where(is.numeric)), na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(Mean_Abundance))
  total_abundance <- sum(df_mean$Mean_Abundance, na.rm = TRUE)
  top_summary <- df_mean %>%
    head(top_n) %>%
    dplyr::select(dplyr::all_of(id_col), Mean_Abundance) %>%
    dplyr::mutate(Relative_Proportion_Pct = (Mean_Abundance / total_abundance) * 100)

  list(
    Top_Summary = top_summary,
    Total_Abundance = total_abundance,
    Top_Sum_Abundance = sum(top_summary$Mean_Abundance, na.rm = TRUE),
    Top_Sum_Proportion_Pct = sum(top_summary$Relative_Proportion_Pct, na.rm = TRUE)
  )
}

# LorMe04 0.x can construct duplicate edges for an undirected correlation
# graph.  Current igraph rejects those in fast-greedy community detection.
# Keep the thresholded unique edges and collapse only duplicate graph entries.
network_analysis_compat <- function(...) {
  tryCatch(
    LorMe04::network_analysis(...),
    error = function(e) {
      if (!grepl("without multi-edges", conditionMessage(e), fixed = TRUE) ||
          !exists("igraph1", envir = .GlobalEnv, inherits = FALSE)) {
        stop(e)
      }
      simple_graph <- igraph::simplify(
        get("igraph1", envir = .GlobalEnv, inherits = FALSE),
        remove.multiple = TRUE,
        remove.loops = TRUE
      )
      warning(
        "LorMe04 produced duplicate undirected edges; using the equivalent simple graph for this network.",
        call. = FALSE
      )
      list(data.frame(), igraph::as_data_frame(simple_graph, what = "edges"))
    }
  )
}

build_sparse_correlation_network <- function(inputframe, prevalence_min,
                                             top_n, rho_min, fdr_alpha,
                                             max_edges) {
  abundance <- as.matrix(inputframe[, -1, drop = FALSE])
  storage.mode(abundance) <- "double"
  rownames(abundance) <- inputframe[[1]]
  prevalence <- rowSums(abundance > 0, na.rm = TRUE)
  mean_abundance <- rowMeans(abundance, na.rm = TRUE)
  candidates <- names(sort(mean_abundance[prevalence >= prevalence_min], decreasing = TRUE))
  candidates <- head(candidates, top_n)
  if (length(candidates) < 3L) {
    stop("Fewer than three taxa satisfy the network prevalence filter.", call. = FALSE)
  }

  correlation <- Hmisc::rcorr(t(abundance[candidates, , drop = FALSE]), type = "spearman")
  upper <- upper.tri(correlation$r)
  edge_pool <- data.frame(
    source = rownames(correlation$r)[row(correlation$r)[upper]],
    target = colnames(correlation$r)[col(correlation$r)[upper]],
    rho = correlation$r[upper],
    p_value = correlation$P[upper],
    stringsAsFactors = FALSE
  )
  edge_pool$fdr <- stats::p.adjust(edge_pool$p_value, method = "BH")
  edges <- edge_pool[edge_pool$fdr <= fdr_alpha & abs(edge_pool$rho) >= rho_min, , drop = FALSE]
  edges <- edges[order(abs(edges$rho), decreasing = TRUE), , drop = FALSE]
  if (nrow(edges) > max_edges) edges <- edges[seq_len(max_edges), , drop = FALSE]
  if (!nrow(edges)) {
    stop("No network edges satisfy the configured correlation and FDR filters.", call. = FALSE)
  }

  graph <- igraph::simplify(
    igraph::graph_from_data_frame(edges, directed = FALSE),
    remove.multiple = TRUE,
    remove.loops = TRUE
  )
  community <- igraph::cluster_louvain(graph, weights = abs(as.numeric(igraph::edge_attr(graph, "rho"))))
  nodes <- data.frame(
    taxon = igraph::V(graph)$name,
    degree = igraph::degree(graph),
    module = as.integer(igraph::membership(community)),
    stringsAsFactors = FALSE
  )
  nodes$mean_abundance <- mean_abundance[nodes$taxon]
  set.seed(333)
  layout <- as.data.frame(igraph::layout_with_fr(graph))
  names(layout) <- c("x", "y")
  layout$taxon <- nodes$taxon
  nodes <- dplyr::left_join(nodes, layout, by = "taxon")
  edge_plot <- edges |>
    dplyr::left_join(dplyr::select(nodes, taxon, x, y), by = c("source" = "taxon")) |>
    dplyr::left_join(dplyr::select(nodes, taxon, x, y), by = c("target" = "taxon"), suffix = c("", ".end")) |>
    dplyr::rename(xend = x.end, yend = y.end)
  label_cutoff <- stats::quantile(nodes$degree, probs = 0.9, names = FALSE)
  list(
    nodes = nodes,
    edges = edges,
    parameters = data.frame(
      prevalence_min = prevalence_min, top_n = top_n, rho_min = rho_min,
      fdr_alpha = fdr_alpha, max_edges = max_edges,
      selected_taxa = length(candidates), retained_nodes = nrow(nodes),
      retained_edges = nrow(edges)
    ),
    plot = ggplot2::ggplot() +
      ggplot2::geom_segment(data = edge_plot, ggplot2::aes(x = x, y = y, xend = xend, yend = yend, colour = rho), alpha = .55, linewidth = .3) +
      ggplot2::scale_colour_gradient2(low = "#3C5488", mid = "grey85", high = "#E64B35", midpoint = 0, name = "Spearman rho") +
      ggplot2::geom_point(data = nodes, ggplot2::aes(x = x, y = y, size = degree, fill = factor(module)), shape = 21, colour = "white", stroke = .25) +
      ggsci::scale_fill_npg(name = "Module") +
      ggrepel::geom_text_repel(data = nodes[nodes$degree >= label_cutoff, , drop = FALSE], ggplot2::aes(x = x, y = y, label = taxon), size = 2.6, max.overlaps = Inf, show.legend = FALSE) +
      ggplot2::scale_size_continuous(range = c(2.2, 6), name = "Degree") +
      ggplot2::coord_equal() +
      ggplot2::theme_void() +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = NA), legend.position = "right")
  )
}

# 对所有ARG溯源数据
taxnonmy_pan <- function(prefix,df,order,levels){
  # 构建哈希表
  require(dplyr)
  taxlevel <- cbind(
    taxnonmy=c("Domain","Phylum","Class","Order","Family","Genus","Species"),
    levels=c("D","P","C","O","F","G","S")
  ) %>% as.data.frame()
  df_Tax <- left_join(df,Pangenome_tax,by=c("ID"="SpeciesID")) %>% na.omit()
  taxlevel_select <- taxlevel[which(taxlevel$levels %in% levels),]
  num <- nrow(taxlevel_select)
  for (i in 1:num) {
    level_final <- taxlevel_select$taxnonmy[i]
    df_name <- paste0(prefix,"_",taxlevel_select$levels[i])
    df <- df_Tax %>% dplyr::select(all_of(level_final),all_of(order))
    df <- aggregate(x = df[,-1],by = list(df[,1]),FUN = sum)
    colnames(df)[1] <- "ID"
    df <- df[which(rowSums(df[,-1])!=0),]
    assign(df_name,df,envir=.GlobalEnv)
    
  }
}


# prefix一定和df的名称一样
# df是
Group_selID <- function(prefix,df,Group_list){
  require(dplyr) 
  for (i in 1:length(Group_list)) {
    Group_file <- Group_list[[i]]
    Group_name <- names(Group_list[i])
    Group_type <- stringr::str_split(Group_name,pattern = "_",simplify = T)[1,2]
    dfname <- paste0(prefix,"_",Group_type)
    df_new <- df %>%  dplyr::select(ID,all_of(Group_file$ID))
    assign(x = dfname,value = df_new,envir =.GlobalEnv)
  }
}

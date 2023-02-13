library(phyloseq)
library(dplyr)
library(readr)
library(ape)
library(Biostrings)

#' @export
#' @importFrom dplyr mutate
#' @importFrom phyloseq phyloseq otu_table tax_table sample_data
#' @importFrom tidyr replace_na
#' @importFrom ape read.tree
ReadQiime2 <- function(
  asvtab,
  taxtab,
  metatab,
  treenwk = NA,
  seqfasta = NA,
  ...){

    asvtab.raw <- readr::read_tsv(asvtab, show_col_types = FALSE)
    taxtab_raw <- readr::read_tsv(taxtab, show_col_types = FALSE)
    metatab.raw <- readr::read_tsv(metatab, na=c("", "#N/A"), show_col_types = FALSE)

    pseq.tab <- asvtab.raw %>%
        tibble::column_to_rownames(names(asvtab.raw)[1]) %>%
        as.matrix() %>%
        otu_table(taxa_are_rows = TRUE)
    pseq.tax <- taxtab_raw %>%
        dplyr::mutate(across(everything(), ~ replace_na(.x, ""))) %>%
        tibble::column_to_rownames(names(taxtab_raw)[1]) %>%
        as.matrix() %>%
        tax_table()
    pseq.met <- metatab.raw %>%
      tibble::column_to_rownames(names(metatab.raw)[1]) %>%
      sample_data()

    ess_args <- list(pseq.tab, pseq.tax, pseq.met)
    # Optional
    if(! is.null(seqfasta)){
      pseq.seq <- Biostrings::readDNAStringSet(seqfasta)
      ess_args <- append(ess_args, list(pseq.seq))
    }
    if(! is.null(treenwk)){
      pseq.tre <- ape::read.tree(treenwk)
      ess_args <- append(ess_args, list(pseq.tre))
    }

    do.call(phyloseq, ess_args)
}

#' Load folder of microbiome data. The folder must have asv.tab, taxonomy.tsv, metadata.tsv files
#' 
#' @return phyloseq object
#' @export
LoadFolder <- function(folder){
  files_list <- c("asv.tab", "taxonomy.tsv", "metadata.tsv", "rooted-tree.nwk", "repsep.fasta")
  all_paths <- file.path(
    folder, c("asv.tab", "taxonomy.tsv", "metadata.tsv", "rooted-tree.nwk", "repsep.fasta")
  ) %>% unlist %>% as.list

  # Warning if it does not load
  ps <- do.call(ReadQiime2, all_paths)
  return(ps)
}
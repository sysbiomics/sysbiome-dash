library(phyloseq)
library(tidyverse)
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

# Misc

FastMelt <- function(physeq, includeSampleVars = character()) {
  require("phyloseq")
  require("data.table")
  # Fixed output name
  name.sam <- "ID_sample"
  name.abn <- "abn"
  name.tax <- "TaxaID"

  # Check if data.table has these name.

  # supports "naked" otu_table as `physeq` input.
  otutab <- as(otu_table(physeq), "matrix")
  if (!taxa_are_rows(physeq)) {
    otutab <- t(otutab)
  }
  otudt <- data.table(otutab, keep.rownames = name.tax)
  # Enforce character TaxaID key
  otudt[, (name.tax) := as.character(get(name.tax))]
  # Melt count table
  mdt <- melt.data.table(otudt,
    id.vars = name.tax,
    variable.name = name.sam,
    value.name = name.abn
  )
  # Omit NAs
  # mdt <- mdt[!is.na(abn)]
  if (!is.null(tax_table(physeq, errorIfNULL = FALSE))) {
    # If there is a tax_table, join with it. Otherwise, skip this join.
    taxdt <- data.table(as(tax_table(physeq, errorIfNULL = TRUE), "matrix"), keep.rownames = name.tax)
    taxdt[, (name.tax) := as.character(get(name.tax))]
    # Join with tax table
    setkeyv(taxdt, name.tax)
    setkeyv(mdt, name.tax)
    mdt <- taxdt[mdt]
  }

  # Save taxonomy columns

  wh.svars <- which(sample_variables(physeq) %in% includeSampleVars)
  if (length(wh.svars) > 0) {
    # Only attempt to include sample variables if there is at least one present in object
    sdf <- as(sample_data(physeq), "data.frame")[, wh.svars, drop = FALSE]
    sdt <- data.table(sdf, keep.rownames = name.sam)
    # Join with long table
    setkeyv(sdt, name.sam)
    setkeyv(mdt, name.sam)
    mdt <- sdt[mdt]
  }
  setkeyv(mdt, name.tax)
  return(mdt)
}
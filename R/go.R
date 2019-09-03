query_org_db <- function(OrgDb, ont, keytype) {
  con <- dbconn(OrgDb)

  # database tables
  ont_tbl  <- ifelse(ont == "ALL", "go_all", sprintf("go_%s", tolower(ont)))
  gene_tbl <- key2table(keytype, DBI::dbListTables(con))

  # gene table fieldname
  gene_field <- setdiff(DBI::dbListFields(con, gene_tbl), "_id")
  ont_field <- switch(ont, ALL = DBI::SQL("ontology"), ont)

  sql <- glue::glue_sql(
    "SELECT {DBI::SQL(gene_field)}, go_id, evidence, {ont_field} as ontology
     FROM {gene_tbl}
     JOIN {ont_tbl}
     ON {gene_tbl}._id = {ont_tbl}._id",
    .con = con
  )

  result <- DBI::dbGetQuery(con, sql)

  # temporarily force names to match original goAnno by appending 'ALL'
  setNames(result, c(keytype, "GOALL", "EVIDENCEALL", "ONTOLOGYALL"))
}

#' Convert a OrgDB keytype to the corresponding database field name
#' @noRd
key2table <- function(keytype, tables) {
  # copied from AnnotationDbi:::.definePossibleTables
  lookup <- list(
    "ENTREZID" = c("genes", "gene_id"),
    "PFAM" = c("pfam", "pfam_id"),
    "IPI" = c("pfam", "ipi_id"),
    "PROSITE" = c("prosite", "prosite_id"),
    "ACCNUM" = c("accessions", "accession"),
    "ALIAS" = c("alias", "alias_symbol"),
    "ALIAS2EG" = c("alias", "alias_symbol"),
    "ALIAS2PROBE" = c("alias", "alias_symbol"),
    "CHR" = c("chromosomes", "chromosome"),
    "CHRLOCCHR" = c("chromosome_locations", "seqname"),
    "CHRLOC" = c("chromosome_locations", "start_location"),
    "CHRLOCEND" = c("chromosome_locations", "end_location"),
    "ENZYME" = c("ec", "ec_number"),
    "MAP" = c("cytogenetic_locations", "cytogenetic_location"),
    "PATH" = c("kegg", "path_id"),
    "PMID" = c("pubmed", "pubmed_id"),
    "REFSEQ" = c("refseq", "accession"),
    "SYMBOL" = c("gene_info", "symbol"),
    "UNIGENE" = c("unigene", "unigene_id"),
    "ENSEMBL" = c("ensembl", "ensembl_id"),
    "ENSEMBLPROT" = c("ensembl_prot", "prot_id"),
    "ENSEMBLTRANS" = c("ensembl_trans", "trans_id"),
    "GENENAME" = c("gene_info", "gene_name"),
    "UNIPROT" = c("uniprot", "uniprot_id"),
    "GO" = c("go", "go_id"),
    "EVIDENCE" = c("go", "evidence"),
    "ONTOLOGY" = c("go", "ontology"),
    "GOALL" = c("go_all", "go_id"),
    "EVIDENCEALL" = c("go_all", "evidence"),
    "ONTOLOGYALL" = c("go_all", "ontology")
  )
  intersect(tables, lookup[[keytype]])
}

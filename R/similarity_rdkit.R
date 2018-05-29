#' Title
#'
#' @param smiles1 atomic character vector containing the SMILES strings
#' @param smiles2 atomic character vector containing the SMILES strings
#' @param labels1 atomic character vector with compound labels to smiles1
#' @param labels2 atomic character vector with compound labels to smiles2
#' @param data.frame if TRUE then the output will be a data.frame otherwise a matrix
#'
#' @return similarity matrix as a data frame or matrix
#' @export
#'
#' @examples
#'
#' #You need to set anaconda environment prior to running RDKit code
#' reticulate::use_condaenv(condaenv = "my-rdkit-env", conda = "/Users/lrichter/miniconda3/bin/conda")
#' reticulate::use_condaenv(condaenv = "my-rdkit-env", conda = "/Users/lrichter/miniconda3/bin/conda")

sim_maccs <- function(smiles1, smiles2, labels1 = NA, labels2 = NA, data.frame = FALSE ) {

  if (is.na(labels1) || is.na(labels2)) {
    labels1 = 1:length(smiles1) %>% as.character()
    labels2 = 1:length(smiles2) %>% as.character()
  }

  MACCSkeys <- reticulate::import("rdkit.Chem.MACCSkeys")
  Chem <- reticulate::import("rdkit.Chem")
  DataStructs <- reticulate::import("rdkit.DataStructs")

  fps1 <- purrr::map(.x = smiles1, .f = ~ Chem$MolFromSmiles(.x) %>% MACCSkeys$GenMACCSKeys())
  fps2 <- purrr::map(.x = smiles2, .f = ~ Chem$MolFromSmiles(.x) %>% MACCSkeys$GenMACCSKeys())
  sim <- purrr::map(.x = fps1, .f = ~ DataStructs$BulkTanimotoSimilarity(.x, fps2))

  m <- do.call(rbind, sim)
  rownames(m) <- labels1
  colnames(m) <- labels2

  if (data.frame) {
    m %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var="row_labels")
  } else {
    m
  }
}

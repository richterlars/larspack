#' Title
#'
#' @param smiles SMILES string of type character
#' @param descriptor c("BertzCT", "MolLogP", "MolMR", "MolWt", "NumHDonors", "NumHAcceptors", "NumRotatableBonds", "TPSA"
#'
#' @return value of the RDKit descriptor
#' @export
#'
#' @examples
#'
#' #You need to set anaconda environment prior to running RDKit code
#' reticulate::use_condaenv(condaenv = "my-rdkit-env", conda = "/Users/lrichter/miniconda3/bin/conda")
#' reticulate::use_condaenv(condaenv = "my-rdkit-env", conda = "/Users/lrichter/miniconda3/bin/conda")
#'
#' rdkit_descriptor("CCO", "MolWt")

rdkit_descriptor <- function(smiles, descriptor = "MolWt") {

    Chem <- reticulate::import("rdkit.Chem")
    Descriptors <- reticulate::import("rdkit.Chem.Descriptors")

    mol <- Chem$MolFromSmiles(smiles)
    desc <- Descriptors[[descriptor]](mol)
    desc
}





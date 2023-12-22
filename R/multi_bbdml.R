#' Basic heatmap with row labels on left
#'
#' Automate some aspects of bbdml for taxa found to be differentially abundant via differentialTest(). multi_bbdml() takes the output from corncob::differentialTest() along with a single mu and phi predictor variable and returns a list of bbdml objects for each taxon that had significant differential abundance. This is different from differentialTest() in that not just the summary objects are returned. This allows for plots to be generated programmatically using the plot_multi_bbdml() function, which takes the output from multi_bbdml() and returns a single bbdml plot for each significant taxon.
#'
#' @import corncob
#' @import tidyverse
#' @import broom
#' @import purrr
#'
#'
#' @param da_analysis output from corncob::differentialTest()
#' @param ps_object phyloseq object
#' @param mu_predictor Variable in sample_data(ps_object) as character vector of length 1
#' @param phi_predictor Variable in sample_data(ps_object) as character vector of length 1
#' @param seed Random seed (integer) ... Default=123
#' @param taxlevels Numeric vector. Taxonomic levels to include in names. Default = 1:7. 1=Kingdom, 7=Species
#'
#' @return List object of bbdml models for all significant taxa found with corncob::differentialTest()
#'
#' @examples
#' bbs <- multi_bbdml(da_analysis = da_analysis,
#'                    ps_object = ps_family,
#'                    mu_predictor = "FireTreatment",
#'                    phi_predictor = "FireTreatment",
#'                    taxlevels = c(1,2,3,4))
#'
#' @export




multi_bbdml <-
function(da_analysis,ps_object,mu_predictor,phi_predictor,seed=123,taxlevels=1:7){

  # make sure all variables are correct object types, error if not
  if(!class(da_analysis) == "differentialTest"){
    stop("da_analysis must be of 'differentialTest' class")
  }
  else if(!class(ps_object) == "phyloseq"){
    stop("ps_object must be of 'phyloseq' class")
  }
  else if(!class(mu_predictor) == "character" & length(mu_predictor) == 1){
    stop("mu_predictor must be 'character' vector of length 1")
  }
  else if(!class(phi_predictor) == "character" & length(phi_predictor) == 1){
    stop("phi_predictor must be 'character' vector of length 1")
  }
  else if(!class(seed) %in% c("numeric","integer") & length(seed) == 1){
    stop("random seed must be 'numeric' or 'integer' vector of length 1")
  }
  else if(!length(taxlevels) > 0 & !class(taxlevels) %in% c("integer","numeric")){
    stop("'taxlevels' must be a numeric vector of length > 0 that indicated which taxonomic levels to include as a name for significant sequences. Default is 1:7")
  }

  else{

    # create chr vector of significant taxa
    sig_taxa <- unlist(da_analysis["significant_taxa"])

    # turn those into formula objects
    forms <- paste0(sig_taxa," ~ ", mu_predictor)
    forms <- lapply(forms,as.formula)

    phi.form <- as.formula(paste0(" ~ ",phi_predictor))

    bbdml_list <- list()

    for(i in 1:length(forms)){
      set.seed(seed)
      bb <- bbdml(formula = forms[[i]],
            phi.formula = phi.form,
            data = ps_object)
      bbdml_list[[i]] <- bb

      tax_name <- paste(tax_table(ps_object)[da_analysis[["significant_taxa"]],taxlevels][i],sep="_")
      taxon_name <- paste(tax_name[1],tax_name[2],tax_name[3],tax_name[4],tax_name[5],tax_name[6],tax_name[7],sep="_")
      taxon_name <- str_remove_all(taxon_name,"_NA")
      names(bbdml_list)[[i]] <- taxon_name


      }

    return(bbdml_list)

  }
}


#################################################################################

# examples

################################################################################
# bbs <- multi_bbdml(da_analysis = da_analysis,
#                    ps_object = ps_family,
#                    mu_predictor = "FireTreatment",
#                    phi_predictor = "FireTreatment",
#                    taxlevels = c(1,2,3,4))
#
#
# # just make plots for the first 2 significant taxa
# plot_multi_bbdml(bbdml_list = bbs,pointsize=3,color = "FireTreatment",whichtaxa = 1:2)
# library(patchwork)
# bbdml_plot_1 / bbdml_plot_2
#
# # make plots for all taxa (default) with larger point size, not colored by any predictor categories
# plot_multi_bbdml(bbdml_list = bbs,pointsize=5)
# bbdml_plot_1 / bbdml_plot_2 / bbdml_plot_3 / bbdml_plot_4
#
#
#
# # Can be piped
# multi_bbdml(da_analysis = da_analysis,
#             ps_object = ps_family,
#             mu_predictor = "FireTreatment",
#             phi_predictor = "FireTreatment",
#             taxlevels = 1:4) %>%
#   plot_multi_bbdml(color="FireTreatment",pointsize=3)
#



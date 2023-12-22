#' Autmoates plotting multiple bbdml plots from taxa identified as significantly different by multi_bbdml()
#'
#' Returns a series of ggplot objects to .GlobalEnv, named by the obj_basename parameter. This function is called for its side effects. Yeah, that's bad and maybe it should return them as a list. Maybe I'll change that later.
#'
#' @import corncob
#' @import tidyverse
#' @import broom
#' @import purrr
#'
#' @param bbdml_list output list object from multi_bbdml()
#' @param pointsize size of points in plots. Default = 1
#' @param obj_basename Character to begin name of returned ggplot objects. Default = "bbdml_plot_"
#' @param color Character vector of length 1. Name of a variable found in the bbdml_list list object to color points by. Default = 'none'.
#' @param whichtaxa Numeric vector indicating the elements of the bbdml_list to plot. Defaults to all.
#'
#' @return Base R heatmap plot with row labels on left instead of right side
#'
#' @examples
#' plot_multi_bbdml(bbdml_list = bbs,pointsize=3,color = "FireTreatment",whichtaxa = 1:2)
#' library(patchwork)
#' bbdml_plot_1 / bbdml_plot_2
#'
#' @export




plot_multi_bbdml <-
  function(bbdml_list,color="none",obj_basename="bbdml_plot_",pointsize=1,whichtaxa=1:length(bbdml_list)){

    pal.discrete <- c("#c1593c","#688e52","#643d91","#894e7d","#477887","#12aa91","#705f36","#8997b2","#c4a113",
                      "#753c2b","#3c3e44","#b3bf2d","#82b2a4","#820616","#a17fc1","#262a8e","#abb5b5","#000000",
                      "#493829","#816C5B","#A9A18C","#613318","#855723","#B99C6B","#8F3B1B","#D57500","#DBCA69",
                      "#404F24","#668D3C","#BDD09F","#4E6172","#83929F","#A3ADB8")

    if(!class(color) %in% c("character","NULL") & length(color) == 1){
      stop("variable to color by must be character vector of length 1")
    }

    else if(!class(bbdml_list[[1]]) == "bbdml"){
      stop("bbdml_list must be a list of bbdml objects; typically the output from multi_bbdml()")
    }

    else if(!color %in% c(as.character(bbdml_list[[1]]$formula),"none")){
      stop("varible to color by is not found in bbdml model outputs")
    }

    else if(!class(whichtaxa) %in% c("numeric","integer") & length(whichtaxa) > 0){
      stop("'whichtaxa' should be a numeric vector with length > 0, indicating which significant taxa to plot. Default is all of them.")
    }

    else if(color == "none"){
      for(i in whichtaxa){
        p <- plot(bbdml_list[[i]],size=pointsize) + ggtitle(names(bbdml_list)[i])
        assign(paste0(obj_basename,i),p,envir = .GlobalEnv)
      }
    }

    else if(length(unique(color)) > length(pal.discrete)){
      stop("You probably have too many discrete levels of your grouping variable to color by. \nThis isn't a hard no, but if you want to make it work, you'll have to do it manually.")
    }

    else{
      for(i in 1:length(bbdml_list)){
        p <- plot(bbdml_list[[i]], color=color,size=pointsize) + ggtitle(names(bbdml_list)[i]) + scale_color_manual(values = pal.discrete) +
          theme_bw() +
          theme(plot.title = element_text(face="italic"), axis.title.y = element_text(face="bold"), axis.text.x = element_blank())
        assign(paste0(obj_basename,i),p,envir = .GlobalEnv)
      }
    }
  }



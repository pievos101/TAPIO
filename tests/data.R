e <- new.env()
utils::data("pbc2", package="iTAPIO", envir=e)
utils::data("iris", package="iTAPIO", envir=e)
stopifnot(is.data.frame(e$pbc2), nrow(e$pbc2)==1945L, ncol(e$pbc2)==19L,
          length(unique(e$pbc2$id))==312L, all(e$pbc2$event %in% 0:2),
          all(c("serBilir","serChol","albumin","alkaline","SGOT",
                "platelets","prothrombin","years","time") %in% names(e$pbc2)),
          identical(e$iris, datasets::iris))
cat("Bundled PBC2 and Iris data tests passed.\n")

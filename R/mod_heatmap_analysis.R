#' heatmap_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import stats
#' @import seriation
#' @importFrom grDevices rainbow pdf
#' @importFrom ecp e.divisive
#' @importFrom shiny NS tagList
mod_heatmap_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Settings", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          ##ui for analysis
          radioButtons(ns("roworcol"), "Row or column analysis?",
                        choices = list("Row", "Column"),
                        selected = "Row",inline = TRUE),
          helpText(h3("Method")),
          selectInput(ns("m_analysis"),"Choose",c("RunsTest","Phi",
                                              "CorrOrder","ARorder"),selected="RunsTest"),
          #Tabs pour afficher methode
          parameter_tabs <- tabsetPanel(
            id = ns("params"),
            type = "hidden",
            tabPanel("RunsTest",
                     helpText("Expliquer methode RunsTest"),

            ),
            tabPanel("Phi",
                     helpText("Expliquer methode Phi"),

            ),
            tabPanel("CorrOrder",
                     helpText("Expliquer methode CorrOrder"),

            ),
            tabPanel("ARorder",
                     helpText("Expliquer methode ARorder"),

            )
          ),
        helpText(h3("Split signifiant areas")),
        helpText("Splits are made by multiple change-point analysis from the e.divisive function from ecp"),
        column(6,
               numericInput(ns("ecp_minsize"), "MinClusterSize", value = 3, min = 1)
        ),
        column(6,
               numericInput(ns("siglvl"), "Significant level", value = 0.05, min = 0, max = 1)
        ),
        helpText("Use the results of e.divisive (transition) or the results of a student test on group means (mean)"),
        radioButtons(ns("methodSplit"), "Choose",
                     choices = list("transition", "mean"),
                     selected = "transition",inline = TRUE),
        parameter_tabs <- tabsetPanel(
          id = ns("params_msplit"),
          type = "hidden",
          tabPanel("transition",
                   ),
          tabPanel("mean",
                   numericInput(ns("conflvl"), "Confidence level of t.test", value = 0.95, min = 0, max = 1),
          )),
        helpText("Define the upper and lower bounds at which the average of a group is significant"),
        column(6,
               numericInput(ns("Ssup"), "Upper", value = 0),
               ),
        column(6,
               numericInput(ns("Sinf"), "Lower", value = 0),
        ),
        helpText("Display the significant groups (group) or their correspondence to the bounds (bound)"),
        radioButtons(ns("methodshowsplit"), "Choose",
                     choices = list("group", "bound"),
                     selected = "group",inline = TRUE),


        ##ui for heatmap
        helpText(h3("Heatmap")),
        column(6,
               selectInput(ns("color"),"Heatmap color",c("magma","inferno","plasma","viridis",
                                                         "cividis","rocket","mako","turbo"),selected="magma")
        ),
        column(6,
               selectInput(ns("bg_color"),"Background color",c("white","black"),selected="black")
        ),
        textInput(ns("legend_name"),"Enter a legend name",value = "legendname"),
        actionButton(ns("val_a3"), "valider"),
        width=12
      ),
      box(title = "Heatmap", status = "primary", solidHeader = TRUE, collapsible = FALSE,
          shinycssloaders::withSpinner(plotOutput(ns("ht_split"), height = "600px")),
          downloadButton(ns("down"), label = "Download the plot", style="color:#000000; display: block"),
          width=10
      ),

      box(title = "clusters", status = "success", solidHeader = TRUE,
          shinycssloaders::withSpinner(verbatimTextOutput(ns("clust"))),
          downloadButton(ns("down_data"), label = "Download clusters", style="color:#000000; display: block"),
          width=12)



    )

  )
}

#' heatmap_analysis Server Functions
#'
#' @noRd
mod_heatmap_analysis_server <- function(id,r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ##update UI
    observeEvent(input$m_analysis, {
      updateTabsetPanel(inputId = "params", selected = input$m_analysis)
    })
    observeEvent(input$methodSplit, {
      updateTabsetPanel(inputId = "params_msplit", selected = input$methodSplit)
    })

    ##color
    fun_color <- reactive({
      switch(input$color,
             "magma" = viridis::magma(256),
             "inferno" = viridis::inferno(256),
             "plasma" = viridis::plasma(256),
             "viridis" = viridis::viridis(256),
             "cividis" = viridis::cividis(256),
             "rocket" = viridis::rocket(256),
             "mako" = viridis::mako(256),
             "turbo" = viridis::turbo(256)
      )
    })

    aff_color <- reactive({
      if(input$bg_color == "white"){
        return("black")
      }else{
        return("white")
      }
    })

    ##server

    mat <- reactive({
      if(input$roworcol == "Row"){
        return(as.matrix(r$M_ser()))
      }else{
        return(t(as.matrix(r$M_ser())))
      }
    })

    matDist <- reactive({
      if(input$roworcol == "Row"){
        return(as.matrix(r$distm_ml()))
      }else{
        return(as.matrix(r$distm_mc()))
      }
    })

    ser <- reactive({
      if(input$roworcol == "Row"){
        return(r$HC_l())
      }else{
        return(r$HC_c())
      }
    })


    vecAnaly <- reactive({
      if(input$m_analysis == "RunsTest"){
        #fait le Run test en appelant un script c++
        RunTV <- cpprunstest(mat(),"two.sided")
        #traitement des resultats
        RunTV[RunTV > 0] <- 0
        vec <-(-1)*RunTV
      }else if(input$m_analysis == "Phi"){
        matrice <- mat()
        #permet de calculer le coefficient Phi de la premiere ligne avec la suivante
        T <- table(matrice[1,], matrice[2,])#fait un tableau de contingence
        Tab2 = T/sum(T)#fait la moyenne
        a = Tab2[1, 1]
        b = Tab2[1, 2]
        c = Tab2[2, 1]
        d = Tab2[2, 2]
        Phi = (a - (a + b) * (a + c))/sqrt((a + b) * (c + d) * (a + c) * (b + d)) #calcul de Phi
        Vphi <- vector()
        Vphi <-c(Phi)
        #permet de calculer le coefficient Phi de la deuxieme ligne a l'avant derniere ligne.
        Vphi <- append(Vphi, cppcorrNeighbor(matrice))
        #permet de calculer le coefficient Phi de la derniere ligne avec la precedente
        T <- table(matrice[nrow(matrice)-1,], matrice[nrow(matrice),])
        Tab2 = T/sum(T)
        a = Tab2[1, 1]
        b = Tab2[1, 2]
        c = Tab2[2, 1]
        d = Tab2[2, 2]
        Phi = (a - (a + b) * (a + c))/sqrt((a + b) * (c + d) * (a + c) * (b + d))
        Vphi <- append(Vphi, Phi)
      }else if(input$m_analysis == "CorrOrder"){
        dMat <- matDist()
        serdMat <- dMat[seriation::get_order(ser()), seriation::get_order(ser())]
        mat <- matrix(1, ncol(serdMat),ncol(serdMat))
        mat[which(row(mat)>col(mat))] <- -1
        serdMat2 <- serdMat*mat
        serdMat3 <- serdMat2 - serdMat2[,1]
        OrdVec2 <- as.matrix(seq(0,(nrow(dMat)-1)))
        cor <- cor(OrdVec2, t(serdMat3), method = "spearman") #correlation
        vec<- abs(as.vector(cor))
      }else{
        TMdMat <- matDist()
        TMdMat <- TMdMat[seriation::get_order(ser()), seriation::get_order(ser())]
        Tmat <- matrix(1, ncol(TMdMat),ncol(TMdMat))
        Tmat[which(row(Tmat)>col(Tmat))] <- -1
        TMdMat2 <- TMdMat*Tmat
        TMdMat3 <- TMdMat2 - TMdMat2[,1]

        Viar <- cppARorder(TMdMat3)
        vec <- sqrt(abs(Viar))
      }
    })


    #e.divisive fonction du package ECP, analyse point de transition
    Split <- reactive({
      sp <- ecp::e.divisive(as.matrix(vecAnaly()), min.size=input$ecp_minsize, sig.lvl=input$siglvl)$cluster
    })

    #test de student t.test() de chaque 2 groupes successifs trouver par l'analyse des points de transitions
    fsplit <- reactive({
      if(input$methodSplit == "mean"){
        VecIndStruc <- vecAnaly()
        i = 1
        Msplit <- Split()
        while(i<nlevels(as.factor(Msplit))){
          res <- stats::t.test(vecAnaly()[which(Msplit[]==i)],vecAnaly()[which(Msplit[]==i+1)])
          if(res$p.value > 1-input$conflvl){ #si les moyennes ne sont pas significativement differentes on regroupe les 2 groupes
            for(j in i+2:nlevels(as.factor(Msplit))-1){
              Msplit[which(Msplit[]==j)] <- j-1
            }
            if(i!=1){i = i-1}
          }
          else i=i+1
        }
        FSplit <- as.factor(Msplit)
      } else {
        sp <- Split()
        FSplit <- as.factor(sp)
        }
      return(FSplit)
    })

    group_r <- reactive({
      names <- rownames(r$M_ser()) #on recupere les noms des lignes du data.frame
      gr <- list()
      for(i in unique(fsplit())){
        if(i!=0){
          gr[[i]] <- names[which(fsplit()==i)]
        }
      }
      return(gr)
    })

    group_c <- reactive({
      names <- colnames(r$M_ser()) #on recupere les noms des colonnes du data.frame
      gr <- list()
      for(i in unique(fsplit())){
        if(i!=0){
          gr[[i]] <- names[which(fsplit()==i)]
        }
      }
      return(gr)
    })

    #Fait la moyenne des indices de structuration des groupes de l'etape precedente
    vmean <- reactive ({
      vMean <- numeric()
      for(k in levels(fsplit())){
        v <- vecAnaly()[which(fsplit()[]==k)]
        vMean[k] <- mean(v)
      }
      return(vMean)
    })

    ###Fait vecteur pour l'annotation soit les groupes soit les seuils
    vgroup <- reactive({
      vMean <- vmean()
      if(input$methodshowsplit == "bound"){
        vGroup <- character(length = length(vecAnaly()))
        for(l in levels(fsplit())){
          if(vMean[l] >=input$Ssup){
            vGroup[which(fsplit()[]==l)] <- "sup"
          }else if(vMean[l] <=input$Sinf){
            vGroup[which(fsplit()[]==l)] <- "inf"
          }else{
            vGroup[which(fsplit()[]==l)] <- NA
          }
        }
        return(vGroup)
      }else {
        vGroup <- numeric(length = length(vecAnaly()))
        indice = 1;
        for(l in levels(fsplit())){
          if(vMean[l]<input$Ssup && vMean[l]>input$Sinf) {
            vGroup[which(fsplit()[]==l)] <- NA
          }
          else{vGroup[which(fsplit()[]==l)]<- indice
            indice = indice + 1}
        }
        return(vGroup)
      }
    })


    ##heatmap
    anno_r <- reactive({
      vGroup <- vgroup()
      if(is.numeric(vGroup)){
        #Permet d'inverser des indices dans le vecteur vGroup et de faire une palette de couleur
        #(Avoir des couleurs differentes pour des clusters a cote dans l'annotation)
        VmA <- c(1:length(vGroup))
        mrep <- c(length(vGroup),0)
        vrep <- rep(mrep,length(vGroup))
        for(j in VmA){vGroup[which(vGroup[]==j)]<-j+vrep[j]}
        pal = circlize::colorRamp2(as.integer(levels(as.factor(vGroup))),grDevices::rainbow(n = nlevels(as.factor(vGroup))))
      } else{
        pal = c("sup"= "green", "inf" = "red") ##paramètre couleur
      }
      #definition de l'annotation des clusters des lignes et permet d'afficher la valeur de l'analyse de la ligne
      RowAN <- rowAnnotation(Cluster = anno_simple(vGroup, na_col=input$bg_color, col = pal),
                             foo = anno_points(vecAnaly(), gp = gpar(col = ifelse(vecAnaly() >=input$Ssup,"chocolate1",
                                                                                   ifelse(vecAnaly()<=input$Sinf,"turquoise1",aff_color()))),
                                               size=unit(1,"mm"),axis_param = list(gp = gpar(col=aff_color())),),
                             annotation_name_gp = gpar(col =aff_color()))
      return(RowAN)
    })

    anno_c <- reactive({
      vGroup <- vgroup()
      if(is.numeric(vGroup)){
        #Permet d'inverser des indices dans le vecteur vGroup et de faire une palette de couleur
        #(Avoir des couleurs differentes pour des clusters a cote dans l'annotation)
        VmA <- c(1:length(vGroup))
        mrep <- c(length(vGroup),0)
        vrep <- rep(mrep,length(vGroup))
        for(j in VmA){vGroup[which(vGroup[]==j)]<-j+vrep[j]}
        pal = circlize::colorRamp2(as.integer(levels(as.factor(vGroup))),grDevices::rainbow(n = nlevels(as.factor(vGroup))))
      } else{
        pal = c("sup"= "green", "inf" = "red") ##paramètre couleur
      }
      #definition de l'annotation des clusters des lignes et permet d'afficher la valeur de l'analyse de la ligne
      ColumnAN <- HeatmapAnnotation(Cluster = anno_simple(vGroup, na_col=input$bg_color, col = pal),
                             foo = anno_points(vecAnaly(), gp = gpar(col = ifelse(vecAnaly() >=input$Ssup,"chocolate1",
                                                                                  ifelse(vecAnaly()<=input$Sinf,"turquoise1",aff_color()))),
                                               size=unit(1,"mm"),axis_param = list(gp = gpar(col=aff_color())),),
                             annotation_name_gp = gpar(col =aff_color()))
      return(ColumnAN)
    })


    htsplot <- eventReactive(input$val_a3,{
      req(r$M_ser)
      if(input$roworcol == "Row"){
        #multiplication du vecteur d'analyse de la matrice avec la matrice permettant la coloration
        VecIndStruc <- vecAnaly()
        matrice <- as.matrix(VecIndStruc * r$M_ser())
        #creation de la Heatmap
        HM <- Heatmap(matrice, name = input$legend_name,
                      cluster_rows = FALSE, cluster_columns = FALSE,
                      col = fun_color(),
                      column_names_max_height = max_text_width(colnames(matrice)),
                      show_row_names = FALSE,
                      row_names_gp = gpar(fontsize = 0.2 + 1/log10(nrow(matrice)),
                                          col=aff_color()),
                      column_names_gp = gpar(fontsize = 0.2 + 1/log10(ncol(matrice)),
                                             col=aff_color()),
                      #pour split
                      border = aff_color(),
                      heatmap_legend_param = list(title_gp = gpar(col=aff_color()), labels_gp = gpar(col=aff_color())),
                      row_split = fsplit(),
                      row_title = NULL, row_gap = unit(0,"mm"),
                      left_annotation = anno_r(),
        )
        #dessine la Heatmap avec en fond la couleur choisie
        draw(HM, background = input$bg_color)
        #Permet de faire les cadres des groupes >Ssup ou <Sinf
        for(s in levels(fsplit())){
          decorate_annotation("foo",{grid.lines(unit(c(input$Ssup,input$Ssup),"native"),c(0,1), gp = gpar(col="yellow"))
            grid.lines(unit(c(input$Sinf,input$Sinf),"native"),c(0,1), gp = gpar(col="yellow"))
          }, slice = s)
        }
      }else{
        #multiplication du vecteur d'analyse de la matrice avec la matrice permettant la coloration
        VecIndStruc <- vecAnaly()
        matrice <- t(VecIndStruc* t(as.matrix(r$M_ser())))
        #creation de la Heatmap
        HM <- Heatmap(matrice, name = input$legend_name,
                      cluster_rows = FALSE, cluster_columns = FALSE,
                      col = fun_color(),
                      column_names_max_height = max_text_width(colnames(matrice)),
                      show_row_names = FALSE,
                      row_names_gp = gpar(fontsize = 0.2 + 1/log10(nrow(matrice)),
                                          col=aff_color()),
                      column_names_gp = gpar(fontsize = 0.2 + 1/log10(ncol(matrice)),
                                             col=aff_color()),
                      #pour split
                      border = aff_color(),
                      heatmap_legend_param = list(title_gp = gpar(col=aff_color()), labels_gp = gpar(col=aff_color())),
                      column_split = fsplit(),
                      column_title = NULL, column_gap = unit(0,"mm"),
                      top_annotation = anno_c(),
        )
        #dessine la Heatmap avec en fond la couleur choisie
        draw(HM, background = input$bg_color)
        #Permet de faire les cadres des groupes >Ssup ou <Sinf
        for(s in levels(fsplit())){
          decorate_annotation("foo",{grid.lines(c(0,1), unit(c(input$Ssup,input$Ssup),"native"), gp = gpar(col="yellow"))
            grid.lines(c(0,1),unit(c(input$Sinf,input$Sinf),"native"), gp = gpar(col="yellow"))
          }, slice = s)
        }
      }
    })

    group <- eventReactive(input$val_a3,{
      if(input$roworcol == "Row"){
        return(group_r())
      }else{
        return(group_c())
      }
    })



    ##output
    output$ht_split <- renderPlot({
      req(htsplot)
      htsplot()
    })

    output$clust <- renderPrint({
      group()
    })

    output$down <- downloadHandler(
      filename =  function() {
        paste(input$legend_name,"pdf",sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        grDevices::pdf(file) # open the pdf device
        VecIndStruc <- vecAnaly()
        if(input$roworcol == "Row"){
          matrice <- as.matrix(VecIndStruc * r$M_ser())
          #creation de la Heatmap
          HM <- Heatmap(matrice, name = input$legend_name,
                        cluster_rows = FALSE, cluster_columns = FALSE,
                        col = fun_color(),
                        column_names_max_height = max_text_width(colnames(matrice)),
                        show_row_names = FALSE,
                        row_names_gp = gpar(fontsize = 0.2 + 1/log10(nrow(matrice)),
                                            col=aff_color()),
                        column_names_gp = gpar(fontsize = 0.2 + 1/log10(ncol(matrice)),
                                               col=aff_color()),
                        #pour split
                        border = aff_color(),
                        heatmap_legend_param = list(title_gp = gpar(col=aff_color()), labels_gp = gpar(col=aff_color())),
                        row_split = fsplit(),
                        row_title = NULL, row_gap = unit(0,"mm"),
                        left_annotation = anno_r(),
          )
          #dessine la Heatmap avec en fond la couleur choisie
          draw(HM, background = input$bg_color)
          #Permet de faire les cadres des groupes >Ssup ou <Sinf
          for(s in levels(fsplit())){
            decorate_annotation("foo",{grid.lines(unit(c(input$Ssup,input$Ssup),"native"),c(0,1), gp = gpar(col="yellow"))
              grid.lines(unit(c(input$Sinf,input$Sinf),"native"),c(0,1), gp = gpar(col="yellow"))
            }, slice = s)
          }
        }else{
          #multiplication du vecteur d'analyse de la matrice avec la matrice permettant la coloration
          VecIndStruc <- vecAnaly()
          matrice <- t(VecIndStruc* t(as.matrix(r$M_ser())))
          #creation de la Heatmap
          HM <- Heatmap(matrice, name = input$legend_name,
                        cluster_rows = FALSE, cluster_columns = FALSE,
                        col = fun_color(),
                        column_names_max_height = max_text_width(colnames(matrice)),
                        show_row_names = FALSE,
                        row_names_gp = gpar(fontsize = 0.2 + 1/log10(nrow(matrice)),
                                            col=aff_color()),
                        column_names_gp = gpar(fontsize = 0.2 + 1/log10(ncol(matrice)),
                                               col=aff_color()),
                        #pour split
                        border = aff_color(),
                        heatmap_legend_param = list(title_gp = gpar(col=aff_color()), labels_gp = gpar(col=aff_color())),
                        column_split = fsplit(),
                        column_title = NULL, column_gap = unit(0,"mm"),
                        top_annotation = anno_c(),
          )
          #dessine la Heatmap avec en fond la couleur choisie
          draw(HM, background = input$bg_color)
          #Permet de faire les cadres des groupes >Ssup ou <Sinf
          for(s in levels(fsplit())){
            decorate_annotation("foo",{grid.lines(c(0,1), unit(c(input$Ssup,input$Ssup),"native"), gp = gpar(col="yellow"))
              grid.lines(c(0,1),unit(c(input$Sinf,input$Sinf),"native"), gp = gpar(col="yellow"))
            }, slice = s)
          }
        }
        grDevices::dev.off()  # turn the device off
      })

    output$down_data <- downloadHandler(
      filename =  function() {
        paste0("clusters_",input$legend_name,".txt")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        sink(file)
        print(group())
        sink()
      })

  })
}

## To be copied in the UI
# mod_heatmap_analysis_ui("heatmap_analysis_1")

## To be copied in the server
# mod_heatmap_analysis_server("heatmap_analysis_1",r=r)

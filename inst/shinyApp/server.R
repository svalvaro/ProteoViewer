function(input, output) {

    #### Load Demo Data  & Notifications####

    source("server/loadDemoData.R", local = TRUE)

    #### Upload the Proteomics Table ####

    source("server/uploadProteomicsData.R", local = TRUE)

    #### Proteins to select ####

    source("server/proteinsToSelect.R", local = TRUE)

    #### Experiment to select ####

    source("server/experimentToSelect.R", local = TRUE)

    #### Comparison Selector ####

    source("server/comparisonSelector.R", local = TRUE)

    #### Create table Protein No Conditions ####

    source("server/tableNoCondition.R", local = TRUE)

    #### Render proteinImage ####

    source("server/renderImage.R", local = TRUE)

    #### Legend PTMs ####

    source("server/legendPTMs.R", local = TRUE)

    #### Render Comparison One ####

    source("server/renderComparisonOne.R", local = TRUE)

    #### Render Comparison Two ####

    source("server/renderComparisonTwo.R", local = TRUE)

    #### Render the Intensity Legend ####

    source("server/renderIntensityLegend.R", local = TRUE)

    #### Experiment Design ####

    source("server/experimentDesign.R", local = TRUE)

    #### Comparisons regarding experiment design ####

    source("server/comparisonsFromExperimentDesign.R", local = TRUE)

    #### Table with peptide intensities ####

    source("server/tablePeptidesIntensities.R", local = TRUE)

    #### Plot Coverage ####

    source("server/coveragePlot.R", local = TRUE)

    #### User Interface Reactive ####

        # Side Bar

    source("server/dynamicUI/sideBar.R", local = TRUE)

        # UI protein images

    source("server/dynamicUI/bodyProteinImages.R", local = TRUE)

        # UI Coverage

    source("server/dynamicUI/bodyProteinCoverage.R", local = TRUE)

        # UI PTMS

    source("server/dynamicUI/bodyPTMs.R", local = TRUE)
}

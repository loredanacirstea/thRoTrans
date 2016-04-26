My Graduation Thesis
=========

###_Algorithm of translation in the Romanian language for the Terminologia Histologica and APIs for a controlled vocabulary_


thRoTrans is part of my graduation thesis project for the University of Medicine and Pharmacy "Carol Davila", titled: 
 - En: “Algorithm of translation in the Romanian language for the Terminologia Histologica and APIs for a controlled vocabulary”
 - Ro: "Algoritm de traducere în limba română pentru Terminologia Histologica și utilitare de mentenanță pentru un vocabular controlat"

Field: Semantic Bioinformatics - Semantic Web in Medicine

Principles and Concepts
-----------------------

 - transparency in the process of creation, collaboration, evaluation:
    - GitHub (code)
    - Google Docs (paper, methods, suggestions, official documents etc.)
    - Google Groups
 - Open Data
 - Semantic Web
 - access to ontology maintenance and data indexing on the targeted subjects directly from R - for researchers 
 - qualitative analysis in R

Short Description
------------------
 - R code:
    - terminology maintenance and browsing, with a simple User Interface included
    - algorithm for creating a computable translation in Romanian, extendable to any other Latin language, exemplified on 500 histological concepts
 - computability:
    - no linking words
    - only one term per concept (no synonyms for the terms)
    - only one translation for each Latin word / expression (no synonyms for words / expressions)
    - 1:1 ratio between the Latin words and the new language words for each term
    - words similar to Latin will be preferred
    - terms with fewer words will be preferred.
 - suggestions for improving Terminologia Histologica from a morpho-semantic point of view.
 - issues with Terminologia Histologica as it was published at the time when the thesis was written (final version - August 2014)

 - for code testing (Code/ontobrowse.r file):
    - browsing functionality:
        - load Code/Code.Rproj in RStudio; it will automatically load workspace data (Code/onto2.RData)
        - type ontobrowse() in the RStudio console and follow the instructions
    -  without loading preexisting data, make sure to: ontoinstall() and ontoinitialize() first
    - the translation algorithm does not have a simple interface for now; the process steps are commented at the end of the Code/ontobrowse.r file, starting with the 1943 line: `newTranslation(sample500);`; process documented in detail in https://drive.google.com/file/d/0B1GtDMuhK2dnZlhIaEJRUkEtdFU/edit?usp=sharing (page 34), unfortunately only available in Romanian at this moment.

Links
-------

 - GitHub thesis repo: https://github.com/loredanacirstea/thRoTrans
 - R code used in my thesis: https://github.com/loredanacirstea/thRoTrans/blob/master/Code/ontobrowse.r
 - Video demos of the R package:
    - simple browse function: https://www.youtube.com/watch?v=-wN2op1u7s8
    - semi-automatic translation in a new language - user perspective: https://www.youtube.com/watch?v=-BprxBS7qiY#t=10
 - Summary (English): (in work) https://docs.google.com/document/d/1ze-SDLtB4m6dtVfnq6zqriMVXIoaGmQL2SZO3lCgL8Y/edit?usp=sharing 
 - Editable thesis (Romanian) (with versioning): https://docs.google.com/document/d/19GR2oOqCHePJ5lNXs3runAkDD-INixnhkKej4mqjtwc/edit?usp=sharing 
 - Final thesis (Romanian) (.pdf): https://drive.google.com/file/d/0B1GtDMuhK2dnZlhIaEJRUkEtdFU/edit?usp=sharing 
 - Official documents, data, intermediary work: https://drive.google.com/folderview?id=0B1GtDMuhK2dnZHNpRmhaaElibVU&usp=sharing
 - Transparent collaboration and organisational issues: https://groups.google.com/forum/#!categories/public-witness/




License
=========


Data
----

ODbL : http://opendatacommons.org/licenses/odbl/

DbCL : http://opendatacommons.org/licenses/dbcl/1-0/


Code
----

GPL v3 https://www.gnu.org/copyleft/gpl.html

UN_HIV_Circumcision.csv:
	curl -o UN_HIV_Circumcision.csv https://raw.githubusercontent.com/LinRZhou/BIOS-611-LZhou/master/UN_HIV_Circumcision.csv

UN_HIV_Incidence.csv:
	curl -o UN_HIV_Incidence.csv https://raw.githubusercontent.com/LinRZhou/BIOS-611-LZhou/master/UN_HIV_Incidence.csv

UN_HIV_Prevalence.csv:
	curl -o UN_HIV_Prevalence.csv https://raw.githubusercontent.com/LinRZhou/BIOS-611-LZhou/master/UN_HIV_Prevalence.csv

UN_PMTCT_Infection.csv:
	curl -o UN_PMTCT_Infection.csv https://raw.githubusercontent.com/LinRZhou/BIOS-611-LZhou/master/UN_PMTCT_Infection.csv

UN_ART_Coverage.csv:
	curl -o UN_ART_Coverage.csv https://raw.githubusercontent.com/LinRZhou/BIOS-611-LZhou/master/UN_ART_Coverage.csv

p2_HIV_Prevalence_Sex.png: UN_AIDS_Prevalence_Project_2.R UN_HIV_Prevalence.csv
	Rscript UN_AIDS_Prevalence_Project_2.R

p2_HIV_Prevalence.png: UN_AIDS_Prevalence_Project_2.R UN_HIV_Prevalence.csv
	Rscript UN_AIDS_Prevalence_Project_2.R

p2_HIV_Incidence.png: UN_AIDS_Prevalence_Incidence_Project_2.R UN_HIV_Prevalence.csv UN_HIV_Incidence.csv
	Rscript UN_AIDS_Prevalence_Incidence_Project_2.R

p2_HIV_Transmission.png: UN_AIDS_Prevalence_Incidence_Project_2.R UN_HIV_Prevalence.csv UN_HIV_Incidence.csv
	Rscript UN_AIDS_Prevalence_Incidence_Project_2.R

p2_HIV_Circum_Transmission.png: UN_AIDS_Circumcision.R UN_HIV_Prevalence.csv UN_HIV_Incidence.csv UN_HIV_Circumcision.csv
	Rscript UN_AIDS_Circumcision.R

p2_HIV_Circum_Incidence.png: UN_AIDS_Circumcision.R UN_HIV_Prevalence.csv UN_HIV_Incidence.csv UN_HIV_Circumcision.csv
	Rscript UN_AIDS_Circumcision.R

p2_HIV_ART_PMTCT.png: UN_AIDS_ART_and_PMTCT.R UN_ART_Coverage.csv UN_PMTCT_Infection.csv
	Rscript UN_AIDS_ART_and_PMTCT.R

Linran_Zhou_Project_2.html: Linran_Zhou_Project_2.Rmd p2_HIV_Incidence.png p2_HIV_Transmission.png p2_HIV_Prevalence.png p2_HIV_Prevalence_Sex.png p2_HIV_Circum_Incidence.png p2_HIV_Circum_Transmission.png p2_HIV_ART_PMTCT.png
	Rscript -e 'rmarkdown::render("Linran_Zhou_Project_2.Rmd")'

makefile2graph: 
	git clone https://github.com/lindenb/makefile2graph.git && \
	cd makefile2graph && \
	make

make.PHONY: Project_2_workflow.png


Project_2_workflow.png: makefile2graph
	make Linran_Zhou_Project_2.html -Bnd | ./makefile2graph/make2graph | dot -Tpng -o Project_2_workflow.png

.PHONY: run_all

run_all: Linran_Zhou_Project_2.html Project_2_workflow.png



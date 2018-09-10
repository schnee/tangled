data: data/tangled.csv

png: docs/tangled.png
	
plotly: docs/tg.html

data/tangled.csv: FORCE
	Rscript get_edgelist.R

docs/tangled.png: data
	Rscript tangled-local.R
	
docs/tg.html: data
	Rscript plotly.R 

publish: png plotly
	git checkout master
	git commit -m "updated" ./docs/ ./data/
	git push
	git checkout gh-pages
	git merge -Xtheirs master
	git push
	git checkout master
	
FORCE:

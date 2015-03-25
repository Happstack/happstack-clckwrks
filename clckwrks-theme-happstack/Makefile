LESSC=./node_modules/less/bin/lessc

data/css/happstack-theme.css: happstack-theme.less
	${LESSC} happstack-theme.less > data/css/happstack-theme.css
	${LESSC} --compress happstack-theme.less > data/css/happstack-theme.min.css

build:
	docker build -t webcalc .
run:
	docker run -d -i -t -p 3000:3000 webcalc

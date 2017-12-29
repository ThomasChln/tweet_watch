all: deamon gui
deamon:
	nohup docker-compose up &
gui:
	docker-compose -f dash/docker-compose.yml up
torsocks:
	docker-compose -f torsocks/docker-compose.yml up
clean:
	rm -rf .httr-oauth .gitignore geo.rds m_tws.rds dash/tw.db dash/geo.rds nohup.out log.txt
	docker rm -f twit_twit_1

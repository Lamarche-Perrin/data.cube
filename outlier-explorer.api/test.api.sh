## Test format_comments
curl -s http://localhost:8000/format_comments -d '{"data":[{"article_tags":["keyword1","keyword2"],"data_published":"2018-02-22 16:48:23","comments":[{"author_id":"user1","time_stamp":"2018-02-23 10:45:01"},{"author_id":"user2","time_stamp":"2018-02-22 22:25:58"}]},{"article_tags":["keyword1","keyword2","keyword3"],"data_published":"2018-02-22 16:48:23","comments":[{"author_id":"user1","time_stamp":"2018-02-23 10:45:01"},{"author_id":"user2","time_stamp":"2018-02-22 22:25:58"},{"author_id":"user2","time_stamp":"2018-02-23 22:25:58"}]}],"param":{"time":{"reference":"comment","resolution":"day"}}}'

## Test outliers
curl -s http://localhost:8000/outliers -d '{"data":[{"user":"168820","topic":"keyword1","time":"2018-02-23","obs":0.5},{"user":"168820","topic":"keyword2","time":"2018-02-23","obs":0.5},{"user":"985002","topic":"keyword1","time":"2018-02-22","obs":0.5},{"user":"985002","topic":"keyword2","time":"2018-02-22","obs":0.5}],"dataset":null,"param":{"select":[{"dim":"user","select":"some","list":["985002"]},{"dim":"topic","select":"some","head":5},{"dim":"time","select":"all"}],"normalise":["topic","time"],"stat.test":{"type":"poisson","threshold":3}}}'

## Compare API results to APP results
curl -s http://localhost:8000/outliers -d '{"dataset":"guardian.2016","param":{"select":[{"dim":"topic","select":"some","head":5},{"dim":"time","select":"all"}],"normalise":["topic","time"],"stat.test":{"type":"poisson","threshold":3}}}'

## Test full pipeline
curl -H "Content-Type: application/json" -s https://www.fcg-net.org/penelope/data/comment_structure -d '{"collection":"GuardianArticles", "start_date":"2017-01-02T00:00:00.000Z", "end_date":"2017-01-30T00:00:00.000Z", "limit":1}' > guardian.raw

echo '{"data":'$(< guardian.raw)',"param":{"time":{"reference":"comment","resolution":"day"}}}' | curl -s http://localhost:8000/format_comments -d @- > guardian.cube

echo '{"data":'$(< guardian.cube)',"param":{"select":[{"dim":"time","select":"all"},{"dim":"topic","select":"some","head":5},{"dim":"user","select":"some","head":30}],"normalise":["topic","time"],"stat.test":{"type":"poisson","threshold":3}}}' | curl -s http://localhost:8000/outliers -d @- > guardian.outliers


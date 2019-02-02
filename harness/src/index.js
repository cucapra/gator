const express = require('express');
const bodyParser = require('body-parser');
const path = require('path');

const app = express();
const port = 8080;

app.use(bodyParser.urlencoded({ extended: true }))
app.use(bodyParser.json());

app.use('/', express.static(path.join(__dirname, '..', '/dist')));
app.post("/senddata", (req, res) => {
    console.log(req.body);
    res.send("response");
});


app.listen(port, () => console.log(`Example app listening on port ${port}!`))
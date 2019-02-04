const express = require('express');
const bodyParser = require('body-parser');
const path = require('path');

const app = express();
const port = 8080;

app.use(bodyParser.urlencoded({ extended: true }))
app.use(bodyParser.json());

app.use('/', express.static(path.join(__dirname, '..', '/dist')));
app.post("/senddata", (req, res) => {
    const data = req.body;
    const fpsData = data.fpsData;
    const avgFps = fpsData.reduce((a, b) => a + b, 0) / fpsData.length;
    console.log("Average FPS: ", avgFps);
    res.send("response");
});


app.listen(port, () => console.log(`Example app listening on port ${port}!`))
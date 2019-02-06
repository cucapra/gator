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
    const fpsData = data.fpsData.slice(1);
    const avgFps = fpsData.reduce((a, b) => a + b, 0) / fpsData.length;
    const minFps = Math.min(...fpsData), maxFps = Math.max(...fpsData);
    const varFps = fpsData.reduce((a, b) => a + (b - avgFps) * (b + avgFps), 0) / fpsData.length;
    const stdFps = Math.sqrt(varFps);
    console.log("Average FPS: ", avgFps);
    console.log("Min FPS: ", minFps);
    console.log("Max FPS: ", maxFps);
    console.log("Std. FPS: ", stdFps);
    console.log("First 10 frames: ", fpsData.slice(0, 10));
    res.send("response");
});


app.listen(port, () => console.log(`Example app listening on port ${port}!`))
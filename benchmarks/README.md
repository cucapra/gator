# Benchmarking and Visualization
Run
`pip install -r requirements.txt`
to install necessary requirements.

It's also necessary to install chrome driver separately: http://chromedriver.chromium.org/

To run the benchmarks, simply run `python main.py`

The data is written into `data/run_<time>.json` and is mirrored at `data/run.json`.

To run `visualize.py`, simply run `python visualize.py <filename>`. If no argument is passed, it'll visualize the most recent data generated (ie: `data/run.json`). Otherwise, it'll visualize the data in the json file.

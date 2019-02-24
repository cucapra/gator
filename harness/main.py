import subprocess
from threading import Thread
from selenium import webdriver
from urllib.parse import urlencode
from selenium.webdriver.support.ui import WebDriverWait

PORT = 8080  # Should be the same as src/index.js


node_process = None


def openNode():
    global node_process
    node_process = subprocess.Popen(["node", "src/index.js"])


node_thread = Thread(target=openNode)
node_thread.start()

default_args = {'time': 10}
phong = {'name': 'phong',
         'shaders': ['raw', 'default', 'default', 'raw'],
         'args': {
             'num_objects': 40,
         }
         }
texture = {'name': 'texture',
           'shaders': ['raw', 'default', 'default', 'raw'],
           'args': {}}
shadow_map = {'name': 'shadow_map',
              'shaders': ['raw', 'default', 'default', 'raw'],
              'args': {}}
reflection = {'name': 'reflection',
<<<<<<< HEAD
              'shaders': ['raw', 'default'],
=======
              'shaders': ['default', 'raw', 'default', 'raw'],
>>>>>>> c8b4d22abd3a4ae8a2ddfa21df2ac9fd84df614d
              'args': {}}

# benchmarks = [phong, shadow_map, texture, reflection]
benchmarks = [shadow_map]
browser = webdriver.Chrome()
for bench in benchmarks:
    print(f"Running {bench['name']}")
    subprocess.run(
        ["parcel", "build", f"benchmarks/{bench['name']}/index.html"])

    for shader in bench['shaders']:
        print(f"Running with {shader} shader")
        bench_args = {**default_args, **bench['args']}
        bench_args['shader'] = shader
        bench_args['bench_name'] = bench['name']
        browser.get(f"localhost:{PORT}?{urlencode(bench_args)}")

        def found_finish_div():
            try:
                browser.find_element_by_id('finish')
                return True
            except:
                return False

        WebDriverWait(browser, bench_args['time']*10).until(
            lambda session: found_finish_div())
browser.quit()

node_process.kill()
def init_index(path, filename):
    """Initialize index.html on appropriate subfolder"""
    # default HTML messages
    message_head = "<!DOCTYPE html>\n<html lang=\"en\"><head><meta charset=\"UTF-8\"><title>Linguine</title></head><body><canvas width=\"700px\" height=\"500px\" id=\"c\"></canvas><script src=\"dist/"
    message_tail = ".bundle.js\" type=\"text/javascript\"></script></body></html>"
    message = message_head + filename + message_tail
    
    # write into HTML file
    file = open(path + "/index.html", "w+") 
    file.write(json) 
    file.close() 

if __name__ == '__main__':
    path = sys.argv[1]
    filename = sys.argv[2]
    init_index(path, filename)
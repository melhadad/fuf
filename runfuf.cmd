docker run -it -p 8888:8888 -v c:/notebooks:/root/notebooks fuf:5.4 jupyter notebook --no-browser --NotebookApp.token='' --ip '*' --port 8888

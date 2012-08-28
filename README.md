Emacs Realtime Markdown Viewer
===============================
Emacs readltime markdown viewer with websocket.el and Amon2.


Requirements
------------
* Emacs 23 or higher. (version 24 is better than version 23)
* [websocket.el](https://github.com/ahyatt/emacs-websocket)
* [Amon2](https://github.com/tokuhirom/Amon) 3.5 or higher
* [Amon2::Lite](https://github.com/tokuhirom/Amon2-Lite)


Demonstration
-------------
* [youtube](http://www.youtube.com/watch?feature=player_embedded&v=qnoMo0ynyZo)


How to Run
----------
````
 Run server
 % plackup realtime-md-server.psgi

 Browser
   Access http://localhost:5000 (default)

 Client(Emacs)
   M-x realtime-markdown-viewer-mode

````


Limitation
----------
* There are some bugs.

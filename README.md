# Emacs Realtime Markdown Viewer

Emacs Realtime Markdown Viewer with websocket.el and Amon2.


## Requirements
* Emacs 24 or higher.
* Latest [websocket.el](https://github.com/ahyatt/emacs-websocket)
    - websocket.el older than 2012/SEP/01 does not support multibyte characters
* [Amon2::Lite](https://github.com/tokuhirom/Amon2-Lite)


## Demonstration
* [youtube](http://www.youtube.com/watch?feature=player_embedded&v=qnoMo0ynyZo)


## How to Run

### Select language
You can select WebApp implementation, Perl(Amon2) or Ruby(Sinatra).
Default is Perl, because I'm beginner of Ruby programming.

```lisp
;; Use Sinatra App
(setq rtmv:lang 'ruby)
```

### Perl Dependency Setting

```
% cpanm --installdeps .
```

### Ruby Setting

```
% bundle install --path=vender/bundler
```

### Client
Run WebApp and connect to Web application

```
M-x realtime-markdown-viewer-mode
```


### Browser
Access to http://0.0.0.0:5021/


Limitation
----------
* There are some bugs.

#!/usr/bin/env ruby
# -*- coding:utf-8 -*-

require 'sinatra'
require 'sinatra-websocket'
require 'redcarpet'
require 'coderay'

set :server, 'thin'
set :sockets, []

get '/' do
  erb :index
end

class HTMLwithCoderay < Redcarpet::Render::Safe
  def block_code(code, language)
    begin
      CodeRay.scan(code, language.to_sym).div
    rescue
      super
    end
  end
end

get '/emacs' do

  request.websocket do |ws|
    ws.onopen { puts "@@ connect from emacs" }
    ws.onmessage do |msg|
      renderer = HTMLwithCoderay.new()
      markdown = Redcarpet::Markdown.new(renderer, :fenced_code_blocks => true, :no_intra_emphasis => true)
      html = markdown.render(msg)
      EM.next_tick do
        settings.sockets.each{|s| s.send(html) }
      end
    end
    ws.onclose do
      settings.sockets.delete(ws)
    end
  end

end

get '/markdown' do
  request.websocket do |ws|
    ws.onopen do
      settings.sockets << ws
    end
    ws.onclose do
      warn("wetbsocket closed")
      settings.sockets.delete(ws)
    end
  end
end

get '/static/:file.min.:ext' do |file, ext|
  content_type ext
  send_file "static/#{file}.min.#{ext}"
end

__END__
@@ index
<!doctype html>
<html>
<head>
    <meta charset="utf-8">
    <title>Realtime Markdown Viewer</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <script type="text/javascript" src="/static/jquery.min.js"></script>
    <link rel="stylesheet" href="/static/bootstrap.min.css">
    <script type="text/x-mathjax-config">
      MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});
    </script>
    <script type="text/javascript"
      src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
    </script>
</head>
<body>
    <div class="container">
        <header><h1>Demo</h1></header>
        <div id="preview"></div>
    </div>
    <script type="text/javascript">
        $(function () {
            var ws = new WebSocket('ws://localhost:5021/markdown');
            ws.onopen = function () {
                console.log('connected');
            };
            ws.onclose = function (ev) {
                console.log('closed');
            };
            ws.onmessage = function (ev) {
                $('#preview').html(ev.data);
                MathJax.Hub.Queue(["Typeset", MathJax.Hub, "preview"]);
            };
            ws.onerror = function (ev) {
                console.log(ev);
            };
        });
    </script>
</body>
</html>

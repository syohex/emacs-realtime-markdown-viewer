use strict;
use warnings;
use utf8;
use Amon2::Lite;
use Digest::MD5 ();
use Text::MultiMarkdown qw/markdown/;

get '/' => sub {
    my $c = shift;
    return $c->render('index.tt');
};

my $clients = {};

any '/emacs' => sub {
    my $c = shift;

    $c->websocket(sub {
        my $ws = shift;

        $ws->on_receive_message(sub {
            my ($c, $message) = @_;
            my $markdowned = markdown($message);
            for (keys %$clients) {
                $clients->{$_}->send_message($markdowned);
            }
        });

        $ws->on_eof(sub {
            my $c = shift;
            warn "/emacs EOF";
        });

        $ws->on_error(sub {
            my $c = shift;
            warn "/emacs error";
        });
    });
};

any '/markdown' => sub {
    my ($c) = @_;
    my $id = Digest::SHA1::sha1_hex(rand() . $$ . {} . time);

    $c->websocket(sub {
        my $ws = shift;
        $clients->{$id} = $ws;

        $ws->on_eof(sub {
            my ($c) = @_;
            delete $clients->{$id};
        });
        $ws->on_error(sub {
            my ($c) = @_;
            delete $clients->{$id};
        });
    });
};

# load plugins
__PACKAGE__->load_plugin('Web::WebSocket');
__PACKAGE__->enable_middleware('AccessLog');
__PACKAGE__->enable_middleware('Lint');

__PACKAGE__->to_app(handle_static => 1);

__DATA__

@@ index.tt
<!doctype html>
<html>
<head>
    <meta charset="utf-8">
    <title>Realtime Markdown Viewer</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <script type="text/javascript" src="/static/jquery.min.js"></script>
    <link rel="stylesheet" href="/static/bootstrap.min.css">
</head>
<body>
    <div class="container">
        <header><h1>Demo</h1></header>
        <div id="preview"></div>
        <footer>Powered by <a href="http://amon.64p.org/">Amon2::Lite</a></footer>
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
            };
            ws.onerror = function (ev) {
                console.log(ev);
            };
        });
    </script>
</body>
</html>

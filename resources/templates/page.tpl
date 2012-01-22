<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <title>WebCSPM - a CSPM Type Checker</title>

        <link rel="stylesheet" type="text/css" href="/static/screen.css"/>

        <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"></script>
        <script src="/static/bootstrap/bootstrap-scrollspy.js" type="text/javascript"></script>
        <script src="/static/bootstrap/bootstrap-twipsy.js" type="text/javascript"></script>
        <script src="/static/bootstrap/bootstrap-popover.js" type="text/javascript"></script>
        <script src="/static/jquery.smooth-scroll.min.js" type="text/javascript"></script>

        <!-- <link rel="stylesheet/less" type="text/css" href="/static/screen.less"/> -->
        <!--<script src="/static/less.js" type="text/javascript"></script>-->

        <script type="text/javascript">
            $(document).ready(function () {
                $('a').smoothScroll({ offset: -40 });
                $('.entity_name_function_cspm[data-type]').popover({ 
                    "html": true,
                    "title": function () {
                        return '<span class="cspm">'+$(this).text()+'</span> :: <span class="type">'+$(this).data("type")+'</span>';
                    },
                    "template": '<div class="arrow"></div><div class="inner"><h3 class="title"></h3></div>',
                });
            });
        </script>
    </head>
    <body>
        <div class="topbar" data-scrollspy="scrollspy">
            <div class="fill">
                <div class="container">
                    <a class="brand" href="#">WebCSPM</a>
                    <ul class="nav">
                        <extranav/>
                        <li><a href="#typecheck">Type Check File</a></li>
                        <li><a href="#help">Help</a></li>
                        <li><a href="#about">About</a></li>
                    </ul>
                </div>
            </div>
        </div>

        <div class="container">
            <div class="content">
                <content/>

                <footer>
                    <p>&copy; <a href="https://www.cs.ox.ac.uk/people/thomas.gibson-robinson/">Thomas Gibson-Robinson</a>, 2012.</p>
                </footer>
            </div>
        </div>
    </body>
</html>

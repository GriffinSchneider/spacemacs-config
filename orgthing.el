(setq gcs-org-preamble "
<style type='text/css'>
body {
  display: flex;
  flex-direction: column;
  align-items: center;
  font-family: 'Input Sans Condensed', 'Helvetica', 'Arial', sans-serif;
  font-weight: 100;
  color: #eee;
  background-color: #222;
}

#content {
  max-width: 50em;
}

img {
 border: .1em solid #666;
 border-radius: .3em;
}

a:link,
a:visited {
  color: #bbb;
  font-family: 'Input Sans Condensed', 'Helvetica', 'Arial', sans-serif;
  font-weight: normal;
  text-decoration: underline;
}

a:hover {
  background-color: #bbb;
  color: #000;
}

a:active {
  color: #f00;
}

h1, h2, h3, h4, h5, h6 {
  color: #fff;
  font-family: 'Input Sans', 'Helvetica', 'Arial', sans-serif;
  font-weight: normal;
  line-height: 1.5em;
  padding-top: 1em;
}

h4, h5, h6 {
  font-size: 1em;
}

h1.title {
  font-weight: normal;
  margin: 0 auto;
  padding: .2em 0;
  text-align: center;
}

#preamble {
  font-family: 'Input Sans Condensed', 'Helvetica', 'Arial', sans-serif;
  height: 24px;
  text-align: center;
}

#preamble a:link, #preamble a:visited {
  border: none;
  display: block;
  height: 24px;
  line-height: 24px;
  margin: 0 auto;
  text-decoration: none;
}

#preamble a:active, #preamble a:hover {
  border: none;
  background-color: transparent;
  color: #fff;
}

#postamble {
  color: #999;
  font-style: italic;
  text-align: right;
}

#postamble a.source-link:link,
#postamble a.source-link:visited {
  border-bottom: none;
  color: #ccc;
  font-family: 'Input Mono Condensed', monospace;
  font-size: .7em;
  font-style: normal;
  line-height: 24px;
  text-transform: lowercase;
  text-decoration: none;
}

#postamble a.source-link:hover,
#postamble a.source-link:active {
  background-color: transparent;
  color: #0f0;
}

code {
  border-top: solid #000 1px;
  border-bottom: solid #000 1px;
  padding: 0 .2em;
}

pre.src, pre.example {
  background-color: #111;
  border-top: none;
  border-bottom: solid #000 1px;
  border-left: none;
  border-right: solid #000 1px;
  box-shadow: none;
  font-size: .9em;
  padding: 1em 2em;
  overflow: auto;
}

pre.src:before {
  background-color: transparent;
  border: none;
  top: 0;
  right: 0;
}

sup {
  line-height: 0;
}

hr {
  border-top: solid 1px #000;
  border-bottom: solid 1px #333;
}

li p {
  margin: 0;
}

.footpara {
  margin: 0;
}

.footnotes {
  margin-top: 1em;
}

h2, h3, h4, h5, h6,
.footnotes {
  margin: 12px auto;
}

p, ul {
  margin: 24px auto;
}

table {
  margin: 12px auto;
}

li ul {
  margin-top: 0;
  margin-bottom: 0;
}

pre {
  margin: 0 auto;
}

div.figure {
  text-align: center;
}

div.figure p, div.figure img {
}

span.tag {
  background-color: #333;
}

.done {
  color: green;
  font-weight: bold;
}
.todo {
  color: red;
  font-weight: bold;
}

.footnotes {
  font-size: 14px;
  line-height: 24px;
  margin-top: 24px;
  padding: 24px;
}

.footdef {
  margin-top: 24px;
}

.footpara {
  display: inline;
}
</style>
")

(provide 'org-thing);

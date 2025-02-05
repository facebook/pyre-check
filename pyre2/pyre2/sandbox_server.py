import json
import os
import subprocess
from http.server import BaseHTTPRequestHandler, HTTPServer


class RequestHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.end_headers()
        self.wfile.write(
            b"""
            <html>
                <head>
                    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css">
                    <style>
                        body {
                            font-family: monospace;
                        }
                        #code {
                            width: 100%;
                            height: 300px;
                            padding: 10px;
                            font-size: 16px;
                            font-family: monospace;
                            border: 1px solid #ccc;
                            resize: vertical;
                            overflow-y: auto;
                        }
                        #output {
                            padding: 10px;
                            background-color: #f0f0f0;
                            border: 1px solid #ccc;
                        }
                    </style>
                </head>
                <body>
                    <div style="text-align: center;">
                        <img src="https://raw.githubusercontent.com/facebook/pyre-check/main/logo.png" alt="Pyre2 Logo">
                        <h1>Pyre2 Sandbox</h1>
                    </div>
                    <div id="input-box">
                        <pre><code class='language-python' id="code" contenteditable="true"></code></pre>
                    </div>
                    <button onclick="checkCode()">Check</button>
                    <div id="output"></div>

                    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js"></script>
                    <script>
                        hljs.initHighlightingOnLoad();
                        var code = document.getElementById("code");
                        code.addEventListener('input', function() {
                            hljs.highlightBlock(code);
                        });
                        function checkCode() {
                            var codeValue = code.textContent;
                            fetch('/check', {
                                method: 'POST',
                                headers: { 'Content-Type': 'application/json' },
                                body: JSON.stringify({ code: codeValue })
                            })
                            .then(response => response.text())
                            .then(output => {
                                document.getElementById("output").innerHTML = output;
                            });
                        }
                    </script>
                </body>
            </html>
        """
        )

    def do_POST(self):
        try:
            content_length = int(self.headers["Content-Length"])
            body = self.rfile.read(content_length)
            data = json.loads(body.decode("utf-8"))
            code = data["code"]

            with open("sandbox.py", "w") as f:
                f.write(code)

            result = subprocess.run(
                ["cargo", "run", "--", "check", "sandbox.py", "-o", "output"],
                capture_output=True,
                text=True,
            )

            if result.returncode != 0:
                output = result.stdout + result.stderr
                html_output = f"<pre><code class='language-bash'>{output}</code></pre>"
                self.send_response(200)
                self.end_headers()
                self.wfile.write(html_output.encode("utf-8"))
            else:
                with open("output", "r") as f:
                    output = f.read()
                    if not output.strip():
                        html_output = "0 errors"
                    else:
                        html_output = (
                            f"<pre><code class='language-python'>{output}</code></pre>"
                        )
                    self.send_response(200)
                    self.end_headers()
                    self.wfile.write(html_output.encode("utf-8"))
        except Exception as e:
            self.send_response(500)
            self.end_headers()
            self.wfile.write(str(e).encode("utf-8"))
        finally:
            for filename in ["sandbox.py", "output"]:
                if os.path.exists(filename):
                    os.remove(filename)


def run_server():
    server_address = ("", 8000)
    httpd = HTTPServer(server_address, RequestHandler)
    print("Server running on port 8000...")
    httpd.serve_forever()


if __name__ == "__main__":
    run_server()

const http = require("http");
const url = require('url');
const { exec } = require('child_process');

const host = 'localhost';
const port = 8002;

const requestListener = function (req, res) {
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Content-Type", "application/json");

  const _url = url.parse(req.url, true);
  const msg = _url.query;
  switch (_url.pathname) {
    case "/trs":
      console.log(msg['rules']);
      exec(__dirname + `/haskell/provskell-exe 0 ${msg['rules']}`, (error, stdout, stderr) => {
        if (error) {
          console.error(`error: ${error.message}`);
          return;
        }

        if (stderr) {
          console.error(`stderr: ${stderr}`);
          return;
        }

        res.writeHead(200);
        res.end(stdout);

        console.log(`stdout:\n${stdout}`);
      });
      break
    case "/prove":
      console.log(msg);
      exec(__dirname + `/haskell/provskell-exe 1 ${msg['rules']} ${msg['funcs']} ${msg['n']}`, (error, stdout, stderr) => {
        if (error) {
          console.error(`error: ${error.message}`);
          return;
        }

        if (stderr) {
          console.error(`stderr: ${stderr}`);
          return;
        }

        res.writeHead(200);
        res.end(stdout);

        console.log(`stdout:\n${stdout}`);
      });
      break
    case "/calc":
      console.log(msg);
      exec(__dirname + `/haskell/provskell-exe 2 ${msg['expr']}`, (error, stdout, stderr) => {
        if (error) {
          console.error(`error: ${error.message}`);
          return;
        }

        if (stderr) {
          console.error(`stderr: ${stderr}`);
          return;
        }

        res.writeHead(200);
        res.end(stdout);

        console.log(`stdout:\n${stdout}`);
      });
      break
  }
}

const server = http.createServer(requestListener);
server.listen(port, host, () => {
    console.log(`Server is running on http://${host}:${port}`);
});

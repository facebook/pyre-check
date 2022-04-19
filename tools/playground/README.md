# Playground

## Setting up in an Ubuntu machine.

In a fresh ubuntu machine (it has been tested most recently against
an EC2 instance running ami-0a91cd140a1fc148a), you can install the
playground by running:

```bash
curl \
  https://raw.githubusercontent.com/facebook/pyre-check/main/tools/playground/install-in-ubuntu.sh \
  > ~/install-in-ubuntu.sh
```

## Debugging

If the install script has a bug (e.g. the packages go out of date) you
can debug the installation as follows, working your way up:

### Make sure the venv and pyre are installed properly

Run `source ~/play-env/bin/activate` to enter the `venv`.

Then, try running `pyre --noninteractive check` to make sure pyre is installed.

### Make sure the python server works

In the venv activated above, try running
```
python ~/playground/application.py --debug
```
which will start a raw flask server. Many problems could crash the
imports at the start of the script.

Note that if the service is already running (see below) it may crash
claiming that port 5000 is not available; this likely means you can
move to the next stage but if you need to debug in more depth you
can stop the service with
```
sudo systemctl stop playground
```
and then re-run `python application.py --debug`.

### Verify the service is running

The service is a gunicorn application that drives our flask server. You can
test it by running
```
sudo systemctl daemon-reload
sudo systemctl restart playground
```
and then trying to hit it at
```
curl "http://0.0.0.0:5000/check?input=reveal_type(1)"
```

If there are problems, look at the logs in `/var/log/playground.out` and
`/var/log/playground.err` to see what crashed.

If there are no logs, then likely the application failed to start at all;
try running the underlying command in `system/playground.service` directly
from the shell to see what happens.

If you are iterating on the service script (or if you made changes to the
actual python code) you'll need to restart the service each time.

### Verify that nginx is working

Run
```
curl "http://0.0.0.0:80/check?input=reveal_type(1)"
```
to hit the nginx reverse-proxy; it ought to forward to port 5000.

If there are problems, you may be able to look at the access and error
logs in `/var/log/ngix`; one thing to look out for is to make sure that
`/etc/nginx/sites-enabled` has the `playground` config and nothing
else.

If you make changes to nginx, run
```
sudo service restart nginx
```
to reload.

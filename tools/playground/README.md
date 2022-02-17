# Playground

The following guide is an attempt to document how to set up the playground to run on an AWS EC2 Ubuntu machine. All of this should probably be automated.

# Setting up The Playground

## Install Dependencies
Make sure we have an up-to-date package index:

```shell
[~]$ apt-get update
```
Install all Python dependencies with:
```shell
[~]$ apt-get install \
 build-essential \
 python3.8 \
 python3.8-venv \
 python3-dev \
 watchman
 ```

## Get the Source
You can rsync the source directly to the instance with
```shell
[~]$ cd ~/pyre-check/tools/playground
[playground]$ rsync -av --progress -e "ssh -i <YOUR KEY>.pem" . ubuntu@<HOST>.<REGION>.compute.amazonaws.com:/home/ubuntu/playground
```

## Set up the Instance
Login to your instance and set up the Python environment with
```shell
[~]$ cd playground
[playground]$ python3 -m venv venv
[playground]$ source venv/bin/activate
(venv) [playground]$ pip install -r requirements.txt
```

You can check that the playground is working from the virtual environment by running
```shell
(venv) [playground]$ python application.py --debug
...
```

You can also check that uwsgi is working as expected by exiting the virtual environment (run `deactivate`) and running
```shell
[playground] PATH=$PATH:$(pwd)/venv/bin venv/bin/uwsgi --ini playground.ini
```
while tailing `playground.log`.

## Start the service
The source includes a configuration for `systemd`. You can install and run the service with
```shell
[playground]$ cp system/playground.service /etc/systemd/system/
[playground]$ systemctl start playground
[playground]$ systemctl enable playground
```

You should now have a running playground service (check `playground.log`) on your system.

# Exposing the Service
This setup assumes we're running on our wsgi application on nginx.
```shell
[~]$ apt-get install nginx
[~]$ service nginx start
```
You should be able to see the default page of nginx when loading your instances address in your browser (make sure you have opened HTTP(s) ports in your configuration).

The configuration is part of the source. We just need to put it in the right directory (make sure you replace the ip in the configuration with the ip of your instance):
```shell
[playground]$ cp system/playground.nginx /etc/nginx/sites-available/playground
[playground]$ ln -s /etc/nginx/sites-{available,enabled}/playground
```

and restart our webserver
```shell
[playground]$ service nginx restart
```

You should now be able to get type checking results through http://\<HOST\>.\<REGION\>.compute.amazonaws.com/check?input=reveal_type(1).

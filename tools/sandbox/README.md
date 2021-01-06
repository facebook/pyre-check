# Sandbox
The following guide is an attempt to document how to set up the sandbox to run on an AWS EC2 Ubuntu machine. All of this should probably be automated.

# Setting up The Sandbox

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
[~]$ cd ~/pyre-check/tools/sandbox
[sandbox]$ rsync -av --progress -e "ssh -i <YOUR KEY>.pem" . ubuntu@<HOST>.<REGION>.compute.amazonaws.com:/home/ubuntu/sandbox
```

## Set up the Instance
Login to your instance and set up the Python environment with
```shell
[~]$ cd sandbox
[sandbox]$ python3 -m venv venv
[sandbox]$ source venv/bin/activate
(venv) [sandbox]$ pip install -r requirements.txt
```

You can check that the sandbox is working from the virtual environment by running
```shell
(venv) [sandbox]$ python application.py --debug
...
```

You can also check that uwsgi is working as expected by exiting the virtual environment (run `deactivate`) and running
```shell
[sandbox] PATH=$PATH:$(pwd)/venv/bin venv/bin/uwsgi --ini sandbox.ini
```
while tailing `sandbox.log`.

## Start the service
The source includes a configuration for `systemd`. You can install and run the service with
```shell
[sandbox]$ cp system/sandbox.service /etc/systemd/system/
[sandbox]$ systemctl start sandbox
[sandbox]$ systemctl enable sandbox
```

You should now have a running sandbox service (check `sandbox.log`) on your system.

# Exposing the Service
This setup assumes we're running on our wsgi application on nginx.
```shell
[~]$ apt-get install nginx
[~]$ service nginx start
```
You should be able to see the default page of nginx when loading your instances address in your browser (make sure you have opened HTTP(s) ports in your configuration).

The configuration is part of the source. We just need to put it in the right directory (make sure you replace the ip in the configuration with the ip of your instance):
```shell
[sandbox]$ cp system/sandbox.nginx /etc/nginx/sites-available/sandbox
[sandbox]$ ln -s /etc/nginx/sites-{available,enabled}/sandbox
```

and restart our webserver
```shell
[sandbox]$ service nginx restart
```

You should now be able to get type checking results through http://\<HOST\>.\<REGION\>.compute.amazonaws.com/check?input=reveal_type(1).

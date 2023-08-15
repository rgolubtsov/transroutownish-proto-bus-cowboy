# Trans-RoutE-Townish (transroutownish) :small_blue_diamond: Urban bus routing microservice prototype (Erlang/OTP port)

**An Erlang/OTP application, designed and intended to be run as a microservice,
<br />implementing a simple urban bus routing prototype**

**Rationale:** This project is a *direct* **[Erlang](https://erlang.org "Real-time, concurrent and distributed functional language")** port of the earlier developed **urban bus routing prototype**, written in Java using **[Spring Boot](https://spring.io/projects/spring-boot "Stand-alone Spring apps builder and runner")** framework, and tailored to be run as a microservice in a Docker container. The following description of the underlying architecture and logics has been taken **[from there](https://github.com/rgolubtsov/transroutownish-proto-bus-spring-boot)** as is, without any modifications or adjustment.

Consider an IoT system that aimed at planning and forming a specific bus route for a hypothetical passenger. One crucial part of such system is a **module**, that is responsible for filtering bus routes between two arbitrary bus stops where a direct route is actually present and can be easily found. Imagine there is a fictional urban public transportation agency that provides a wide series of bus routes, which covered large city areas, such that they are consisting of many bus stop points in each route. Let's name this agency **Trans-RoutE-Townish Co., Ltd.** or in the Net representation &mdash; **transroutownish.com**, hence the name of the project.

A **module** that is developed here is dedicated to find out quickly, whether there is a direct route in a list of given bus routes between two specified bus stops. It should immediately report back to the IoT system with the result `true` if such a route is found, i.e. it exists in the bus routes list, or `false` otherwise, by outputting a simple JSON structure using the following format:

```
{
    "from"   : <starting_bus_stop_point>,
    "to"     : <ending_bus_stop_point>,
    "direct" : true
}
```

`<starting_bus_stop_point>` and `<ending_bus_stop_point>` above are bus stop IDs: unique positive integers, taken right from inputs.

A bus routes list is a plain text file where each route has its own unique ID (positive integer) and a sequence of its bus stop IDs. Each route occupies only one line in this file, so that they are all representing something similar to a list &mdash; the list of routes. The first number in a route is always its own ID. Other consequent numbers after it are simply IDs of bus stops in this route, up to the end of line. All IDs in each route are separated by whitespace, usually by single spaces or tabs, but not newline.

There are some constraints:
1. Routes are considered not to be a round trip journey, that is they are operated in the forward direction only.
2. All IDs (of routes and bus stops) must be represented by positive integer values, in the range `1 .. 2,147,483,647`.
3. Any bus stop ID may occure in the current route only once, but it might be presented in any other route too.

The list of routes is usually mentioned throughout the source code as a **routes data store**, and a sample routes data store can be found in the `data/` directory of this repo.

Since the microservice architecture for building independent backend modules of a composite system are very prevalent nowadays, this seems to be natural for creating a microservice, which is containerized and run as a daemon, serving a continuous flow of HTTP requests.

This microservice is intended to be built locally and to be run like a conventional daemon in the VM environment, as well as a containerized service, managed by Docker.

One may consider this project has to be suitable for a wide variety of applied areas and may use this prototype as: (1) a template for building a similar microservice, (2) for evolving it to make something more universal, or (3) to simply explore it and take out some snippets and techniques from it for *educational purposes*, etc.

---

## Table of Contents

* **[Building](#building)**
  * **[Creating a Docker image](#creating-a-docker-image)**
* **[Running](#running)**
  * **[Running a Docker image](#running-a-docker-image)**
  * **[Exploring a Docker image payload](#exploring-a-docker-image-payload)**
* **[Consuming](#consuming)**
  * **[Logging](#logging)**
  * **[Error handling](#error-handling)**

## Building

The microservice is known to be built and run successfully under **Ubuntu Server (Ubuntu 22.04.3 LTS x86-64)** and **Arch Linux**. Install the necessary dependencies (`erlang-nox`, `erlang-dev`, `rebar3`, `make`, `docker.io`):

* In Ubuntu Server:

```
$ sudo apt-get update && \
  sudo apt-get install erlang-nox erlang-dev make docker.io -y
...
```

* In Arch Linux:

```
$ sudo pacman -Syu erlang-nox make docker
...
```

Rebar3 is preferred to install everywhere the same way:

```
$ curl -sO https://s3.amazonaws.com/rebar3/rebar3      && \
  chmod -v 700 rebar3 && ./rebar3 local install        && \
  export PATH=/home/<username>/.cache/rebar3/bin:$PATH && \
  rm -vf rebar3
...
```

**Build** the microservice using **Rebar3**:

```
$ rebar3 clean;   \
  rebar3 as prod clean
...
$ rebar3 compile; \
  rebar3 as prod compile
...
$ rebar3 release; \
  rebar3 as prod release
...
```

Or **build** the microservice using **GNU Make** (optional, but for convenience &mdash; it covers the same **Rebar3** build workflow under the hood):

```
$ make clean
...
$ make      # <== Compilation phase.
...
$ make all  # <== Assembling releases of the microservice.
...
```

---

The following command given is for demonstrational purposes only &mdash; Rebar3 will always fetch necessary dependencies during a one of their building phases, even at the `clean`-ing phase:

```
$ rebar3 tree
===> Verifying dependencies...
===> Fetching cowboy v2.10.0
===> Fetching jsx v3.1.0
===> Fetching syslog v1.1.0
===> Fetching pc v1.14.0
===> Analyzing applications...
===> Compiling pc
===> Fetching cowlib v2.12.1
===> Fetching ranch v1.8.0
└─ bus─0.3.4 (project app)
   ├─ cowboy─2.10.0 (hex package)
   │  ├─ cowlib─2.12.1 (hex package)
   │  └─ ranch─1.8.0 (hex package)
   ├─ jsx─3.1.0 (hex package)
   └─ syslog─1.1.0 (hex package)
```

### Creating a Docker image

**Build** a Docker image for the microservice:

```
$ # Pull the Erlang image first, if not already there:
$ sudo docker pull erlang:alpine
...
$ # Then build the microservice image:
$ sudo docker build -ttransroutownish/buscow .
...
```

## Running

**Run** the microservice using its startup script along with the `foreground` command, that is meant "*Start release with output to stdout*":

```
$ ./_build/prod/rel/bus/bin/bus foreground; echo $?
...
```

The microservice then can be stopped, again by using its startup script along with the `stop` command, that is meant "*Stop the running node*". It should be issued in another terminal session, not the current one:

```
$ ./_build/prod/rel/bus/bin/bus stop; echo $?
0
```

To identify, which commands are available and what they mean, the startup script can be run without specifying a command or arguments:

```
$ ./_build/prod/rel/bus/bin/bus
Usage: bus [COMMAND] [ARGS]

Commands:

  foreground              Start release with output to stdout
  remote_console          Connect remote shell to running node
...
  stop                    Stop the running node
  restart                 Restart the applications but not the VM
...
  daemon                  Start release in the background with run_erl (named pipes)
...
  daemon_attach           Connect to node started as daemon with to_erl (named pipes)
...
```

Thus, to **run** the microservice as a daemon, in the background, the `daemon` command should be used instead:

```
$ ./_build/prod/rel/bus/bin/bus daemon; echo $?
0
```

The `daemon_attach` command then allows connecting to the microservice to make interactions with them. But the latter is not required at all regarding the true purpose of the microservice. And it can be stopped again with the `stop` command in the same terminal session.

### Running a Docker image

**Run** a Docker image of the microservice, deleting all stopped containers prior to that:

```
$ sudo docker rm `sudo docker ps -aq`; \
  export PORT=8765 && sudo docker run -dp${PORT}:${PORT} --name buscow transroutownish/buscow; echo $?
...
```

### Exploring a Docker image payload

The following is not necessary but might be considered interesting &mdash; to look up into the running container, and check out that the microservice's startup script, application BEAMs, log, and routes data store are at their expected places and in effect:

```
$ sudo docker ps -a
CONTAINER ID   IMAGE                    COMMAND                    CREATED             STATUS             PORTS                                       NAMES
<container_id> transroutownish/buscow   "bus/bin/bus foregro..."   About an hour ago   Up About an hour   0.0.0.0:8765->8765/tcp, :::8765->8765/tcp   buscow
$
$ sudo docker exec -it buscow sh; echo $?
/var/tmp #
/var/tmp # bus/erts-14.0.2/bin/erl -version
Erlang (SMP,ASYNC_THREADS) (BEAM) emulator version 14.0.2
/var/tmp #
/var/tmp # ls -al
total 20
drwxrwxrwt    1 root     root          4096 Aug 15 22:46 .
drwxr-xr-x    1 root     root          4096 Jan  9  2023 ..
drwxr-xr-x    1 root     root          4096 Aug 15 22:50 bus
/var/tmp #
/var/tmp # ls -al bus/
total 32
drwxr-xr-x    1 root     root          4096 Aug 15 22:50 .
drwxrwxrwt    1 root     root          4096 Aug 15 22:46 ..
drwxr-xr-x    2 root     root          4096 Aug 15 22:46 bin
drwxr-xr-x    3 root     root          4096 Aug 15 22:46 erts-14.0.2
drwxr-xr-x   14 root     root          4096 Aug 15 22:46 lib
drwxr-xr-x    2 root     root          4096 Aug 15 22:50 log
drwxr-xr-x    3 root     root          4096 Aug 15 22:46 releases
/var/tmp #
/var/tmp # ls -al bus/bin/ bus/lib/bus-0.3.4/ebin/ bus/lib/bus-0.3.4/priv/data/ bus/log/
bus/bin/:
total 112
drwxr-xr-x    2 root     root          4096 Aug 15 22:46 .
drwxr-xr-x    1 root     root          4096 Aug 15 22:50 ..
-rwxr-xr-x    1 root     root         35983 Aug 15 22:46 bus
-rwxr-xr-x    1 root     root         35983 Aug 15 22:46 bus-0.3.4
-rw-r--r--    1 root     root         14214 Aug 15 22:46 install_upgrade.escript
-rw-r--r--    1 root     root          6681 Aug 15 22:46 no_dot_erlang.boot
-rw-r--r--    1 root     root          7560 Aug 15 22:46 nodetool

bus/lib/bus-0.3.4/ebin/:
total 28
drwxr-xr-x    2 root     root          4096 Aug 15 22:46 .
drwxr-xr-x    4 root     root          4096 Aug 15 22:46 ..
-rw-r--r--    1 root     root           607 Aug 15 22:46 bus.app
-rw-r--r--    1 root     root          1808 Aug 15 22:46 bus_app.beam
-rw-r--r--    1 root     root           991 Aug 15 22:46 bus_controller.beam
-rw-r--r--    1 root     root          1703 Aug 15 22:46 bus_handler.beam
-rw-r--r--    1 root     root           512 Aug 15 22:46 bus_sup.beam

bus/lib/bus-0.3.4/priv/data/:
total 56
drwxr-xr-x    2 root     root          4096 Aug 15 22:46 .
drwxr-xr-x    3 root     root          4096 Aug 15 22:46 ..
-rw-rw-r--    1 root     root         46218 May 26  2022 routes.txt

bus/log/:
total 16
drwxr-xr-x    2 root     root          4096 Aug 15 22:50 .
drwxr-xr-x    1 root     root          4096 Aug 15 22:50 ..
-rw-r--r--    1 root     root          5478 Aug 15 22:50 bus.log
/var/tmp #
/var/tmp # netstat -plunt
Active Internet connections (only servers)
Proto Recv-Q Send-Q Local Address           Foreign Address         State       PID/Program name
tcp        0      0 0.0.0.0:8765            0.0.0.0:*               LISTEN      1/bus
tcp        0      0 0.0.0.0:44859           0.0.0.0:*               LISTEN      1/bus
tcp        0      0 0.0.0.0:4369            0.0.0.0:*               LISTEN      50/epmd
tcp        0      0 :::4369                 :::*                    LISTEN      50/epmd
/var/tmp #
/var/tmp # ps ax
PID   USER     TIME  COMMAND
    1 root      0:02 {beam.smp} /var/tmp/bus/bin/bus -Bd -K true -A30 -- -root /var/tmp/bus -bindir /var/...
   50 root      0:00 /var/tmp/bus/erts-14.0.2/bin/epmd -daemon
   78 root      0:00 [epmd]
   79 root      0:00 [epmd]
  112 root      0:00 erl_child_setup 1048576
  131 root      0:00 sh
  143 root      0:00 ps ax
/var/tmp #
/var/tmp # exit # Or simply <Ctrl-D>.
0
```

## Consuming

All the routes are contained in a so-called **routes data store**. It is located in the `data/` directory. The default filename for it is `routes.txt`, but it can be specified explicitly (if intended to use another one) in the `apps/bus/src/bus.app.src` file.

**Identify**, whether there is a direct route between two bus stops with IDs given in the **HTTP GET** request, searching for them against the underlying **routes data store**:

HTTP request param | Sample value | Another sample value | Yet another sample value
------------------ | ------------ | -------------------- | ------------------------
`from`             | `4838`       | `82`                 | `2147483647`
`to`               | `524987`     | `35390`              | `1`

The direct route is found:

```
$ curl 'http://localhost:8765/route/direct?from=4838&to=524987'
{"direct":true,"from":4838,"to":524987}
```

The direct route is not found:

```
$ curl 'http://localhost:8765/route/direct?from=82&to=35390'
{"direct":false,"from":82,"to":35390}
```

### Logging

The microservice has the ability to log messages to a logfile and to the Unix syslog facility. When running under Ubuntu Server or Arch Linux (not in a Docker container), logs can be seen and analyzed in an ordinary fashion, by `tail`ing the `_build/prod/rel/bus/log/bus.log` logfile:

```
$ tail -f _build/prod/rel/bus/log/bus.log
...
[2023-07-07|23:25:15.443954+03:00][info]  Server started on port 8765
[2023-07-07|23:25:15.445469+03:00][info]  Application: bus. Started at: bus@localhost.
[2023-07-07|23:25:30.911540+03:00][debug]  from=4838 | to=524987
[2023-07-07|23:25:40.105888+03:00][debug]  from=82 | to=35390
[2023-07-07|23:30:10.904381+03:00][info]  Server stopped
```

Messages registered by the Unix system logger can be seen and analyzed using the `journalctl` utility:

```
$ journalctl -f
...
Jul 07 23:25:14 <hostname> bus[<pid>]: Starting up
Jul 07 23:25:15 <hostname> bus[<pid>]: Server started on port 8765
Jul 07 23:25:30 <hostname> bus[<pid>]: from=4838 | to=524987
Jul 07 23:25:40 <hostname> bus[<pid>]: from=82 | to=35390
Jul 07 23:30:10 <hostname> bus[<pid>]: Server stopped
```

Inside the running container logs might be queried also by `tail`ing the `bus/log/bus.log` logfile:

```
/var/tmp # tail -f bus/log/bus.log
...
[2023-07-07|19:20:15.525769+00:00][info]  Server started on port 8765
[2023-07-07|19:20:15.526437+00:00][info]  Application: bus. Started at: bus@<container_id>.
[2023-07-07|19:20:30.573195+00:00][debug]  from=4838 | to=524987
[2023-07-07|19:20:40.973890+00:00][debug]  from=82 | to=35390
[2023-07-07|19:20:50.576683+00:00][notice]  SIGTERM received - shutting down
[2023-07-07|19:20:50.578264+00:00][info]  Server stopped
```

And of course Docker itself gives the possibility to read log messages by using the corresponding command for that:

```
$ sudo docker logs -f buscow
...
[2023-07-07|19:20:15.525769+00:00][info]  Server started on port 8765
[2023-07-07|19:20:15.526437+00:00][info]  Application: bus. Started at: bus@<container_id>.
[2023-07-07|19:20:30.573195+00:00][debug]  from=4838 | to=524987
[2023-07-07|19:20:40.973890+00:00][debug]  from=82 | to=35390
[2023-07-07|19:20:50.576683+00:00][notice]  SIGTERM received - shutting down
[2023-07-07|19:20:50.578264+00:00][info]  Server stopped
```

### Error handling

When the query string passed in a request, contains inappropriate input, or the URI endpoint doesn't contain anything else at all after its path, the microservice will respond with the **HTTP 400 Bad Request** status code, including a specific response body in JSON representation, like the following:

```
$ curl 'http://localhost:8765/route/direct?from=qwerty4838&to=-i-.;--089asdf../nj524987'
{"error":"Request parameters must take positive integer values, in the range 1 .. 2,147,483,647. Please check your inputs."}
```

Or even simpler:

```
$ curl http://localhost:8765/route/direct
{"error":"Request parameters must take positive integer values, in the range 1 .. 2,147,483,647. Please check your inputs."}
```

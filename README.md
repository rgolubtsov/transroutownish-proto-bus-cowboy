# Trans-RoutE-Townish (transroutownish) :small_blue_diamond: Urban bus routing microservice prototype (Erlang/OTP port)

**A daemon written in Erlang/OTP, designed and intended to be run as a microservice,
<br />implementing a simple urban bus routing prototype**

(*This is a work in progress &mdash; please wait for a while...*)

This project is a *direct* **[Erlang](https://erlang.org "Real-time, concurrent and distributed functional language")** port of the earlier developed **urban bus routing prototype**, written in Java using **[Spring Boot](https://spring.io/projects/spring-boot "Stand-alone Spring apps builder and runner")** framework, and tailored to be run as a microservice in a Docker container. The following description of the underlying architecture and logics has been taken **[from there](https://github.com/rgolubtsov/transroutownish-proto-bus-spring-boot)** as is, without any modifications or adjustment.

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
* **[Running](#running)**

## Building

The microservice is known to be built and run successfully under **Ubuntu Server (Ubuntu 20.04.4 LTS x86-64)**. Install the necessary dependencies (`erlang-nox`, `rebar3`, `make`, `docker.io`):

```
$ sudo apt-get update && \
  sudo apt-get install erlang-nox make docker.io -y
...
$ curl -sO https://s3.amazonaws.com/rebar3/rebar3        && \
  chmod -v 700 rebar3 && ./rebar3 local install          && \
  export PATH=/home/<vmusername>/.cache/rebar3/bin:$PATH && \
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

## Running

**Run** the microservice using its startup script along with the `foreground` command, that is meant "*Start release with output to stdout*":

```
$ ./_build/prod/rel/bus/bin/bus foreground; echo $?
...
```

**TBD**

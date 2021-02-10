# 🔥 Bobby's Algebraic Graph Toolkit 🔥

### Setup
#### Build from source

Start the tool either using `stack run`. If you don't have [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) you can install it with `curl -sSL https://get.haskellstack.org/ | sh`

#### Use a pre-built binary

You can find binaries for OSX, Linux & Windows in the [releases page](https://github.com/bobymicroby/boby-alga-toolkit/releases)

> If you are on linux and receive `error while loading shared libraries: libtinfo.so.5`
> please install  the `libncurses5` pkg  . The binary is supposed to be statically built
> but one PopOS user reported that it's not working without installing ncurses.



### How to create a graph

With the tool open enter the following commands 

```
follow elon bitcoin
follow elon bobby
follow bitcoin greta
follow greta bobby
```

<img src="assets/building-the-graph.svg" width="600" >



### Viewing the graph

Typing `show graph` will generate a link wich will open the
following image:

![demo](assets/graph.svg)



### Finding 10 friends of friends

Typing `suggest elon 10` will generate a link wich will open the
following image:

![demo](assets/suggest.svg)


### Show who Elon is following 

Typing `following elon` will generate a link wich will open the
following image:

![demo](assets/following.svg)


### Show followers of Bitcoin

Typing `followers bitcoin` will generate a link wich will open the
following image:

![demo](assets/followers.svg)

Initially, an attempt to visualize the Michael D Cohen / Essential Consulting financial data released by Michael Avenatti in the [Executive Summary](./Executive Summary.pdf), but has since grown to include various news reports. At one time, it included a link all the way to Kevin Bacon, but that was getting silly.

The source of the visual is on a [Google spreadsheet](https://docs.google.com/spreadsheets/d/119L80r1ZVgBgN0qDI-ovMb6s40iVFg3TWW15uqsxfBk/edit?usp=sharing) which you are invited to contribute to. The main rule is that it has to link through Cohen somehow, but I'm starting to break it (see the Tucker Carlson branch). I sort of expect that everything will tie in through Cohen in the end.

The "group" membership is found by using a random walk through the network, and applying the ["walktrap"](https://arxiv.org/abs/physics/0512106) algorithm. Group names are the name of the node in the group with the greatest pagerank in the whole network.

The source code is on [Github](https://github.com/schnee/tangled) - if you clone that repo, you can run the tangled.R file and generate your own copy of the visuals.

A static visual is [available](./tangled.png), as is a [d3.js](./tangled-d3.html) version. I welcome pull requests, especially ones that will allow me to annotate the d3 version.

[![A tangled web](./tangled.png)](./tangled.png)
MANETs, or "Mobile Ad-Hoc Networks", are a technology for radio communication between multiple nodes.
They are very useful in applications like 
autonomous robotics, sensor networks and Internet of Things devices.
They are also good for environments where the fixed network may be unreliable,
such as disaster response, search-and-rescue, and hospital medical devices.
These applications have gained increased **relevance** in recent years.

MANETs are not like traditional fixed networks (such as Wi-Fi access points or cellular phone networks),
which use a single central router that establishes point-to-point connections to each of the client devices.
Instead, MANETs involve each node being connected to a number of other nodes, acting as both client and router.

Unlike fixed networks,
MANETs don't require administrative oversight to set up connections.
Instead, the nodes decide for themselves what connections to make,
forming a dynamic mesh network.
These networks can be used in different applications with low human interaction.

In this work, we develop a simulator application
that allows testing different algorithms for maintaining connectivity and routing in a MANET,
and then use that to develop a mesh network appliance for a mobile robot.

Similar simulators already exist for testing fixed networks,
but, because routing algorithms for fixed networks are relatively mature,
they are commonly used instead for planning network layouts,
or else for teaching system administration
(examples include GNS3, Mininet and Cisco PacketTracer).

In addition, there are also several commercially available
toolkits for production mesh networks.
Examples include Zigbee (international standard IEEE 802.15.4),
Z-Wave (by Zensys)
and various proprietary protocols like Amazon Sidewalk (which works on top of Bluetooth Low Energy).

The **novelty** of this work is in developing a simulator app
specifically for MANETs,
especially in the context of mobile robots,
while not being tied to any specific physical implementation.
Because MANETs require no administrator input,
the simulator will involve robots moving around the environment
and making or breaking connections as they move in and out of radio range of each other --
a thing that's difficult to do in a regular network simulator.


The **goal** of this project is to use the simulator application
to measure the performance of some such algorithms in different conditions.
The algorithms can be tuned for different criteria,
like low compute/memory footprint,
high re-routing speed,
or low traffic overhead.
Therefore, several **methods** will be used to evaluate the performance on different kinds of environment conditions.
Afterwards, we will test the developed algorithms in real-world conditions using Wi-Fi based network interface cards.

TODO: add people
TODO: move relevance first, explanation second, then novelty, then goals, then methods

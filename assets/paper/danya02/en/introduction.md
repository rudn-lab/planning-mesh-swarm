MANETs, or "Mobile Ad-Hoc Networks", are a technology for radio communication between multiple nodes.
They are not like traditional fixed networks (such as Wi-Fi access points or cellular phone networks),
which use a single central router that establishes point-to-point connections to each of the client devices.
Instead, MANETs involve each node being connected to a number of other nodes, acting as both client and router.

Another feature of ad-hoc networks is that, unlike fixed networks,
these don't require administrative oversight to set up connections.
Instead, the nodes decide for themselves what connections to make,
forming a dynamic mesh network.

These networks can be used in different applications with low human interaction,
like autonomous robotics, sensor networks and Internet of Things devices.
They are also good for environments where the fixed network may be unreliable,
such as disaster response, search-and-rescue, and hospital medical devices.
Many of these subjects have become particularly important in recent years,
so the **relevance** of this work is in laying a foundation for future development in these areas.

The **goal** of this project is to develop a simulator application
that allows testing different algorithms for maintaining connectivity and routing in a MANET,
and then to measure the performance of some such algorithms in different conditions.
The algorithms can be tuned for different criteria,
like low compute/memory footprint,
high re-routing speed,
or low traffic overhead.
Therefore, several **methods** will be used to evaluate the performance on different kinds of environment conditions.

Similar programs already exist for testing fixed networks,
but, because routing algorithms for fixed networks are relatively mature,
they are commonly used instead for planning network layouts,
or else for teaching system administration
(examples include GNS3, Mininet and Cisco PacketTracer).
The **novelty** of this work is in developing such a simulator specifically for MANETs,
especially in the context of mobile robots.
Because MANETs require no administrator input,
the simulator will involve robots moving around the environment
and making or breaking connections as they move in and out of radio range of each other.
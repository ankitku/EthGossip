Progress Report
Milestone 1: Formalization of refinement models and Ethereum application logic

Executive Summary:

The main objective of our work is to develop a fundamental, flexible
and understandable framework for analyzing P2P protocols such as
Gossipsub. The motivation is that the developers of these protocols do
not provide any correctness properties, which leaves open the
possibility of unknown vulnerabilities. We plan to tackle this
challenge by developing correctness using the theory of refinement and
the ACL2s theorem prover. The idea is to define an abstract P2P,
publish/subscribe protocol in which messages for any particular topic
are delivered atomically to all subscribers. The goal is to show that
any behaviors allowed by the protocols under analysis are behaviors
also allowed by our abstract P2P protocol, under appropriate
conditions. As our now published S&P 2024 paper shows, such
correctness criteria are both unknown and necessary to eliminate
attacks such as the novel ones we found. Therefore, it is an open
problem what these conditions are, how they can be exploited at the
protocol and application layers, and whether they can be used to
improve the protocols.

For this milestone, we have formalized a sequence of refinement models
and Ethereum application logic relevant to the behavior of the
protocols.

We have developed open, formal and executable ACL2s models for
Broadcastsub, Floodsub and Gossipsub. Each of these models are
available here: https://github.com/ankitku/EthGossip.

Here is a brief overview of the protocols.

1. Broadcastsub (Broadcastnet.lisp) : This is a high level
   specification for P2P pubsub protocols. Nodes are allowed to leave
   and join in arbitrary ways. Nodes can also subscribe to and
   unsubscribe from topics in arbitrary ways. Messages are broadcast
   in a single step such that all nodes that subscribes to the topic
   of the message receive the message atomically. Of course, this is
   not a realizable protocol, as atomic delivery in a distributed
   system is not possible, but it is a great specification because the
   point of P2P pubsub protocols is the efficient delivery of
   messages. 

2. Floodsub (Floodnet.lisp) : This is a lower level specification that
   adds flooding as a delivery mechanism to P2P pubsub
   networks. Instead of atomic broadcast, nodes maintain subscription
   information of their neighbors, and forward messages to subscribing
   neighboring peers. Note that it may take an arbitrary number of
   hops for a message sent from one peer to be received by another
   peer and that an arbitrary number of messages can be in flight at
   any point in time. 

3. Gossipsub (Gossipsub Folder) : This folder contains the executable
   specification for the Gossipsub protocol, where nodes can join or
   leave the network, score their neighbors using locally observable
   counters, form mesh connections with highly scored neighboring
   peers, prune low scoring peers, forward full messages to their mesh
   neighbors, send gossips to non-mesh subscribing peers, have fanouts
   and perform mesh maintaining activities. We conjectured some
   properties that a scoring function should satisfy, and found
   counter-examples to them (scoring-eth2.lisp). We also constructed
   attack gadgets and used them to show possible attacks, including
   eclipse and network partition attacks, in the Gossipsub/attacks
   folder. These attacks are verified on traces of runs of the
   Gossipsub model on actual Ethereum test net topologies: Goerli,
   Ropsten and Rinkeby. For these tests we are using actual Eth2.0
   application parameters to configure our Gossipsub model, such that
   the test runs represent possible runs of the Ethereum test net.

Detailed Summary.

You can find a detailed summary of our work in the two attached
papers.

The first paper is entitled "Formal Model-Driven Analysis of
Resilience of GossipSub to Attacks from Misbehaving Peers" and was
published in S&P 2024. Here is the abstract:

GossipSub is a new peer-to-peer communication protocol designed to
counter attacks from misbehaving peers by controlling what information
is sent and to whom, via a score function computed by each peer that
captures positive and negative behaviors of its neighbors. The score
function depends on several parameters (weights, caps, thresholds)
that can be configured by applications using GossipSub. The
specification for GossipSub is written in English and its resilience
to attacks from misbehaving peers is supported empirically by
emulation testing using an implementation in Golang.  In this work we
take a foundational approach to understanding the resilience of
GossipSub to attacks from misbehaving peers. We build the first
formal model of GossipSub, using the ACL2s theorem prover. Our model
is officially endorsed by the GossipSub developers. It can simulate
GossipSub networks of arbitrary size and topology, with arbitrarily
configured peers, and can be used to prove and disprove theorems about
the protocol. We formalize fundamental security properties stating
that the score function is fair, penalizes bad behavior, and rewards
good behavior. We prove that the score function is always fair, but
can be configured in ways that either penalize good behavior or ignore
bad behavior. Using our model, we run GossipSub with the specific
configurations for two popular real-world applications: the FileCoin
and Eth2.0 blockchains.  We show that all properties hold for
FileCoin. However, given any Eth2.0 network (of any topology and size)
with any number of potentially misbehaving peers, we can synthesize
attacks where these peers are able to continuously misbehave by never
forwarding topic messages, while maintaining positive scores so that
they are never pruned from the network by GossipSub.


The second paper is entitled "A Formalization of the Correctness of
the Floodsub Protocol" and is under review for ACL2 2025. It includes
the formalization of the broadcast and floodsub protocols, which are
part of the milestone and progress towards the second milestone, which
involves the refinement proofs. Here is the abstract:

Floodsub is a simple, robust and popular peer-to-peer network
protocol, where nodes can arbitrarily leave or join the network,
subscribe to or unsubscribe from topics and forward newly received
mes- sages to all of their neighbors, except the sender or the
originating peer. To show the correctness of Floodsub, we propose its
specification: Broadcastsub, in which implementation details like
network connections and neighbor subscriptions are elided. To show
that Floodsub does really implement Broadcastsub, one would have to
show that the two systems have related infinite computations. We prove
this by reasoning locally about states and their successors using
Wellfounded Equivalence Bisimulation (WEB). In this paper, we focus on
the mechanization of a proof which shows that Floodsub is a stuttering
WEB refinement of Broadcastsub. To the best of our knowledge, ours is
the first mechanized proof of refinement of a P2P protocol.

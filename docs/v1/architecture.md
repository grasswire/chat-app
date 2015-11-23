# Architecture - V1

<!--
PLEASE ADD ANYTHING I FORGOT OR ASK QUESTIONS AS A COMMENT!
Feel free to email/slack as well.
-->

# Tenancy

Tenancy is defined as one server "owning" all traffic and websocket connections for a single chat room.

## Tenancy Pros

- Potentially lower latency (doesn't round-trip through Redis/Kafka)

- Hypothetically more efficient in terms of overall resource usage (`sum listOfServers`) since we're not chit-chatting over the network a bunch. These needn't be blocking operations (it's haskell dammit), but still.

## Tenancy Cons

- Forces Zookeeper-or-similar distributed consensus problems (who owns a channel? What if a server goes down? We have to do master election for each channel.)

- More complicated

- We'd have to write an algorithm to solve the https://en.wikipedia.org/wiki/Knapsack_problem or assign chat room size tiers to classes of servers to use resources efficiently w/ tenancy.

# Load-balanced/non-tenancy

Non-tenancy/load-balanced is defined as clients getting routed to one server out of a load-balanced pool.

## Load-balanced Pros

- Simpler. Don't have to write code to manage tenancy or server migrations/master election among other things.

- Hypothetically means better scale-out (large chat rooms aren't limited by the network/memory of a single server)

- Better resource allocation. Load is automatically fairly evenly distributed.

## Load-balanced Cons

- Gossipy af. Shouldn't bite us for awhile

- Probably want to start doing some timeout-limited batching eventually

- Forces pub-sub listener to Redis per channel


# Tenancy decision

Load-balanced + Redis/pubsub for now if no one has objections.


# Persistence

Messages should always land somewhere safe, at least _eventually_. Messages table with a PK to the chat room is probably fine for now.

Client sends message -> backend, backend is responsible for sending the message to the relevant Redis pub/sub *and* to save it in the Message table in PostgreSQL.

## Scaling message persistence

We may need something more scalable for the permanent persistence of messages than a single messages table in postgresql, but the nature of channels make it pretty easy to shard the database and route inserts via middleware to the current postgresql shard responsible for that chatroom. Distributed hash ring + middleware probably. This will suck and we should put this off as long as possible. Single PostgreSQL server should survive the first 500m - 1 billion messages. Hopefully we can afford a PGSQL expert by then.


# Propagation

Redis pub/sub. Client context is per-channel (browser tab per chat channel). Websocket per chat channel, which may need to change later. Could change to Kafka or something embedded in Haskell later but doesn't need to happen right now.

Backend uses [unagi-chan](http://hackage.haskell.org/package/unagi-chan) or something with a fan-out of listeners to the websockets in order to propagate messages from the sub to the clients.

## Message type

Frontend-generated UUIDV4 per message so that messages can be synchronized via heartbeat gossip without duplication. Remember, the client knows the truth - the backend is only a middleman!

```haskell
-- Should look something like this pseudo-code
data Message =
  Message UUIDV4 UTCTime Text
```

## Heartbeat sync

Randomized interval of 3.5-7 seconds (to avoid [thundering herd](https://en.wikipedia.org/wiki/Thundering_herd_problem))

Can be piggybacked onto "presence" heartbeat. Just an extra field in the JSON document. Would look something like:

```JSON
{
"amhere": true,
"msgSeen":
  ["2b190304-9788-4dfb-be16-723cdcf6bb51"
   "2b190304-9788-4dfb-be16-723cdcf6bb52"
   "2b190304-9788-4dfb-be16-723cdcf6bb53"]
}
```

It's telling the backend what messages it's seen in the last ~minute or so. Server returns with "sync" message if-and-only-if there are UUIDs not included from the last minute in what the client reported. It returns only messages that didn't seem to be known to the frontend.

### Where the fuck does the heartbeat data come from?

We could keep an STM of a `Set` of the last ~2 minutes of messages per server to start.

```haskell
TVar (HashMap Channel (Set Message))
```

If we [want to get real clever](http://hackage.haskell.org/package/stm-containers).

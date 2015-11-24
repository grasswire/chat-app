# TapLike API - V1

# REST API

## Event Types

# Real Time Messaging API

# Models

## Channels

### Primary key

> you saying using int64 pk is a real issue?
> could use guids?

Problem with auto-incrementing primary keys is they force coordination on channel creation. If we use UUIDs or Flake IDs the API servers can generate the channel PKs. That doesn't save us a ​_ton_​ of compute on the database as PG still has to index the column. It spares us some grief if people start suddenly making 1,000s of channels.

Falls under nice problem to have, but might be worth planning ahead for.

If we did non-auto-inc PKs, we might as well use the slugs for the room URI. Unique title and then slugified title is how channels are looked up from the URI, but they're _not_ the PK. PK becomes user-invisible just like Slack.

#### Auto-inc PK

##### Pros

- Doesn't go against grain with Persistent

##### Cons

- Forces centralized coordination for channel creation (hostile to sharding)

- Allows hostile actors to enumerate/scrape channels because we'd be exposing PK via the URI

#### UUID/Flake PK

##### Pros

- Avoids coordination

- Changes `room/4/hiphop` -> `/room/hiphop` (more human friendly)

##### Cons

- Bloats index size (indexes would take up more memory on the PG instance)

- worsens index lookup perf a bit (not a lot)

- Custom Key type w/ Persistent, have to write code to generate server-side (not a big deal IMO)

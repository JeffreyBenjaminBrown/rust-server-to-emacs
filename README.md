# simple/ contains a simple solution

This works for small payloads. A connection is opened for each message. The folder has some Rust code and some Emacs code. The evidence that it works appears both in Emacs and in the stdou stream from the Rust server.

# For working at bigger scales

I will need to keep a persistent connection.

# How to run

Run the `docker run` command from `docker.sh`,
or something resembling it.
In the container, run `cd` to get to the user folder
(`/home/user`).

Choose an executable (see below) from Cargo.toml.
Supposing for this paragraph you chose `2-persistent`,
the next step is to run `cargo run --bin 2-persistent`.
And now, *outside* of Docker,
evaluate the commands in `2-persistent/main.el`
by hand, one at a time.

# The two executables

See the READMEs in 1-simple/ and 2-persistent/ for what they do. But in brief:

## 1-simple/ contains a simple solution.

It's good for low-traffic scenarios.

## 2-persistent contains a heavier-duty solution.

It keeps a persistent connection open,
which is better for higher traffic loads.
See `2-persistent/main.el` for more guidance.

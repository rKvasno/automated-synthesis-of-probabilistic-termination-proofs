FROM rust:1.72.1
WORKDIR /usr/src/asptp

COPY . .
RUN cargo install --features=cli --path .

ENTRYPOINT ["asptp"]

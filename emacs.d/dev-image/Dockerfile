FROM ubuntu:latest

ENV DEBIAN_FRONTEND="noninteractive" TZ="Europe/Stockholm"

# Tools
RUN apt-get -y update && apt-get -y install apt-utils
RUN apt-get -y install git curl unzip make tzdata python3-pip npm

# Javascript / Typescript / Json / YAML
RUN npm i -g yarn typescript typescript-language-server \
    && yarn global add yaml-language-server

# Erlang
RUN apt-get -y install ncurses-dev \
    && curl -q -L https://erlang.org/download/otp_src_24.0.tar.gz -o /tmp/otp_src_24.0.tar.gz \
    && curl -q -L https://github.com/erlang-ls/erlang_ls/archive/refs/heads/main.zip -o /tmp/erlang_ls.zip \
    && curl -q -L https://s3.amazonaws.com/rebar3/rebar3 -o /usr/bin/rebar3 && chmod +x /usr/bin/rebar3 \
    && tar -C /tmp -xf /tmp/otp_src_24.0.tar.gz \
    && unzip /tmp/erlang_ls.zip -d /tmp/
RUN cd /tmp/otp_src_24.0 && ./configure --prefix=/usr && make && make install \
    && cd /tmp/erlang_ls-main && make && make install \
    && cd /

# User environment
RUN useradd -m -u 1001 -g users dev
USER dev
RUN cd /home/dev && mkdir .config && mkdir -p .local/bin && mkdir .cargo/bin
ENV PATH="/home/dev/.local/bin:/home/dev/.cargo/bin:${PATH}" PS1='[DEV]\w\$ '

# Python
COPY ./pycodestyle /home/dev/.config/pycodestyle
COPY ./requirements.txt /requirements.txt
RUN pip3 install --user -r /requirements.txt

# RUST
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs -o /tmp/rustup \
    && sh /tmp/rustup -y \
    && rustup component add rls rust-analysis rust-src

WORKDIR /home/dev

FROM erlang:21.0

WORKDIR /usr/src/app
COPY . /usr/src/app
RUN rebar3 as prod tar
RUN mkdir -p /opt/rel
RUN tar -zxvf /usr/src/app/_build/prod/rel/*/*.tar.gz -C /opt/rel

CMD ["/opt/rel/bin/contacts_crud", "foreground"]
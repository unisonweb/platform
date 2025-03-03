FROM debian:stable

RUN adduser --home /unison --disabled-password unison

RUN apt-get update                                           && \
    apt-get install -y git libncurses5 less locales fzf      && \
    echo "en_US.UTF-8 UTF-8" > /etc/locale.gen               && \
    dpkg-reconfigure --frontend=noninteractive locales       && \
    update-locale LANG=en_US.UTF-8


COPY tmp/ucm/ /usr/local/bin/ucm/

ENV UCM_WEB_UI=/usr/local/share/ucm
ENV UCM_PORT=8080
ENV UCM_TOKEN=pub

RUN chmod 555 /usr/local/bin/ucm/ucm

EXPOSE 8080
ENTRYPOINT ["/usr/local/bin/ucm/ucm"]
CMD ["--codebase-create","/codebase"]

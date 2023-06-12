#FROM ubuntu:22.04
#
#RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends \
#        curl \
#     && apt-get update && apt-get -y upgrade
#
#RUN bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
#
#
#ENV PATH=~/.opam/
FROM ocaml/opam

ARG USERNAME=vim
ARG USER_UID=1000
ARG USER_GID=$USER_UID

USER root


RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
     && apt-get -y install --no-install-recommends \
        m4 \
     && apt-get update && apt-get -y upgrade

RUN if [ "$USER_GID" != "1000" ] || [ "$USER_UID" != "1000" ]; then \
    groupmod --gid $USER_GID $USERNAME \
    && usermod --uid $USER_UID --gid $USER_GID $USERNAME \
    && chown -R $USER_UID:$USER_GID /home/$USERNAME; \
    fi

USER opam

RUN opam update \
    && opam install async base core_kernel ounit qcheck dune ocamlformat

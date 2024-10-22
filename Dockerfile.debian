FROM debian:bookworm-slim as debbase

RUN apt-get update && apt-get -q -y install sbcl curl

ENV USER=app NB_UID=1000
ENV HOME=/home/${USER} 
ENV JUPYTER_PATH=${HOME}/.local/share/jupyter/ JUPYTERLAB_DIR=${HOME}/.local/share/jupyter/lab/ \
    PATH="${HOME}/.local/bin:${PATH}"


RUN useradd --create-home --shell=/bin/false --uid=${NB_UID} ${USER}



FROM debbase as buildsys

RUN apt-get update && apt-get -q -y install cmake gcc libtool git autoconf \
                                    python3 binutils g++ gperf make curl \
				    texinfo \
				    python3-minimal python3-pip \
				    python3-venv \
				    libzmq3-dev


ENV maxima_build tags/5.47.0

RUN git clone https://git.code.sf.net/p/maxima/code maxima-code && \
    cd maxima-code && \
    git checkout ${maxima_build} && \
    mkdir dist && \
    ./bootstrap && \
    ./configure --enable-sbcl-exec --enable-quiet-build --prefix=/maxima && \
    make -s -j$(nproc) && \
    make install


WORKDIR ${HOME}
USER ${USER}

RUN python3 -m venv jupyterenv && \
    jupyterenv/bin/pip install   jupyter


ENV NVM_DIR=${HOME}/nvm NODE_VERSION=22

RUN mkdir -p $NVM_DIR &&  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash

SHELL ["/bin/bash", "-c"] 

RUN source $NVM_DIR/nvm.sh \
    && nvm install $NODE_VERSION \
    && nvm alias default $NODE_VERSION \
    && nvm use default

RUN source $NVM_DIR/nvm.sh && \
    jupyterenv/bin/jupyter server extension enable --user --py jupyterlab && \
    jupyterenv/bin/jupyter labextension install @jupyter-widgets/jupyterlab-manager && \
    curl -kLO https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive --load quicklisp.lisp \
      --eval "(quicklisp-quickstart:install)" \
      --eval "(ql-util:without-prompting (ql:add-to-init-file))"

USER root


WORKDIR ${HOME}/maxima-jupyter

# Use .dockerignore to avoid copying unnecsary files such as ".git" directory
COPY .  ${HOME}/maxima-jupyter/

RUN chown -R ${NB_UID} ${HOME} && chgrp -R ${USER} ${HOME}

USER ${USER}

RUN source $NVM_DIR/nvm.sh && ~/jupyterenv/bin/jupyter lab build

# This is needed to find  the sbcl contrib directory for asdf etc
ENV SBCL_HOME /usr/lib/sbcl

RUN /maxima/bin/maxima --batch-string="load(\"load-maxima-jupyter.lisp\");jupyter_install();"

# Remove unneeded files (newer docker could use --exclude on copy?)
RUN rm -rf ${HOME}/.local/share/jupyter/lab/staging

FROM debbase

RUN apt-get update &&  apt-get -q -y install  python3-minimal libzmq3-dev gnuplot-nox
RUN find /usr -iname "*.a" -delete

USER ${USER}

# Older Docker does not support merging these statements together...
COPY --from=buildsys /maxima /maxima
COPY --from=buildsys /home/app/jupyterenv /home/app/jupyterenv
COPY --from=buildsys /home/app/quicklisp /home/app/quicklisp
COPY --from=buildsys /home/app/maxima-jupyter /home/app/maxima-jupyter
COPY --from=buildsys /home/app/.sbclrc /home/app/.sbclrc
COPY --from=buildsys /home/app/.local /home/app/.local
COPY --from=buildsys /home/app/.jupyter /home/app/.jupyter
COPY --from=buildsys /home/app/.cache /home/app/.cache

ENV PATH="${PATH}:/maxima/bin:${HOME}/jupyterenv/bin" SBCL_HOME=/usr/lib/sbcl
WORKDIR ${HOME}/maxima-jupyter/examples


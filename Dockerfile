FROM archlinux:latest

ARG NB_USER=app
ARG NB_UID=1000

ENV USER ${NB_USER}
ENV HOME /home/${NB_USER}
ENV JUPYTER_PATH=${HOME}/.local/share/jupyter/
ENV JUPYTERLAB_DIR=${HOME}/.local/share/jupyter/lab/
ENV PATH "${HOME}/.local/bin:${PATH}"

RUN echo -e "[multilib]\nInclude = /etc/pacman.d/mirrorlist" >> /etc/pacman.conf


RUN pacman -Syu --noconfirm --needed base-devel lib32-zeromq git \
      npm jupyterlab jupyter_console

RUN pacman -Syu --noconfirm --needed sbcl gnuplot maxima maxima-sbcl
RUN useradd --create-home --shell=/bin/false --uid=${NB_UID} ${NB_USER}

WORKDIR ${HOME}

USER ${NB_USER}

RUN jupyter serverextension enable --user --py jupyterlab; \
    jupyter labextension install @jupyter-widgets/jupyterlab-manager; \
    curl -kLO https://beta.quicklisp.org/quicklisp.lisp; \
    sbcl --non-interactive --load quicklisp.lisp \
      --eval "(quicklisp-quickstart:install)" \
      --eval "(ql-util:without-prompting (ql:add-to-init-file))"

USER root

WORKDIR ${HOME}/maxima-jupyter

COPY . ${HOME}/maxima-jupyter

RUN chown -R ${NB_UID} ${HOME} && chgrp -R ${NB_USER} ${HOME}

USER ${NB_USER}

RUN jupyter lab build

RUN maxima --batch-string="load(\"load-maxima-jupyter.lisp\");jupyter_install();"

WORKDIR ${HOME}/maxima-jupyter/examples



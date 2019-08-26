FROM archlinux/base

ARG NB_USER=mj
ARG NB_UID=1000

ENV USER ${NB_USER}
ENV HOME /home/${NB_USER}

#RUN echo bust cache
RUN pacman -Syu --noconfirm base-devel jupyter gnuplot maxima && \
    useradd --create-home --shell=/bin/false --uid=${NB_UID} ${NB_USER}

WORKDIR ${HOME}/maxima-jupyter

COPY . ${HOME}/maxima-jupyter
COPY maxima.js /usr/lib/python3.7/site-packages/notebook/static/components/codemirror/mode/maxima
COPY maxima_lexer.py /usr/lib/python3.7/site-packages/pygments/lexers

RUN patch /usr/lib/python3.7/site-packages/notebook/static/components/codemirror/mode/meta.js codemirror-mode-meta-patch && \
    patch /usr/lib/python3.7/site-packages/pygments/lexers/_mapping.py pygments-mapping-patch && \
    chown -R ${NB_UID} ${HOME} && chgrp -R ${NB_USER} ${HOME}

USER ${NB_USER}

RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp --load docker-install-quicklisp.lisp && \
    python install-maxima-jupyter.py --user --root=`pwd` && \
    echo quit | jupyter-console --no-confirm-exit --kernel=maxima --ZMQTerminalInteractiveShell.kernel_timeout=240

USER root
RUN pacman -Scc --noconfirm

USER ${NB_USER}
WORKDIR ${HOME}/maxima-jupyter/examples

EXPOSE 8888
CMD ["console", "--kernel=maxima"]
ENTRYPOINT ["jupyter"]

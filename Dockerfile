FROM archlinux/base:latest

ARG NB_USER=app
ARG NB_UID=1000

ENV USER ${NB_USER}
ENV HOME /home/${NB_USER}
ENV PATH "${HOME}/.local/bin:${PATH}"

RUN pacman -Syu --noconfirm base-devel zeromq gnuplot maxima python-pip npm && \
    useradd --create-home --shell=/bin/false --uid=${NB_UID} ${NB_USER} && \
    pacman -Scc --noconfirm

WORKDIR ${HOME}/maxima-jupyter

COPY . ${HOME}/maxima-jupyter

RUN chown -R ${NB_UID} ${HOME} && chgrp -R ${NB_USER} ${HOME}

USER ${NB_USER}

RUN pip install --user jupyter jupyterlab && \
    jupyter serverextension enable --user --py jupyterlab && \
    jupyter labextension install @jupyter-widgets/jupyterlab-manager && \
    jupyter nbextension enable --user --py widgetsnbextension && \
    export PYTHON_USER_SITE=$(python -m site --user-site) && \
    mkdir -p ${PYTHON_USER_SITE}/notebook/static/components/codemirror/mode/maxima/ && \
    cp maxima.js ${PYTHON_USER_SITE}/notebook/static/components/codemirror/mode/maxima/ && \
    patch ${PYTHON_USER_SITE}/notebook/static/components/codemirror/mode/meta.js codemirror-mode-meta-patch && \
    cp maxima_lexer.py ${PYTHON_USER_SITE}/pygments/lexers/ && \
    patch ${PYTHON_USER_SITE}/pygments/lexers/_mapping.py pygments-mapping-patch && \
    curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp --load docker-install-quicklisp.lisp && \
    maxima --batch-string="load(\"load-maxima-jupyter.lisp\");jupyter_install();"

WORKDIR ${HOME}/maxima-jupyter/examples



# Maxima-Jupyter

[![Binder][mybinder-badge]][mybinder]

An enhanced interactive environment for the computer algebra system Maxima,
based on CL-Jupyter, a Jupyter kernel for Common Lisp, by Frederic Peschanski.
Thanks, Frederic! These days Maxima-Jupyter depends heavily on Common-Lisp-Jupyter,
by Tarn Burton. Thanks, Tarn!

This file describes the installation and usage of Maxima-Jupyter on a local
machine, but you can try out Maxima-Jupyter without installing anything by
clicking on the Binder badge above.

## Examples

- [MaximaJupyterExample.ipynb][] &mdash; General usage of Maxima from within
  JupyterLab.

- [MaximaJupyterTalk.ipynb][] &mdash; My notes for a talk given to the Portland
  Python User Group.

- [Plots.ipynb][] &mdash; Usage of plotting facilities from within JupyterLab.

These examples make use of [nbviewer][].
You can submit a link to your own notebook to tell nbviewer to render it.

## Installation

Maxima-Jupyter may be installed on a machine using a local installation, a
[repo2docker][] installation, or via a Docker image.

## Local Installation

### Requirements

To try Maxima-Jupyter you need :

 - a Maxima executable

   - built with a Common Lisp implementation which has native threads

     - SBCL works for sure

     - Clozure CL works for sure

     - Other implementations which support the Bordeaux Threads package
       might work. The [Bordeaux Threads project description][] says
       "Supports all major Common Lisp implementations: SBCL, CCL, Lispworks, 
       Allegro, ABCL, ECL, Clisp." Aside from SBCL and CCL (i.e. Clozure CL) 
       which are known to work, the others in that list are untested with 
       maxima-jupyter.

     - Note also that ECL might theoretically work, since it is supported
       by Bordeaux Threads. However, nobody (neither Maxima-Jupyter 
       developers nor users) has been able to get ECL to work,
       therefore you should assume **ECL does not work** with Maxima-Jupyter.
       SBCL and Clozure CL are known to work, try those instead.

     - Note specifically that GCL is not supported by Bordeaux Threads,
       and therefore **GCL cannot work** with maxima-jupyter.

   - You might or might not need to build Maxima. (A) If you have available
     a Maxima binary package compiled with a compatible Lisp implementation
     (i.e. SBCL, Clozure CL, Lispworks, etc. as enumerated above)
     **and** ASDF is available to the Lisp runtime when Maxima is executed
     (more on this immediately below), then you do not need to build Maxima.
     (B) Otherwise, you must install a compatible Lisp implementation
     and compile Maxima yourself.

   - NOTE about how to determine if ASDF is available to the Lisp runtime.
     Execute Maxima, and then, at the Maxima input prompt, enter
     `:lisp (require :asdf)`
     If that returns T or NIL without an error message, then ASDF is
     available. If that causes an error about "don't know how to require
     ASDF" or something like that, then ASDF is not available.

 - [Quicklisp][]

   - When you load Maxima-Jupyter into Maxima for the first time,
     Quicklisp will download some dependencies automatically.
     Good luck.

 - Python 3.2 or above

 - [JupyterLab][]

 - If the build aborts because the file `zmq.h` is missing, you may need to
   install the development files for the high-level C binding for ZeroMQ.
   On debian-based systems, you can satisfy this requirement by installing
   the package `libczmq-dev`.

### Installing Maxima-Jupyter

#### Installing Maxima-Jupyter on MS Windows

Here is one recipe for pulling everything together for MS Windows.
There are other ways to achieve the same end;
this is one way which is known to work.

1. Install Maxima and Gnuplot.

   My advice is to install Maxima from an executable installer, which also includes Gnuplot.
   Obtain an installer from the [Maxima project download page for Windows][].

1. Update `Path` to include the folders which contain `maxima.bat` and `gnuplot.exe`.

   To edit the environment variable `Path`,
   click the "Start" button and enter "environment variables" in the search box,
   then select "System Properties" from the search results.
   Click "Environment Variables" in the "System Properties" popup window.
   Then select "Path" in the list of variables, and click "Edit...".

   I added the following folders to `Path`, in this order:
   ```
   C:\maxima-5.47.0\gnuplot\bin
   C:\maxima-5.47.0\bin
   ```

1. Install `conda`, a package manager for Python.

   My advice is to install Miniconda, which includes a minimal set of components.
   Follow the instructions on the [Miniconda installation page][].

1. Install GCC, ZeroMQ, and Jupyter via `conda`.

   In a command window (`cmd.exe`), execute the following commands:

   ```
   > conda install --channel conda-forge m2w64-gcc
   > conda install --channel conda-forge m2w64-zeromq
   > conda install --channel conda-forge jupyterlab jupyter_console
   ```

1. Update `Path` to include the `conda` folders.

   I added the following folders to `Path`, in this order:
   ```
   C:\ProgramData\miniconda3\Library\mingw-w64\bin
   C:\ProgramData\miniconda3\Library\bin
   C:\ProgramData\miniconda3\Scripts
   C:\ProgramData\miniconda3
   ```

1. Install Quicklisp.

   Download the [Quicklisp installer][]
   and then execute `(load "quicklisp.lisp")` in a Lisp interpreter.
   The simplest way to arrange that is to execute `maxima.bat`
   and then, in your Maxima session, execute
   ```
   (%i1) :lisp (load "quicklisp.lisp")
   ```

1. Obtain Maxima-Jupyter source code.

   To obtain the Maxima-Jupyter source code,
   navigate to the [Maxima-Jupyter project page][]
   and click the down arrow on the button labeled "Code",
   and select "Download ZIP".

   Extract the contents of the zip archive into a suitable folder.

1. Install Maxima-Jupyter.

   Assuming that the folder which now contains Maxima-Jupyter is named `path\to\maxima-jupyter`,
   open a command window (`cmd.exe`) and execute `maxima.bat`,

   ```
   > cd path\to\maxima-jupyter
   > maxima.bat
   ```
   and then execute the following commands in the Maxima session,
   ```
   (%i1) load ("load-maxima-jupyter.lisp");
   (%i2) jupyter_install_image ();
   ```
   which creates an image (binary executable) containing Maxima-Jupyter in addition to Maxima itself.

1. Launch Jupyter.

   In a command window (`cmd.exe`), execute the following commands.
   Here `path\to\a\writeable\folder` is any folder in which you can create files.

   ```
   > cd path\to\a\writeable\folder
   > jupyter lab
   ```

   Jupyter will open a tab in a web browser (MS Edge when I tried it),
   showing icons for any installed kernels (language-specific plugins),
   which should now include Maxima.

#### Installing Maxima-Jupyter on Unix-like systems

On Unix-like systems, Jupyter is installed via `pip` (Python package installer).
If you plan on using JupyterLab (which provides the notebook interface) then you
must install with the `--user` option.

```
python3 -m pip --user install jupyterlab jupyter-console
```

Once Jupyter is installed, you can install Maxima-Jupyter.

You can install Maxima-Jupyter from the source files of this repository
on both Unix-like systems and MS Windows.

If you are using Arch Linux, you can install Maxima-Jupyter via the AUR.

##### Method 1. Source Based Installation

To install Maxima-Jupyter from the current source files,
first download the source files and then start a shell in the source directory.
Then start Maxima and load the initialization script.

```sh
$ maxima
Maxima 5.43.0 http://maxima.sourceforge.net
using Lisp SBCL 1.5.5
Distributed under the GNU Public License. See the file COPYING.
Dedicated to the memory of William Schelter.
The function bug_report() provides bug reporting information.
(%i1) load("load-maxima-jupyter.lisp");
```

Then install Maxima-Jupyter using *one* of the kernel types.

1. User-specific installation, with kernel loaded by Quicklisp: `jupyter_install();`
2. User-specific installation, with kernel saved in binary image: `jupyter_install_image();`
3. System-wide installation, with kernel loaded by Quicklisp: `jupyter_system_install(true, "pkg/");`

After selecting the kernel type, exit Maxima.
For the system-wide installation, copy the files in `pkg` to the system root, i.e. 
`sudo cp -r pkg/* /` on Linux.

##### Method 2. Installing Maxima-Jupyter on Arch/Manjaro Linux

The package for Arch Linux is [maxima-jupyter-git][]. Building and installing
(including dependencies) can be accomplished with:

```sh
yaourt -Sy maxima-jupyter-git
```

Alternatively use ``makepkg``:

```sh
curl -L -O https://aur.archlinux.org/cgit/aur.git/snapshot/maxima-jupyter-git.tar.gz
tar -xvf maxima-jupyter-git.tar.gz
cd maxima-jupyter-git
makepkg -Csri
```

Please consult the [Arch Wiki][] for more information regarding installing
packages from the AUR.

### Code Highlighting Installation

Highlighting Maxima code is handled by [CodeMirror][] in the notebook
and [Pygments][] in HTML export.

A [CodeMirror mode for Maxima][] has been published on [npmjs.com][].
It is not clear how that needs to be installed in order for Maxima-Jupyter to make use of it;
stay tuned for further info.

A Maxima lexer for Pygments has been submitted and accepted by the Pygments project,
and it will be bundled with the next release of Pygments (2.11).
In the meantime, we are lacking highlighting in HTML export.

### Running Maxima-Jupyter

Maxima-Jupyter may be run from a local installation in console mode by the following.

```sh
jupyter-console --kernel=maxima
```

Notebook mode is initiated by the following.

```sh
jupyter-lab
```

When you enter stuff to be evaluated, you must include the usual trailing
semicolon or dollar sign:

```
In [1]: 2*21;
Out[1]: 42

In [2]:
```

## repo2docker Usage

Maxima-Jupyter may be run as a Docker image managed by repo2docker which will
fetch the current code from GitHub and handle all the details of running the
JupyterLab server.

First you need to install repo2docker (`sudo` may be required)

```sh
pip install jupyter-repo2docker
```

Once repo2docker is installed then the following will build and start the
server. Directions on accessing the server will be displayed once the image
is built.

```sh
jupyter-repo2docker --user-id=1000 --user-name=mj https://github.com/robert-dodier/maxima-jupyter
```

## Docker Image

A Docker image of Maxima-Jupyter may be built using the following command
(`sudo` may be required). This image is based on the docker image
`archlinux/base`.

```sh
docker build --tag=maxima-jupyter .
```
If you'd like to build with a different user than the default (`mj`), you may
override it with the following:

```sh
docker build --build-arg NB_USER=alice --tag=maxima-jupyter .
```

After the image is built the container may be run with:

```sh
docker run -it maxima-jupyter
```

The `Dockerfile` makes use of the `ENTRYPOINT` command; the default behaviour
executes the `jupyter` binary with the arguments `console --kernel=maxima`. 

If you'd like to run using Juypter's notebook web server, you may do the
following to override the default use of `console`:

```sh
docker run -it \
    -v `pwd`/notebooks:/home/USER/maxima-jupyter/examples \
    -p 8888:8888 \
    maxima-jupyter \
    notebook --ip=0.0.0.0 --port=8888
```

where the last line is the set of arguments to `jupyter` that cause it to run
in the notebook server mode.

To run the Bash shell on the container, just override the entry point:

```sh
docker run -it --entrypoint=bash maxima-jupyter
``` 

If you cannot build the Docker image, you may use a 
[pre-built one](https://hub.docker.com/r/calyau/maxima-jupyter)
by subsituting the Docker image name `maxima-jupyter` in the above `docker`
commands with `calyau/maxima-jupyter`. Note that the default user on the
`calyau` image is not `mj`, but is rather `oubiwann`.

Additional examples of notebooks created using this mode have been created
here (taken from the Maxima tutorial):
[https://github.com/calyau/maxima-tutorial-notebooks](https://github.com/calyau/maxima-tutorial-notebooks).

----

Have fun! If you run into problems, please open a ticket on the issue tracker for this project.

<!--refs-->

[Arch Wiki]: https://wiki.archlinux.org/index.php/Arch_User_Repository#Installing_packages
[Bordeaux Threads project description]: https://common-lisp.net/project/bordeaux-threads/
[CodeMirror]: https://codemirror.net 
[npmjs.com]: https://www.npmjs.com
[CodeMirror mode for Maxima]: https://www.npmjs.com/package/codemirror-mode-maxima
[Pygments]: https://pygments.org
[JupyterLab]: https://jupyter.org/install.html
[Maxima project download page for Windows]: https://sourceforge.net/projects/maxima/files/Maxima-Windows/
[Miniconda installation page]: https://docs.anaconda.com/miniconda/install/
[Quicklisp installer]: https://beta.quicklisp.org/quicklisp.lisp
[Maxima-Jupyter project page]: https://github.com/robert-dodier/maxima-jupyter
[maxima-jupyter-git]: https://aur.archlinux.org/packages/maxima-jupyter-git/
[MaximaJupyterExample.ipynb]: http://nbviewer.jupyter.org/github/robert-dodier/maxima-jupyter/blob/master/examples/MaximaJupyterExample.ipynb
[MaximaJupyterTalk.ipynb]: http://nbviewer.jupyter.org/github/robert-dodier/maxima-jupyter/blob/master/examples/MaximaJupyterTalk.ipynb
[mybinder-badge]: https://mybinder.org/badge_logo.svg
[mybinder]: https://mybinder.org/v2/gh/robert-dodier/maxima-jupyter/master?urlpath=lab
[nbviewer]: http://nbviewer.jupyter.org
[Plots.ipynb]: http://nbviewer.jupyter.org/github/robert-dodier/maxima-jupyter/blob/master/examples/Plots.ipynb
[Quicklisp]: http://www.quicklisp.org
[repo2docker]: https://repo2docker.readthedocs.io/en/latest/

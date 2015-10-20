Irmin Filer
===========

Copyright Thomas Leonard, 2015

This is an **experimental** web-based Irmin file editor.
It can clone an Irmin repository from a server and edit the files locally.
Changes can be sync'd with the server. 
Files can be edited while offline and sync'd later.
If the repository is open in multiple tabs, the tabs will stay in sync.
It can also run without any server, as a local notebook.

Note: for a more complete example, see [CueKeeper][] (in fact, this project was hacked together quickly by starting from CueKeeper and removing most of the code...).


Installation
------------

You'll need the [opam](http://opam.ocaml.org/) package manager.
It should be available through your distribution, but you can use a [generic opam binary](http://tools.ocaml.org/opam.xml) if it's missing or too old (I use opam 1.2).
Ensure you're using OCaml 4.01 (check with `ocaml -version`).
If not, switch to 4.01.0 (4.02 is not yet supported, as the `bin_prot` patches need updating):

    opam sw 4.01.0

Pin a few patches we require:

    opam pin add -n sexplib 'https://github.com/talex5/sexplib.git#js_of_ocaml'
    opam pin add -n reactiveData https://github.com/hhugo/reactiveData.git
    opam pin add -n bin_prot 'https://github.com/talex5/bin_prot.git#js_of_ocaml'
    opam pin add -n dolog 'https://github.com/UnixJunkie/dolog.git#no_unix'

    opam update

Install the dependencies:

    opam install sexplib irmin tyxml reactiveData js_of_ocaml base64 tar-format crunch cohttp irmin-indexeddb ounit mirage-http

Build:

    make

Load `test.html` in a browser to test locally (no server required).


Instructions
------------

Open the `test.html` file in a browser.
A local Irmin repository is created in the browser and the display shows the current state of the `master` branch.
The left column shows the files, arranged in a tree.
Click on a file to view it.
Click on a `+` sign to create a new file.
To create a directory, create a file with a `/` in its name (e.g. creating `/foo/bar/baz` will create the two required directories automatically.)

To edit a file, click the `(edit)` button and edit the text.
To rename or move a file, click on the panel title and enter the new path.
You can also move a file by clicking on the name of its parent directory and choosing a new parent from the menu.

There are some buttons along the top:

* **Sync** will synchronise with the server (if running without a server, this is not shown).
* **Export** allows you to save the current revision as a tar archive.
* **Show history** shows the Irmin history, which also allows visiting previous states and reverting changes.
* **Close all** closes all open panels in the right column.


Running a server
----------------

While `test.html` can be opened directly in a browser, as above, you can also build a server.
This allows you to sync between devices (e.g. a laptop and mobile phone).

**Warning: This is a work-in-progress**:

- The server does not yet persist the data itself
  (the client sends the whole history the first time it connects after the service is restarted).
- You have to sync manually by clicking the `Sync` button - it does not send or fetch changes automatically.

First, generate an access token (a *long* random string that grants access to the server).
The `pwgen` command is useful for this:

    $ pwgen -s 32 1
    dtXZ7fQfX52VsnJNk22J6uKy8JSn6klb

To avoid storing the secret in the server binary, generate its SHA256 hash:

    $ echo -n dtXZ7fQfX52VsnJNk22J6uKy8JSn6klb | sha256sum
    774400f3384a6f37cc2bc54b2fd0280193b613a5bc401c0e54fd17fe4ec19572

Copy the file `server/devices.ml.example` as `server/devices.ml` and add the hash
you generated above, e.g.:

    let lookup = function
      | "774400f3384a6f37cc2bc54b2fd0280193b613a5bc401c0e54fd17fe4ec19572" -> Some "Laptop"
      | _ -> None

The string at the end ("Laptop") is just used for logging.
You can generate a different access token for each device you want to sync and list them all here, one per line.
Make sure the `None` line comes last - this rejects all unknown tokens.

To build the server component:

    opam install mirage
    make server

You will be prompted to create a self-signed X.509 certificate. Just enter your server's hostname
as the "Common Name" (for testing, you could use "localhost" here and generate a proper one later).

To run the server:

    ./server/mir-irmin-filer

By default the server listens on TCP port 8443, but this can be changed by editing `server/unikernel.ml`.

Open the URL in a browser, e.g.

    https://localhost:8443/

You'll probably now get some scary-looking warning about the certificate not being trusted.
To get rid of the warning, add your newly-generated server.pem as follows:

In Firefox:

1. Firefox will say "This Connection is Untrusted".
2. Expand the **I Understand the Risks** section.
3. Click **Add Exception**, then **Confirm Security Exception** (and "Permanently store this exception").

In Chrome:

1. It will say "Your connection is not private" (in fact, the opposite is true; if encryption wasn't being used it wouldn't have complained at all).
2. Go to **Settings** -> **Show advanced settings**.
3. Click the **Manage certificates** button (in the HTTPS/SSL section).
4. In the **Authorities** tab, click **Import...** and select your `server/conf/tls/server.pem` file.
5. Select **Trust this certificate for identifying websites**.

Finally, you should be prompted for your access key.
Paste in the token you generated above (e.g. `dtXZ7fQfX52VsnJNk22J6uKy8JSn6klb` in the example above - *not* the hash).

Deploying as a Xen VM
---------------------

In fact, the server is a [Mirage unikernel][mirage] and can also be compiled and booted as a Xen virtual machine:

    make server MIRAGE_FLAGS="--xen"
    cd server
    xl create -c irmin-filer.xl


Bugs
----

Please any send questions or comments to the mirage mailing list:

http://lists.xenproject.org/cgi-bin/mailman/listinfo/mirageos-devel

Bugs can be reported on the mailing list or as GitHub issues:

https://github.com/talex5/irmin-filer/issues


Conditions
----------

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
USA


This project includes Foundation (http://foundation.zurb.com). These files
are released under the MIT license.


This project includes FileSaver.js (https://github.com/eligrey/FileSaver.js), which
is released under a permissive license.


Full details of all licenses can be found in the LICENSE file.


[mirage]: http://openmirage.org/
[CueKeeper]: https://github.com/talex5/CueKeeper

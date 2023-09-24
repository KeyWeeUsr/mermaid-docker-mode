# mermaid-docker-mode

Render mermaid graphs with Docker service

This library attempts to create Mermaid graphs via
[mermaid.ink](https://github.com/jihchi/mermaid.ink) aka
[mermaid](https://mermaid.js.org/) as an API via a custom locally-built Docker
image with restricted network access, so that you are sure nothing gets out and
your system is kept isolated from random Node.js deps/files noise all around
the filesystem.

## How to

1. Have [`mermaid-mode`](https://melpa.org/#/mermaid-mode) installed.
2. Run the install function `M-x mermaid-docker-install`
3. Activate this mode with `M-x mermaid-docker-mode`
4. After successful installation `C-c C-c` for `mermaid-mode` is patched and
   renders the graphs via Docker container

## Customization

By default the rendering is set to happen in an external viewer (such as
`xviewer`) and can be changed to any other binary or set to render within Emacs
itself via inserting an image into a new buffer.

Make sure to check related `defconst` parts of the file to customize.

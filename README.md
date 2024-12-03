# mermaid-docker-mode
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

Render mermaid graphs with Docker service

This library attempts to create Mermaid graphs via the official
[`mermaid-cli`](https://github.com/mermaid-js/mermaid-cli) using the official
Docker image and restricting network access, so that you are sure nothing gets
out and your system is kept isolated from random Node.js deps/files noise all
around the filesystem.

## How to

Install it from [Melpa](https://melpa.org/#/getting-started) or clone and
install manually, then:

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

[melpa-badge]: http://melpa.org/packages/mermaid-docker-mode-badge.svg
[melpa-package]: http://melpa.org/#/mermaid-docker-mode
[melpa-stable-badge]: http://stable.melpa.org/packages/mermaid-docker-mode-badge.svg
[melpa-stable-package]: http://stable.melpa.org/#/mermaid-docker-mode
[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr

# mermaid-docker-mode
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![CI][ci-badge]][ci-workflow]
[![Coverage Status][cover-badge]][cover-link]
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

Name                                |Type     |Default               |Description                                                 |
------------------------------------|---------|----------------------|------------------------------------------------------------|
`mermaid-docker-verbose`            |`boolean`|`t`                   |Emit messages when something's happening in the background. |
`mermaid-docker-always-check-deps`  |`boolean`|`t`                   |Always look up binaries, libraries and other required tools.|
`mermaid-docker-image-name`         |`string` |`"minlag/mermaid-cli"`|Official mermaid-cli image.                                 |
`mermaid-docker-image-tag`          |`string` |`"11.4.1"`            |Tag for official mermaid-cli image.                         |
`mermaid-docker-output-format`      |`string` |`"png"`               |Output format for rendered diagram.                         |
`mermaid-docker-output`             |`string` |`""`                  |Default file output ('' / empty string).                    |
`mermaid-docker-external-viewer-bin`|`string` |`"/usr/bin/xviewer"`  |Path to external image viewer.                              |
`mermaid-docker-focus-steal-fix`    |`boolean`|`t`                   |Should attempt to fix focus stealing?                       |
`mermaid-docker-focus-steal-ms`     |`number` |`200`                 |Milliseconds to wait before stealing focus back.            |
`mermaid-docker-external`           |`boolean`|`nil`                 |Use external viewer to display rendered mermaid graph.      |
`mermaid-docker-stay-in-window`     |`boolean`|`nil`                 |Stay in window with the diagram after rendering.            |

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
[ci-badge]: https://github.com/KeyWeeUsr/mermaid-docker-mode/actions/workflows/test.yml/badge.svg
[ci-workflow]: https://github.com/KeyWeeUsr/mermaid-docker-mode/actions/workflows/test.yml
[cover-badge]: https://coveralls.io/repos/github/KeyWeeUsr/mermaid-docker-mode/badge.svg?branch=master
[cover-link]: https://coveralls.io/github/KeyWeeUsr/mermaid-docker-mode?branch=master

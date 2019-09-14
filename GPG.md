# GPG Refresher commands

## List keys

`gpg --list-keys`

## Import public/private key

`gpg --import key.asc`

`key.asc` can be private or a public key.

## Import your public key from MIT server

`gpg --keyserver hkp://pgp.mit.edu --recv 0xD19E3E0EBB557613`

## Export to keyserver

`gpg --keyserver hkp://pgp.mit.edu --send-key keyid`

Use no-grab option: http://unix.stackexchange.com/a/266351/29539

Reference: 

* GPG Tutorial
  - [https://www.paulfurley.com/gpg-for-humans-why-care-about-cryptography/](Part 1)
  - [https://www.paulfurley.com/gpg-for-humans-public-key-crypto-primer/](Part 2)
  - [https://www.paulfurley.com/gpg-for-humans-protecting-your-primary-key/](Part 3 - Found very useful)
  - [https://www.paulfurley.com/gpg-for-humans-preparing-an-offline-machine/](Part 4)
* [GPG Best practices](https://riseup.net/en/security/message-security/openpgp/best-practices)

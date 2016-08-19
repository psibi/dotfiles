# GPG Refresher commands

## List keys

`gpg --list-keys`

## Import public/private key

`gpg --import key.asc`

`key.asc` can be private or a public key.

## Import your public key from MIT server

`gpg --keyserver hkp://pgp.mit.edu --recv BB557613`

## Export to keyserver

`gpg --keyserver hkp://pgp.mit.edu --send-key keyid`

Hints:

Typing passwords suck. Use keepassx for auto typing.
Use no-grab option: http://unix.stackexchange.com/a/266351/29539

Reference: 

* [Fedora GPG guide](https://fedoraproject.org/wiki/Creating_GPG_Keys)

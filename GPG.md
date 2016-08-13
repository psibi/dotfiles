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


Reference: 

* [Fedora GPG guide](https://fedoraproject.org/wiki/Creating_GPG_Keys)





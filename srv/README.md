# Sibi's Salt state

## Installation

* Install `salt-master` and `salt-minion` in your system.
* Edit `/etc/salt/master`:

```
file_roots:
 base:
   - /home/sibi/github/dotfiles/srv/salt/
```

* Edit `/etc/salt/minion`:

```
master: 127.0.0.1
```

* Restart the master and minion:

```
$ sudo systemctl salt-master
$ sudo systemctl salt-minion
```

* Accept the keys

```
$ salt-key --accept-all
```

* Apply the salt state:

```
$ salt '*' state.apply
```

## Other tips

### Command to apply specific state

```
$ salt '*' state.apply common.emacs
```

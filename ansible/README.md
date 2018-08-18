# My ansible configuration

## Ping test

```
ansible all -m ping
```
Or

```
ansible "*" -m ping
```
## Running shell commands

``` shellsession
$ ansible -m shell -a 'hostname' all
$ ansible -m shell -a 'df -h /' all
$ ansible -m shell -a 'who am i' all
```

## Run playbook

``` shellsession
ansible-playbook ./playbooks/apply-common.yml
```

You need to pass the parameter `-K` to make it sudo

```
ansible-playbook -K ./playbooks/apply-common.yml


```

A handler is just like another ansible task but it will executed only if it is triggered by task.

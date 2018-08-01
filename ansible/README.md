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



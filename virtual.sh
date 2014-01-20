# Make Caps as Ctrl key for Virtual Console.
# Once you run this command, put this command in /etc/rc.local
# loadkeys -v /usr/share/keymaps/Caps2Ctrl.map
# The above command should be put before exit 0.

##Todo: Write a script to automize the above stuff.

if [[ $EUID -ne 0 ]]; then
    echo "This script must be run as root" 1>&2
    exit 1
else
    if [ ! -d "/usr/share/keymaps" ]; then
        mkdir -p /usr/share/keymaps
    fi
    cd /usr/share/keymaps
    dumpkeys | head -1 > /usr/share/keymaps/Caps2Ctrl.map
    echo "keycode 58 = Control #Makes Caps as Ctrl" >> /usr/share/keymaps/Caps2Ctrl.map
    loadkeys -v /usr/share/keymaps/Caps2Ctrl.map
fi

## To revert: loadkeys -d





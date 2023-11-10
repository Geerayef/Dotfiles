#!/usr/bin/env bash

# ~  Dependencies

# bluetoothctl (package: bluez)
# tofi

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Global variables

DIVIDER="--------------------"
BACK="Back"

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Functions

# - Power
status_powered() {
    if bluetoothctl show | grep -q "Powered: yes"; then
        return 0
    else
        return 1
    fi
}

toggle_power() {
    if status_powered; then
        bluetoothctl power off
        show_menu
    else
        if rfkill list bluetooth | grep -q 'blocked: yes'; then
            rfkill unblock bluetooth && sleep 3
        fi
        bluetoothctl power on
        show_menu
    fi
}

# - Scan
# Echoes
status_scaning() {
    if bluetoothctl show | grep -q "Discovering: yes"; then
        echo "Scan: on"
        return 0
    else
        echo "Scan: off"
        return 1
    fi
}

toggle_scan() {
    if status_scaning; then
        kill $(pgrep -f "bluetoothctl scan on")
        bluetoothctl scan off
        show_menu
    else
        bluetoothctl scan on &
        echo "Scanning..."
        sleep 5
        show_menu
    fi
}

# - Pairable
# Echoes
status_pairable() {
    if bluetoothctl show | grep -q "Pairable: yes"; then
        echo "Pairable: on"
        return 0
    else
        echo "Pairable: off"
        return 1
    fi
}

toggle_pairable() {
    if status_pairable; then
        bluetoothctl pairable off
        show_menu
    else
        bluetoothctl pairable on
        show_menu
    fi
}

# - Discoverable
# Echoes
status_discoverable() {
    if bluetoothctl show | grep -q "Discoverable: yes"; then
        echo "Discoverable: on"
        return 0
    else
        echo "Discoverable: off"
        return 1
    fi
}

toggle_discoverable() {
    if status_discoverable; then
        bluetoothctl discoverable off
        show_menu
    else
        bluetoothctl discoverable on
        show_menu
    fi
}

# - Connect
status_connected() {
    device_info=$(bluetoothctl info "$1")
    if echo "$device_info" | grep -q "Connected: yes"; then
        return 0
    else
        return 1
    fi
}

toggle_connect() {
    if status_connected "$1"; then
        bluetoothctl disconnect "$1"
        device_menu "$device"
    else
        bluetoothctl connect "$1"
        device_menu "$device"
    fi
}

# - Pair
# Echoes
status_paired() {
    device_info=$(bluetoothctl info "$1")
    if echo "$device_info" | grep -q "Paired: yes"; then
        echo "Paired: yes"
        return 0
    else
        echo "Paired: no"
        return 1
    fi
}

toggle_pair() {
    if status_paired "$1"; then
        bluetoothctl remove "$1"
        device_menu "$device"
    else
        bluetoothctl pair "$1"
        device_menu "$device"
    fi
}

# - Trusted
# Echoes
device_trusted() {
    device_info=$(bluetoothctl info "$1")
    if echo "$device_info" | grep -q "Trusted: yes"; then
        echo "Trusted: yes"
        return 0
    else
        echo "Trusted: no"
        return 1
    fi
}

toggle_trust() {
    if device_trusted "$1"; then
        bluetoothctl untrust "$1"
        device_menu "$device"
    else
        bluetoothctl trust "$1"
        device_menu "$device"
    fi
}

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Main

print_status() {
    if status_powered; then
        printf ''

        paired_devices_cmd="devices Paired"
        mapfile -t paired_devices < <(bluetoothctl $paired_devices_cmd | grep Device | cut -d ' ' -f 2)

        counter=0
        for device in "${paired_devices[@]}"; do
            if status_connected "$device"; then
                device_alias=$(bluetoothctl info "$device" | grep "Alias" | cut -d ' ' -f 2-)

                if [ $counter -gt 0 ]; then
                    printf ", %s" "$device_alias"
                else
                    printf " %s" "$device_alias"
                fi

                ((counter++))
            fi
        done
        printf "\n"
    else
        echo ""
    fi
}

device_menu() {
    device=$1

    device_name=$(echo "$device" | cut -d ' ' -f 3-)
    mac=$(echo "$device" | cut -d ' ' -f 2)

    if status_connected "$mac"; then
        connected="Connected: yes"
    else
        connected="Connected: no"
    fi
    paired=$(status_paired "$mac")
    trusted=$(device_trusted "$mac")
    options="Connected:\n$connected\nPaired:\n$paired\nTrusted:\n$trusted\n$DIVIDER\n$BACK\nExit"

    chosen="$(echo -e "$options" | $tofi_cmd "$device_name")"

    case "$chosen" in
        "" | "$DIVIDER")
            echo "No option chosen."
            ;;
        "$connected")
            toggle_connect "$mac"
            ;;
        "$paired")
            toggle_pair "$mac"
            ;;
        "$trusted")
            toggle_trust "$mac"
            ;;
        "$BACK")
            show_menu
            ;;
    esac
}

show_menu() {
    if status_powered; then
        power="Power: on"
        devices=$(bluetoothctl devices | grep Device | cut -d ' ' -f 3-)

        scan=$(status_scaning)
        pairable=$(status_pairable)
        discoverable=$(status_discoverable)

        options="Devices:\n$devices\n$DIVIDER\n$power\n$scan\n$pairable\n$discoverable\nExit"
    else
        power="Power: off"
        options="$power\nExit"
    fi

    chosen="$(echo -e "$options" | $tofi_cmd)"

    case "$chosen" in
        "" | "$DIVIDER")
            echo "No option chosen."
            ;;
        "$power")
            toggle_power
            ;;
        "$scan")
            toggle_scan
            ;;
        "$discoverable")
            toggle_discoverable
            ;;
        "$pairable")
            toggle_pairable
            ;;
        *)
            device=$(bluetoothctl devices | grep "$chosen")
            if [[ $device ]]; then device_menu "$device"; fi
            ;;
    esac
}

tofi_cmd="tofi $* --config $XDG_CONFIG_HOME/tofi/bluetooth"

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Entry

case "$1" in
    --status)
        print_status
        ;;
    *)
        show_menu
        ;;
esac

#!/usr/bin/env bash

# ~  Dependencies

# nmcli
# tofi

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Global variables

DIVIDER="--------------------"
BACK="Back"

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Functions

# - Enabled
status_enabled() {
  if nmcli radio wifi | grep -q "enabled"; then
    return 0
  else
    return 1
  fi
}

toggle_enable() {
  if status_enabled; then
    nmcli radio wifi off
    main_menu
  else
    if rfkill list wifi | grep -q "blocked: yes"; then
      rfkill unblock wifi && sleep 3
    fi
    nmcli radio wifi on
    main_menu
  fi
}

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Main

main_menu() {
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

tofi_cmd="tofi $* --config $XDG_CONFIG_HOME/tofi/interactive --placeholder-text=WiFi"

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Entry

case "$1" in
--status)
  print_status
  ;;
*)
  main_menu
  ;;
esac

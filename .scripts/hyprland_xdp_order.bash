#!/usr/bin/env bash

sleep 2
killall -e xdg-desktop-portal-hyprland
killall -e xdg-desktop-portal-gtk
killall xdg-desktop-portal
/usr/lib/xdg-desktop-portal-hyprland &
sleep 5

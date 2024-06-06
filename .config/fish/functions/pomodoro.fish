function pomodoro -a time unit
    tclock -c green timer -M -d $time$unit -e 'notify-send -u low -t 10000 "Reminder" "Take a break and stretch!"'
end

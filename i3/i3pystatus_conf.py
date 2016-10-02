from i3pystatus import Status
import sh

status = Status()

# Date and Time
status.register("clock", format="%H:%M - %a %d %b")

status.register("pulseaudio", 
    format=" {volume}%",
    on_middleclick='switch_mute',
    on_rightclick=lambda: sh.pavucontrol_qt(),
    on_leftclick=None
    )

# Show disk usage and launch usage analyzer on click
status.register("disk",
    path="/",
    format="  {used:.0f} / {total:.0f} GB ",
    on_leftclick=lambda: sh.filelight('/')
)

#status.register("mem_bar")
status.register("mem", 
    divisor=1024**3,
    format="{percent_used_mem}%")

status.register("xkblayout", layouts=['gb', 'gr'])

status.register("now_playing",
    format=" {artist} - {title} \[ {album} \]")

status.run()

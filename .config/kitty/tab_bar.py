import datetime
from kitty.fast_data_types import Screen, get_options
from kitty.tab_bar import (
    DrawData,
    ExtraData,
    TabBarData,
    draw_title,
    as_rgb,
    draw_tab_with_powerline,
)
from kitty.utils import color_as_int

opts = get_options()
ICON: str = " LEI "
ICON_LENGTH: int = len(ICON)
ICON_FG: int = as_rgb(color_as_int(opts.color16))
ICON_BG: int = as_rgb(color_as_int(opts.color8))
CLOCK_FG = 0
CLOCK_BG = as_rgb(color_as_int(opts.color15))


def _draw_left_status(
    draw_data: DrawData,
    screen: Screen,
    tab: TabBarData,
    before: int,
    max_title_length: int,
    index: int,
    is_last: bool,
    extra_data: ExtraData,
    use_kitty_render_function: bool = False,
) -> int:
    if use_kitty_render_function:
        # Use `kitty` function render tab
        end = draw_tab_with_powerline(
            draw_data, screen, tab, before, max_title_length, index, is_last, extra_data
        )
        return end
    if draw_data.leading_spaces:
        screen.draw(" " * draw_data.leading_spaces)
    # draw tab title
    draw_title(draw_data, screen, tab, index)
    trailing_spaces = min(max_title_length - 1, draw_data.trailing_spaces)
    max_title_length -= trailing_spaces
    extra = screen.cursor.x - before - max_title_length
    if extra > 0:
        screen.cursor.x -= extra + 1
        # Don't change `ICON`
        screen.cursor.x = max(screen.cursor.x, ICON_LENGTH)
        screen.draw("…")
    if trailing_spaces:
        screen.draw(" " * trailing_spaces)
    screen.cursor.bold = screen.cursor.italic = False
    screen.cursor.fg = 0
    if not is_last:
        screen.cursor.bg = as_rgb(color_as_int(draw_data.inactive_bg))
        screen.draw(draw_data.sep)
    screen.cursor.bg = 0
    return screen.cursor.x


def _draw_right_status(screen: Screen, is_last: bool) -> int:
    if not is_last:
        return screen.cursor.x
    cells = [
        (CLOCK_FG, CLOCK_BG, datetime.datetime.now().strftime(" %H:%M ")),
    ]
    right_status_length = 0
    for _, _, cell in cells:
        right_status_length += len(cell)
    draw_spaces = screen.columns - screen.cursor.x - right_status_length
    if draw_spaces > 0:
        screen.draw(" " * draw_spaces)
    for fg, bg, cell in cells:
        screen.cursor.fg = fg
        screen.cursor.bg = bg
        screen.draw(cell)
    screen.cursor.fg = 0
    screen.cursor.bg = 0
    screen.cursor.x = max(screen.cursor.x, screen.columns - right_status_length)
    return screen.cursor.x


def draw_tab(
    draw_data: DrawData,
    screen: Screen,
    tab: TabBarData,
    before: int,
    max_title_length: int,
    index: int,
    is_last: bool,
    extra_data: ExtraData,
) -> int:
    BG_TRANSPARENT = 0
    BG_TAB = screen.cursor.bg
    if index == 1:
        screen.cursor.bg = BG_TRANSPARENT
        screen.draw(" ")
    screen.cursor.bg = BG_TAB
    screen.cursor.fg = BG_TRANSPARENT
    screen.cursor.bg = BG_TAB
    draw_title(draw_data, screen, tab, index)
    screen.cursor.bg = BG_TRANSPARENT
    screen.cursor.fg = BG_TAB
    if not is_last:
        screen.draw("")
    screen.cursor.bg = BG_TAB

    return screen.cursor.x
